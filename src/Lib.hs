{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Text ( splitOn, append, Text )
-- Imports that will be needed later:
import qualified Data.Text.IO as T
import Data.Map as Map ( fromList, lookup, Map )
import Control.Applicative ()

data LoginError =
    InvalidEmail | NoSuchUser | WrongPassword
    deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email =
    case splitOn "@" email of
        [_, domain] ->
            Right domain
        _ ->
            Left InvalidEmail

printResult'' :: Either LoginError Text -> IO ()
printResult'' domain =
    case domain of
        Right text ->
            T.putStrLn (append "Domain: " text)
        Left InvalidEmail ->
            T.putStrLn "ERROR: Invalid domain"

printResult' :: Either LoginError Text -> IO ()
printResult' =
    T.putStrLn . either
        (const "ERROR: Invalid domain")
        (append "Domain: ")

getToken' :: IO (Either LoginError Text)
getToken' = do
    T.putStrLn "Enter email address:"
    fmap getDomain T.getLine
    
users :: Map Text Text
users = Map.fromList
    [ ("example.com", "qwerty123")
    , ("localhost", "password")
    ]

userLogin' :: IO (Either LoginError Text)
userLogin' = do
    token <- getToken'
    case token of
        Right domain ->
            case Map.lookup domain users of
                Just userpw -> do
                    T.putStrLn "Enter password:"
                    password <- T.getLine
                    if userpw == password then
                        return token
                    else
                        return (Left WrongPassword)
                Nothing ->
                    return (Left NoSuchUser)
        left -> return left

newtype EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}

instance Functor (EitherIO e) where
    fmap :: (a -> b) -> EitherIO e a -> EitherIO e b
    fmap f ex = wrapped
        where
            unwrapped = runEitherIO ex
            fmapped   = fmap (fmap f) unwrapped
            wrapped   = EitherIO fmapped

instance Applicative (EitherIO e) where
  pure :: a -> EitherIO e a
  pure = EitherIO . return . Right
  (<*>) :: EitherIO e (a -> b) -> EitherIO e a -> EitherIO e b
  EitherIO f <*> EitherIO v = EitherIO $ do
    mf <- f
    case mf of
      Left e -> return $ Left e
      Right k -> do
        mv <- v
        case mv of
          Left e -> return (Left e)
          Right x -> return (Right $ k x)

instance Monad (EitherIO e) where
  (>>=) :: EitherIO e a -> (a -> EitherIO e b) -> EitherIO e b
  x >>= f = EitherIO $ do
    runEitherIO x >>= either (return . Left) (runEitherIO . f)

getToken :: EitherIO LoginError Text
getToken = do
    EitherIO $ fmap Right (T.putStrLn "Enter email address: ")
    input <- EitherIO (fmap Right T.getLine)
    EitherIO (return (getDomain input))

liftEither :: Either e a -> EitherIO e a
liftEither x = EitherIO $ return x

liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO $ fmap Right x

getTokenRedone :: EitherIO LoginError Text
getTokenRedone = do
    liftIO $ T.putStrLn "Enter email address: "
    input <- liftIO T.getLine
    liftEither $ getDomain input

userLogin :: EitherIO LoginError Text
userLogin = do
    token <- getTokenRedone
    userpw <- maybe (throwE NoSuchUser)
        return (Map.lookup token users)
    password <- liftIO (
        T.putStrLn "Enter your password:" >>
        T.getLine)
    if userpw == password then
        return token
    else
        throwE WrongPassword

throwE :: e -> EitherIO e a
throwE x = liftEither (Left x)

newtype ExceptIO e a = ExceptIO {
    runExceptIO :: IO (Either e a)
}

catchE :: ExceptIO e a -> (e -> ExceptIO e a)
       -> ExceptIO e a
catchE throwing handler =
    ExceptIO $ do
        result <- runExceptIO throwing
        case result of
            Left failure ->
                runExceptIO (handler failure)
            success ->
                return success
