{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Text
-- Imports that will be needed later:
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative

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