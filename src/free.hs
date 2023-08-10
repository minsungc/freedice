data Toy b next =
    Output b next
  | Bell next
  | Done

program1 :: Toy Char (Toy a next)
program1 = Output 'A' Done

-- newtype Fix f = Fix (f (Fix f))

-- program1' :: Fix (Toy Char)
-- program1' =  Fix (Output 'A' (Fix Done))

-- program2' :: Fix (Toy Char)
-- program2' = Fix (Bell (Fix (Output 'A' (Fix Done))))

data FixE f e = Fix (f (FixE f e)) | Throw e

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f = Fix (fmap (`catch` f) x)
catch (Throw e) f = f e

instance Functor (Toy b) where
    fmap :: (a -> b2) -> Toy b1 a -> Toy b1 b2
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap _  Done           = Done

data IncompleteException = IncompleteException

subroutine' :: FixE (Toy Char) IncompleteException
subroutine' = Fix (Output 'A' (Throw IncompleteException))

program' :: FixE (Toy Char) e2
program' = subroutine' `catch` (\_ -> Fix (Bell (Fix Done)))

data Free f r = Free (f (Free f r)) | Pure r

instance (Functor f) => Functor (Free f) where
    fmap :: (a -> b) -> Free f a -> Free f b
    fmap f (Pure r) = Pure (f r)
    fmap f (Free fa) = Free (fmap (fmap f) fa)

instance (Functor f) => Applicative (Free f) where
  pure :: a -> Free f a
  pure = Pure
  (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  Pure fn <*> Pure a = Pure $ fn a
  Pure fn <*> Free a = Free $ fmap (fmap fn) a
  Free fn <*> b =
    -- Free $ (<*> b) <$> fn
    let x = (<*> b) in
    let y = fmap x fn in
    Free y

instance (Functor f) => Monad (Free f) where
    return :: a -> Free f a
    return = pure
    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    (Pure r) >>= g = g r
    (Free a) >>= g =
        let x = (>>= g) in
        let y = fmap x in
        let z = y a in
        Free z

liftF :: (Functor f) => f r -> Free f r
liftF command = 
    let x = fmap Pure in
    let y = x command in 
    Free y

output :: b -> Free (Toy b) ()
output x = liftF (Output x ())
bell :: Free (Toy b) ()
bell     = liftF (Bell     ())
done :: Free (Toy b) r
done     = liftF  Done

subroutine :: Free (Toy Char) ()
subroutine = output 'A'

program :: Free (Toy Char) ()
program = do
    subroutine
    bell

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) =
    "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x)) =
    "bell\n" ++ showProgram x
showProgram (Free Done) =
    "done\n"
showProgram (Pure r) =
    "return " ++ show r ++ "\n"