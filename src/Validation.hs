{-# LANGUAGE ApplicativeDo #-}

module Validation
    ( validationTest
    ) where

data Foo = Foo { x :: Int, ys :: [Bool] }
  deriving (Show)

-- You've seen that `Either err a` represents computations that can fail with an error of type `err`.  Accordingly, the
-- expression `pure Foo <*> reqPos -1 <*> reqAny [True, False]` will fail on the first argument.  This can be less than
-- ideal when validating data, as you'd rather see all the errors at once, rather than terminating on the first error.

-- Exercise 1: Implement a data type `Validation err a` that represents either a success of type `a` or a list of errors
-- of type`err`.

data Validation err a = Errors [ err ] | Valid a
  deriving (Eq, Show)

-- Exercise 2: Implement `reqPos` and `reqAny` in terms of your validation type.  The first should fail if the given
-- integer is non-positive.  The second should fail if all elements of the given list are `False`.

reqPos:: Int -> Validation String Int
reqPos value = if value >= 0 then Valid value else Errors [ show value <> " must be positive" ]

reqAny:: [ Bool ] -> Validation String [ Bool ]
reqAny value = if (or value) then Valid value else Errors [ show value <> " must have a true value" ]

-- Exercise 3: Implement an `Applicative` instance for your validation type that accumulates errors as described above.

instance Functor (Validation err) where
  fmap f (Valid a) = Valid (f a)
  fmap _ (Errors e) = Errors e

instance Applicative (Validation err) where
  pure = Valid

  (<*>) (Valid f) (Valid value) = Valid (f value)
  (<*>) (Valid _) (Errors e) = Errors e
  (<*>) (Errors e) (Valid _) = Errors e
  (<*>) (Errors e1) (Errors e2) = Errors (e1 ++ e2)

makeValidFoo:: Int -> [ Bool ] -> Validation String Foo
makeValidFoo x ys = pure Foo <*> reqPos x <*> reqAny ys

--Typing of steps of the above expression
--
-- (Validation String (Int -> [ Bool ] -> Foo))
--                                               (Validation String ([ Bool ] -> Foo))
--                                                                                      (Validation String  Foo)

-- Written using ApplicativeDo:
-- makeValidFooAD x ys = do
--   x' <- reqPos x
--   ys' <- reqAny ys
--   pure $ Foo x' ys'

-- Written to accept a single tuple argument
--
-- makeValidFooFromTuple:: (Int, [ Bool ]) -> Validation String Foo
-- makeValidFooFromTuple (x, ys) = pure Foo <*> reqPos x <*> reqAny ys


-- Written using uncurry
-- makeValidFooFromTuple = uncurry makeValidFoo

-- Exercise 4: Verify that your `Applicative` instance satisfies the applicative laws.

validationTest :: IO ()
validationTest = do
  putStrLn $ "makeValidFoo: " <> show (makeValidFoo 1 [True])
  putStrLn $ "makeValidFoo: " <> show (makeValidFoo (-1) [True, False])
  putStrLn $ "makeValidFoo: " <> show (makeValidFoo (-10) [True, False])
  putStrLn $ "makeValidFoo: " <> show (makeValidFoo (-10) [False, False])


  putStrLn $ "reqPos 2: " <> show (reqPos 2)
  putStrLn $ "reqPos -2: " <> show (reqPos (-2))

  putStrLn $ "reqAny []: " <> show (reqAny [])
  putStrLn $ "reqAny [False]: " <> show (reqAny [False])
  putStrLn $ "reqAny [True]: " <> show (reqAny [True])
  putStrLn $ "reqAny [False, False]: " <> show (reqAny [False, False])
  putStrLn $ "reqAny [True, False]: " <> show (reqAny [True, False])
  putStrLn $ "reqAny [False, True]: " <> show (reqAny [False, True])
  putStrLn $ "reqAny [True, True]: " <> show (reqAny [True, True])

  putStrLn "Validation!"
