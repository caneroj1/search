module SearchTests.Utils where

import Control.Search

type Strategy s a = Searchable s a -> Maybe (Path s a)

type Label = String
data Test st a = Test Label (Searchable st a) (Maybe (Path st a)) (Strategy st a)

runTest :: (Ord st, Ord a, Show st, Show a) => Test st a -> IO Bool
runTest (Test _ p a s) = do
  let sln = s p
  if sln == a
    then putStrLn "Pass!" >> return True
    else logAnswer sln a
  where
    logAnswer Nothing   (Just ex) = do
      putStrLn   "Failure! No solution found!"
      putStrLn $ "Expected: " ++ show ex
      return False
    logAnswer Nothing    Nothing  = putStrLn "Pass!" >> return True
    logAnswer (Just got) Nothing  = do
      putStrLn   "Failure! Got an answer when none was expected"
      putStrLn $ "Got: " ++ show got
      return False
    logAnswer (Just got) (Just ex)
      | got == ex = putStrLn "Pass!" >> return True
      | otherwise = do
        putStrLn $ "Expected: " ++ show ex
        putStrLn $ "Got:      " ++ show got
        return False

instance Show (Test st a) where
  show (Test lbl _ _ _) = "[" ++ lbl ++ "]"

data Suite st a = Suite Int Label [Test st a]

mkSuite :: String -> Suite st a
mkSuite l = Suite 0 l []

addToSuite :: Test st a -> Suite st a -> Suite st a
addToSuite t (Suite tc l ts) = Suite (tc + 1) l (t:ts)

runSuite :: (Ord st, Ord a, Show st, Show a) => Suite st a -> IO ()
runSuite (Suite _ l ts) = do
  putStrLn $ "\nSearch Test Suite: " ++ l
  putStrLn   "-----------------"
  run 0 0 ts
  where
    run t p []     = putStrLn $ show p ++ "/" ++ show t ++ " passed.\n"
    run i p (t:ts) = do
      putStrLn $ "Test " ++ show (i+1) ++ ": " ++ show t
      runTest t >>= \b -> run (i+1) (incIfPass b p) ts
      where
        incIfPass b p = if b then p+1 else p
