import Control.Monad.Cont

p :: String -> Cont r ()
p x = ContT $ \_ -> putStrLn x

f :: Cont r ()
f = do
  p "a"
  p "b"
  p "c"
  p "d"

main :: IO ()
main = runContT f return
