module ThinkBayes.Util

-- this is merged post 1.0 as `Control.Pipeline`@contrib
%access export

infixl 9 |>
(|>) : a -> (a -> b) -> b
a |> f = f a

-- https://github.com/idris-lang/Idris-dev/pull/3877
||| Similar to 'foldl', but uses a function wrapping its result in a 'Monad'.
||| Consequently, the final value is wrapped in the same 'Monad'.
foldlM : (Foldable t, Monad m) => (funcM: a -> b -> m a) -> (init: a) -> (input: t b) -> m a
foldlM fm a0 = foldl (\ma,b => ma >>= flip fm b) (pure a0)

-- okay.jpg
pow : Double -> Double -> Double
pow x y = exp (y * log x) 