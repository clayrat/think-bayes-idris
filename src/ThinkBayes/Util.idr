module ThinkBayes.Util

-- this is merged post 1.0 as `Control.Pipeline`@contrib
%access export

infixl 9 |>
(|>) : a -> (a -> b) -> b
a |> f = f a

-- TODO contribute to prelude

foldM : (Foldable f, Monad m) => (a -> b -> m a) -> a -> f b -> m a
foldM f a0 = foldl (\ma, b => ma >>= flip f b) (pure a0)
