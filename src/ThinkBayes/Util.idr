module ThinkBayes.Util

-- this is merged post 1.0 as `Control.Pipeline`@contrib
%access export

infixl 9 |>
(|>) : a -> (a -> b) -> b
a |> f = f a