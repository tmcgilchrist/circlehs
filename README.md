[![CircleCI](https://circleci.com/gh/tmcgilchrist/circlehs.svg?style=svg)](https://circleci.com/gh/tmcgilchrist/circlehs) [![Hackage](https://img.shields.io/badge/hackage-v0.0.3-blue.svg)](http://hackage.haskell.org/package/circlehs)



# CircleHs

The [CircleCI](https://circleci.com/) REST API implementation in Haskell. For more info please see [official API reference](https://circleci.com/docs/api/).

Work in progress.

## Hello, CircleCI!

Let's obtain information about the user:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Network.CircleCI

main :: IO ()
main = runCircleCI getUserInfo
                   (AccountAPIToken "e64c67410f96ba2whatever")
    >>= \case
        Left problem -> print problem
        Right info   -> print info
```

