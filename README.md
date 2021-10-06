# LλMDA from scratch

## Before you begin

Make sure you have the following installed:

1. [Haskell](https://www.haskell.org/ghcup/) with `stack`.

    ```sh
    # For Linux, MacOS, FreeBSD or WSL2
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
    ```

## Running tests

```sh
stack test
```

> ℹ️ It might take several minutes to run the first time since it will download, compile, and install everything.
