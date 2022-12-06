# Adder
Adder is a small but usable subset of the Python language. It is named for the Blackadder comedy series, much as the Python language is named for Monty Python.

## Configuration Tips

1. Make sure that you have the appropriate version of GHC installed globally via [GHCup](https://www.haskell.org/ghcup/). As of this update, that is GHC 9.0.2, which is the latest available version that is still compatible with the latest HLS (Haskell Language Server) version that is also available in GHCup.
    ```
    > ghcup install ghc 9.0.2
    ```
2. Make sure that your Haskell Stack configuration is set to use the global GHC.
    ```
    > stack config set system-ghc --global true
    ```
3. Avoid running `stack init` or doing anything else that will modify the `stack.yaml` configuration file.

## Build Instructions

1. Build the project in the Terminal/Powershell with the command `stack build`.
2. Run the desired executable with the appropriate `stack run ...` command.
    a. To see a representation of the abtract syntax for an Adder file: `stack run parseradder`
    b. To run a source file containing an Adder program: `stack run runadder`
    c. To launch an interactive REPL for Adder: `stack run adder`
