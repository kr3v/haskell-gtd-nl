1. make the VS Code extension start the server if it is in the PATH variable
2. add an integration test for the server:
  1. create a test repo
  2. check if the server can provide definitions for certain identifiers
3. implement a support for new export or import cases - in other words, improve the Haskell support
  - additional things here:
    - Prelude support (an implicit import of certain functions)
  - module re-export
