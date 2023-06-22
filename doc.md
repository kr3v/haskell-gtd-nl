1. extension:
  - should not kill the server if there are multiple users of it
  - it should re-use the server if it is active
  - consider per-extension server instance
  
2. implement a support for new export or import cases - in other words, improve the Haskell support
  - additional things here:
    - Prelude support (an implicit import of certain functions)
  - module re-export
