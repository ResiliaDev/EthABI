
# EthAbi

This is an Elm implementation of the Ethereum ABI Encoding/Decoding procedures.

It strives to be:

- Feature complete
- Intuitive to use.
- Type-safe.
- Not care about anything else of Ethereum, or how we might talk with a Blockchain. This library handles the ABI-part only, and could be integrated in any library that needs this

## Progress

- Types (for types that do not map 1:1 to Elm types)
  - [x] int256
  - [x] runtime-selectable int size encoder.
  - [ ] wrappers for all available int types.
  - [x] uint256
  - [x] runtime-selectable uint size encoder.
  - [ ] wrappers for all available uint types.
  - [x] bytes32
  - [x] `static_bytes` where you pass in the length.
  - [ ] wrappers for all available static bytes types.
  - [x] bytes
  - [x] Hexstring, the type of encoded hexadecimal strings that represent bytes (which are the output of the Encoder and input of the Decoder)


- Encoding
  - Main encoding function `encode`.
  - [x] int256
  - [x] uint256
  - [x] bool
  - [x] bytes32
  - [x] bytes
  - [x] string
  - [x] Tuples
  - [x] Arrays of static size (that properly switch between static and dynamic locations based on element type!)
  - [x] Arrays with dynamic size


- Decoding
  - [x] Main decoding function `decodeHexstring`
  - [x] `map`
  - [x] `map2`
  - [x] `apply`
  - [x] `andThen`
  - [x] `succeed`
  - [x] `fail`
  - [x] int256
  - [x] uint256
  - [x] bool
  - [ ] bytes32
  - [x] `static_bytes` where you pass in the length.
  - [ ] Arrays of static size (that properly switch between static and dynamic body locations based on element type!) -- WIP!!
  - [x] Arrays of dynamic size
  - [x] Tuples, by using `map`, `map2` and `apply`.


- Other
  - [ ] Possibility to create function signature from given encoder types and function name.

- [ ] Tests
 
 
