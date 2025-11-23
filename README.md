A full Morse code encoder/decoder implemented in Haskell, demonstrating functional programming techniques, recursion, pattern matching, data structures, and parsing.

This project includes:

### 🔹 Morse Code Encoding
- Converting characters to Morse code using a lookup table
- Encoding entire words and sentences
- Handling spacing rules (short gaps between letters, medium gaps between words)
- Working generically with any encoding table

### 🔹 Morse Code Decoding
- Decoding Morse code back into text using a lookup table
- Splitting Morse sequences based on timing gaps
- Ensuring decodeText t (encodeText t s) = s

### 🔹 Tree-Based Decoding
- Building a binary Morse tree where left = dit and right = dah
- Traversing the tree to decode Morse without lookup tables
- Converting between tables and tree structures (ramify + tabulate)

### 🔹 Well-Bracketed String Parser
- A recursive parser for validating and reconstructing nested bracket structures
- Supports `()` and `{}` with arbitrary nesting
- Returns `Nothing` for invalid bracket strings
- Demonstrates recursive descent parsing in Haskell

This was originally a functional programming assignment, now turned into a clean, structured Haskell project showcasing encoding/decoding, recursion, algebraic data types, and tree manipulation.
