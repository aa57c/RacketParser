
# Lexer and Parser for Simple Math Language

This project implements a lexer and parser for simple mathematical language. The lexer tokenizes the input source code, and the parser processes these tokens according to predefined grammar rules. The project is designed to handle a language with basic constructs such as variables, arithmetic operations, and simple statements (e.g., assignment, input/output).

## Features
- **Lexer**: Tokenizes the input into meaningful components like keywords, operators, identifiers, integers, and parentheses.
- **Parser**: Parses the tokenized input based on a set of grammar rules to ensure that the syntax is correct.
- **Error Handling**: Provides detailed error messages for unexpected tokens or syntax errors.
- **Multiple File Processing**: Processes multiple input files and reports errors or success for each file.
  
## Installation

1. Install **Racket** if it is not already installed. You can download Racket from [https://racket-lang.org/](https://racket-lang.org/).

2. Clone the repository or download the project files.

```bash
git clone https://github.com/aa57c/RacketParser.git
```

3. Install any dependencies if needed (the project uses `parser-tools` and `2htdp/batch-io`, which come with Racket).

## Usage

1. Prepare your input files in the same directory as your project or specify the correct file paths in the script.
2. Run the project in Racket by executing the main script.

```bash
racket parser.rkt
```

The program will process a list of files, tokenize their contents, and attempt to parse the tokenized input. If any syntax errors are found, they will be displayed in the output.

### Input Format
The input files should follow the syntax of the simple language defined by the lexer and parser. Some of the tokens that the lexer recognizes include:

- **Keywords**: `write`, `read`
- **Operators**: `+`, `-`, `*`, `/`, `:=` (assignment)
- **Identifiers**: Any sequence of letters (`a-z`, `A-Z`)
- **Integers**: Sequences of digits (`0-9`)
- **Parentheses**: `(` and `)`

Example input file (`input01.txt`):

```
read x
x := 5
write x
```

### Example Output

```
Processing file: input01.txt
Token list: (READ ID x ASS_OP INT 5 END)
Parsing completed successfully
```

If there are syntax errors, the output will display an error message with details about the unexpected token and the part of the input causing the error.

## File Structure

```
.
├── parser.bak # Backup file for the lexer and parser
├── parser.rkt  # Lexer and parser definitions
├── input01.txt     # Sample input file 1
├── input02.txt     # Sample input file 2
├── input03.txt     # Sample input file 3
├── input04.txt     # Sample input file 4
├── input05.txt     # Sample input file 5
└── README.md       # Project documentation
└── Sources.txt       # Sources used to develop lexer and parser

```

- `parser.rkt`: The main script that defines and loads the lexer and parser, processes input files, and handles errors.
- `input01.txt`, `input02.txt`, etc: Example input files that the lexer and parser process.
- `README.md`: This file, containing the project documentation.

## Sources

There is a text file that contains sources used to develop the lexer and parser. Further detail is included in those sources
