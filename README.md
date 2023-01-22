# Plutonomy-cli

A simple command-line wrapper for [Plutonomy](https://github.com/well-typed/plutonomy) -- an optimizer for UPLC.

See disclaimer notice on Plutonomy.

## Getting Started

### Installation instructions

```
cabal install --overwrite-policy=always --install-method=copy plutonomy-cli
```

### Usage

```
plutonomy-cli --help
```

#### Example

For your program to be parsed correctly, you need to ensure that each variable name is unique.
Further, you need to ensure that your code adheres to the Plutus dialect of UPLC.
This can be achieved using [uplc](https://githubg.com/ImperatorLang/uplc).

```bash
uplc dump foo.uplc --dialect plutus --unique-varnames > foo.plutus.uplc
```

Finally, optimize the code using `plutonomy-cli`.

```
plutonomy-cli foo.plutus.uplc > foo.optimized.uplc
```

