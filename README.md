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

```
plutonomy-cli foo.uplc > foo.optimized.uplc
```

> For your program to be parsed correctly, you need to ensure that each variable name is unique. This can be automated via [aiken](https://github.com/aiken-lang/aiken) with `aiken uplc flat foo.uplc -o /dev/stdout | aiken uplc unflat /dev/stdin -o foo_unique.uplc`
