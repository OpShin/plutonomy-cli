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
