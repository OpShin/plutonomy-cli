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

For your program to be parsed correctly, you need to ensure that each variable name is unique. This can be automated via [aiken](https://github.com/aiken-lang/aiken) and [uplc](https://githubg.com/ImperatorLang/uplc).

```bash
uplc dump foo.uplc --dialect aiken > foo.aiken.uplc
aiken uplc flat foo.aiken.uplc -o /dev/stdout | aiken uplc unflat /dev/stdin -o foo.unique.uplc
```

Further, you need to ensure that your code adheres to the Plutus dialect of UPLC.
This can be achieved using [uplc](https://githubg.com/ImperatorLang/uplc).

```bash
uplc dump foo.unique.uplc --dialect plutus > foo.plutus.uplc
```

Finally, optimize the code using `plutonomy-cli`.

```
plutonomy-cli foo.plutus.uplc > foo.optimized.uplc
```

