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

First, generate or obtain the script as hex encoded single-wrapped cbor of the flat encoding.
This is usually among the output of the build commands of [uplc](https://github.com/OpShin/uplc), [opshin](https://github.com/OpShin/opshin) or [aiken](https://github.com/aiken-lang/aiken).

```bash
uplc build foo.uplc
```

Then optimize the code using `plutonomy-cli`.

```
plutonomy-cli --default foo/script.cbor > foo/optimized.cbor
```

### Alternatives

While plutonomy is an extremely powerful tool, it is unfortunately not being actively maintained and may not be an adequate choice for new and upcoming projects.
Alternatives include

- the official [plutus](https://github.com/input-output-hk/plutus/releases) `uplc` binary. Run ` uplc-x86_64-linux-ghc928 optimize foo.uplc`.
- _WIP_: [aiken](https://github.com/aiken-lang/aiken). Run `aiken shrink foo/script.cbor --cbor --hex`.
- _WIP_: [uplc](https://github.com/OpShin/uplc). Run `uplc optimize foo.uplc`.

