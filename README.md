## 1. Development

The preferred way to develop `mapv` and `fold` is using [Nix](https://nixos.org)
and [direnv](https://direnv.net).

```fish
direnv allow
```

Once the development shell is active, the `fold` binary can either be built
using `dune` or `Nix`.

```fish
# build and run
dune build --profile release
./_build/install/default/bin/fold
# or
nix build .#fold
./result/bin/fold

# build and run in one go
dune exec --profile release fold
# or
nix run .#fold
```

The `mapv` library is available as a package that can be used as follow:

```nix
# Add mapv to inputs
inputs.mapv.url = "git+https://codeberg.org/debarchito/mapv";

# Using the overlay
overlays = [
  inputs.mapv.overlays.default
];
# then
buildInputs = [
  pkgs.ocamlPackages.mapv
]

# or, inline it directly
buildInputs = [
  inputs.mapv.packages.${system}.mapv
                       # ^ don't forget to make sure "system" is defined! 
];
```

> NOTE: We do not use [opam](https://opam.ocaml.org). Instead, add the required
> dependencies to [dune-project](/dune-project) and invoke `dune build`. This
> will fail the first time but prepare [mapv.opam](/mapv.opam) and/or
> [fold.opam](/fold.opam) accordingly. Now, to install the dependencies, invoke
> `direnv reload` and they'll be made available for all subsequent `dune build`
> and `nix build`/`nix run` invocations.

## 2. Formatting

```fish
nix fmt
# or
nix run .#fmt
# or
fd -e ml -e mli --exclude _build | xargs ocamlformat --inplace
```

## 3. Licensing

`mapv` is licensed under [GNU LGPLv3](/LICENSE-LGPLv3) only while `fold` is
licensed under [GNU GPLv3](/LICENSE-GPLv3) only.
