## 1. Development

The preferred way to develop `map` and `fold` is using [Nix](https://nixos.org)
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

The `map` library is available as a Nix package aliased `map-vm` to avoid name
collisions with the built-in `map` function. This alias is Nix-specific; when
referencing it in your `dune-project`, you should use the public name `map`.

```nix
# Add map to inputs
inputs.map-vm.url = "git+https://codeberg.org/debarchito/map";

# Using the overlay
overlays = [
  inputs.map-vm.overlays.default
];
# then
buildInputs = [
  pkgs.ocamlPackages.map-vm
]

# or, inline it directly
buildInputs = [
  inputs.map-vm.packages.${system}.map-vm
                       # ^ don't forget to make sure "system" is defined! 
];
```

> NOTE: We do not use [opam](https://opam.ocaml.org). Instead, add the required
> dependencies to [dune-project](/dune-project) and invoke `dune build`. This
> will fail the first time but prepare [map.opam](/map.opam) and/or
> [fold.opam](/fold.opam) accordingly. Now, to install the dependencies, invoke
> `direnv reload` and they'll be made available for all subsequent `dune build`
> and `nix build`/`nix run` invocations.

## 2. Formatting

```fish
# either
nix fmt
# or
nix run .#fmt
```

## 3. Licensing

`map` is licensed under [GNU LGPLv3](/LICENSE-LGPLv3) only while `fold` is
licensed under [GNU GPLv3](/LICENSE-GPLv3) only.
