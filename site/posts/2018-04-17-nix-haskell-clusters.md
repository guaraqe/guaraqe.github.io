---
title: Running Haskell applications in clusters using Nix
---

Haskell is my favorite programming language, and Nix my favorite way of setting up development environments for programming and running algorithms.

One drawback of using these is the fact that they are difficult to use in very restricted environments, like high-performance scientific clusters, since they are non-standard tools.
GHC is almost never included in these environments, which is a blocker for Haskell.
While Nix can solve this problem, users do not have root access in general.

A possible solution for this is [nix-bundle](https://github.com/matthewbauer/nix-bundle).
However, it uses [nix-user-chroot](https://github.com/lethalman/nix-user-chroot), that needs user namespaces, requiring at least Linux 3.8.
I had version 2.6.

This post describes a rustic solution inspired by the above (i.e. reading the source) that worked very well in my case.
The idea is to copy the closure of of the desired executable to a local `nix` folder, and create a virtual environment where this folder is mounted on `/` using PRoot.

## Generating the closure

We suppose that `./nixpkgs.nix` is an overlayed Nixpkgs that contains the executable we want to run, identified as `my-executable`.
More information about using Haskell with Nix can be found in [Gabriel Gonzalez guide](https://github.com/Gabriel439/haskell-nix).
The script for setting up the local environment is the following:

```bash
#!/bin/sh

# Instantiate the derivation
drv=$(nix-instantiate --no-gc-warning -E 'with import ./nixpkgs.nix {}; my-executable')

# Build the derivation
bld=$(nix-store --no-gc-warning -r "$drv")

# Copy all its dependencies to the local nix store
nix-store -qR "$bld" | xargs -I {} cp -r {} ./nix/store/

# Copy the executable to the local folder
cp package/"$bld"/bin/my-executable my-executable
```

This should give you a local `./nix` folder with all the necessary libraries for your executable, and `./my-executable` locally.

## Running the executable

The first step is to download [PRoot](https://proot-me.github.io/#downloads), which we will refer to as `proot` in the command line.
Since this is a statically compiled binary, it has no dependencies.
In order to run the executable one only needs to:

```bash
./proot -b ./nix:/nix ./my-executable --flag value
```

The `-b` flag says that we want to mount `./nix` to `/nix`, and the following arguments are the executable itself and the flags passed to it.

And that's it!
Now you should be able to use Haskell in scientific clusters!

## Comments

In order for this to work, the processor architecture of your personal computer and the cluster's one should be the same.
In my case, both were `x86-64`.

When running, I got the following error message:

```
ERROR: ld.so: object 'libcr_run.so' from LD_PRELOAD cannot be preloaded (cannot open shared object file): ignored.
```

However, it did not affect the execution of my process.
