if [[ ! -f ~/.cachixinstalled ]]; then
  nix-env -iA cachix -f https://cachix.org/api/v1/install
  cachix use digitallyinduced
  touch ~/.cachixinstalled
fi
