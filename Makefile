.envrc: default.nix
	rm -f build/envrc
	mkdir -p build
	echo "PATH_add $$(nix-shell -j auto --cores 0 --pure --run 'printf %q $$PATH')" > .envrc