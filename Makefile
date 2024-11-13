.PHONY: build
build:
	pushd rustnif && cargo build --release && popd
	mkdir -p priv
	cp rustnif/target/release/librustnif.so priv/
