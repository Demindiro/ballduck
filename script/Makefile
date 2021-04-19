.PHONY: test

default: test

watch-run-%:
	cargo watch -c -x "run $$PWD/examples/$*.bs"

run-%:
	cargo build --release
	bash -c 'time ../target/release/bs $$PWD/examples/$*.bs'

vg-run-%:
	cargo build --release
	valgrind ../target/release/bs $$PWD/examples/$*.bs

run-unsafe-%:
	cargo build --release --features unsafe-loop
	bash -c 'time ../target/release/bs $$PWD/examples/$*.bs'

run-examples: run-hello run-factorial run-fizzbuzz run-count run-vec2

watch-test:
	cargo watch -c -x test

get-size:
	nm -C --print-size --radix=d ../target/release/bs | less

test:
	cargo build
	../target/debug/bs $$PWD/test/malformed/undefined_variable.bs || true
	../target/debug/bs $$PWD/test/expression/indices.bs || true
