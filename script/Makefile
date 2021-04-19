.PHONY: test

default: test

watch-run-%:
	cargo watch -c -x "run --features build-binary $$PWD/examples/$*.bs"

run-%:
	cargo build --release --features build-binary
	bash -c 'time ../target/release/bs $$PWD/examples/$*.bs'

vg-run-%:
	cargo build --release --features build-binary
	valgrind ../target/release/bs $$PWD/examples/$*.bs

run-unsafe-%:
	cargo build --release --features unsafe-loop --features build-binary
	bash -c 'time ../target/release/bs $$PWD/examples/$*.bs'

run-examples: run-hello run-factorial run-fizzbuzz run-count run-vec2

watch-test:
	cargo watch -c -x 'test --features build-binary'

get-size:
	nm -C --print-size --radix=d ../target/release/bs | less

test-loop:
	cargo watch -c -s 'make test'

test:
	cargo build --features build-binary
	../target/debug/bs $$PWD/test/malformed/undefined_variable.bs || true
	../target/debug/bs $$PWD/test/expression/indices.bs || true
	../target/debug/bs $$PWD/test/expression/call_no_assign.bs || true
