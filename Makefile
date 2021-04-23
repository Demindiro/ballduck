OUTPUT?=target/release/bs
OUTPUT_DEBUG?=target/debug/bs

.PHONY: test

default: test

test-examples: run-array run-dictionary run-fizzbuzz run-iter_str run-sort-selection run-vec2 run-count run-factorial run-hello run-locals run-sieve run-vars run-while

watch-run-%:
	cargo watch -c -x "run $$PWD/examples/$*.bs"

run-%:
	cargo build --release
	bash -c 'time $(OUTPUT) $$PWD/examples/$*.bs'

run-copy-%:
	cargo build --release --features copy-variant
	bash -c 'time $(OUTPUT) $$PWD/examples/$*.bs'

vg-run-%:
	cargo build --release
	valgrind $(OUTPUT) $$PWD/examples/$*.bs

run-unsafe-%:
	cargo build --release --features unsafe-loop
	bash -c 'time $(OUTPUT) $$PWD/examples/$*.bs'

watch-test:
	cargo watch -c -x 'test'

get-size:
	nm -C --print-size --radix=d $(OUTPUT) | less

test-loop:
	cargo watch -c -s 'make test'

test:
	cargo build
	$(OUTPUT_DEBUG) $$PWD/test/malformed/undefined_variable.bs || true
	$(OUTPUT_DEBUG) $$PWD/test/expression/indices.bs || true
	$(OUTPUT_DEBUG) $$PWD/test/expression/call_no_assign.bs || true
	$(OUTPUT_DEBUG) $$PWD/test/constant.bs || true
