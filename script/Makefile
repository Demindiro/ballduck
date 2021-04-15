watch-run-%:
	cargo watch -c -x "run $$PWD/examples/$*.bs"

run-%:
	cargo build --release
	bash -c 'time ../target/release/ballscript $$PWD/examples/$*.bs'

run-unsafe-%:
	cargo build --release --features unsafe-loop
	bash -c 'time ../target/release/ballscript $$PWD/examples/$*.bs'

run-examples: run-hello run-factorial run-fizzbuzz run-count run-vec2

watch-test:
	cargo watch -c -x test

get-size:
	nm --print-size --radix=d ../target/release/ballscript | less
