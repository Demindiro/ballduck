watch-run-%:
	cargo watch -c -x "run $$PWD/examples/$*.bs"

run-%:
	cargo build --release
	bash -c 'time ../target/release/ballscript $$PWD/examples/$*.bs'
