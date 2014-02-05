all: clean compile

compile:
	./rebar compile

clean:
	./rebar clean
	rm -rf ./sample_data

sample: clean compile samples
	erl -pa ./ebin -eval "ob_rcopy_sample:all()"

samples: sample_data/source sample_data/dest

sample_data:
	mkdir sample_data

sample_data/source: sample_data
	mkdir sample_data/source
	cd sample_data/source && dd if=/dev/zero of=10mb bs=1M count=10
	cd sample_data/source && dd if=/dev/zero of=15mb bs=1M count=15
	cd sample_data/source && dd if=/dev/zero of=20mb bs=1M count=20
	mkdir sample_data/source/parent_dir
	cd sample_data/source/parent_dir && dd if=/dev/zero of=1mb bs=1M count=1
	cd sample_data/source/parent_dir && dd if=/dev/zero of=2mb bs=1M count=2
	mkdir sample_data/source/parent_dir/child_dir
	cd sample_data/source/parent_dir/child_dir && dd if=/dev/zero of=3mb bs=1M count=3
	cd sample_data/source/parent_dir/child_dir && dd if=/dev/zero of=4mb bs=1M count=4
	cd sample_data/source/parent_dir/child_dir && dd if=/dev/zero of=5mb bs=1M count=5

sample_data/dest: sample_data
	mkdir sample_data/dest
