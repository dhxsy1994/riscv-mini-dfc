default: compile

base_dir   = $(abspath .)
src_dir    = $(base_dir)/src/main
gen_dir    = $(base_dir)/generated-src
out_dir    = $(base_dir)/outputs
dfc_out_dir = $(base_dir)/dfc-outputs
fpga_out_dir = $(base_dir)/fpga-outputs

SBT       = sbt
SBT_FLAGS = -ivy $(base_dir)/.ivy2

sbt:
	$(SBT) $(SBT_FLAGS)

compile: $(gen_dir)/Tile.v

$(gen_dir)/Tile.v: $(wildcard $(src_dir)/scala/*.scala)
	$(SBT) $(SBT_FLAGS) "run $(gen_dir)"

CXXFLAGS += -std=c++11 -Wall -Wno-unused-variable

# compile verilator
VERILATOR = verilator --cc --exe
VERILATOR_FLAGS = --assert -Wno-STMTDLY -O3 --trace \
	--top-module Tile -Mdir $(gen_dir)/VTile.csrc \
	-CFLAGS "$(CXXFLAGS) -include $(gen_dir)/VTile.csrc/VTile.h" 

$(base_dir)/VTile: $(gen_dir)/Tile.v $(src_dir)/cc/top.cc $(src_dir)/cc/mm.cc $(src_dir)/cc/mm.h
	$(VERILATOR) $(VERILATOR_FLAGS) -o $@ $< $(word 2, $^) $(word 3, $^)
	$(MAKE) -C $(gen_dir)/VTile.csrc -f VTile.mk

verilator: $(base_dir)/VTile

# isa tests + benchmarks with verilator
test_hex_files = $(wildcard $(base_dir)/src/test/resources/*.hex)
test_out_files = $(foreach f,$(test_hex_files),$(patsubst %.hex,%.out,$(out_dir)/$(notdir $f)))

$(test_out_files): $(out_dir)/%.out: $(base_dir)/VTile $(base_dir)/src/test/resources/%.hex
	mkdir -p $(out_dir)
	$^ $(patsubst %.out,%.vcd,$@) 2> $@

run-tests: $(test_out_files)


# run custom benchamrk
custom_bmark_hex ?= $(base_dir)/custom-bmark/main.hex
custom_bmark_out  = $(patsubst %.hex,%.out,$(out_dir)/$(notdir $(custom_bmark_hex)))
$(custom_bmark_hex):
	$(MAKE) -C custom-bmark

$(custom_bmark_out): $(base_dir)/VTile $(custom_bmark_hex)
	mkdir -p $(out_dir)
	$^ $(patsubst %.out,%.vcd,$@) 2> $@

run-custom-bmark: $(custom_bmark_out) 

# run dfc_bmark
dfc_bmark_hex ?= $(base_dir)/dfc-bmark/main.hex
dfc_bmark_out  = $(patsubst %.hex,%.out,$(dfc_out_dir)/$(notdir $(dfc_bmark_hex)))
$(dfc_bmark_hex):
	$(MAKE) -C dfc_bmark

$(dfc_bmark_out): $(base_dir)/VTile $(dfc_bmark_hex)
	mkdir -p $(dfc_out_dir)
	$^ $(patsubst %.out,%.vcd,$@) 2> $@

run-dfc-bmark: $(dfc_bmark_out) 

# run bmark for fpga tests with loop interrupts
fpga_bmark_hex ?= $(base_dir)/fpga-bmark/main.hex
fpga_bmark_out  = $(patsubst %.hex,%.out,$(fpga_out_dir)/$(notdir $(fpga_bmark_hex)))
$(fpga_bmark_hex):
	$(MAKE) -C fpga-bmark

$(fpga_bmark_out): $(base_dir)/VTile $(fpga_bmark_hex)
	mkdir -p $(fpga_out_dir)
	$^ $(patsubst %.out,%.vcd,$@) 2> $@

run-fpga-bmark: $(fpga_bmark_out) 

# unit tests + integration tests 
test:
	$(SBT) $(SBT_FLAGS) test

clean:
	rm -rf $(gen_dir) $(out_dir) $(dfc_out_dir) $(fpga_out_dir) test_run_dir

cleanall: clean
	rm -rf target project/target

.PHONY: sbt compile verilator run-tests run-custom-bmark test clean cleanall
