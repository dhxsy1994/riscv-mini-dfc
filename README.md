# riscv-mini-dfc

Author: Hessen Du (258414420@qq.com)

Owner: SKT-CPUOS

`riscv-mini-dfc` is a branch development from [riscv-mini](https://github.com/ucb-bar/riscv-mini).

It implements RV32I of the User-level ISA Version 2.0 and the Machine-level ISA of the Privileged Architecture Version 1.7.
Unlike other simple pipelines, it also contains simple instruction and data caches.

Note that a real-world processor is not the goal of `riscv-mini-dfc`.
This repo is a extension of DFC runtime env. The goal is to demo a fast way to reduce Thread context switching.

Next, this repo will diving into [rocket-chip](https://github.com/freechipsproject/rocket-chip) to implement a powerful processor. 


## Datapath Diagram
![pipeline](pipeline-modified.png)

## Getting Started

    $ git clone git@github.com:dhxsy1994/riscv-mini-dfc.git
    $ cd riscv-mini
    $ make            # generate firrtl & verilog files in generated-src
    
The verilog output file can be used for verilator simulation or the ASIC tool flow.
    
## Running Verilator Simulation

First, generate the verilator binary:

    $ make verilator
    
This will generate `VTile` in the top-level directory.

Now, you can run verilator simulation for a given hex file as follows:

    $ ./VTile <hex file> [<vcd file> 2> <log file>]
    
`<vcd file>` and the pipe to `<log file>` are optional. The waveform is dumped to `dump.vcd` and the execution trace is printed in the screen by default.

The following command runs the whole test hex files in verilator and dumps the traces and the waveforms to the 'outputs' directory:

    $ make run-tests

## Unit and Integration Tests with `sbt`

`riscv-mini` provides synthesizable unit & integration tests.
Theres have some unit tests(`ALUTests`, `BrCondTests`, `ImmGenTests`, `CSRTests`, `CacheTests`, `DatapathTests`, `TableATests`,`TableDTests`, `DFCTileTests`),
running user-defined test vectors.
To execute them, first launch sbt with `make sbt` and run:

    > testOnly mini.[testname]
  
There are also six sets of integration tests, running the hex files from [riscv-tests](https://github.com/riscv/riscv-tests).
To execute them, also launch `sbt` and run:

    > testOnly mini.[Core|Tile][Simple|ISA|Bmark]Tests
    
`Core` only contains the datapath and the control unit, while `Tile` also contains I$ and D$. `Simple` only runs `rv32ui-p-simple`,
`ISA` runs the whole ISA tests, and `Bmark` runs five benchmarks(`median`, `multiply`, `qsort`, `towers`, `vvadd`). 
Note that all tests in a set run in parallel.

Finally, to run all the tests, just in sbt:

    > test
    
## Running DataFlow Program on `riscv-mini-dfc`

you can execute the data flow application on `riscv-mini-dfc`. In this case, you need to install RISC-V tools for priv 1.7. This repo provides a script to install the correct version of tools. Run the script as follows:

    $ export RISCV=<path to riscv tools for priv 1.7>
    $ ./build-riscv-tools
    
It takes a while to install the toolchain, so please be patient, and you should install verilator with 3.886 version, otherwise may encounter errors.

This DataFlow program and syscall in `dfc-bmark`, more detail please see `dfc-bmark/README.md`. To compile program, run `make` in `dfc-bmark` to generate the binary, dump, and the hex files and waveform. Finally, run the following command in the base directory:

    $ make run-dfc-bmark
    
Also we have a version for DataFlow program running on FPGA, You can monit a continual expt singal. Run the following command in the base directory:

    $ make run-fpga-bmark
