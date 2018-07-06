# Simulations

Simulation studies in Section 4.1 of Laria, Aguilera-Morillo, Lillo (2018) *An iterative sparse-group lasso*.

## Installation
You will need an `R` and `python2` installation. Easiest way to get everything ready is through the [`conda` environment](https://conda.io/docs/)

Follow the instructions in https://github.com/jlaria/sglfast to install the iterative sparse-group lasso.
Then, in a (linux) console run the script `install_GD.sh` to clone the git repo https://github.com/jjfeng/nonsmooth-joint-opt and prepare the necessary folders. You can do it manually if you encounter any error.

Install all the `python` dependencies you may need through the [anaconda cloud](https://anaconda.org).

## Usage

Follow this instructions to replicate Table 1.

1.  Open `make_data.sh` in a text editor and change lines 3-8 according to the simulation design.
2.  In a terminal, type `sh make_data.sh` to create the matrices for the simulations.
3.  Run the `R` script `simulate.R`. You may want to edit `simulate.R: line 29` if you do not want to test all the algorithms.

    *Note:* If you are using a HPC cluster with PBS, you may want to use the configuration files `make_data_script.pbs` and `script.pbs` to run `make_data.sh` and `simulate.R`, respectively. Modify them to fit your needs.

4. Run `get_results.R` to gather the results from the simulations in a tex-like output. You may have to edit lines 2 and 4, according to the simulation design you chose.

To replicate Figure 2 of the paper, run the `R` script `fig2.R`.
