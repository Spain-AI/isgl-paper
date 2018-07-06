# Simulations

Simulation studies in Section 4.2 of Laria, Aguilera-Morillo, Lillo (2018) *An iterative sparse-group lasso*.

## Usage

Follow this instructions to replicate Table 2.

1.  Open `gen_data.R` in a text/R editor and change lines 3-8 according to the simulation design.
2.  In a terminal, type `sh make_data.sh` to create the matrices for the simulations.
3.  Run the `R` script `simulate.R`. You may want to edit `simulate.R: line 28` if you do not want to test all the algorithms.

    *Note:* If you are using a HPC cluster with PBS, you may want to use the configuration files `make_data_script.pbs` and `script.pbs` to run `make_data.sh` and `simulate.R`, respectively. Modify them to fit your needs.

4. Run `get_results.R` to gather the results from the simulations in a tex-like output. You may have to edit lines 2 and 4, according to the simulation design you chose.
