# Instructions

Simulations and real-data analysis conducted in the paper from Laria, Aguilera-Morillo, Lillo (2018) *An iterative sparse-group lasso*. Detailed instructions are provided with each particular simulation. In general, to run all the simulations you should have a working `R` installation in your system. To run some simulations, you will also need python2, so we strongly suggest to install both `R` and `python2` through the [`conda` environment](https://conda.io/docs/).
It is also required to install the `R` package [`sglfast`](https://github.com/jlaria/sglfast)

Some scripts in these simulations are designed to run in a high performance cluster with a PBS queue, specifically a Rocks 6.1.1 Sand Boa cluster, with the HPC Roll. You will probably have to comment the following  lines if you want to run the `R` scripts in a PC.

    library(Rmpi)
    library(doMPI)
    cl <- startMPIcluster()
    registerDoMPI(cl)
    closeCluster(cl)
    mpi.quit()

However, they are not designed to run in a PC, so they might take some time.
