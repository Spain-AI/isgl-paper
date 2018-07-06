# -*- coding: utf-8 -*-
"""
Contact: github.com/jlaria
Original script from <https://github.com/jjfeng/nonsmooth-joint-opt>

"""

import sys
import traceback

sys.path.append("nonsmooth-joint-opt")

from iteration_models import Simulation_Settings, Iteration_Data
from common           import *
from data_generator   import DataGenerator

def generate(train_size, validate_size, num_features, true_num_groups, num_runs):
    # Algorithm's settings
    settings = SGL_Settings()

    settings.train_size = train_size
    settings.validate_size = validate_size
    settings.test_size = 200
    settings.snr = 2  # signal-to-noise ratio
    settings.num_features = num_features  # number of covariates
    settings.expert_num_groups = true_num_groups  # number of groups for the computations
    settings.true_num_groups = true_num_groups  # real number of groups
    settings.spearmint_numruns = 50

    # Generate the data for the simulations

    data_gen = DataGenerator(settings)
    for i in range(num_runs):
        # Generate data Train/Validation/Test
        observed_data = data_gen.sparse_groups()

        t = 'data/X_train-run%i' % (i)
        np.savetxt( fname=t,  X=observed_data.X_train )
        t = 'data/X_validate-run%i' %  (i)
        np.savetxt( fname=t,  X=observed_data.X_validate)
        t = 'data/X_test-run%i' %  (i)
        np.savetxt( fname=t,  X=observed_data.X_test )

        t = 'data/y_train-run%i' %  (i)
        np.savetxt(fname=t, X=observed_data.y_train)
        t = 'data/y_validate-run%i' %  (i)
        np.savetxt(fname=t, X=observed_data.y_validate)
        t = 'data/y_test-run%i' %  (i)
        np.savetxt(fname=t, X=observed_data.y_test)

class SGL_Settings(Simulation_Settings):
    results_folder = "results/sgl"
    num_train = 10
    num_validate = 3
    num_test = 200
    num_features = 30
    expert_num_groups = 3
    true_num_groups = 3
    spearmint_numruns = 100
    gs_lambdas1 = np.power(10, np.arange(-3, 1, 4.0/10))
    gs_lambdas2 = gs_lambdas1
    assert(gs_lambdas1.size == 10)
    method_result_keys = [
        "test_err",
        "validation_err",
        "beta_err",
        "runtime",
        "num_solves",
        "correct_selection",
    ]

    def print_settings(self):
        print "SETTINGS"
        obj_str = "method %s\n" % self.method
        obj_str += "expert_num_groups %d\n" % self.expert_num_groups
        obj_str += "num_features %d\n" % self.num_features
        obj_str += "t/v/t size %d/%d/%d\n" % (self.train_size, self.validate_size, self.test_size)
        obj_str += "snr %f\n" % self.snr
        obj_str += "sp runs %d\n" % self.spearmint_numruns
        obj_str += "nm_iters %d\n" % self.nm_iters
        print obj_str

    def get_true_group_sizes(self):
        assert(self.num_features % self.true_num_groups == 0)
        return [self.num_features / self.true_num_groups] * self.true_num_groups

    def get_expert_group_sizes(self):
        assert(self.num_features % self.expert_num_groups == 0)
        return [self.num_features / self.expert_num_groups] * self.expert_num_groups


def main():
    train_size = int(sys.argv[1])
    validate_size = int(sys.argv[2])
    num_features = int(sys.argv[3])
    true_num_groups = int(sys.argv[4])
    num_runs = int(sys.argv[5])
    generate(train_size, validate_size, num_features, true_num_groups, num_runs)


if __name__ == "__main__":
    main()
