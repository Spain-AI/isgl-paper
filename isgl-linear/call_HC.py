# -*- coding: utf-8 -*-
"""
Contact: github.com/jlaria
Interface to call the HC/HC0 algorithm Feng and Simon (2017)

usage:
    python call_HC.py expert_num_groups run simple

example:
    python call_HC.py 5 0 simple
    python call_HC.py 5 0 full

"""

import sys
import traceback

sys.path.append("nonsmooth-joint-opt")

from iteration_models import Simulation_Settings, Iteration_Data
from sgl_hillclimb    import SGL_Hillclimb_Simple, SGL_Hillclimb
from common           import *
from data_generator   import ObservedData


def main():
    expert_num_groups = int(sys.argv[1])
    run = int(sys.argv[2])
    method = sys.argv[3]
    simple = False
    if method == 'simple':
		simple = True
    obs = load_saved_data(run)
    fmodel = SGL_HC(obs, expert_num_groups, simple)
    save_results(fmodel, run, simple)


def load_saved_data(run):
    X_train = np.matrix(np.loadtxt('data/X_train-run%i' % (run)))
    y_train = np.matrix(np.loadtxt('data/y_train-run%i' % (run)))
    y_train.shape = (X_train.shape[0], 1)

    #X_test = np.matrix(np.loadtxt('data/X_test-run%i' % (run)))
    #y_test = np.loadtxt('data/y_test-run%i' % (run))
    #y_test.shape = (X_test.shape[0], 1)
    
    X_test = X_train[1:2,]
    y_test = y_train[1:2,]

    X_validate = np.matrix(np.loadtxt('data/X_validate-run%i' % (run)))
    y_validate = np.loadtxt('data/y_validate-run%i' % (run))
    y_validate.shape = (X_validate.shape[0], 1)

    obs = ObservedData(X_train, y_train, X_validate, y_validate, X_test, y_test)
    return obs

def save_results(fmodel, run, simple = False):
	if simple:
		path = 'results/HC0'
	else:
		path = 'results/HC'

	np.savetxt(fname = ('%s/beta%i'%(path, run)), X =  np.concatenate(fmodel.best_model_params) )
	np.savetxt(fname = ('%s/time%i'%(path, run)), X = np.matrix(fmodel.runtime) )

def SGL_HC(obs, expert_num_groups, simple = False):
    # Algorithm's settings
    settings = SGL_Settings()

    settings.train_size = obs.num_train
    settings.validate_size = obs.num_validate
    settings.test_size = obs.num_test
    settings.snr = 2  # signal-to-noise ratio
    settings.num_features = obs.num_features  # number of covariates
    settings.expert_num_groups =  expert_num_groups # number of groups for the computations
    settings.true_num_groups = expert_num_groups  # real number of groups

    if simple:
        one_vec2 = np.ones(2)
        simple_initial_lambdas_set = [one_vec2, one_vec2 * 1e-1]
        algo = SGL_Hillclimb_Simple(obs, settings)
        algo.run(simple_initial_lambdas_set, debug = False)
    else:
        one_vec = np.ones(settings.expert_num_groups + 1)
        initial_lambdas_set = [one_vec, one_vec * 1e-1]
        algo = SGL_Hillclimb(obs, settings)
        algo.run(initial_lambdas_set, debug = False)

    #algo.fmodel.best_lambdas
    #np.concatenate(algo.fmodel.best_model_params)
    #algo.fmodel.runtime
    return algo.fmodel

class SGL_Settings(Simulation_Settings):
    results_folder = "nonsmooth-joint-opt/results/sgl"
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

if __name__ == '__main__':
    main()
