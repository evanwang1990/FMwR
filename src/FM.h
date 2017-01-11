#ifndef FM_H_
#define FM_H_

#include <Rcpp.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include <map>
#include <string>

#include "util/Macros.h"
#include "util/Dmatrix.h"
#include "util/Dvector.h"
#include "util/Random.h"
#include "util/Smatrix.h"
#include "util/Swrap.h"

#include "core/Data.h"
#include "core/Model.h"
#include "core/Learner.h"
#include "core/Evaluation.h"
#include "core/Tracker.h"

#include "solver/MCMC_ALS_Learner.h"
#include "solver/SGD_Learner.h"
#include "solver/FTRL_Learner.h"
#include "solver/TDAP_Learner.h"

#endif
