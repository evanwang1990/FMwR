#ifndef __FM_LEARN__
#define __FM_LEARN__

#include <Rcpp.h>
using namespace Rcpp;

#define CLASSIFICATION 10
#define REGRESSION     20
#define RANKING        30

class fm_learner
{
  public:
    int TASK;
    int SOLVER;
    int max_iter;
    double stepsize;
    int counter;

  public:
    int num_factor;
    double init_stdev;
    double init_mean;
    double init_lambda_W;
    double init_lambda_V;
    bool ignore_w0;
    bool ignore_W;
    bool ignore_V;
    bool warm_start;

  public:
    learner() {};
    ~learner() {};

    void init();

};

learner::learner()
{
  num_factor = 0;
  init_mean  = 0;
  init_stdev = 0.01;

}

#endif
