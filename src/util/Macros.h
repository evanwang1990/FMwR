#ifndef TOOL_H_
#define TOOL_H_

// define data type for windows
typedef unsigned int uint;

// some const value
#define SQRT2PI 2.5066282746310002

// TASK
#define CLASSIFICATION 10
#define REGRESSION     20
#define RANKING        30

// SOLVER
#define MCMC 100
#define ALS  200
#define SGD  300
#define BGD  400
#define FTRL 500
#define TDAP 600

// Evaluation
#define LL   000
#define AUC  111
#define ACC  222
#define RMSE 333
#define MSE  444
#define MAE  555


// update parameters
#define TMP(_X_) _X_##_
#define OLD(_X_) _X_##_old

#define CHECK_PARAM(_X_, _ACTION_)                             \
{                                                              \
  if (R_IsNaN(TMP(_X_)) || TMP(_X_) == R_PosInf || TMP(_X_) == R_NegInf) { \
    TMP(_X_) = OLD(_X_);                                       \
    _ACTION_;}                                                 \
}

// nan or inf counter
#define INIT_CNTR(_X_)                                         \
{                                                              \
  nan_cntr_##_X_ = 0;                                          \
  inf_cntr_##_X_ = 0;                                          \
}

#define CNTR_WARNING(_X_)                                      \
{                                                              \
  if (nan_cntr_##_X_ > 0 || inf_cntr_##_X_ > 0) {              \
    cout<<"nans in "<<#_X_<<": "<<nan_cntr_##_X_<<"\t";        \
    cout<<"infs in "<<#_X_<<": "<<inf_cntr_##_X_<<endl;        \
  }                                                            \
}



#endif
