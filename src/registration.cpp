// RegisteringDynamic Symbols

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "api.hh"

static const R_CallMethodDef callMethods[]  = {
  { "C_try_readline", (DL_FUNC) &C_try_readline, 1 },
  { NULL, NULL, 0 }
};

void R_init_experiment(DllInfo* info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
