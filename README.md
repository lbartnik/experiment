experiment
==========

| Travis build status   | AppVeyor | Coverage |
|:---------------------:|:--------:|:--------:|
| [![Build Status](https://travis-ci.org/lbartnik/experiment.svg?branch=master)](https://travis-ci.org/lbartnik/experiment) | [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/lbartnik/experiment?branch=master&svg=true)](https://ci.appveyor.com/project/lbartnik/experiment) | [![codecov](https://codecov.io/gh/lbartnik/experiment/branch/master/graph/badge.svg)](https://codecov.io/gh/lbartnik/experiment)|

Simple way to take care of your data while doing MC/simulation experiments.


installation
============


First, install two other dependencies available from GitHub:

```r
devtools::install_github("lbartnik/storage")
devtools::install_github("lbartnik/defer")
```

Finally, install the `experiment` package:

```r
devtools::install_github("lbartnik/experiment")
```


