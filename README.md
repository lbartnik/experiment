experiment
==========

| Travis build status   | AppVeyor | Coverage |
|:---------------------:|:--------:|:--------:|
| [![Build Status](https://travis-ci.org/lbartnik/subprocess.svg?branch=master)](https://travis-ci.org/lbartnik/subprocess) | [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/lbartnik/subprocess?branch=master&svg=true)](https://ci.appveyor.com/project/lbartnik/subprocess) | [![codecov](https://codecov.io/gh/lbartnik/subprocess/branch/master/graph/badge.svg)](https://codecov.io/gh/lbartnik/subprocess)|

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


