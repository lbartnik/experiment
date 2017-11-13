experiment
==========

Simple way to take care of your data while doing MC/simulation experiments.


installation
============

Install `igraph` from GitHub; follow https://github.com/igraph/rigraph/blob/dev/CONTRIBUTING.md.
You might need extra dependencies like `flex`, `bison` or `libxml2-dev`.


```sh
git clone git@github.com:igraph/rigraph.git
cd rigraph
git submodule init
git submodule update
make
R CMD INSTALL igraph_1.1.2.9000.tar.gz
```

Install two other dependencies available from GitHub:

```r
devtools::install_github("rich-iannone/DiagrammeR")
devtools::install_github("lbartnik/storage")
```

Finally, install the `experiment` package:

```r
devtools::install_github("lbartnik/experiment")
```


