<!-- badges: start -->
[![Travis-CI Build Status](https://travis-ci.org/frycast/rsar.svg?branch=master)](https://travis-ci.org/frycast/rsar) 
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/frycast/rsar/master?urlpath=rstudio) 
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) 
[![Lifecycle Status](https://img.shields.io/badge/lifecycle-experimental-red.svg)](https://www.tidyverse.org/lifecycle/)
[![Codecov test coverage](https://codecov.io/gh/frycast/rsar/branch/master/graph/badge.svg)](https://codecov.io/gh/frycast/rsar?branch=master)
<!-- badges: end -->

# Welcome to rsar

Installation with devtools:
```{r}
devtools::install_github("frycast/rsar")
library(rsar)
```

The load_SAR_matrix will import a raster brick from a `tif` file and
convert the raster brick to a `SAR_matrix`. Each column of a `SAR_matrix` 
corresponds to a band in the original raster brick, and each row 
corresponds to a pixel. So if the raster brick has dimensions `A x B x C`
then the `SAR_matrix` has dimensions `A*B x C`.
The file path specified below is to a sample dataset in the package directory.
```{r}
filename <- system.file("extdata", "MG_CC_sub_norm_testclip.tif", package="rsar")
sample <- load_SAR_matrix(filename)
```

The sample raster brick consists of a 43x50 raster image with 29 bands.
```{r}
dim(sample)
```


