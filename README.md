[![Travis-CI Build Status](https://travis-ci.org/frycast/rsar.svg?branch=master)](https://travis-ci.org/frycast/rsar) 
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/frycast/rsar/master?urlpath=rstudio) 
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) 
[![Lifecycle Status](https://img.shields.io/badge/lifecycle-experimental-red.svg)](https://www.tidyverse.org/lifecycle/)

# Welcome to rsar

Installation with devtools:
```{r}
devtools::install_github("frycast/rsar")
library(rsar)
```

Start by importing SAR data as a raster brick.
The file path specified below is to a sample dataset in the package directory.
```{r}
filepath <- system.file("extdata", "MG_CC_sub_norm_testclip.tif", package="rsar")
sample <- raster::brick(filepath)
```
