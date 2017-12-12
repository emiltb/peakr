# peakr: Interactive tools for simple datasets

[![Build Status](https://travis-ci.org/emiltb/peakr.svg?branch=master)](https://travis-ci.org/emiltb/peakr)
[![CRAN\_Release\_Badge](https://www.r-pkg.org/badges/version-ago/peakr)](https://CRAN.R-project.org/package=peakr)
[![CRAN\_Download\_Badge](https://cranlogs.r-pkg.org/badges/grand-total/peakr)](https://CRAN.R-project.org/package=peakr)


`peakr` contains gadgets for common operations on data obtained in natural sciences (e.g. spectra and time series). The aim is to ease common operations that would otherwise need a lot of iterative refinements in an analysis script.

Hopefully `peakr` can also be used to make the transition into R easier and more attractive for beginners.

All functions try to encourage reproducibility by returning suggested code to produce the same result as in the gadget.

## Gadgets (`peak_<function>(df, x, y, ...)`)
All gadgets follows a common naming scheme `peak_<function>(df, x, y, ...)`. `df` is a dataframe with a dataset, `x` and `y` specify columns in the dataset and `...` can be a range of function specific parameters to fine-tune behaviour.

The following gadgets are available:

* `peak_pick()`: Peak picking in dataset, guided by a simple algorithm to help get the highest points on a specific peak. Other packages contain algorithms to find peaks in data, but often require a lot of fine-tuning to only find real peaks. This gadget is suitable for e.g. the student who has to report a few spectra in a report or lab journal.
* `peak_integrate()`: Peak integration
* `peak_subtract()`: Baseline subtraction
* `peak_normalise()`: Normalisation (i.e. with respect to peak intensity)
* `peak_smooth()`: Smoothing

## Reproducibility (`add_<function>(df, ...)`, `plot_<function>(df, x, y)`)
When doing interactive analysis in a gadget, the result should still be completely reproducible so that the same result is obtained the next time the script is run. For this reason all `peak_*()` functions return a line of R code with a corresponding `add_*()` function, that will produce the same result as the interactive analysis. This code is automatically copied to the clipboard, ready to be pasted into the analysis script.

The plot from the gadget can be reproduced with the corresponding `plot_*()` function.

## Installation
`peakr` is not on CRAN yet. Install using `devtools`.

```
devtools::install_github('emiltb/peakr')
```
