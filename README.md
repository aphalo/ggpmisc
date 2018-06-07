# ggpmisc #

[![](http://www.r-pkg.org/badges/version/ggpmisc)](https://cran.r-project.org/package=ggpmisc) 
[![](http://cranlogs.r-pkg.org/badges/ggpmisc)](https://cran.r-project.org/package=ggpmisc) 
[![](http://cranlogs.r-pkg.org/badges/grand-total/ggpmisc)](https://cran.r-project.org/package=ggpmisc)

Package '**ggpmisc**' (Miscellaneous Extensions to 'ggplot2') is a set of
extensions to R package 'ggplot2' (>= 2.3.0) useful when plotting diverse types
of data.  

**Currently there are two development branches of 'ggpmisc' being maintained: 
one at CRAN with normal version
numbering and compatible with 'ggplot2' (>=2.2.1) and this one with version
numbers such as 0.9.17.9900 with additional functionality but requiring the
development version of 'ggplot2' (>=2.2.1.9000 future >=2.3.0) currently 
available only from Github.**

Functions `try_data_frame()` and `try_tibble()` can be used to convert time
series objects into data frames or tibbles suitable for plotting. To complement
these functions ggplot() methods for `"ts"` and `"xts"` classes are defined.

A geometry, `geom_table()` allows addition of tables from tibble objects mapped 
to the `label` aesthetic. 

Two statistics automate finding the location and labeling peaks and/or valleys.

Several statistics are provided for annotations related to model fits: 
fitted polynomial equations as labels, fitted equations for other model fits
including non-linear ones, ANOVA tables, model summary tables, highlighting
deviations from a fitted curve, and plotting residuals on-the-fly.

Statistics are provided for filtering and/or tagging data from regions of plot
panels with high/low densities of observations (the stats are designed to work
nicely together with package 'ggrepel').

A set of functions facilitates the manipulation of layers in ggplot objects and 
statistics and geometries that echo their data input are now in package 
'gginnards'.

Please, see the web site [r4photobiology](http://www.r4photobiology.info) for
details and update notices, and [the docs
site](http://docs.r4photobiology.info/ggpmisc). Other packages, aimed at easing
photobiology-related calculations including the quantification of biologically
effective radiation in meteorology are available as part of a suite described at
the same website.

The current release of '__ggpmisc__' is available through
[CRAN](https://cran.r-project.org/package=ggpmisc) for R (>= 3.3.0).

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
