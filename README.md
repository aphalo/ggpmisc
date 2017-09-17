# ggpmisc #

Package '**ggpmisc**' (Miscellaneous Extensions to 'ggplot2') is a set of
extensions to R package 'ggplot2' (> 2.2.1) useful when plotting diverse types
of data.  

Functions `try_data_frame()` and `try_tibble()` can be used to convert time series
objects into data frames or tibbles suitable for plotting. To complement these
functions ggplot methods for `"ts"` and `"xts"` classes are defined.

A geometry, `geom_table()` allows adding tables from tibble objects mapped to
the label aesthetic. 

Two statistics automate finding the location and labeling peaks and/or valleys.

Several statistics are provided for annotations related to model fits: 
fitted polynomial equations as labels, fitted equations for other model fits
including non-linear ones, ANOVA tables, model summary tables, highlighting
deviations from a fitted curve, and plotting residuals on-the-fly.

Statistics are provided for filtering and/or tagging data from regions of plot 
panels with high/low densities of observations (the stats are designed to work nicely together with package 'ggrepel'). 

Another group of ggplot statistics and geometries which echo their data input to
the R console and/or plot aim at easing debugging during development of new geoms and statistics (or learning how ggplot layers work).

Please, see the web site [r4photobiology](http://www.r4photobiology.info) for
details and update notices, and 
[the docs site](http://docs.r4photobiology.info/ggpmisc). Other packages, aimed at easing photobiology-related
calculations including the quantification of biologically effective radiation in
meteorology are available as part of a suite described at the same website.

The current release of '__ggpmisc__' is available through [CRAN](https://cran.r-project.org/package=ggpmisc) 
for R (>= 3.3.0).

