Package '__ggpmisc__' (Miscellaneous Extensions to 'ggplot2') is a set of extensions to R package 'ggplot2' (>= 2.1.0) which
I hope will be useful when plotting diverse types of data. Currently available stats
add the following statistics, geoms and function:

* `stat_peaks()` find and label local or global maxima in _y_.
* `stat_valleys()` find and label local or global minima in _y_.
* `stat_poly_eq()` add a label for a fitted linear model (using `lm()`) to a plot, label can be the fitted polynomial equation, R^2, BIC, AIC. Fits are done by _group_ as defined by the mapped aesthetics.
* `stat_fit_deviations()` display residuals from a model fit as segments linking observed and fitted values. Fits are done by _group_ depending on the mapped aesthetics.
* `stat_fit_residuals()` residuals from a model fit.
* `stat_fit_augment()` data augmented with fitted values and statistics using package 'broom'. Not limited to linear models.  Fits are done by _group_ as defined by the mapped aesthetics.
* `stat_fit_glance()` one-row summary data frame for a fitted model using package 'broom', useful for producing customized annotations based on the fit results. Not limited to linear models.  Fits are done by _group_ as defined by the mapped aesthetics.
* `stat_debug_group()`, `stat_debug_panel()` print data received as input by a stat's group and panel functions respectively. Useful for debugging new statistics and for exploring how ggplot works.
* `geom_debug()` print data received as input by a geom.
* `try_data_frame()` convert an R object into a data frame. Specially useful for plotting time series (using internally package 'xts') which are returned with _x_ data as POSIXct, allowing direct plotting with 'ggplot2' and packages extending 'ggplot2'.

The package [manual](https://cran.r-project.org/web/packages/ggpmisc/ggpmisc.pdf) describes in more detail the items listed above, and the [vignette](https://cran.r-project.org/web/packages/ggpmisc/vignettes/examples.html) gives several examples of plots produced with the package.

Please, see the web site [r4photobiology](http://www.r4photobiology.info) for 
details and update notices. Other packages, aimed at easing photobiology-related
calculations including the quantification of biologically effective radiation
in meteorology are available as part of a suite described at the same
website.

The current release of '__ggpmisc__' is available through [CRAN](https://cran.r-project.org/web/packages/ggpmisc/index.html) 
for R (>= 3.2.0).

Package __ggpmisc__ is a small set of extensions to ggplot2 (>= 2.0.0) which
I hope will be useful when plotting diverse types of data and/or debugging
ggplot plots.

Extensions:
- Function for conversion of time series data into data frames that can be
plotted with ggplot.
- Stats for locating and tagging "peaks" and "valleys" (local or global maxima
and minima).
- Stat for generating labels from a `lm()` model fit, including formatted
equation.
- _Debug_ stats and a geom that print to the console their `data` input.

Please, see the web site [R4Photobiology](http://www.r4photobiology.info) for 
details and update notices. Other packages, aimed at easing photobiology-related
calculations including the quantification of biologically effective radiation
in meteorology are available as part of a suite described at the same
website.

The current release of __ggpmisc__ is available through [CRAN](https://cran.r-project.org/package=ggpmisc) 
for R (>= 3.2.0). 
