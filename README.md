# ggpmisc #

[![](http://www.r-pkg.org/badges/version/ggpmisc)](https://cran.r-project.org/package=ggpmisc) 
[![](http://cranlogs.r-pkg.org/badges/ggpmisc)](https://cran.r-project.org/package=ggpmisc) 
[![](http://cranlogs.r-pkg.org/badges/grand-total/ggpmisc)](https://cran.r-project.org/package=ggpmisc)

Package '**ggpmisc**' (Miscellaneous Extensions to 'ggplot2') is a set of
extensions to R package 'ggplot2' (>= 3.0.0) with emphasis on annotations and
highlighting related to fitted models and data summaries. To complement these,
the widely useful `geom_table()` and `stat_fmt_tb()` are defined as well as
`ggplot` constructors for time series objects. The provided `ggplot.ts()` and
`ggplot.xts()` use `try_tibble()` which is also exported and accepts objects of
additional classes as input.

Statistics useful for highlighting and/or annotating individual data points 
in regions of plot panels with high/low densities of observations. These stats are designed to work
well together with `geom_text_repel()` and `geom_label_repel()` from package 
'ggrepel'.

Functions for the manipulation of layers in ggplot objects and statistics and
geometries that echo their data input to the R console, earlier included in this
package are now in package 'gginnards'.

Please, see the web site [r4photobiology](http://www.r4photobiology.info) for
details and update notices, and [the docs
site](http://docs.r4photobiology.info/ggpmisc). Other packages, aimed at easing
photobiology-related calculations including the quantification of biologically
effective radiation in meteorology are available as part of a suite described at
the same website.

The current release of 'ggpmisc' is available through
[CRAN](https://cran.r-project.org/package=ggpmisc) for R (>= 3.3.0).

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
