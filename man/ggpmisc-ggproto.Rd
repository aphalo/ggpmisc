% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-correlation.R, R/stat-fit-broom-tb.R,
%   R/stat-fit-broom.R, R/stat-fit-deviations.R, R/stat-fit-residuals.R,
%   R/stat-ma-eq.R, R/stat-ma-line.R, R/stat-multcomp.R, R/stat-peaks.R,
%   R/stat-poly-eq.R, R/stat-poly-line.R, R/stat-quant-band.r,
%   R/stat-quant-eq.R, R/stat-quant-line.r
\docType{data}
\name{cor_test_compute_fun}
\alias{cor_test_compute_fun}
\alias{StatCorr}
\alias{fit_tb_compute_panel_fun}
\alias{StatFitTb}
\alias{fit_glance_compute_group_fun}
\alias{StatFitGlance}
\alias{fit_augment_compute_group_fun}
\alias{StatFitAugment}
\alias{fit_tidy_compute_group_fun}
\alias{StatFitTidy}
\alias{deviations_compute_group_fun}
\alias{StatFitDeviations}
\alias{fitted_compute_group_fun}
\alias{StatFitFitted}
\alias{residuals_compute_group_fun}
\alias{StatFitResiduals}
\alias{ma_eq_compute_group_fun}
\alias{StatMaEq}
\alias{ma_line_compute_group_fun}
\alias{StatMaLine}
\alias{multcomp_compute_fun}
\alias{StatMultcomp}
\alias{peaks_compute_group_fun}
\alias{valleys_compute_group_fun}
\alias{Stats}
\alias{StatPeaks}
\alias{StatValleys}
\alias{poly_eq_compute_group_fun}
\alias{StatPolyEq}
\alias{StatPolyLine}
\alias{quant_band_compute_group_fun}
\alias{StatQuantBand}
\alias{quant_eq_compute_group_fun}
\alias{StatQuantEq}
\alias{quant_line_compute_group_fun}
\alias{StatQuantLine}
\title{\code{Stat*} Objects}
\description{
All \code{stat_*} functions (like \code{stat_bin}) return a layer that
contains a \code{Stat*} object (like \code{StatBin}). The \code{Stat*}
object is responsible for rendering the data in the plot.
}
\details{
Each of the \code{Stat*} objects is a \code{\link[ggplot2]{ggproto}} object,
derived from the top-level \code{Stat}, and each implements various methods
and fields. To create a new type of Stat object, you typically will want to
implement one or more of the following:
}
\seealso{
\code{\link[ggplot2]{ggplot2-ggproto}}
}
\keyword{datasets}
\keyword{internal}
