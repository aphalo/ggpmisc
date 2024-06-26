% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-eq-label.R
\name{typeset_numbers}
\alias{typeset_numbers}
\title{Typeset/format numbers preserving trailing zeros}
\usage{
typeset_numbers(eq.char, output.type)
}
\arguments{
\item{eq.char}{character A polynomial model equation as a character string.}

\item{output.type}{character One of "expression", "latex", "tex", "text",
"tikz", "markdown".}
}
\value{
A \code{character} string.
}
\description{
Typeset/format numbers preserving trailing zeros
}
\note{
exponential number notation to typeset equivalent: Protecting trailing
  zeros in negative numbers is more involved than I would like. Not only we
  need to enclose numbers in quotations marks but we also need to replace
  dashes with the minus character. I am not sure we can do the replacement
  portably, but that recent R supports UTF gives some hope.
}
