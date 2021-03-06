% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roll.R
\name{r}
\alias{r}
\title{Get full distribution of a roll}
\usage{
r(roll, precise = FALSE)
}
\arguments{
\item{roll}{A roll expression (e.g., \code{2 * d6 + 5}) or a data frame returned
by \code{\link[=r]{r()}}.}

\item{precise}{Whether to return values with arbitrary precision.}
}
\value{
A data frame with four columns: \code{outcome}, \code{n}, \code{d}, and \code{p}.
}
\description{
Return a data frame with most information necessary to work with the roll
distribution: a column containing the possible outcomes of the roll
expression, a column counting how many different ways each outcome can be
obtained, a column with the associated densities, and a column with the
associated probabilities. See below for more details.
}
\details{
A roll expression is a piece of R code that describes a dice roll with or
without modifiers, e.g., \code{2 * d6 + 5}. This function processes roll
expressions in order to power both the \link{roll} and \link{roll-plot} family of
functions. Given an expression of this form it calculates the complete
distribution of the outcomes. This is possible because the distribution is
discrete and has a finite number of outcomes.

Standard \href{https://en.wikipedia.org/wiki/Dice_notation}{dice notation} should
mostly work out of the box, with the notable exception of \code{NdX}, i.e., "roll
\code{N} dice with \code{X} faces and add the results". In this case, the user must
write \code{N * dX}; this also means that, when translating "roll a die with \code{X}
faces and multiply the result by \code{N}" to a roll expression, the user must
then write \code{dX * N}. All other expressions involving dice can usually be
pasted straight into these functions.

For more details on what operations are supported, see the \link{Dice} S4 class.
}
\section{Built-in Dice}{

It is possible to define any die with \code{\link[=d]{d()}}, but some are already built-in.
Because of R's restrictions on what kind of object can be exported, they are
not readily available for the user, but can be used inside a roll expression
nonetheless These are the standard D&D dice: \code{d4}, \code{d6}, \code{d8}, \code{d10}, \code{d12},
\code{d20}, and \code{d100}.
}

\section{Arbitrary Precision}{

Most dice programs that can calculate probabilities are forced to round
their results due to the fact that these quantities might become
exceptionally low when dealing with a lot of dice. This, however, can lead
to error magnification.

In order to avoid rounding as much as possible, all functions described here
use \code{\link[Ryacas:yac_str]{Ryacas::yac_str()}} to run computations symbolically. By default,
results are converted to numeric vectors just before returning to the user,
but one is able to access the symbolic strings returned by Ryacas by setting
\code{precise = TRUE}.
}

\examples{
# Get full distribution of 2d6 + 5
r(2 * d6 + 5)
}
\seealso{
\link{roll}, \link{roll-plot}, \link{Dice}.
}
