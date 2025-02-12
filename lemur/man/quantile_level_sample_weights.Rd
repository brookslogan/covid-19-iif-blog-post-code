% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hayman-ensemble.R
\name{quantile_level_sample_weights}
\alias{quantile_level_sample_weights}
\title{Convert quantile levels to importance weights to assign to quantiles (in a particular way)}
\usage{
quantile_level_sample_weights(taus)
}
\description{
This is  a utility  function used  when approximating  the distribution  of a
random variable by a pmf based on a fixed selection of quantiles. It converts
the  quantile  levels  into  importance  sampling  weights  (more  precisely,
sampling probabilities / pmf y-values, as the output is normalized) to attach
to the quantiles (pdf x-values).
}
\details{
There  are multiple  ways to  go about  this task.  This approach appears to satisfy two desirable properties:
(a) \code{all.equal(result, rev(result))} if \code{all.equal(taus,rev(1-taus))}, and (b)\if{html}{\out{<div class="r">}}\preformatted{all.equal(q, weighted_quantile_type1(q, quantile_level_sample_weights_alt(taus), taus))
}\if{html}{\out{</div>}}

for all tested \code{q} and \code{taus}.

Other approaches  can be  formulated if  we think  of this  task as
interpolating  and extrapolating  an  ECDF from  its  defining values  by
optionally extending or contracting horizontal lines without crossing any
of its defining x values. The  approach for this function interpolates so
that internal  jumps occur halfway between  consecutive defining quantile
levels (\code{taus}).
}
\examples{

quantile_level_sample_weights(c(0.05,0.1,0.3,0.5,0.7,0.9,0.95))

library("pipeR")

set.seed(42L)
taus = runif(100L) \%>>\% c(1-.) \%>>\% sort()
all.equal(taus, rev(1-taus))
all.equal(quantile_level_sample_weights(taus), rev(quantile_level_sample_weights(taus)))

taus = sort(runif(100L))
q = qexp(taus)
all.equal(q, weighted_quantile_type1(q, quantile_level_sample_weights(taus), taus))

}
\seealso{
\code{\link{weighted_quantile_type1}}
}
