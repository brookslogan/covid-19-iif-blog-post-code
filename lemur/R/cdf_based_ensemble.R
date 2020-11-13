
#' @source hayman-ensemble.R
NULL


#' Get jump CDF from quantiles and raw probs, overweighting top prob
#'
#' Fits a piecewise constant CDF from the quantiles using approxfun, assigning
#' \code{1-max(probs)} mass to \code{max(quantiles)+epsilon}.
#'
#' @param quantiles vector of quantiles
#' @param probs the levels of these quantiles
panterpolate_raw_jump_cdf <- function(quantiles, probs) {
  approxfun(quantiles, probs,
            ties = "mean",
            method = "constant",
            yleft = 0, yright = 1)
}

#' Get jump CDF from quantiles
#'
#' Fits a piecewise constant CDF from the quantiles using approxfun, using
#' \code{\link{quantile_level_sample_weights}} to handle ambiguity in a "symmetric" way
#' at the low and high tails and make this correspond to a somewhat natural pdf.
#'
#' @param quantiles vector of quantiles
#' @param probs the levels of these quantiles
panterpolate_reweight_bad_jump_cdf <- function(quantiles, probs) {
  approxfun(
    quantiles, cumsum(quantile_level_sample_weights(probs)),
    ties = "mean",
    method = "constant",
    yleft = 0, yright = 1
  )
}

jump_cdf_from_pmf_contribs <- function(quantiles, level.weights) {
  if (length(quantiles) != length(level.weights) || is.unsorted(quantiles, na.rm=TRUE)) {
    stop ('quantiles must be sorted and of same length as level.weights')
  }
  if (any(level.weights < -1e-8) || sum(level.weights) <= 0) {
    stop ('level.weights should be nonnegative and contain at least one nonzero element')
  }
  ## if (!isTRUE(all.equal(sum(level.weights), 1))) {
  ##   print(all.equal(sum(level.weights), 1))
  ##   print(level.weights)
  ##   stop ('level.weights are not on simplex')
  ## }
  level.weights[level.weights < 0] <- 0
  level.weights <- level.weights/sum(level.weights)
  level.probs = cumsum(level.weights)
  to.skip = rev(duplicated(rev(quantiles)))
  quantiles <- quantiles[!to.skip]
  level.probs <- level.probs[!to.skip]
  level.weights <- diff(c(0,level.probs))
  subresult = approxfun(
    quantiles, level.probs,
    ## ties = "max",
    ties = "ordered",
    method = "constant",
    yleft = 0, yright = 1
  )
  result = function(v) subresult(v)
  class(result) <- "jump_cdf"
  return (result)
}

mean_jump_cdf = function(jump.cdfs) {
  if (!all(vapply(jump.cdfs, FUN.VALUE=logical(1L), inherits, "jump_cdf"))) {
    stop ('Expected all args to inherit "jump_cdf"')
  }
  quantiles = unlist(lapply(jump.cdfs, function(cdf) environment(cdf)[["quantiles"]]))
  level.weights = unlist(lapply(jump.cdfs, function(cdf) environment(cdf)[["level.weights"]]))
  ordering = order(quantiles)
  quantiles <- quantiles[ordering]
  level.weights <- level.weights[ordering]/length(jump.cdfs)
  return (jump_cdf_from_pmf_contribs(quantiles, level.weights))
}

#' Get jump CDF from quantiles
#'
#' Fits a piecewise constant CDF from the quantiles using approxfun, using
#' \code{\link{quantile_level_sample_weights}} to handle ambiguity in a "symmetric" way
#' at the low and high tails and make this correspond to a somewhat natural pdf.
#'
#' @param quantiles vector of quantiles
#' @param probs the levels of these quantiles
panterpolate_reweight2_jump_cdf <- function(quantiles, probs) {
  level.weights = quantile_level_sample_weights(probs)
  jump_cdf_from_pmf_contribs(quantiles, level.weights)
}

##' Calculates \int_{-\infty}^{+\infty} [F_1(z)F_2(z) - I(z > 0)] dz
offset_cdf_inner_product <- function(cdf1, cdf2, fallback, ...) UseMethod("offset_cdf_inner_product", cdf1)
offset_cdf_inner_product.default <- function(cdf1, cdf2, fallback, ...) fallback(cdf1, cdf2, ...)
## FIXME may need to report the minimum upper integration bound that is
## acceptable, requiring CDF's to correspond to upper-bounded RV's. A better
## approach, somewhat similar to the QP expression noted below for a
## transformation of the CRPS RV's with finite means, is to manipulate the
## objective so to avoid integrals that will become infinite with the full
## infinite limits using a different type of offset which will not take the
## integral argument below 0 ever in a more general way (probably involving
## terms dealing with cdf1 and cdf2 separately).

offset_cdf_inner_product.jump_cdf <- function(cdf1, cdf2, fallback, ...) UseMethod("offset_cdf_inner_product.jump_cdf", cdf2)
offset_cdf_inner_product.jump_cdf.default <- function(cdf1, cdf2, fallback, ...) fallback(cdf1, cdf2)
offset_cdf_inner_product.jump_cdf.jump_cdf <- function(cdf1, cdf2, fallback, ...) {
  env1 = environment(cdf1)
  env2 = environment(cdf2)
  quantiles1 = env1[["quantiles"]]
  quantiles2 = env2[["quantiles"]]
  zs = sort(unique(c(quantiles1,quantiles2,-42,+42))) # \pm 42 to ensure length long enough for 
  eval.zs = head(n=-1L, zs)
  result = sum(cdf1(eval.zs)*cdf2(eval.zs)*diff(zs)) - max(zs)
  result
}

meanabsdiff_proddist <- function(dist1, dist2, fallback, ...) UseMethod("meanabsdiff_proddist", dist1)
meanabsdiff_proddist.default <- function(dist1, dist2, fallback, ...) fallback(dist1, dist2, ...)

meanabsdiff_proddist.jump_cdf <- function(dist1, dist2, fallback, ...) UseMethod("meanabsdiff_proddist.jump_cdf", dist2)
meanabsdiff_proddist.jump_cdf.default <- function(dist1, dist2, fallback, ...) fallback(dist1, dist2)
meanabsdiff_proddist.jump_cdf.jump_cdf <- function(dist1, dist2, fallback, ...) {
  env1 = environment(dist1)
  env2 = environment(dist2)
  quantiles1 = env1[["quantiles"]]
  quantiles2 = env2[["quantiles"]]
  level.weights1 = env1[["level.weights"]]
  level.weights2 = env2[["level.weights"]]
  level.weights1 * level.weights2
  sum(tcrossprod(level.weights1, level.weights2)*abs(outer(quantiles1, quantiles2, `-`)))
}
## The convexity issue with this approach should probably be resolved by writing
## the ensemble CRPS as:
##
##   0.5 sumj1 sumj2 wj1 wj2 E E (|Xj1 - y| + |y - Xj2| - |Xj1 - Xj2|).

#' Get quantile ensemble by ruled-approximation-CRPS-based CDF stacking, v0
#'
#' This is intended to have the same interface as, or one similar to,
#' \code{\link{quantgen::quantile_ensemble}} function.
#'
#' This function is a work in progress.
#'
#' See \code{\link{quantgen::quantile_ensemble}} for arguments.
#'
#' @param panterpolate_cdf a function like \code{\link{approxfun}} to
#'   interpolate and extrapolate CDF values from a discrete set of xs and ys
#'   (quantiles and probs)
#'
#' @importFrom purrr map_dbl
#' @importFrom quadprog solve.QP
quantile_ensemble_via_ruled_crps_v0 <- function(qarr, y, tau, panterpolate_cdf,
                                             weights = NULL
                                             , prerange.constant.extension=1
                                             , prerange.proportional.extension=0.2
                                             ## , verbose = FALSE
                                             ) {
  ## Set up some basic objects that we will need:
  n = dim(qarr)[[1L]] # number of prediction points
  p = dim(qarr)[[2L]] # number of ensemble components
  r = dim(qarr)[[3L]] # number of quantile levels
  N = n * r

  if (p == 1L) {
    beta.hat = 1
    names(beta.hat) <- dimnames(qarr)[[2L]]
    return (beta.hat)
  }

  ## Adjust some arguments to fit expectations:
  storage.mode(qarr) <- "double"
  ## qarr <- unname(qarr)
  if (is.null(weights)) weights <- rep(1, n)

  cdfs = apply(qarr, 1:2, panterpolate_cdf, tau)

  ## Form an evenly-spaced ruler of candidate y values to use to form
  ## approximate CRPS:
  zs.prerange = range(c(y, qarr)) # not quite the range of the zs we will produce
  n.zs = 1000L
  zs = seq(zs.prerange[[1L]] - prerange.constant.extension - prerange.proportional.extension * diff(zs.prerange),
           zs.prerange[[2L]] + prerange.constant.extension + prerange.proportional.extension * diff(zs.prerange),
           length.out = n.zs)

  Q = matrix(0, p, p)
  for (j in seq_len(p)) {
    for (jj in seq_len(j)) {
      ## form function sum_i w_i * F_ij(x) * F_ij'(x)
      ## cat(j, jj, fill = TRUE)
      ## TODO modular way of swapping ruler-approximation vs. Integrate vs. specialized closed-form solutions
      ##
      ## TODO use form that subtracts from I(z >= 0) to make CRPS approximation comparable across fits with different z ranges?
      inner_prod_fun = function(x) sum(weights * map_dbl(cdfs[, j], ~ .x(x)) * map_dbl(cdfs[, jj], ~ .x(x)))
      Q[[j, jj]] <- sum(Vectorize(inner_prod_fun)(zs))*(diff(zs)[[1L]])
    }
  }
  Q <- Q + t(Q - diag(diag(Q)))

  d = matrix(0, p)
  for (j in seq_len(p)) {
    inner_prod_fun <- function(x) sum(weights * (y<=x) * map_dbl(cdfs[, j], ~ .x(x)))
    d[[j]] <- sum(Vectorize(inner_prod_fun)(zs))*(diff(zs)[[1L]])
  }

  A = cbind(rep(-1, p), diag(p))
  b = c(-1, rep(0, p))
  meq = 1L

  beta.hat = solve.QP(Q, d, A, b, meq)[["solution"]]
  names(beta.hat) <- dimnames(qarr)[[2L]]

  ## TODO fit and predict interface

  return (beta.hat)
}

#' Get quantile ensemble by ruled-approximation-CRPS-based CDF stacking, v1pre
#'
#' This is intended to have the same interface as, or one similar to,
#' \code{\link{quantgen::quantile_ensemble}} function.
#'
#' This function is a work in progress.
#'
#' See \code{\link{quantgen::quantile_ensemble}} for arguments.
#'
#' @param panterpolate_cdf a function like \code{\link{approxfun}} to
#'   interpolate and extrapolate CDF values from a discrete set of xs and ys
#'   (quantiles and probs)
#'
#' @importFrom purrr map_dbl
#' @importFrom quadprog solve.QP
quantile_ensemble_via_ruled_crps_v1pre <- function(qarr, y, tau, panterpolate_cdf,
                                                   weights = NULL
                                                 , prerange.constant.extension=1
                                                 , prerange.proportional.extension=0.2
                                                   ## , verbose = FALSE
                                                   ) {
  ## Set up some basic objects that we will need:
  n = dim(qarr)[[1L]] # number of prediction points
  p = dim(qarr)[[2L]] # number of ensemble components
  r = dim(qarr)[[3L]] # number of quantile levels
  N = n * r

  if (p == 1L) {
    beta.hat = 1
    names(beta.hat) <- dimnames(qarr)[[2L]]
    return (beta.hat)
  }

  ## Adjust some arguments to fit expectations:
  storage.mode(qarr) <- "double"
  ## qarr <- unname(qarr)
  if (is.null(weights)) weights <- rep(1, n)

  cdfs = apply(qarr, 1:2, panterpolate_cdf, tau)

  ## Form an evenly-spaced ruler of candidate y values to use to form
  ## approximate CRPS:
  zs.prerange = range(c(y, qarr), na.rm=TRUE) # not quite the range of the zs we will produce
  ## n.zs = 1000L
  ## n.zs = 3000L # FIXME even this appears too coarse now
  n.zs = 10000L
  zs = seq(zs.prerange[[1L]] - prerange.constant.extension - prerange.proportional.extension * diff(zs.prerange),
           zs.prerange[[2L]] + prerange.constant.extension + prerange.proportional.extension * diff(zs.prerange),
           length.out = n.zs)
  fallback_offset_cdf_inner_product = function(cdf1, cdf2, ...) {
    ## (uniformly-ruled/gridded trapezoidal integration rule)
    sum(cdf1(zs)*cdf2(zs)*c(0.5, rep(1, length(zs)-2L), 0.5))*(diff(zs)[[1L]])
    ## sum(cdf1(zs)*cdf2(zs)*c(0.5, rep(1, length(zs)-2L), 0.5))*(diff(zs)[[1L]]) - max(zs)
    ## sum(cdf1(zs)*cdf2(zs))*(diff(zs)[[1L]]) - max(zs)
    ## sum(cdf1(zs)*cdf2(zs))*(diff(zs)[[1L]])
  }

  Q = matrix(0, p, p)
  for (j in seq_len(p)) {
    for (jj in seq_len(j)) {
      ## form function sum_i w_i * F_ij(x) * F_ij'(x)
      ## cat(j, jj, fill = TRUE)
      ## inner_prod_fun = function(x) sum(weights * map_dbl(cdfs[, j], ~ .x(x)) * map_dbl(cdfs[, jj], ~ .x(x)))
      ## Q[[j, jj]] <- sum(Vectorize(inner_prod_fun)(zs))*(diff(zs)[[1L]])
      Q[[j, jj]] <- sum(vapply(seq_len(n), FUN.VALUE=numeric(1L), function(i) {
        weights[[i]]*fallback_offset_cdf_inner_product(cdfs[[i,j]], cdfs[[i,jj]])
      }))
      ## Q[[j, jj]] <- sum(vapply(seq_len(n), FUN.VALUE=numeric(1L), function(i) {
      ##   weights[[i]]*offset_cdf_inner_product(cdfs[[i,j]], cdfs[[i,jj]], fallback_offset_cdf_inner_product)
      ## }))
    }
  }
  Q <- Q + t(Q - diag(diag(Q)))

  d = matrix(0, p)
  for (j in seq_len(p)) {
    ## inner_prod_fun = function(x) sum(weights * (y<=x) * map_dbl(cdfs[, j], ~ .x(x)))
    ## d[[j]] <- sum(Vectorize(inner_prod_fun)(zs))*(diff(zs)[[1L]])
    d[[j]] <- sum(vapply(seq_len(n), FUN.VALUE=numeric(1L), function(i) {
      ## weights[[i]]*offset_cdf_inner_product(cdfs[[i,j]], jump_cdf_from_pmf_contribs(y[[i]],1), fallback_offset_cdf_inner_product)
      weights[[i]]*fallback_offset_cdf_inner_product(cdfs[[i,j]], jump_cdf_from_pmf_contribs(y[[i]],1))
    }))
  }

  ## We need Q to be positive semidefinite; however, the Q and d integrals can
  ## be arbitrarily large based on the upper limit of integration when using the
  ## function inner product form; a later implementation replaces these
  ## integrals of products with integrals of products minus I(z>0) to give a
  ## value that is independent of the upper bound of integration selected, but
  ## which can be negative and which can lead to non-PSD Q even after adjusting
  ## Q to have all non-negative entries and at least one 0. Readjust this
  ## "offset" in integrals inside both Q and d so that Q is PSD by just adding
  ## back the weights[[i]]*max(zs) terms, assuming that we are still using
  ## jump_cdf's with the largest quantile <= max(zs); note that entries of Q and
  ## d here are all single unscaled offset integrals and can be adjusted by the
  ## same amount.
  ##
  ## reoffset = -min(Q) # not sufficient to ensure Q will become PSD
  reoffset = sum(weights)*max(zs)
  Q <- Q + reoffset # * 1
  d <- d + reoffset # * 1

  ## ## Q could now be PSD but not PD, but QP solver used expects it to be PD; add a ridge penalty with heuristic regularization
  ## ##
  ## ## XXX maybe this should affect d as well, based on regularizing toward
  ## ## equal-weights vector. Maybe lambda should be selected with some validation
  ## ## strategy.
  ## lambda = mean(diag(Q)) / sum(weights) * 1e-0
  ## if (lambda == 0) {
  ##   lambda <- 1e-8
  ## }
  ## Q <- Q + lambda*diag(p)

  ## ## Large Q and d can cause issues in solve.QP, making it think that the
  ## ## simplex constraints below are unsatisfiable
  ## ## (https://stackoverflow.com/questions/28381855/r-function-solve-qp-error-constraints-are-inconsistent-no-solution);
  ## ## scale them down:
  ## scale.factor = max(Q, d)
  ## Q <- Q/scale.factor
  ## d <- d/scale.factor

  A = cbind(rep(-1, p), diag(p))
  b = c(-1, rep(0, p))
  meq = 1L

  beta.hat = solve.QP(Q, d, A, b, meq)[["solution"]]
  names(beta.hat) <- dimnames(qarr)[[2L]]

  ## TODO fit and predict interface

  return (beta.hat)
}

#' Get quantile ensemble by ruled-approximation-CRPS-based CDF stacking, v1
#'
#' This is intended to have the same interface as, or one similar to,
#' \code{\link{quantgen::quantile_ensemble}} function.
#'
#' This function is a work in progress.
#'
#' See \code{\link{quantgen::quantile_ensemble}} for arguments.
#'
#' @param panterpolate_cdf a function like \code{\link{approxfun}} to
#'   interpolate and extrapolate CDF values from a discrete set of xs and ys
#'   (quantiles and probs)
#'
#' @importFrom purrr map_dbl
#' @importFrom quadprog solve.QP
quantile_ensemble_via_ruled_crps_v1 <- function(qarr, y, tau, panterpolate_cdf,
                                             weights = NULL
                                             , prerange.constant.extension=1
                                             , prerange.proportional.extension=0.2
                                             ## , verbose = FALSE
                                             ) {
  ## Set up some basic objects that we will need:
  n = dim(qarr)[[1L]] # number of prediction points
  p = dim(qarr)[[2L]] # number of ensemble components
  r = dim(qarr)[[3L]] # number of quantile levels
  N = n * r

  if (p == 1L) {
    beta.hat = 1
    names(beta.hat) <- dimnames(qarr)[[2L]]
    return (beta.hat)
  }

  ## Adjust some arguments to fit expectations:
  storage.mode(qarr) <- "double"
  ## qarr <- unname(qarr)
  if (is.null(weights)) weights <- rep(1, n)

  cdfs = apply(qarr, 1:2, panterpolate_cdf, tau)

  ## Form an evenly-spaced ruler of candidate y values to use to form
  ## approximate CRPS:
  zs.prerange = range(c(y, qarr), na.rm=TRUE) # not quite the range of the zs we will produce
  ## n.zs = 1000L
  ## n.zs = 3000L # FIXME even this appears too coarse now
  n.zs = 10000L
  zs = seq(zs.prerange[[1L]] - prerange.constant.extension - prerange.proportional.extension * diff(zs.prerange),
           zs.prerange[[2L]] + prerange.constant.extension + prerange.proportional.extension * diff(zs.prerange),
           length.out = n.zs)
  fallback_offset_cdf_inner_product = function(cdf1, cdf2, ...) {
    ## (uniformly-ruled/gridded trapezoidal integration rule)
    sum(cdf1(zs)*cdf2(zs)*c(0.5, rep(1, length(zs)-2L), 0.5))*(diff(zs)[[1L]]) - max(zs)
    ## sum(cdf1(zs)*cdf2(zs))*(diff(zs)[[1L]]) - max(zs)
    ## sum(cdf1(zs)*cdf2(zs))*(diff(zs)[[1L]])
  }

  Q = matrix(0, p, p)
  for (j in seq_len(p)) {
    for (jj in seq_len(j)) {
      ## form function sum_i w_i * F_ij(x) * F_ij'(x)
      ## cat(j, jj, fill = TRUE)
      ## inner_prod_fun = function(x) sum(weights * map_dbl(cdfs[, j], ~ .x(x)) * map_dbl(cdfs[, jj], ~ .x(x)))
      ## Q[[j, jj]] <- sum(Vectorize(inner_prod_fun)(zs))*(diff(zs)[[1L]])
      ## Q[[j, jj]] <- sum(vapply(seq_len(n), FUN.VALUE=numeric(1L), function(i) {
      ##   weights[[i]]*fallback_offset_cdf_inner_product(cdfs[[i,j]], cdfs[[i,jj]])
      ## }))
      Q[[j, jj]] <- sum(vapply(seq_len(n), FUN.VALUE=numeric(1L), function(i) {
        weights[[i]]*offset_cdf_inner_product(cdfs[[i,j]], cdfs[[i,jj]], fallback_offset_cdf_inner_product)
      }))
    }
  }
  Q <- Q + t(Q - diag(diag(Q)))

  d = matrix(0, p)
  for (j in seq_len(p)) {
    ## inner_prod_fun = function(x) sum(weights * (y<=x) * map_dbl(cdfs[, j], ~ .x(x)))
    ## d[[j]] <- sum(Vectorize(inner_prod_fun)(zs))*(diff(zs)[[1L]])
    d[[j]] <- sum(vapply(seq_len(n), FUN.VALUE=numeric(1L), function(i) {
      weights[[i]]*offset_cdf_inner_product(cdfs[[i,j]], jump_cdf_from_pmf_contribs(y[[i]],1), fallback_offset_cdf_inner_product)
    }))
  }

  ## We need Q to be positive semidefinite; however, the Q and d integrals can
  ## be arbitrarily large based on the upper limit of integration when using the
  ## function inner product form; a later implementation replaces these
  ## integrals of products with integrals of products minus I(z>0) to give a
  ## value that is independent of the upper bound of integration selected, but
  ## which can be negative and which can lead to non-PSD Q even after adjusting
  ## Q to have all non-negative entries and at least one 0. Readjust this
  ## "offset" in integrals inside both Q and d so that Q is PSD by just adding
  ## back the weights[[i]]*max(zs) terms, assuming that we are still using
  ## jump_cdf's with the largest quantile <= max(zs); note that entries of Q and
  ## d here are all single unscaled offset integrals and can be adjusted by the
  ## same amount.
  ##
  ## reoffset = -min(Q) # not sufficient to ensure Q will become PSD
  reoffset = sum(weights)*max(zs)
  Q <- Q + reoffset # * 1
  d <- d + reoffset # * 1

  ## Q could now be PSD but not PD, but QP solver used expects it to be PD; add a ridge penalty with heuristic regularization
  ##
  ## XXX maybe this should affect d as well, based on regularizing toward
  ## equal-weights vector. Maybe lambda should be selected with some validation
  ## strategy.
  lambda = mean(diag(Q)) / sum(weights) * 1e-0
  if (lambda == 0) {
    lambda <- 1e-8
  }
  Q <- Q + lambda*diag(p)

  ## Large Q and d can cause issues in solve.QP, making it think that the
  ## simplex constraints below are unsatisfiable
  ## (https://stackoverflow.com/questions/28381855/r-function-solve-qp-error-constraints-are-inconsistent-no-solution);
  ## scale them down:
  scale.factor = max(Q, d)
  Q <- Q/scale.factor
  d <- d/scale.factor

  A = cbind(rep(-1, p), diag(p))
  b = c(-1, rep(0, p))
  meq = 1L

  beta.hat = solve.QP(Q, d, A, b, meq)[["solution"]]
  names(beta.hat) <- dimnames(qarr)[[2L]]

  ## TODO fit and predict interface

  return (beta.hat)
}

#' Get quantile ensemble by S3-or-ruled-approximation-CRPS-based CDF stacking with imputation
#'
#' This is intended to have the same interface as, or one similar to,
#' \code{\link{quantgen::quantile_ensemble}} function.
#'
#' This function is a work in progress.
#'
#' See \code{\link{quantgen::quantile_ensemble}} for arguments.
#'
#' @param panterpolate_cdf a function like \code{\link{approxfun}} to
#'   interpolate and extrapolate CDF values from a discrete set of xs and ys
#'   (quantiles and probs)
#'
#' @importFrom purrr map_dbl
#' @importFrom quadprog solve.QP
quantile_ensemble_via_ruled_crps_with_imputation <- function(qarr, y, tau, panterpolate_cdf,
                                                             weights = NULL
                                                           , prerange.constant.extension=1
                                                           , prerange.proportional.extension=0.2
                                                             ## , verbose = FALSE
                                                             ) {
  ## Set up some basic objects that we will need:
  n = dim(qarr)[[1L]] # number of prediction points
  p = dim(qarr)[[2L]] # number of ensemble components
  r = dim(qarr)[[3L]] # number of quantile levels
  N = n * r

  if (p == 1L) {
    beta.hat = 1
    names(beta.hat) <- dimnames(qarr)[[2L]]
    return (beta.hat)
  }

  ## Adjust some arguments to fit expectations:
  storage.mode(qarr) <- "double"
  ## qarr <- unname(qarr)
  if (is.null(weights)) weights <- rep(1, n)
  cdfs = qarr %>>%
    apply(1:2, function(quantiles) {
      if (all(is.na(quantiles))) {
        NULL
      } else {
        panterpolate_cdf(quantiles, tau)
      }
    }) %>>%
    apply(1L, function(cdfs.for.instance) {
      cdf.is.usable = !vapply(cdfs.for.instance, is.null, logical(1L))
      usable.cdfs = cdfs.for.instance[cdf.is.usable]
      ## ## XXX this wastes a little computation by repeating the imputed cdf
      ## ## calculations when there are multiple unusable cdf's and by not
      ## ## building on the panterpolaters to take multiple cdfs, but this might
      ## ## allow generalization more easily later where these would have
      ## ## different imputed cdf's
      ##
      ## imputed_cdf = function(v) {
      ##   rowMeans(vapply(usable.cdfs, function(cdf) cdf(v), numeric(length(v))))
      ## }
      imputed_cdf = mean_jump_cdf(usable.cdfs)
      cdfs.for.instance[!cdf.is.usable] <- list(imputed_cdf)
      cdfs.for.instance
    }) %>>%
    ## FIXME robustify
    simplify2array() %>>%
    t()

  ## Form an evenly-spaced ruler of candidate y values to use to form
  ## approximate CRPS:
  zs.prerange = range(c(y, qarr), na.rm=TRUE) # not quite the range of the zs we will produce
  ## n.zs = 1000L
  ## n.zs = 3000L # FIXME even this appears too coarse now
  n.zs = 10000L
  zs = seq(zs.prerange[[1L]] - prerange.constant.extension - prerange.proportional.extension * diff(zs.prerange),
           zs.prerange[[2L]] + prerange.constant.extension + prerange.proportional.extension * diff(zs.prerange),
           length.out = n.zs)
  fallback_offset_cdf_inner_product = function(cdf1, cdf2, ...) {
    ## (uniformly-ruled/gridded trapezoidal integration rule)
    sum(cdf1(zs)*cdf2(zs)*c(0.5, rep(1, length(zs)-2L), 0.5))*(diff(zs)[[1L]]) - max(zs)
    ## sum(cdf1(zs)*cdf2(zs))*(diff(zs)[[1L]]) - max(zs)
    ## sum(cdf1(zs)*cdf2(zs))*(diff(zs)[[1L]])
  }

  Q = matrix(0, p, p)
  for (j in seq_len(p)) {
    for (jj in seq_len(j)) {
      ## form function sum_i w_i * F_ij(x) * F_ij'(x)
      ## cat(j, jj, fill = TRUE)
      ## inner_prod_fun = function(x) sum(weights * map_dbl(cdfs[, j], ~ .x(x)) * map_dbl(cdfs[, jj], ~ .x(x)))
      ## Q[[j, jj]] <- sum(Vectorize(inner_prod_fun)(zs))*(diff(zs)[[1L]])
      ## Q[[j, jj]] <- sum(vapply(seq_len(n), FUN.VALUE=numeric(1L), function(i) {
      ##   weights[[i]]*fallback_offset_cdf_inner_product(cdfs[[i,j]], cdfs[[i,jj]])
      ## }))
      Q[[j, jj]] <- sum(vapply(seq_len(n), FUN.VALUE=numeric(1L), function(i) {
        weights[[i]]*offset_cdf_inner_product(cdfs[[i,j]], cdfs[[i,jj]], fallback_offset_cdf_inner_product)
      }))
    }
  }
  Q <- Q + t(Q - diag(diag(Q)))

  d = matrix(0, p)
  for (j in seq_len(p)) {
    ## inner_prod_fun = function(x) sum(weights * (y<=x) * map_dbl(cdfs[, j], ~ .x(x)))
    ## d[[j]] <- sum(Vectorize(inner_prod_fun)(zs))*(diff(zs)[[1L]])
    d[[j]] <- sum(vapply(seq_len(n), FUN.VALUE=numeric(1L), function(i) {
      weights[[i]]*offset_cdf_inner_product(cdfs[[i,j]], jump_cdf_from_pmf_contribs(y[[i]],1), fallback_offset_cdf_inner_product)
    }))
  }

  ## We need Q to be positive semidefinite; however, the Q and d integrals can
  ## be arbitrarily large based on the upper limit of integration when using the
  ## function inner product form; a later implementation replaces these
  ## integrals of products with integrals of products minus I(z>0) to give a
  ## value that is independent of the upper bound of integration selected, but
  ## which can be negative and which can lead to non-PSD Q even after adjusting
  ## Q to have all non-negative entries and at least one 0. Readjust this
  ## "offset" in integrals inside both Q and d so that Q is PSD by just adding
  ## back the weights[[i]]*max(zs) terms, assuming that we are still using
  ## jump_cdf's with the largest quantile <= max(zs); note that entries of Q and
  ## d here are all single unscaled offset integrals and can be adjusted by the
  ## same amount.
  ##
  ## reoffset = -min(Q) # not sufficient to ensure Q will become PSD
  reoffset = sum(weights)*max(zs)
  Q <- Q + reoffset # * 1
  d <- d + reoffset # * 1

  ## Q could now be PSD but not PD, but QP solver used expects it to be PD; add a ridge penalty with heuristic regularization
  ##
  ## XXX maybe this should affect d as well, based on regularizing toward
  ## equal-weights vector. Maybe lambda should be selected with some validation
  ## strategy.
  lambda = mean(diag(Q)) / sum(weights) * 1e-0
  if (lambda == 0) {
    lambda <- 1e-8
  }
  Q <- Q + lambda*diag(p)

  ## Large Q and d can cause issues in solve.QP, making it think that the
  ## simplex constraints below are unsatisfiable
  ## (https://stackoverflow.com/questions/28381855/r-function-solve-qp-error-constraints-are-inconsistent-no-solution);
  ## scale them down:
  scale.factor = max(Q, d)
  Q <- Q/scale.factor
  d <- d/scale.factor

  ## sum to 1, >=0
  A = cbind(rep(-1, p), diag(p))
  b = c(-1, rep(0, p))
  meq = 1L

  ## .GlobalEnv[["debug.env"]] <- environment()
  beta.hat.undiscounted = solve.QP(Q, d, A, b, meq)[["solution"]]
  names(beta.hat.undiscounted) <- dimnames(qarr)[[2L]]

  cdf.was.usable = apply(is.na(qarr), 1:2, sum) != dim(qarr)[[3L]]
  beta.hat.contributions = array(NA_real_, dim(cdfs), dimnames(cdfs))
  for (instance.i in seq_len(nrow(cdfs))) {
    ## FIXME use instance weights
    cdf.is.usable = cdf.was.usable[instance.i,]
    beta.hat.contributions[instance.i, !cdf.is.usable] <- 0
    beta.hat.contributions[instance.i, cdf.is.usable] <-
      beta.hat.undiscounted[cdf.is.usable] + sum(beta.hat.undiscounted[!cdf.is.usable])/sum(cdf.is.usable)
  }
  beta.hat = colMeans(beta.hat.contributions)

  ## TODO fit and predict interface

  return (beta.hat)
}

#' Form a cdf-stacking v0 forecast
#'
#' @param response length-1 character vector, optionally named; the response for
#'   which to generate a forecast; this ensemble is formed from components with
#'   a response matching either the content or name of this argument
#' @param incidence_period length-1 character vector; the
#'   \code{incidence_period} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{incidence_period}
#' @param ahead length-1 integer-valued \code{is.numeric}; the \code{ahead} for
#'   which to generate a forecast; this ensemble is formed from components with
#'   the same \code{ahead}
#' @param forecast_date ignored (to completely discover whether an ensemble can
#'   be formed requires reading card files to find whether they are NA, but this
#'   makes \code{\link{quantileEnsemble::get_forecasters}} run slowly;
#'   currently, no checks are done on the "outer" \code{forecast_date} fed to
#'   this function, and only the \code{forecast_date} fed to a forecaster
#'   function output from this function is considered)
#' @param geo_type length-1 character vector; the \code{geo_type} for which to
#'   generate a forecast; this ensemble is formed from components with the same
#'   \code{geo_type}
#' @param n_locations length-1 integer-valued \code{is.numeric}; the
#'   \code{n_locations} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{n_locations}
#' @param component_forecaster_names names of forecasters to try to include
#' @param shared_naming_scheme (re)naming scheme used as default for historical,
#'   prospective, and hub forecasters; see details
#' @param repo_root_dirpath length-1 character vector; path to root of the
#'   covidcast-forecast repository working directory from which to read
#'   component forecasts; defaults to the lowest ancestor of the \code{getwd}
#'   with a .git file/directory
#' @param historical_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for historical_components_dirpath
#' @param prospective_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for prospective_components_dirpath
#' @param hub_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for hub_components_dirpath
#' @param historical_components_dirpath override for the path to the directory
#'   (tree) containing historical component prediction/score cards
#' @param prospective_components_dirpath override for the path to the directory
#'   (tree) containing prospective component prediction/score cards
#' @param hub_components_dirpath override for the path to the directory (tree)
#'   containing COVID-19 Forecasting Hub component prediction/score cards
#'
#' @details Each (re)naming scheme is a character vector: each named vector
#'   entry denotes renaming forecasters with the entry's name to the entry
#'   itself; each unnamed vector entry denotes a name that is expected to be
#'   found somewhere in the searched paths; each entry (named or unnamed) is
#'   checked for appearance somewhere in any of the searched paths. However, the
#'   converse is not checked, i.e., it is fine for there to be paths found that
#'   do not correspond to an entry in the naming schemes.
#'
#' @importFrom data.table is.data.table
#' @importFrom pipeR %>>%
cdf_stacking_forecaster_v0 <- function(response, incidence_period, ahead, geo_type, n_locations,
                                       component_forecaster_names,
                                       panterpolate_cdf,
                                       n_training_dates = 3L,
                                       n_required_instances_per_forecaster = 10L,
                                       prerange.constant.extension=1,
                                       prerange.proportional.extension=0.2,
                                       shared_naming_scheme=character(0L),
                                       repo_root_dirpath=get_repo_root_dirpath(),
                                       historical_naming_scheme=shared_naming_scheme,
                                       prospective_naming_scheme=shared_naming_scheme,
                                       hub_naming_scheme=shared_naming_scheme,
                                       historical_components_dirpath=get_historical_components_dirpath(repo_root_dirpath),
                                       prospective_components_dirpath=get_prospective_components_dirpath(repo_root_dirpath),
                                       hub_components_dirpath=get_hub_components_dirpath(repo_root_dirpath)) {
    if (incidence_period != "epiweek") {
        NA
    } else {
            function(df, forecast_date) {
                target_locations = df[["location"]][df[["variable_name"]]=="location_to_be_forecast" & df[["value"]]==1]
                rm(df)
                training_forecast_dates = forecast_date - 7L*(ahead-1L+seq_len(n_training_dates))

                fetch_available_components_forecast_distributions = function(forecast_dates, file_basename) {
                  read_backoff_ensemble_components_dt(
                    response, incidence_period, ahead, forecast_dates, geo_type, n_locations,
                    only_read_forecaster_names = component_forecaster_names,
                    file_basename = file_basename,
                    shared_naming_scheme=shared_naming_scheme,
                    historical_components_dirpath=historical_components_dirpath,
                    prospective_components_dirpath=prospective_components_dirpath,
                    hub_components_dirpath=hub_components_dirpath,
                    historical_naming_scheme=historical_naming_scheme,
                    prospective_naming_scheme=prospective_naming_scheme,
                    hub_naming_scheme=hub_naming_scheme
                  ) %>>%
                    desired_components_predcards_dt_or_feedback(FALSE, character(0L), FALSE, component_forecaster_names) %>>%
                    {if (is.character(.)) stop(.) else .} %>>%
                    set(,"forecast_date", NULL) %>>%
                    unnest_dt_column_in_dt("predcard_dt") %>>%
                    {.[!vapply(.[["forecast_distribution_dt"]], is.null, logical(1L))]} %>>%
                    {.[!vapply(.[["forecast_distribution_dt"]], function(forecast.distribution.dt) any(is.na(forecast.distribution.dt[["quantiles"]])), logical(1L))]} %>>%
                    ## {.[!vapply(.[["forecast_distribution_dt"]], function(forecast.distribution.dt) all(is.na(forecast.distribution.dt[["quantiles"]])), logical(1L))]} %>>%
                    identity()
                }

                testing_forecast_distributions = fetch_available_components_forecast_distributions(forecast_date, "out.RDS")
                training_forecast_distributions = fetch_available_components_forecast_distributions(training_forecast_dates, "scorecard.RDS")
                set(training_forecast_distributions,,"instance", stri_paste(training_forecast_distributions[["forecast_date"]],";",training_forecast_distributions[["location"]]))

                available_forecasters_by_location = (
                  rbindlist(list(testing_forecast_distributions, training_forecast_distributions), fill=TRUE)
                  [, list(n_dates_available=.N), by=c("forecaster_name","location")]
                  [n_dates_available == 1L + n_training_dates]
                  [, list(forecaster_names = list(forecaster_name)), by = "location"]
                )
                if (nrow(available_forecasters_by_location) == 0L) {
                  print("Not enough data to fit for any location")
                  return (NA)
                }
                unique_forecaster_sets = unique(available_forecasters_by_location[["forecaster_names"]])
                print(sprintf("response: %s", response))
                print(sprintf("geo_type: %s", geo_type))
                print(sprintf("Ahead: %d", ahead))
                print(sprintf("Train: %s", toString(training_forecast_dates)))
                print(sprintf("Pred: %s", toString(forecast_date)))
                print(sprintf("Producing %d fits...", length(unique_forecaster_sets)))
                fits = lapply(unique_forecaster_sets, function(forecaster_names) {
                  ## FIXME use corrected DF's to get actual
                  selected_training_forecast_distributions = (
                    training_forecast_distributions
                    [forecaster_names, on = "forecaster_name", nomatch = NULL]
                    [, n_available_forecasters := .N, by = "instance"]
                    [n_available_forecasters == length(forecaster_names)]
                  )
                  selected_instances = unique(selected_training_forecast_distributions[["instance"]])
                  selected_indices = CJ(forecaster_name=forecaster_names, instance=selected_instances)
                  ordered_selected_training_forecast_distributions =
                    selected_training_forecast_distributions[selected_indices, on=c("forecaster_name", "instance"), nomatch=NA]
                  qarr = aperm(array(
                    sapply(ordered_selected_training_forecast_distributions[["forecast_distribution_dt"]], `[[`, "quantiles"),
                    c(length(cdc_probs), length(unique(selected_training_forecast_distributions[["instance"]])), length(forecaster_names)),
                    ), c(2:3, 1L))
                  dimnames(qarr) <- list(
                    instance = unique(selected_training_forecast_distributions[["instance"]]),
                    forecaster_name = forecaster_names,
                    prob = as.character(cdc_probs)
                  )
                  y = (
                    ordered_selected_training_forecast_distributions
                    [, list(actual=actual[[1L]]), by="instance"]
                    [selected_instances, on="instance", nomatch=NA]
                    [, setNames(actual, instance)]
                  )
                  stopifnot(identical(dimnames(qarr)[[1L]], names(y)))
                  include_instance = !is.na(y)
                  qarr <- qarr[include_instance,,,drop=FALSE]
                  y <- y[include_instance]
                  min_length_y = n_required_instances_per_forecaster * length(forecaster_names)
                  beta_hats =
                    if (length(y) < min_length_y) {
                      ## FIXME just don't produce forecasts instead?
                      print(sprintf("Using uniform weights as have only %d training instances but require %f = %f * %d", length(y), min_length_y, n_required_instances_per_forecaster, length(forecaster_names)))
                      rep(1/length(forecaster_names), length(forecaster_names))
                    } else {
                      quantile_ensemble_via_ruled_crps_v0(qarr, y, cdc_probs, panterpolate_cdf, prerange.constant.extension=prerange.constant.extension, prerange.proportional.extension=prerange.proportional.extension)
                    }
                  fit = setDT(data.frame(forecaster_name = forecaster_names, beta_hat = beta_hats))
                  if (any(fit[["beta_hat"]] < -1e-8)) {
                    stop (sprintf('Fit had beta_hat < -1e-8; min was %f', min(fit[["beta_hat"]])))
                  }
                  fit[["beta_hat"]][fit[["beta_hat"]] < 0] <- 0
                  cat(sprintf("Testing locations: %s\n", toString(testing_locations)))
                  print(copy(fit)[, rounded:=round(beta_hat, 4L)][])
                  fit
                })
                set(available_forecasters_by_location,,"fit", fits[match(available_forecasters_by_location[["forecaster_names"]], unique_forecaster_sets)])
                set(available_forecasters_by_location,,"forecaster_names", NULL)

                predcard_dt = validate_predcard_dt(
                  testing_forecast_distributions
                  [unnest_dt_column_in_dt(available_forecasters_by_location, "fit"),
                   on=c("location","forecaster_name"), nomatch=NA]
                  [, forecast_distribution := lapply(forecast_distribution_dt, function(forcast.distribution.dt) {
                    ## FIXME just make cdc_probs match the scorecard probs? right now there are numerical issues in construction
                    set(forcast.distribution.dt,,"probs", cdc_probs)
                  })]
                  [, unnest_dt_column_in_dt(.SD, "forecast_distribution_dt")]
                  [, level_weights := quantile_level_sample_weights(probs), by=c("forecast_date","location","forecaster_name")]
                  [, list(forecast_distribution_dt=list(setDT(data.frame(
                            probs=cdc_probs,
                            quantiles=weighted_quantile_type1(quantiles, beta_hat*level_weights, cdc_probs, na.rm=TRUE)
                          )))), by=c("forecast_date","location")]
                )
                result = as_tibble(unnest_dt_column_in_dt(predcard_dt[,list(location,forecast_distribution_dt)],"forecast_distribution_dt"))
                result <- result[result[["location"]] %in% target_locations,]
                rm(training_forecast_distributions, testing_forecast_distributions, predcard_dt)
                gc()
                return (result)
            }
    }
}

#' Form a cdf-stacking v1pre forecast
#'
#' @param response length-1 character vector, optionally named; the response for
#'   which to generate a forecast; this ensemble is formed from components with
#'   a response matching either the content or name of this argument
#' @param incidence_period length-1 character vector; the
#'   \code{incidence_period} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{incidence_period}
#' @param ahead length-1 integer-valued \code{is.numeric}; the \code{ahead} for
#'   which to generate a forecast; this ensemble is formed from components with
#'   the same \code{ahead}
#' @param forecast_date ignored (to completely discover whether an ensemble can
#'   be formed requires reading card files to find whether they are NA, but this
#'   makes \code{\link{quantileEnsemble::get_forecasters}} run slowly;
#'   currently, no checks are done on the "outer" \code{forecast_date} fed to
#'   this function, and only the \code{forecast_date} fed to a forecaster
#'   function output from this function is considered)
#' @param geo_type length-1 character vector; the \code{geo_type} for which to
#'   generate a forecast; this ensemble is formed from components with the same
#'   \code{geo_type}
#' @param n_locations length-1 integer-valued \code{is.numeric}; the
#'   \code{n_locations} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{n_locations}
#' @param component_forecaster_names names of forecasters to try to include
#' @param shared_naming_scheme (re)naming scheme used as default for historical,
#'   prospective, and hub forecasters; see details
#' @param repo_root_dirpath length-1 character vector; path to root of the
#'   covidcast-forecast repository working directory from which to read
#'   component forecasts; defaults to the lowest ancestor of the \code{getwd}
#'   with a .git file/directory
#' @param historical_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for historical_components_dirpath
#' @param prospective_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for prospective_components_dirpath
#' @param hub_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for hub_components_dirpath
#' @param historical_components_dirpath override for the path to the directory
#'   (tree) containing historical component prediction/score cards
#' @param prospective_components_dirpath override for the path to the directory
#'   (tree) containing prospective component prediction/score cards
#' @param hub_components_dirpath override for the path to the directory (tree)
#'   containing COVID-19 Forecasting Hub component prediction/score cards
#'
#' @details Each (re)naming scheme is a character vector: each named vector
#'   entry denotes renaming forecasters with the entry's name to the entry
#'   itself; each unnamed vector entry denotes a name that is expected to be
#'   found somewhere in the searched paths; each entry (named or unnamed) is
#'   checked for appearance somewhere in any of the searched paths. However, the
#'   converse is not checked, i.e., it is fine for there to be paths found that
#'   do not correspond to an entry in the naming schemes.
#'
#' @importFrom data.table is.data.table
#' @importFrom pipeR %>>%
cdf_stacking_forecaster_v1pre <- function(response, incidence_period, ahead, geo_type, n_locations,
                                          component_forecaster_names,
                                          panterpolate_cdf,
                                          n_training_dates = 3L,
                                          n_required_instances_per_forecaster = 10L,
                                          prerange.constant.extension=1,
                                          prerange.proportional.extension=0.2,
                                          shared_naming_scheme=character(0L),
                                          repo_root_dirpath=get_repo_root_dirpath(),
                                          historical_naming_scheme=shared_naming_scheme,
                                          prospective_naming_scheme=shared_naming_scheme,
                                          hub_naming_scheme=shared_naming_scheme,
                                          historical_components_dirpath=get_historical_components_dirpath(repo_root_dirpath),
                                          prospective_components_dirpath=get_prospective_components_dirpath(repo_root_dirpath),
                                          hub_components_dirpath=get_hub_components_dirpath(repo_root_dirpath)) {
    if (incidence_period != "epiweek") {
        NA
    } else {
            function(df, forecast_date) {
                target_locations = df[["location"]][df[["variable_name"]]=="location_to_be_forecast" & df[["value"]]==1]
                rm(df)
                training_forecast_dates = forecast_date - 7L*(ahead-1L+seq_len(n_training_dates))

                fetch_available_components_forecast_distributions = function(forecast_dates, file_basename) {
                  read_backoff_ensemble_components_dt(
                    response, incidence_period, ahead, forecast_dates, geo_type, n_locations,
                    only_read_forecaster_names = component_forecaster_names,
                    file_basename = file_basename,
                    shared_naming_scheme=shared_naming_scheme,
                    historical_components_dirpath=historical_components_dirpath,
                    prospective_components_dirpath=prospective_components_dirpath,
                    hub_components_dirpath=hub_components_dirpath,
                    historical_naming_scheme=historical_naming_scheme,
                    prospective_naming_scheme=prospective_naming_scheme,
                    hub_naming_scheme=hub_naming_scheme
                  ) %>>%
                    desired_components_predcards_dt_or_feedback(FALSE, character(0L), FALSE, component_forecaster_names) %>>%
                    {if (is.character(.)) stop(.) else .} %>>%
                    set(,"forecast_date", NULL) %>>%
                    unnest_dt_column_in_dt("predcard_dt") %>>%
                    {.[!vapply(.[["forecast_distribution_dt"]], is.null, logical(1L))]} %>>%
                    {.[!vapply(.[["forecast_distribution_dt"]], function(forecast.distribution.dt) any(is.na(forecast.distribution.dt[["quantiles"]])), logical(1L))]} %>>%
                    ## {.[!vapply(.[["forecast_distribution_dt"]], function(forecast.distribution.dt) all(is.na(forecast.distribution.dt[["quantiles"]])), logical(1L))]} %>>%
                    identity()
                }

                testing_forecast_distributions = fetch_available_components_forecast_distributions(forecast_date, "out.RDS")
                training_forecast_distributions = fetch_available_components_forecast_distributions(training_forecast_dates, "scorecard.RDS")
                set(training_forecast_distributions,,"instance", stri_paste(training_forecast_distributions[["forecast_date"]],";",training_forecast_distributions[["location"]]))

                available_forecasters_by_location = (
                  rbindlist(list(testing_forecast_distributions, training_forecast_distributions), fill=TRUE)
                  [, list(n_dates_available=.N), by=c("forecaster_name","location")]
                  [n_dates_available == 1L + n_training_dates]
                  [, list(forecaster_names = list(forecaster_name)), by = "location"]
                )
                if (nrow(available_forecasters_by_location) == 0L) {
                  print("Not enough data to fit for any location")
                  return (NA)
                }
                unique_forecaster_sets = unique(available_forecasters_by_location[["forecaster_names"]])
                print(sprintf("response: %s", response))
                print(sprintf("geo_type: %s", geo_type))
                print(sprintf("Ahead: %d", ahead))
                print(sprintf("Train: %s", toString(training_forecast_dates)))
                print(sprintf("Pred: %s", toString(forecast_date)))
                print(sprintf("Producing %d fits...", length(unique_forecaster_sets)))
                fits = lapply(unique_forecaster_sets, function(forecaster_names) {
                  ## FIXME use corrected DF's to get actual
                  selected_training_forecast_distributions = (
                    training_forecast_distributions
                    [forecaster_names, on = "forecaster_name", nomatch = NULL]
                    [, n_available_forecasters := .N, by = "instance"]
                    [n_available_forecasters == length(forecaster_names)]
                  )
                  selected_instances = unique(selected_training_forecast_distributions[["instance"]])
                  selected_indices = CJ(forecaster_name=forecaster_names, instance=selected_instances)
                  ordered_selected_training_forecast_distributions =
                    selected_training_forecast_distributions[selected_indices, on=c("forecaster_name", "instance"), nomatch=NA]
                  qarr = aperm(array(
                    sapply(ordered_selected_training_forecast_distributions[["forecast_distribution_dt"]], `[[`, "quantiles"),
                    c(length(cdc_probs), length(unique(selected_training_forecast_distributions[["instance"]])), length(forecaster_names)),
                    ), c(2:3, 1L))
                  dimnames(qarr) <- list(
                    instance = unique(selected_training_forecast_distributions[["instance"]]),
                    forecaster_name = forecaster_names,
                    prob = as.character(cdc_probs)
                  )
                  y = (
                    ordered_selected_training_forecast_distributions
                    [, list(actual=actual[[1L]]), by="instance"]
                    [selected_instances, on="instance", nomatch=NA]
                    [, setNames(actual, instance)]
                  )
                  stopifnot(identical(dimnames(qarr)[[1L]], names(y)))
                  include_instance = !is.na(y)
                  qarr <- qarr[include_instance,,,drop=FALSE]
                  y <- y[include_instance]
                  min_length_y = n_required_instances_per_forecaster * length(forecaster_names)
                  beta_hats =
                    if (length(y) < min_length_y) {
                      ## FIXME just don't produce forecasts instead?
                      print(sprintf("Using uniform weights as have only %d training instances but require %f = %f * %d", length(y), min_length_y, n_required_instances_per_forecaster, length(forecaster_names)))
                      rep(1/length(forecaster_names), length(forecaster_names))
                    } else {
                      quantile_ensemble_via_ruled_crps_v1pre(qarr, y, cdc_probs, panterpolate_cdf, prerange.constant.extension=prerange.constant.extension, prerange.proportional.extension=prerange.proportional.extension)
                    }
                  fit = setDT(data.frame(forecaster_name = forecaster_names, beta_hat = beta_hats))
                  if (any(fit[["beta_hat"]] < -1e-8)) {
                    stop (sprintf('Fit had beta_hat < -1e-8; min was %f', min(fit[["beta_hat"]])))
                  }
                  fit[["beta_hat"]][fit[["beta_hat"]] < 0] <- 0
                  print(copy(fit)[, rounded:=round(beta_hat, 4L)][])
                  fit
                })
                set(available_forecasters_by_location,,"fit", fits[match(available_forecasters_by_location[["forecaster_names"]], unique_forecaster_sets)])
                set(available_forecasters_by_location,,"forecaster_names", NULL)

                predcard_dt = validate_predcard_dt(
                  testing_forecast_distributions
                  [unnest_dt_column_in_dt(available_forecasters_by_location, "fit"),
                   on=c("location","forecaster_name"), nomatch=NA]
                  [, forecast_distribution := lapply(forecast_distribution_dt, function(forcast.distribution.dt) {
                    ## FIXME just make cdc_probs match the scorecard probs? right now there are numerical issues in construction
                    set(forcast.distribution.dt,,"probs", cdc_probs)
                  })]
                  [, unnest_dt_column_in_dt(.SD, "forecast_distribution_dt")]
                  [, level_weights := quantile_level_sample_weights(probs), by=c("forecast_date","location","forecaster_name")]
                  [, list(forecast_distribution_dt=list(setDT(data.frame(
                            probs=cdc_probs,
                            quantiles=weighted_quantile_type1(quantiles, beta_hat*level_weights, cdc_probs, na.rm=TRUE)
                          )))), by=c("forecast_date","location")]
                )
                result = as_tibble(unnest_dt_column_in_dt(predcard_dt[,list(location,forecast_distribution_dt)],"forecast_distribution_dt"))
                result <- result[result[["location"]] %in% target_locations,]
                rm(training_forecast_distributions, testing_forecast_distributions, predcard_dt)
                gc()
                return (result)
            }
    }
}

#' Form a cdf-stacking v1 forecast
#'
#' @param response length-1 character vector, optionally named; the response for
#'   which to generate a forecast; this ensemble is formed from components with
#'   a response matching either the content or name of this argument
#' @param incidence_period length-1 character vector; the
#'   \code{incidence_period} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{incidence_period}
#' @param ahead length-1 integer-valued \code{is.numeric}; the \code{ahead} for
#'   which to generate a forecast; this ensemble is formed from components with
#'   the same \code{ahead}
#' @param forecast_date ignored (to completely discover whether an ensemble can
#'   be formed requires reading card files to find whether they are NA, but this
#'   makes \code{\link{quantileEnsemble::get_forecasters}} run slowly;
#'   currently, no checks are done on the "outer" \code{forecast_date} fed to
#'   this function, and only the \code{forecast_date} fed to a forecaster
#'   function output from this function is considered)
#' @param geo_type length-1 character vector; the \code{geo_type} for which to
#'   generate a forecast; this ensemble is formed from components with the same
#'   \code{geo_type}
#' @param n_locations length-1 integer-valued \code{is.numeric}; the
#'   \code{n_locations} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{n_locations}
#' @param component_forecaster_names names of forecasters to try to include
#' @param shared_naming_scheme (re)naming scheme used as default for historical,
#'   prospective, and hub forecasters; see details
#' @param repo_root_dirpath length-1 character vector; path to root of the
#'   covidcast-forecast repository working directory from which to read
#'   component forecasts; defaults to the lowest ancestor of the \code{getwd}
#'   with a .git file/directory
#' @param historical_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for historical_components_dirpath
#' @param prospective_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for prospective_components_dirpath
#' @param hub_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for hub_components_dirpath
#' @param historical_components_dirpath override for the path to the directory
#'   (tree) containing historical component prediction/score cards
#' @param prospective_components_dirpath override for the path to the directory
#'   (tree) containing prospective component prediction/score cards
#' @param hub_components_dirpath override for the path to the directory (tree)
#'   containing COVID-19 Forecasting Hub component prediction/score cards
#'
#' @details Each (re)naming scheme is a character vector: each named vector
#'   entry denotes renaming forecasters with the entry's name to the entry
#'   itself; each unnamed vector entry denotes a name that is expected to be
#'   found somewhere in the searched paths; each entry (named or unnamed) is
#'   checked for appearance somewhere in any of the searched paths. However, the
#'   converse is not checked, i.e., it is fine for there to be paths found that
#'   do not correspond to an entry in the naming schemes.
#'
#' @importFrom data.table is.data.table
#' @importFrom pipeR %>>%
cdf_stacking_forecaster_v1 <- function(response, incidence_period, ahead, geo_type, n_locations,
                                       component_forecaster_names,
                                       panterpolate_cdf,
                                       n_training_dates = 3L,
                                       n_required_instances_per_forecaster = 10L,
                                       prerange.constant.extension=1,
                                       prerange.proportional.extension=0.2,
                                       shared_naming_scheme=character(0L),
                                       repo_root_dirpath=get_repo_root_dirpath(),
                                       historical_naming_scheme=shared_naming_scheme,
                                       prospective_naming_scheme=shared_naming_scheme,
                                       hub_naming_scheme=shared_naming_scheme,
                                       historical_components_dirpath=get_historical_components_dirpath(repo_root_dirpath),
                                       prospective_components_dirpath=get_prospective_components_dirpath(repo_root_dirpath),
                                       hub_components_dirpath=get_hub_components_dirpath(repo_root_dirpath)) {
    if (incidence_period != "epiweek") {
        NA
    } else {
            function(df, forecast_date) {
                target_locations = df[["location"]][df[["variable_name"]]=="location_to_be_forecast" & df[["value"]]==1]
                rm(df)
                training_forecast_dates = forecast_date - 7L*(ahead-1L+seq_len(n_training_dates))

                fetch_available_components_forecast_distributions = function(forecast_dates, file_basename) {
                  read_backoff_ensemble_components_dt(
                    response, incidence_period, ahead, forecast_dates, geo_type, n_locations,
                    only_read_forecaster_names = component_forecaster_names,
                    file_basename = file_basename,
                    shared_naming_scheme=shared_naming_scheme,
                    historical_components_dirpath=historical_components_dirpath,
                    prospective_components_dirpath=prospective_components_dirpath,
                    hub_components_dirpath=hub_components_dirpath,
                    historical_naming_scheme=historical_naming_scheme,
                    prospective_naming_scheme=prospective_naming_scheme,
                    hub_naming_scheme=hub_naming_scheme
                  ) %>>%
                    desired_components_predcards_dt_or_feedback(FALSE, character(0L), FALSE, component_forecaster_names) %>>%
                    {if (is.character(.)) stop(.) else .} %>>%
                    set(,"forecast_date", NULL) %>>%
                    unnest_dt_column_in_dt("predcard_dt") %>>%
                    {.[!vapply(.[["forecast_distribution_dt"]], is.null, logical(1L))]} %>>%
                    {.[!vapply(.[["forecast_distribution_dt"]], function(forecast.distribution.dt) any(is.na(forecast.distribution.dt[["quantiles"]])), logical(1L))]} %>>%
                    ## {.[!vapply(.[["forecast_distribution_dt"]], function(forecast.distribution.dt) all(is.na(forecast.distribution.dt[["quantiles"]])), logical(1L))]} %>>%
                    identity()
                }

                testing_forecast_distributions = fetch_available_components_forecast_distributions(forecast_date, "out.RDS")
                training_forecast_distributions = fetch_available_components_forecast_distributions(training_forecast_dates, "scorecard.RDS")
                set(training_forecast_distributions,,"instance", stri_paste(training_forecast_distributions[["forecast_date"]],";",training_forecast_distributions[["location"]]))

                available_forecasters_by_location = (
                  rbindlist(list(testing_forecast_distributions, training_forecast_distributions), fill=TRUE)
                  [, list(n_dates_available=.N), by=c("forecaster_name","location")]
                  [n_dates_available == 1L + n_training_dates]
                  [, list(forecaster_names = list(forecaster_name)), by = "location"]
                )
                if (nrow(available_forecasters_by_location) == 0L) {
                  print("Not enough data to fit for any location")
                  return (NA)
                }
                unique_forecaster_sets = unique(available_forecasters_by_location[["forecaster_names"]])
                print(sprintf("response: %s", response))
                print(sprintf("geo_type: %s", geo_type))
                print(sprintf("Ahead: %d", ahead))
                print(sprintf("Train: %s", toString(training_forecast_dates)))
                print(sprintf("Pred: %s", toString(forecast_date)))
                print(sprintf("Producing %d fits...", length(unique_forecaster_sets)))
                fits = lapply(unique_forecaster_sets, function(forecaster_names) {
                  ## FIXME use corrected DF's to get actual
                  selected_training_forecast_distributions = (
                    training_forecast_distributions
                    [forecaster_names, on = "forecaster_name", nomatch = NULL]
                    [, n_available_forecasters := .N, by = "instance"]
                    [n_available_forecasters == length(forecaster_names)]
                  )
                  selected_instances = unique(selected_training_forecast_distributions[["instance"]])
                  selected_indices = CJ(forecaster_name=forecaster_names, instance=selected_instances)
                  ordered_selected_training_forecast_distributions =
                    selected_training_forecast_distributions[selected_indices, on=c("forecaster_name", "instance"), nomatch=NA]
                  qarr = aperm(array(
                    sapply(ordered_selected_training_forecast_distributions[["forecast_distribution_dt"]], `[[`, "quantiles"),
                    c(length(cdc_probs), length(unique(selected_training_forecast_distributions[["instance"]])), length(forecaster_names)),
                    ), c(2:3, 1L))
                  dimnames(qarr) <- list(
                    instance = unique(selected_training_forecast_distributions[["instance"]]),
                    forecaster_name = forecaster_names,
                    prob = as.character(cdc_probs)
                  )
                  y = (
                    ordered_selected_training_forecast_distributions
                    [, list(actual=actual[[1L]]), by="instance"]
                    [selected_instances, on="instance", nomatch=NA]
                    [, setNames(actual, instance)]
                  )
                  stopifnot(identical(dimnames(qarr)[[1L]], names(y)))
                  include_instance = !is.na(y)
                  qarr <- qarr[include_instance,,,drop=FALSE]
                  y <- y[include_instance]
                  min_length_y = n_required_instances_per_forecaster * length(forecaster_names)
                  beta_hats =
                    if (length(y) < min_length_y) {
                      ## FIXME just don't produce forecasts instead?
                      print(sprintf("Using uniform weights as have only %d training instances but require %f = %f * %d", length(y), min_length_y, n_required_instances_per_forecaster, length(forecaster_names)))
                      rep(1/length(forecaster_names), length(forecaster_names))
                    } else {
                      quantile_ensemble_via_ruled_crps_v1(qarr, y, cdc_probs, panterpolate_cdf, prerange.constant.extension=prerange.constant.extension, prerange.proportional.extension=prerange.proportional.extension)
                    }
                  fit = setDT(data.frame(forecaster_name = forecaster_names, beta_hat = beta_hats))
                  if (any(fit[["beta_hat"]] < -1e-8)) {
                    stop (sprintf('Fit had beta_hat < -1e-8; min was %f', min(fit[["beta_hat"]])))
                  }
                  fit[["beta_hat"]][fit[["beta_hat"]] < 0] <- 0
                  print(copy(fit)[, rounded:=round(beta_hat, 4L)][])
                  fit
                })
                set(available_forecasters_by_location,,"fit", fits[match(available_forecasters_by_location[["forecaster_names"]], unique_forecaster_sets)])
                set(available_forecasters_by_location,,"forecaster_names", NULL)

                predcard_dt = validate_predcard_dt(
                  testing_forecast_distributions
                  [unnest_dt_column_in_dt(available_forecasters_by_location, "fit"),
                   on=c("location","forecaster_name"), nomatch=NA]
                  [, forecast_distribution := lapply(forecast_distribution_dt, function(forcast.distribution.dt) {
                    ## FIXME just make cdc_probs match the scorecard probs? right now there are numerical issues in construction
                    set(forcast.distribution.dt,,"probs", cdc_probs)
                  })]
                  [, unnest_dt_column_in_dt(.SD, "forecast_distribution_dt")]
                  [, level_weights := quantile_level_sample_weights(probs), by=c("forecast_date","location","forecaster_name")]
                  [, list(forecast_distribution_dt=list(setDT(data.frame(
                            probs=cdc_probs,
                            quantiles=weighted_quantile_type1(quantiles, beta_hat*level_weights, cdc_probs, na.rm=TRUE)
                          )))), by=c("forecast_date","location")]
                )
                result = as_tibble(unnest_dt_column_in_dt(predcard_dt[,list(location,forecast_distribution_dt)],"forecast_distribution_dt"))
                result <- result[result[["location"]] %in% target_locations,]
                rm(training_forecast_distributions, testing_forecast_distributions, predcard_dt)
                gc()
                return (result)
            }
    }
}


#' Form a cdf-stacking v2 forecast
#'
#' @param response length-1 character vector, optionally named; the response for
#'   which to generate a forecast; this ensemble is formed from components with
#'   a response matching either the content or name of this argument
#' @param incidence_period length-1 character vector; the
#'   \code{incidence_period} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{incidence_period}
#' @param ahead length-1 integer-valued \code{is.numeric}; the \code{ahead} for
#'   which to generate a forecast; this ensemble is formed from components with
#'   the same \code{ahead}
#' @param forecast_date ignored (to completely discover whether an ensemble can
#'   be formed requires reading card files to find whether they are NA, but this
#'   makes \code{\link{quantileEnsemble::get_forecasters}} run slowly;
#'   currently, no checks are done on the "outer" \code{forecast_date} fed to
#'   this function, and only the \code{forecast_date} fed to a forecaster
#'   function output from this function is considered)
#' @param geo_type length-1 character vector; the \code{geo_type} for which to
#'   generate a forecast; this ensemble is formed from components with the same
#'   \code{geo_type}
#' @param n_locations length-1 integer-valued \code{is.numeric}; the
#'   \code{n_locations} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{n_locations}
#' @param component_forecaster_names names of forecasters to try to include
#' @param shared_naming_scheme (re)naming scheme used as default for historical,
#'   prospective, and hub forecasters; see details
#' @param repo_root_dirpath length-1 character vector; path to root of the
#'   covidcast-forecast repository working directory from which to read
#'   component forecasts; defaults to the lowest ancestor of the \code{getwd}
#'   with a .git file/directory
#' @param historical_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for historical_components_dirpath
#' @param prospective_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for prospective_components_dirpath
#' @param hub_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for hub_components_dirpath
#' @param historical_components_dirpath override for the path to the directory
#'   (tree) containing historical component prediction/score cards
#' @param prospective_components_dirpath override for the path to the directory
#'   (tree) containing prospective component prediction/score cards
#' @param hub_components_dirpath override for the path to the directory (tree)
#'   containing COVID-19 Forecasting Hub component prediction/score cards
#'
#' @details Each (re)naming scheme is a character vector: each named vector
#'   entry denotes renaming forecasters with the entry's name to the entry
#'   itself; each unnamed vector entry denotes a name that is expected to be
#'   found somewhere in the searched paths; each entry (named or unnamed) is
#'   checked for appearance somewhere in any of the searched paths. However, the
#'   converse is not checked, i.e., it is fine for there to be paths found that
#'   do not correspond to an entry in the naming schemes.
#'
#' @importFrom data.table is.data.table .N copy
#' @importFrom pipeR %>>%
#' @importFrom stats na.omit
cdf_stacking_forecaster_v2 <- function(response, incidence_period, ahead, geo_type, n_locations,
                                       component_forecaster_names,
                                       panterpolate_cdf,
                                       n_max_training_dates = 3L,
                                       n_required_instances_to_include_forecaster = 10L,
                                       n_required_instances_per_forecaster = 10L,
                                       prerange.constant.extension=1,
                                       prerange.proportional.extension=0.2,
                                       shared_naming_scheme=character(0L),
                                       repo_root_dirpath=get_repo_root_dirpath(),
                                       historical_naming_scheme=shared_naming_scheme,
                                       prospective_naming_scheme=shared_naming_scheme,
                                       hub_naming_scheme=shared_naming_scheme,
                                       historical_components_dirpath=get_historical_components_dirpath(repo_root_dirpath),
                                       prospective_components_dirpath=get_prospective_components_dirpath(repo_root_dirpath),
                                       hub_components_dirpath=get_hub_components_dirpath(repo_root_dirpath)) {
    if (incidence_period != "epiweek") {
        NA
    } else {
            function(df, forecast_date) {
                target_locations = df[["location"]][df[["variable_name"]]=="location_to_be_forecast" & df[["value"]]==1]
                training_forecast_dates = forecast_date - 7L*(ahead-1L+seq_len(n_max_training_dates))
                ## Fetches scorecards by dates & type (prediction vs. score)
                fetch_available_components_forecast_distributions = function(forecast_dates, file_basename) {
                  read_backoff_ensemble_components_dt(
                    response, incidence_period, ahead, forecast_dates, geo_type, n_locations,
                    only_read_forecaster_names = component_forecaster_names,
                    file_basename = file_basename,
                    shared_naming_scheme=shared_naming_scheme,
                    historical_components_dirpath=historical_components_dirpath,
                    prospective_components_dirpath=prospective_components_dirpath,
                    hub_components_dirpath=hub_components_dirpath,
                    historical_naming_scheme=historical_naming_scheme,
                    prospective_naming_scheme=prospective_naming_scheme,
                    hub_naming_scheme=hub_naming_scheme
                  ) %>>%
                    ## Card-level availability checks:
                    desired_components_predcards_dt_or_feedback(
                      complete_output=FALSE,
                      required_forecaster_names=character(0L),
                      include_all_other_forecasters=FALSE,
                      optional_forecaster_names=component_forecaster_names) %>>%
                    {if (is.character(.)) stop(.) else .} %>>%
                    ## Discard card location forecast_date; forecast_date will be reintroduced by unnesting card:
                    set(,"forecast_date", NULL) %>>%
                    unnest_dt_column_in_dt("predcard_dt") %>>%
                    ## throw out NULL forecast_distribution_dt's:
                    {.[!vapply(.[["forecast_distribution_dt"]], is.null, logical(1L))]} %>>%
                    ## throw out incomplete forecast_distribution_dt's:
                    {.[!vapply(.[["forecast_distribution_dt"]], function(forecast.distribution.dt) any(is.na(forecast.distribution.dt[["quantiles"]])), logical(1L))]} %>>%
                    ## ## throw out completely missing forecast.distribution.dt's:
                    ## {.[!vapply(.[["forecast_distribution_dt"]], function(forecast.distribution.dt) all(is.na(forecast.distribution.dt[["quantiles"]])), logical(1L))]} %>>%
                    identity()
                }

                ## Fetch and annotate testing prediction cards and training score cards:
                testing_forecast_distributions = fetch_available_components_forecast_distributions(forecast_date, "out.RDS")
                testing_forecast_distributions <- testing_forecast_distributions[testing_forecast_distributions[["location"]] %in% target_locations]
                set(testing_forecast_distributions,,"instance_type", "testing")
                training_forecast_distributions = fetch_available_components_forecast_distributions(training_forecast_dates, "scorecard.RDS")
                set(training_forecast_distributions,, "instance_type", "training")
                set(training_forecast_distributions,,"instance", stri_paste(training_forecast_distributions[["forecast_date"]],";",training_forecast_distributions[["location"]]))

                ## Select which testing forecasters to use in each location
                selected_testing_forecasters_by_location = (
                  ## rbindlist(list(testing_forecast_distributions, training_forecast_distributions), fill=TRUE)
                  ## [, list(n_dates_available=.N,
                  ##         n_testing_dates_available=sum(instance_type=="testing")),
                  ##  by=c("forecaster_name","location")]
                  ## [n_dates_available >= 1L + n_min_training_dates]
                  ## [n_testing_dates_available >= 1L]
                  ##
                  testing_forecast_distributions
                  [, list(forecaster_names = list(forecaster_name)), by = "location"]
                  ## TODO filter based on forecaster availability in training data over all locations?
                )
                if (nrow(selected_testing_forecasters_by_location) == 0L) {
                  print("Not enough data to fit for any location")
                  return (NA)
                }
                unique_forecaster_sets = unique(selected_testing_forecasters_by_location[["forecaster_names"]])
                set(selected_testing_forecasters_by_location,,"forecaster_set_i",
                    match(selected_testing_forecasters_by_location[["forecaster_names"]], unique_forecaster_sets))
                testing_locations_for_unique_forecaster_sets = selected_testing_forecasters_by_location[, list(locations=list(location)), by="forecaster_set_i"]

                cat(sprintf("response: %s\n", response))
                cat(sprintf("geo_type: %s\n", geo_type))
                cat(sprintf("Ahead: %d\n", ahead))
                cat(sprintf("Train: %s\n", toString(training_forecast_dates)))
                cat(sprintf("Pred: %s\n", toString(forecast_date)))
                cat(sprintf("Producing %d fits...\n", length(unique_forecaster_sets)))
                fits = lapply(seq_along(unique_forecaster_sets), function(unique_forecaster_set_i) {
                  forecaster_names = unique_forecaster_sets[[unique_forecaster_set_i]]
                  ## FIXME use corrected DF's to get actual
                  selected_training_forecast_distributions = (
                    training_forecast_distributions
                    [forecaster_names, on = "forecaster_name", nomatch = NULL]
                  )
                  selected_instances = unique(selected_training_forecast_distributions[["instance"]])
                  selected_indices = CJ(forecaster_name=forecaster_names, instance=selected_instances)
                  ordered_selected_training_forecast_distributions = (
                    selected_training_forecast_distributions
                    [selected_indices, on=c("forecaster_name", "instance"), nomatch=NA]
                    [vapply(forecast_distribution_dt, is.null, logical(1L)),
                     forecast_distribution_dt := rep(list(setDT(data.frame(probs=cdc_probs, quantiles=NA))), .N)
                     ]
                  )
                  qarr = aperm(array(
                    sapply(ordered_selected_training_forecast_distributions[["forecast_distribution_dt"]], `[[`, "quantiles"),
                    c(length(cdc_probs), length(unique(selected_training_forecast_distributions[["instance"]])), length(forecaster_names)),
                    ), c(2:3, 1L))
                  dimnames(qarr) <- list(
                    instance = unique(selected_training_forecast_distributions[["instance"]]),
                    forecaster_name = forecaster_names,
                    prob = as.character(cdc_probs)
                  )
                  y = (
                    ordered_selected_training_forecast_distributions
                    [, list(actual=na.omit(actual)[1L][[1L]]), by="instance"]
                    [selected_instances, on="instance", nomatch=NA]
                    [, setNames(actual, instance)]
                  )
                  stopifnot(identical(dimnames(qarr)[[1L]], names(y)))
                  include_instance = !is.na(y)
                  qarr <- qarr[include_instance,,,drop=FALSE]
                  y <- y[include_instance]
                  min_length_y = n_required_instances_per_forecaster * length(forecaster_names)
                  beta_hats =
                    if (length(y) < min_length_y) {
                      ## FIXME just don't produce forecasts instead?
                      print(sprintf("Using uniform weights as have only %d training instances but require %f = %f * %d", length(y), min_length_y, n_required_instances_per_forecaster, length(forecaster_names)))
                      rep(1/length(forecaster_names), length(forecaster_names))
                    } else {
                      quantile_ensemble_via_ruled_crps_with_imputation(qarr, y, cdc_probs, panterpolate_cdf, prerange.constant.extension=prerange.constant.extension, prerange.proportional.extension=prerange.proportional.extension)
                    }
                  fit = setDT(data.frame(forecaster_name = forecaster_names, beta_hat = beta_hats))
                  ## TODO relocate this check (in all versions) into the fitting procedures?
                  if (any(fit[["beta_hat"]] < -1e-8)) {
                    stop (sprintf('Fit had beta_hat < -1e-8; min was %f', min(fit[["beta_hat"]])))
                  }
                  fit[["beta_hat"]][fit[["beta_hat"]] < 0] <- 0
                  testing_locations = testing_locations_for_unique_forecaster_sets[unique_forecaster_set_i, locations, nomatch=NA]
                  cat(sprintf("Testing locations: %s\n", toString(testing_locations)))
                  print(copy(fit)[, rounded:=round(beta_hat, 4L)][])
                  fit
                })
                set(selected_testing_forecasters_by_location,,"fit", fits[selected_testing_forecasters_by_location[["forecaster_set_i"]]])
                set(selected_testing_forecasters_by_location,,"forecaster_names", NULL)

                predcard_dt = validate_predcard_dt(
                  testing_forecast_distributions
                  [unnest_dt_column_in_dt(selected_testing_forecasters_by_location, "fit"),
                   on=c("location","forecaster_name"), nomatch=NA]
                  [, forecast_distribution := lapply(forecast_distribution_dt, function(forcast.distribution.dt) {
                    ## FIXME just make cdc_probs match the scorecard probs? right now there are numerical issues in construction
                    set(forcast.distribution.dt,,"probs", cdc_probs)
                  })]
                  [, unnest_dt_column_in_dt(.SD, "forecast_distribution_dt")]
                  [, level_weights := quantile_level_sample_weights(probs), by=c("forecast_date","location","forecaster_name")]
                  [, list(forecast_distribution_dt=list(setDT(data.frame(
                            probs=cdc_probs,
                            quantiles=weighted_quantile_type1(quantiles, beta_hat*level_weights, cdc_probs, na.rm=TRUE)
                          )))), by=c("forecast_date","location")]
                )
                result = as_tibble(unnest_dt_column_in_dt(predcard_dt[,list(location,forecast_distribution_dt)],"forecast_distribution_dt"))
                rm(df, training_forecast_distributions, testing_forecast_distributions, predcard_dt)
                gc()
                return (result)
            }
    }
}
