is_integer <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x == round(x)
}



is_number <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x)
}



# Upper control limit of Hotelling's T-squared for n observations and k components.
# "beta": exact limit for the observations used to estimate the mean and
#   covariance, e.g. the PCA scores themselves (Tracy, Young & Mason, 1992).
# "f": k(n - 1)/(n - k) F-quantile, as used in HotellingEllipse <= 1.2.0.
tsq_limit <- function(n, k, conf.limit, method) {
  switch(method,
    beta = ((n - 1)^2 / n) * stats::qbeta(p = conf.limit, shape1 = k / 2, shape2 = (n - k - 1) / 2),
    f = (k * (n - 1) / (n - k)) * stats::qf(p = conf.limit, df1 = k, df2 = (n - k))
  )
}



# Confidence level as a percentage label: 0.95 -> "95", 0.975 -> "97.5", 0.999 -> "99.9"
level_label <- function(level) {
  as.character(signif(100 * level, 10))
}



check_nobs <- function(n, k) {
  if (n < k + 2) {
    stop(sprintf("At least %d observations are needed to use %d components (got %d).", k + 2, k, n))
  }
}



# TRUE when the columns behind covariance matrix S are uncorrelated up to rounding,
# as for PCA scores and PLS X-scores of the samples the model was fitted on.
is_uncorrelated <- function(S) {
  r <- stats::cov2cor(S)
  all(abs(r[upper.tri(r)]) < sqrt(.Machine$double.eps))
}



# Semi-axes and rotation angle of the 2D ellipse {u : (u - m)' S^-1 (u - m) = Tsq_limit}.
# `a` is the semi-axis closest to the x-axis. Uncorrelated scores give exactly the
# axis-aligned ellipse of HotellingEllipse <= 1.2.0: angle = 0, a = sqrt(Tsq_limit * var(x))
# and b = sqrt(Tsq_limit * var(y)). This also avoids an arbitrary angle when the
# ellipse is a circle (equal variances, e.g. SIMPLS scores).
ellipse_axes <- function(S, Tsq_limit) {
  if (is_uncorrelated(S)) {
    return(list(a = sqrt(Tsq_limit * S[1, 1]), b = sqrt(Tsq_limit * S[2, 2]), angle = 0))
  }
  e <- eigen(S, symmetric = TRUE)
  i <- if (abs(e$vectors[1, 1]) >= abs(e$vectors[1, 2])) 1L else 2L
  v <- e$vectors[, i]
  if (v[1] < 0) v <- -v
  list(
    a = sqrt(Tsq_limit * e$values[i]),
    b = sqrt(Tsq_limit * e$values[3L - i]),
    angle = atan2(v[2], v[1])
  )
}



# Symmetric square root of a covariance matrix. It maps the unit circle (sphere)
# onto the ellipse (ellipsoid) with shape S; for uncorrelated scores it is diag(sd),
# which gives the axis-aligned ellipse of HotellingEllipse <= 1.2.0.
sqrtm <- function(S) {
  if (is_uncorrelated(S)) {
    return(diag(sqrt(diag(S)), nrow = nrow(S)))
  }
  e <- eigen(S, symmetric = TRUE)
  e$vectors %*% diag(sqrt(pmax(e$values, 0)), nrow = length(e$values)) %*% t(e$vectors)
}
