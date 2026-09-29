#' @title Hotelling’s T-squared Statistic and Ellipse Parameters
#'
#' @author Christian L. Goueguel <christian.goueguel@gmail.com>
#'
#' @description
#' This function calculates Hotelling’s T-squared statistic and, when applicable,
#' the lengths of the semi-axes of the Hotelling’s ellipse. It can work with a
#' specified number of components or use a cumulative variance threshold.
#'
#' @param x A matrix, data frame or tibble containing scores from PCA, PLS, ICA, or other similar methods. Each column should represent a component, and each row an observation.
#' @param k An integer specifying the number of components to use (default is 2). This parameter is ignored if `threshold` is provided.
#' @param pcx An integer specifying which component to use for the x-axis when `k = 2` (default is 1).
#' @param pcy An integer specifying which component to use for the y-axis when `k = 2` (default is 2).
#' @param threshold A numeric value between 0 and 1 specifying the desired cumulative explained variance threshold (default is `NULL`). If provided, the function determines the minimum number of components needed to explain at least this proportion of total variance. When `NULL`, the function uses the fixed number of components specified by `k`.
#' @param rel.tol A numeric value specifying the minimum proportion of total variance a component should explain to be considered non-negligible (default is 0.001, i.e., 0.1%).
#' @param abs.tol A numeric value specifying the minimum absolute variance a component should have to be considered non-negligible (default is `.Machine$double.eps`).
#' @param method A character string specifying how the T-squared cutoffs are computed: `"f"` (default) or `"beta"`. See Details.
#'
#' @return A list containing the following elements:
#'  - `Tsquare`: A data frame containing the T-squared statistic for each observation (the squared Mahalanobis distance), on the same scale as `cutoff.99pct` and `cutoff.95pct`. When `k = 2`, it is computed on components `pcx` and `pcy`.
#'  - `Ellipse`: A data frame containing the lengths of the semi-axes at the 99% and 95% confidence levels (`a.99pct`, `b.99pct`, `a.95pct`, `b.95pct`) and the rotation `angle` of the ellipse in radians (only when `k = 2`). `a` is the semi-axis closest to the `pcx` direction. For uncorrelated scores, such as the PCA or PLS scores of the samples the model was fitted on, `angle` is 0.
#'  - `cutoff.99pct`: The T-squared cutoff value at the 99% confidence level.
#'  - `cutoff.95pct`: The T-squared cutoff value at the 95% confidence level.
#'  - `nb.comp`: The number of components used in the calculation.
#'
#' @details
#' When `threshold` is used, the function selects the minimum number of `k` components
#' that cumulatively explain at least the specified proportion of variance. This
#' parameter allows for dynamic component selection based on explained variance,
#' rather than using a fixed number of components. It must be greater than `rel.tol`.
#' Typical values range from 0.8 to 0.95.
#'
#' The `rel.tol` parameter sets a minimum variance threshold for individual components.
#' Components with variance below this threshold are considered negligible and are
#' removed from the analysis. Setting `rel.tol` too high
#' may remove potentially important components, while setting it too low may
#' retain noise or cause computational issues. Adjust based on your data
#' characteristics and analysis goals.
#'
#' Note that components are considered to have near-zero variance and are removed
#' if their relative variance is below `rel_tol` or their absolute variance is
#' below `abs_tol`. This dual-threshold approach helps ensure numerical stability
#' while also accounting for the relative importance of components. The default
#' value for `abs.tol` is set to `.Machine$double.eps`, providing a lower bound
#' for detecting near-zero variance that may cause numerical instability.
#'
#' The `method` parameter sets the distribution used for the cutoffs. With
#' `method = "f"` (default), the cutoffs are
#' \eqn{\frac{k(n-1)}{n-k} F_{\alpha}(k, n-k)}{k(n - 1)/(n - k) * F(k, n - k)},
#' as in previous versions of the package. With `method = "beta"`, the cutoffs
#' follow the exact distribution of T-squared for the observations used to
#' estimate the mean and covariance,
#' \eqn{\frac{(n-1)^2}{n} B_{\alpha}(k/2, (n-k-1)/2)}{(n - 1)^2 / n * Beta(k/2, (n - k - 1)/2)}
#' (Tracy, Young and Mason, 1992), e.g. the scores of the samples a PCA or PLS
#' model was built on. The F-based limit is more conservative, especially for
#' small `n`: it can even exceed the largest T-squared value any observation can
#' reach, \eqn{(n-1)^2/n}{(n - 1)^2 / n}. For `n` larger than about 100, the two
#' limits are close.
#'
#' When the selected components are correlated (e.g. new samples projected onto a model, or ICA scores), the
#' ellipse is rotated so that it matches the T-squared statistic: an observation
#' lies outside the ellipse exactly when its T-squared value exceeds the cutoff.
#'
#' @references
#' Tracy, N. D., Young, J. C. and Mason, R. L. (1992). Multivariate control charts
#' for individual observations. \emph{Journal of Quality Technology}, 24(2), 88--95.
#'
#' @export ellipseParam
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(HotellingEllipse)
#' library(dplyr)
#'
#' data("specData", package = "HotellingEllipse")
#'
#' # Perform PCA
#' set.seed(123)
#' pca_mod <- specData %>%
#'   select(where(is.numeric)) %>%
#'   FactoMineR::PCA(scale.unit = FALSE, graph = FALSE)
#'
#' # Extract PCA scores
#' pca_scores <- pca_mod$ind$coord %>% as.data.frame()
#'
#' # Example 1: Calculate Hotelling’s T-squared and ellipse parameters using
#' # the 2nd and 4th components
#' T2_fixed <- ellipseParam(x = pca_scores, pcx = 2, pcy = 4)
#'
#' # Example 2: Calculate using the first 4 components
#' T2_comp <- ellipseParam(x = pca_scores, k = 4)
#'
#' # Example 3: Calculate using a cumulative variance threshold
#' T2_threshold <- ellipseParam(x = pca_scores, threshold = 0.95)
#' }
#'
#'
ellipseParam <- function(x, k = 2, pcx = 1, pcy = 2, threshold = NULL, rel.tol = 0.001, abs.tol = .Machine$double.eps, method = c("f", "beta")) {

  if (missing(x)) {
    stop("Missing input data.")
  }
  if (!is.matrix(x) && !is.data.frame(x) && !tibble::is_tibble(x)) {
    stop("The input data must be a matrix, data frame or tibble.")
  }
  if (!is_number(rel.tol) || rel.tol < 0) {
    stop("'rel.tol' must be a non-negative numeric value.")
  }
  if (!is_number(abs.tol) || abs.tol < 0) {
    stop("'abs.tol' must be a non-negative numeric value.")
  }
  if (abs.tol > rel.tol) {
    stop("'abs.tol' must be less than or equal to 'rel.tol'.")
  }
  method <- match.arg(method)

  x <- as.matrix(x)
  p <- as.integer(ncol(x))

  if (!is.null(threshold)) {
    if (!is_number(threshold) || threshold <= 0 || threshold > 1) {
      stop("Threshold must be a numeric value between 0 and 1.")
    }
  } else {
    if (!is_integer(k) || k < 2L || k > p) {
      stop(sprintf("'k' must be an integer between 2 and the number of components in the data (%d).", p))
    }
  }
  if (!is_integer(pcx) || pcx < 1L || pcx > p) {
    stop(sprintf("'pcx' must be an integer between 1 and the number of components in the data (%d).", p))
  }
  if (!is_integer(pcy) || pcy < 1L || pcy > p) {
    stop(sprintf("'pcy' must be an integer between 1 and the number of components in the data (%d).", p))
  }
  if (pcx == pcy) {
    stop("'pcx' and 'pcy' must be different integers.")
  }

  comp_var <- apply(x, 2, stats::var)
  total_var <- sum(comp_var)
  relative_var <- comp_var / total_var
  nearzero_var <- (relative_var < rel.tol) | (comp_var < abs.tol)

  if (is.null(threshold)) {
    res_param <- process_fixed_comp(x, k, pcx, pcy, nearzero_var, comp_var, relative_var, rel.tol, method)
  } else {
    res_param <- process_threshold(x, threshold, nearzero_var, relative_var, method)
  }

  return(res_param)
}



process_fixed_comp <- function(x, k, pcx, pcy, nearzero_var, comp_var, relative_var, rel.tol, method = "f") {
  res <- list()
  if (k == 2) {
    if (relative_var[pcx] < rel.tol) {
      stop("'pcx' has a relative variance lower than 'rel.tol'. Please check!")
    }
    if (relative_var[pcy] < rel.tol) {
      stop("'pcy' has a relative variance lower than 'rel.tol'. Please check!")
    }
    # T-squared must be computed on the same components as the ellipse
    x <- x[, c(pcx, pcy), drop = FALSE]
  } else if (any(nearzero_var[1:k])) {
    removed_comp <- colnames(x)[nearzero_var[1:k]]
    message(sprintf("Components with explained variance lower than 'rel.tol' detected: %s removed.", paste(removed_comp, collapse = ", ")))
    x <- x[, !nearzero_var, drop = FALSE]
    k <- min(k, ncol(x))
    if (k < 2) {
      stop("Fewer than two components remain after removing near-zero variance components.")
    }
  }
  t2_values <- tryCatch(
    compute_tsquared(x, k, method),
    error = function(e) {
      stop(sprintf("Error in T-squared calculation: %s", e$message))
    }
  )
  res$Tsquare <- t2_values$Tsq
  res$cutoff.99pct <- t2_values$Tsq_limit1
  res$cutoff.95pct <- t2_values$Tsq_limit2
  res$nb.comp <- as.integer(k)
  if (k == 2) {
    S <- stats::cov(x)
    ax99 <- ellipse_axes(S, t2_values$Tsq_limit1)
    ax95 <- ellipse_axes(S, t2_values$Tsq_limit2)
    res$Ellipse <- tibble::tibble(
      a.99pct = ax99$a,
      b.99pct = ax99$b,
      a.95pct = ax95$a,
      b.95pct = ax95$b,
      angle = ax95$angle
    )
  }
  return(res)
}



process_threshold <- function(x, threshold, nearzero_var, relative_var, method = "f") {
  res <- list()
  # Tolerance so that threshold = 1 is reachable despite floating-point rounding
  tol <- sqrt(.Machine$double.eps)
  cum_var <- cumsum(relative_var)
  k <- unname(which(cum_var >= threshold - tol)[1])
  if (is.na(k)) {
    stop("Threshold is too high. Cannot find enough components to meet the threshold.")
  }
  if (k == 1) {
    warning(sprintf("The specified threshold (%.3f) is lower than the variance explained by the first component (%.3f). The first two components (k = 2) is used.", threshold, relative_var[1]))
    k <- 2
  }
  if (any(nearzero_var[1:k])) {
    removed_comp <- colnames(x)[nearzero_var[1:k]]
    warning(sprintf("Components with explained variance lower than 'rel.tol' detected within the first %d components to meet the threshold: %s removed.", k, paste(removed_comp, collapse = ", ")))
    x <- x[, !nearzero_var, drop = FALSE]
    relative_var <- relative_var[!nearzero_var]
    cum_var <- cumsum(relative_var)
    # The removed components carry negligible variance, so use all remaining
    # components if the threshold is no longer reached
    k <- unname(which(cum_var >= threshold - tol)[1])
    if (is.na(k)) k <- ncol(x)
    k <- max(k, 2)
    if (ncol(x) < 2) {
      stop("Fewer than two components remain after removing near-zero variance components.")
    }
  }
  t2_values <- tryCatch(
    compute_tsquared(x, k, method),
    error = function(e) {
      stop(sprintf("Error in T-squared calculation: %s", e$message))
    }
  )
  res$Tsquare <- t2_values$Tsq
  res$cutoff.99pct <- t2_values$Tsq_limit1
  res$cutoff.95pct <- t2_values$Tsq_limit2
  res$nb.comp <- as.integer(k)
  return(res)
}



compute_tsquared <- function(x, ncomp, method = "f") {
  n <- nrow(x)
  check_nobs(n, ncomp)
  x <- x[, 1:ncomp, drop = FALSE]
  MDsq <- stats::mahalanobis(
    x = x,
    center = colMeans(x),
    cov = stats::cov(x),
    inverted = FALSE
  )
  Tsq_limit1 <- tsq_limit(n, ncomp, 0.99, method)
  Tsq_limit2 <- tsq_limit(n, ncomp, 0.95, method)
  Tsq <- tibble::tibble(value = unname(MDsq))
  res <- list(
    Tsq = Tsq,
    Tsq_limit1 = Tsq_limit1,
    Tsq_limit2 = Tsq_limit2
  )
  return(res)
}
