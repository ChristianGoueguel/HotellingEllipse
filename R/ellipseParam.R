#' @title Hotelling's T-squared Statistic and Ellipse Parameters
#'
#' @author Christian L. Goueguel <christian.goueguel@gmail.com>
#'
#' @description
#' This function calculates Hotelling's T-squared statistic and, when applicable,
#' the lengths of the semi-axes of the Hotelling's ellipse. It can work with a
#' specified number of components or use a cumulative variance threshold.
#'
#' @param x A matrix, data frame or tibble containing scores from PCA, PLS, ICA, or other similar methods. Each column should represent a component, and each row an observation.
#' @param k An integer specifying the number of components to use (default is 2). This parameter is ignored if `threshold` is provided.
#' @param pcx An integer specifying which component to use for the x-axis when `k = 2` (default is 1).
#' @param pcy An integer specifying which component to use for the y-axis when `k = 2` (default is 2).
#' @param threshold A numeric value between 0 and 1 specifying the desired cumulative explained variance threshold (default is `NULL`). If provided, the function determines the minimum number of components needed to explain at least this proportion of total variance. When `NULL`, the function uses the fixed number of components specified by `k`.
#' @param rel.tol A numeric value specifying the minimum proportion of total variance a component should explain to be considered non-negligible (default is 0.001, i.e., 0.1%).
#' @param abs.tol A numeric value specifying the minimum absolute variance a component should have to be considered non-negligible (default is `.Machine$double.eps`).
#'
#' @return A list containing the following elements:
#'  - `Tsquare`: A data frame containing the T-squared statistic for each observation.
#'  - `Ellipse`: A data frame containing the lengths of the semi-minor and semi-major axes (only when `k = 2`).
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
#' # Example 1: Calculate Hotelling's T-squared and ellipse parameters using
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
ellipseParam <- function(x, k = 2, pcx = 1, pcy = 2, threshold = NULL, rel.tol = 0.001, abs.tol = .Machine$double.eps) {

  if (missing(x)) {
    stop("Missing input data.")
  }
  if (!is.matrix(x) && !is.data.frame(x) && !tibble::is_tibble(x)) {
    stop("The input data must be a matrix, data frame or tibble.")
  }
  if (!is.numeric(rel.tol) || rel.tol < 0) {
    stop("'rel.tol' must be a non-negative numeric value.")
  }
  if (!is.numeric(abs.tol) || abs.tol < 0) {
    stop("'abs.tol' must be a non-negative numeric value.")
  }
  if (abs.tol > rel.tol) {
    stop("'abs.tol' must be less than or equal to 'rel.tol'.")
  }

  x <- as.matrix(x)
  p <- as.integer(ncol(x))
  is_integer <- function(x) { x == as.integer(x) }

  if (!is.null(threshold)) {
    if (!is.numeric(threshold) || threshold <= 0 || threshold > 1) {
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

  res_param <- list()

  if (is.null(threshold)) {
    res_param<- process_fixed_comp(x, k, pcx, pcy, nearzero_var, comp_var, relative_var, rel.tol)
  } else {
    res_param<- process_threshold(x, threshold, nearzero_var, relative_var)
  }

  return(res_param)
}



process_fixed_comp <- function(x, k, pcx, pcy, nearzero_var, comp_var, relative_var, rel.tol) {
  res <- list()
  if (any(nearzero_var[1:k])) {
    removed_comp <- colnames(x)[nearzero_var[1:k]]
    message(sprintf("Components with explained variance lower than 'rel.tol' detected: %s removed.", paste(removed_comp, collapse = ", ")))
    x <- x[, !nearzero_var, drop = FALSE]
    k <- min(k, ncol(x))
  }
  t2_values <- tryCatch(
    compute_tsquared(x, k),
    error = function(e) {
      stop(sprintf("Error in T-squared calculation: %s", e$message))
    }
  )
  res$Tsquare <- t2_values$Tsq
  res$cutoff.99pct <- t2_values$Tsq_limit1
  res$cutoff.95pct <- t2_values$Tsq_limit2
  res$nb.comp <- k
  if (k == 2) {
    if (relative_var[pcx] < rel.tol) {
      stop("'pcx' has a relative variance lower than 'rel.tol'. Please check!")
    }
    if (relative_var[pcy] < rel.tol) {
      stop("'pcy' has a relative variance lower than 'rel.tol'. Please check!")
    }
    res$Ellipse <- tibble::tibble(
      a.99pct = as.numeric(sqrt(t2_values$Tsq_limit1 * comp_var[pcx])),
      b.99pct = as.numeric(sqrt(t2_values$Tsq_limit1 * comp_var[pcy])),
      a.95pct = as.numeric(sqrt(t2_values$Tsq_limit2 * comp_var[pcx])),
      b.95pct = as.numeric(sqrt(t2_values$Tsq_limit2 * comp_var[pcy]))
    )
  }
  return(res)
}



process_threshold <- function(x, threshold, nearzero_var, relative_var) {
  res <- list()
  cum_var <- cumsum(relative_var)
  k <- which(cum_var >= threshold)[1]
  k <- as.numeric(k)
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
    k <- which(cum_var >= threshold)[1]
    k <- as.numeric(k)
  }
  t2_values <- tryCatch(
    compute_tsquared(x, k),
    error = function(e) {
      stop(sprintf("Error in T-squared calculation: %s", e$message))
    }
  )
  res$Tsquare <- t2_values$Tsq
  res$cutoff.99pct <- t2_values$Tsq_limit1
  res$cutoff.95pct <- t2_values$Tsq_limit2
  res$nb.comp <- k
  return(res)
}




compute_tsquared <- function(x, ncomp) {
  n <- nrow(x)
  x <- x[, 1:ncomp, drop = FALSE]
  MDsq <- stats::mahalanobis(
    x = x,
    center = colMeans(x),
    cov = stats::cov(x),
    inverted = FALSE
  )
  Tsq_limit1 <- (ncomp * (n - 1) / (n - ncomp)) * stats::qf(p = 0.99, df1 = ncomp, df2 = (n - ncomp))
  Tsq_limit2 <- (ncomp * (n - 1) / (n - ncomp)) * stats::qf(p = 0.95, df1 = ncomp, df2 = (n - ncomp))
  Tsq <- tibble::tibble(value = ((n - ncomp) / (ncomp * (n - 1))) * MDsq)
  res <- list(
    Tsq = Tsq,
    Tsq_limit1 = Tsq_limit1,
    Tsq_limit2 = Tsq_limit2
  )
  return(res)
}
