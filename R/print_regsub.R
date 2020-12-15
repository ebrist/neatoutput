
#' Neat Table of Regsubsets
#' 
#' @description prints a neat table of the a best subsets regression produced by the regsubsets function in the leaps package
#'
#' @param regfit a regsubsets object.
#' @param format table format. Must be one of: "data.frame", "latex", "html"
#' @param bold_crit option to bold the best criterion (html, latex)
#' @param bold_header option to bold the headers (html, latex)
#' @param generic_header option to use generic headers for the predictors: x1, x2, ...
#' @param angle option to align predictor headers at an angle (html, latex)
#' @param full_width option to use 100% width (html, latex)
#' @param font_size option to set the font size (html, latex)
#' @param landscape option to use landscape orientation (html, latex)
#' @param decimals option to set number of decimals
#' @param caption option to give the table a caption (html, latex)
#' @param booktabs option to use booktabs (html, latex)
#' @param linesep option to set line separator (html, latex)
#'
#' @return a data.frame, latex table, or html table
#' @export
#'
print_regsub <- function(regfit, format = "data.frame", 
                         caption = "Best Subsets", bold_crit = T, 
                         bold_header = T, generic_header = F, 
                         angle = 90, full_width = F, font_size = 12, 
                         landscape = T, booktabs = T, linesep = "", decimals = 4) {
  # function to format values
  fval <- function(x) {
    format(round(x, digits = decimals), nsmall = decimals)
  }
  # pull info from regfit
  n <- regfit$nn
  reg_sum <- summary(regfit)
  var_id <- apply(reg_sum$which, 2, as.numeric)[ , -1]
  k <- apply(var_id, 1, sum)
  best_out1 <- data.frame(cbind(k, var_id))
  reg_sum$aic <- reg_sum$bic - (k + 1) * log(n) + 2 * (k + 1)
  crit_out <- data.frame(reg_sum[c("rsq", "adjr2", "cp", "aic", "bic")])
  # best crit
  max_rsq <- which.max(crit_out$rsq)
  max_adjr2 <- which.max(crit_out$adjr2)
  min_cp <- which.min(crit_out$cp)
  min_aic <- which.min(crit_out$aic)
  min_bic <- which.min(crit_out$bic)
  # format table
  crit_out <- fval(crit_out)
  # change headers if needed
  if (generic_header) {
    names(best_out1)[-1] <- paste0("x", 1:(ncol(best_out1) - 1))
  }
  if (format == "data.frame") {
    out <- cbind(best_out1, crit_out)
  } else if (format %in% c("latex", "html")) {
    # set predictor name angles
    names(best_out1)[-1] <- cell_spec(names(best_out1[-1]), angle = angle)
    # bold best crit 
    if (bold_crit == T) {
      crit_out$rsq[max_rsq] <- cell_spec(crit_out$rsq[max_rsq], bold = T)
      crit_out$adjr2[max_adjr2] <- cell_spec(crit_out$adjr2[max_adjr2], bold = T)
      crit_out$cp[min_cp] <- cell_spec(crit_out$cp[min_cp], bold = T)
      crit_out$aic[min_aic] <- cell_spec(crit_out$aic[min_aic], bold = T)
      crit_out$bic[min_bic] <- cell_spec(crit_out$bic[min_bic], bold = T)
    }
    # html or latex output table
    out <- cbind(best_out1, crit_out) %>%
      knitr::kable(format = format, escape = F, caption = caption,
                   booktabs = booktabs, linesep = linesep) %>%
      row_spec(0, bold = bold_header) %>%
      kable_styling(full_width = full_width, font_size = font_size)
    if (landscape) {
      out <- out %>% kableExtra::landscape()
    }
  } else {
    stop("Format must be one of: 'data.frame', 'latex', 'html'.")
  }
  # return table
  out
}

