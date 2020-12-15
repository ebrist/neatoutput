
#' Neat Table of GLM Output
#' 
#' @description print_glm() generates neat tables (pandoc, latex, or html formats) of summary output from a generalized linear model object. 
#'
#'
#' @param fit a linear model object produced by lm.
#' @param caption option to add a customized caption to the output table. default caption shows the linear model.
#' @param ci option to show confidence intervals for each model parameter.
#' @param ci_level option to set the confidence level. 
#' @param ci_wald TRUE gives wald intervals based on asymptotic normality. FALSE gives profile likelihood intervals.  
#' @param decimals option to set the number of decimals displayed. Must be 1 or more.
#' @param scipen_val option to reset scipen.  
#' @param format A character string: kable format. Possible values are latex, html, markdown, pandoc, and rst. The recommended format is "html" when knitting to .html files, and "latex" when knitting to .pdf files. The pandoc format (default) is recommended when printing to the Console. 
#' @param full_width Logical: whether html tables should span full width of page.
#' @param align Column alignment: a character vector consisting of 'l' (left), 'c' (center) and/or 'r' (right).
#' @param boot_options A character vector: arguments to be passed to bootstrap_options for html tables.
#'
#' @return  a formatted table of generalized linear model fit results
#' @export
#'
print_glm <- function(fit, caption = NULL, ci = T, ci_level = 0.95, ci_wald = T, 
                     decimals = 4, scipen_val = 999, format = "pandoc", 
                     full_width = F, align = "r", boot_options = c("striped")) {
  # function to format values
  fval <- function(x) {
    format(round(x, digits = decimals), nsmall = decimals)
  }
  # scipen to revert after tables have been printed
  init_scipen <- getOption("scipen")
  # reset scipen
  options(scipen = scipen_val)
  # digit condition for p-values
  if (decimals < 1) {stop("Number of decimals must be 1 or more.")}
  dig_cond <- (paste0("0.", paste(rep(0, decimals - 1), collapse = ""), 1)) %>%
    as.numeric()
  # p-value replacement if digit condition is met
  dig_replace <- paste0("<", dig_cond)
  # family
  glm_fam <- fit$family$family
  # summary of fit
  fit_sum <- summary(fit)
  # table of coefficient estimates, se, t, and p-values
  coef_matrix <- fit_sum$coefficients
  coef_param <- rownames(coef_matrix)
  param_cond <- grepl("\\^", coef_param)
  coef_param <- gsub("\\^", "^", coef_param)
  coef_param <- ifelse(!param_cond, coef_param,
                       substr(coef_param, 3, nchar(coef_param) - 1))
  coef_param <- paste0("$", coef_param, "$")
  coef_est <- fval(coef_matrix[ , 1])
  coef_se <- coef_matrix[ , 2]
  coef_se <- ifelse(coef_se < dig_cond, dig_replace, fval(coef_se))
  coef_z <- fval(coef_matrix[, 3])
  coef_p <- coef_matrix[ , 4]
  coef_p <- ifelse(coef_p < dig_cond, dig_replace, fval(coef_p))
  coef_table <- tibble(Parameter = coef_param,
                       Estimate = coef_est, SE = coef_se,
                       z = coef_z, p = coef_p)
  # add confidence intervals
  if (ci) {
    if (ci_wald) {
      ci_df <- fval(confint.default(fit, level = ci_level))
    } else {
      ci_df <- fval(confint(fit, level = ci_level))
    }
    ci_tib <- tibble(ci = paste0("(", ci_df[ , 1], ", ", ci_df[ , 2], ")"))
    names(ci_tib) <- ifelse(format == "latex" | format == "html",
                            paste0(ci_level * 100, "\\% C.I."), 
                            paste0(ci_level * 100, "% C.I."))
    coef_table <- coef_table %>%
      bind_cols(ci_tib)
  }
  # null dev
  null_dev <- fval(fit$null.deviance)
  null_df <- fit$df.null
  # residual dev
  res_dev <- fval(fit$deviance)
  res_df <- fit$df.residual
  # information crit. 
  loglike <- fval(-2 * logLik(fit)) 
  aic <- fval(stats::AIC(fit))
  bic <- fval(stats::BIC(fit))
  
  # print output ----
  
  if (format == "latex" | format == "html") {
    # footnote spaces
  if (format == "latex") {
    space_lg <- c("\\\\ \\\\ \\\\ \\\\")
    space_sm <- c("\\\\ \\\\")
  } else {
    space_lg <- c("\\ \\ \\ \\")
    space_sm <- c("\\ \\")
  }
  # output table 
  k <- kable(coef_table, format = format, caption = caption, align = align, 
              booktabs = T, escape = F, linesep = "") %>%
    footnote(general = paste("-2 log-like:", loglike, space_lg, 
                             "AIC:", aic, space_lg, "BIC:", bic),
             footnote_as_chunk = T, escape = F, general_title = " ") %>%
    footnote(general = paste("Residual Deviance:", res_dev, "on", res_df, "degrees of freedom"),
             footnote_as_chunk = T, escape = F, general_title = " ") %>%
    footnote(general = paste("    Null Deviance:", null_dev, "on", null_df, "degrees of freedom"),
             footnote_as_chunk = T, escape = F, general_title = " ")
  k <- kableExtra::kable_styling(k, latex_options = "hold_position",
                                 bootstrap_options = boot_options,
                                 full_width = full_width)
  return(k)
  } else {
    coef_table$Parameter <- substr(coef_table$Parameter, 2, nchar(coef_table$Parameter) - 1)
    k <- kable(coef_table, format = format, caption = caption, align = align, 
              booktabs = T, escape = F, linesep = "")
    print(k)
    # print other info
    paste("\n     Null Deviance:", null_dev, "on", null_df, "degrees of freedom") %>% 
      pander() 
    paste("\n Residual Deviance:", res_dev, "on", res_df, "degrees of freedom") %>%
      pander()
    paste("\n -2 log-like:", loglike, "  ", "AIC:", aic, "  ", "BIC:", bic) %>%
      pander()
  }
  # reset scipen 
  options(scipen = init_scipen)
}
