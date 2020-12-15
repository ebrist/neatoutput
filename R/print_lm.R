

#' Neat Table of LM Output
#' 
#' @description print_lm() generates neat tables (pandoc, latex, or html formats) of summary output from a linear model object. 
#'
#' @param fit a linear model object produced by lm.
#' @param caption option to add a customized caption to the output table. default caption shows the linear model.
#' @param ci option to show confidence intervals for each model parameter.
#' @param ci_level option to set the confidence level. 
#' @param decimals option to set the number of decimals displayed. Must be 1 or more.
#' @param scipen_val option to reset scipen.  
#' @param format A character string: kable format. Possible values are latex, html, markdown, pandoc, and rst. The recommended format is "html" when knitting to .html files, and "latex" when knitting to .pdf files. The pandoc format (default) is recommended when printing to the Console. 
#' @param full_width Logical: whether html tables should span full width of page.
#' @param align Column alignment: a character vector consisting of 'l' (left), 'c' (center) and/or 'r' (right).
#' @param boot_options A character vector: arguments to be passed to bootstrap_options for html tables.
#'
#' @return a formatted table of linear model fit results
#' @export
#' 
#' @examples 
#' # produce LaTeX table for a .pdf document
#' lm_fit <- lm(mpg ~ ., data = mtcars)
#' print_lm(lm_fit)
#' 
#' # produce HTML table for a .html document
#' lm_fit <- lm(mpg ~ ., data = mtcars)
#' print_lm(lm_fit, format = "html")
#' 
print_lm <- function(fit, caption = "LM Results", ci = T, ci_level = 0.95, decimals = 4, 
                     scipen_val = 999, format = "pandoc", full_width = F, align = "r", 
                     boot_options = c("striped")) {
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
  coef_t <- fval(coef_matrix[, 3])
  coef_p <- coef_matrix[ , 4]
  coef_p <- ifelse(coef_p < dig_cond, dig_replace, fval(coef_p))
  coef_table <- tibble(Parameter = coef_param,
                       Estimate = coef_est, SE = coef_se,
                       t = coef_t, p = coef_p)
  # add confidence intervals
  if (ci) {
    ci_df <- fval(confint(fit, level = ci_level))
    ci_tib <- tibble(ci = paste0("(", ci_df[ , 1], ", ", ci_df[ , 2], ")"))
    names(ci_tib) <- ifelse(format == "latex" | format == "html",
                            paste0(ci_level * 100, "\\% C.I."), 
                            paste0(ci_level * 100, "% C.I."))
    coef_table <- coef_table %>%
      bind_cols(ci_tib)
  }
  # residual squared error
  rse <- fval(sigma(fit)) 
  # residual squared error df
  rse_df <- fit$df.residual
  # r-squared
  rsq <- fval(fit_sum$r.squared)
  # adjusted r-squared
  a_rsq <- fval(fit_sum$adj.r.squared)
  # predicted r-squared
  p_rsq <- fval(prsq(fit)) 
  # f-test
  f_stat <- fval(fit_sum$fstatistic[[1]])
  # model df
  f_mdf <- fit_sum$fstatistic[[2]]
  # error df
  f_edf <- fit_sum$fstatistic[[3]]
  # p-value
  f_p <- (1 - pf(as.numeric(f_stat), f_mdf, f_edf))
  f_p <- ifelse(f_p < dig_cond, dig_replace, fval(f_p))
  # information crit. 
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
    footnote(general = paste(aic, space_lg, "$BIC$:", bic),
             footnote_as_chunk = T, escape = F, general_title = "AIC:") %>%
    footnote(general = paste(rse, "on", rse_df, "df"),
             footnote_as_chunk = T, escape = F, general_title = "RSE:") %>%
    footnote(general = paste(rsq, space_lg,
                             "Adj. $R^2$:", a_rsq, space_lg,
                             "Predicted $R^2$:", p_rsq),
             footnote_as_chunk = T, escape = F, general_title = "$R^2:$") %>%
    footnote(general = paste("statistic:", f_stat, "on", f_mdf, "and",
                             f_edf, "df,", space_sm, "p-value:", f_p),
             footnote_as_chunk = T, escape = F, general_title = "F")
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
    paste("\n F-statistic:", f_stat, "on", f_mdf, "and",
          f_edf, "df,", "p-value:", f_p) %>% 
      pander()
    paste("\n R-sq:", rsq, "   Adj. R-sq:", a_rsq, "   Predicted R-sq:", p_rsq) %>%
      pander()
    paste("\n RSE:", rse, "on", rse_df, "df") %>%
      pander()
    paste("\n AIC:", aic, "   BIC:", bic) %>%
      pander()
  }
  # reset scipen 
  options(scipen = init_scipen)
}
