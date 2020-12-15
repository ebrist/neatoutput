
#' Predicted R-Squared from a Linear Regression Model
#'
#' @description calculates the predicted r-squared from a linear model object produced by lm(). 
#'
#' @param fit  a linear model object produced by lm.
#'
#' @return the predicted r-squared of the linear model
#' @export
#'
#' @examples
#' lm_fit <- lm(mpg ~ ., data = mtcars)
#' prsq(lm_fit)
#' 
prsq <- function(fit) {
  # predicted residual sums of squares
  press <- sum((fit$residuals / (1 - lm.influence(fit)$hat))^2)
  # total sums of squares
  tss <- sum(anova(fit)$"Sum Sq")
  # predicted r-squared
  1 - press / tss
}