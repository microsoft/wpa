# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Ljung and Box Portmanteau Test
#'
#' @description The Ljung-Box (1978) modified portmanteau test. In the
#'   multivariate time series, this test statistic is asymptotically equal to
#'   `Hosking`.
#'
#'   This method and the bottom documentation is taken directly from the
#'   original 'portes' package.
#'
#' @param obj a univariate or multivariate series with class "numeric",
#'   "matrix", "ts", or ("mts" "ts"). It can be also an object of fitted
#'   time-series model with class "ar", "arima0", "Arima", ("ARIMA forecast
#'   ARIMA Arima"), "lm", ("glm" "lm"), or "varest". obj may also an object with
#'   class "list" (see details and following examples).
#'
#' @param lags vector of lag auto-cross correlation coefficients used for
#'   `Hosking` test.
#'
#' @param order Default is zero for testing the randomness of a given sequence
#'   with class "numeric", "matrix", "ts", or ("mts" "ts"). In general order
#'   equals to the number of estimated parameters in the fitted model. If obj is
#'   an object with class "ar", "arima0", "Arima", "varest", ("ARIMA forecast
#'   ARIMA Arima"), or "list" then no need to enter the value of order as it
#'   will be automatically determined. For obj with other classes, the order is
#'   needed for degrees of freedom of asymptotic chi-square distribution.
#'
#' @param season 	seasonal periodicity for testing seasonality. Default is 1 for
#'   testing the non seasonality cases.
#'
#' @param squared.residuals if `TRUE` then apply the test on the squared values.
#'   This checks for Autoregressive Conditional Heteroscedastic, `ARCH`,
#'   effects. When `squared.residuals = FALSE`, then apply the test on the usual
#'   residuals.
#'
#' @details However the portmanteau test statistic can be applied directly on
#'   the output objects from the built in R functions ar(), ar.ols(), ar.burg(),
#'   ar.yw(), ar.mle(), arima(), arim0(), Arima(), auto.arima(), lm(), glm(),
#'   and VAR(), it works with output objects from any fitted model. In this
#'   case, users should write their own function to fit any model they want,
#'   where they may use the built in R functions FitAR(), garch(), garchFit(),
#'   fracdiff(), tar(), etc. The object obj represents the output of this
#'   function. This output must be a list with at least two outcomes: the fitted
#'   residual and the order of the fitted model (list(res = ..., order = ...)).
#'   See the following example with the function FitModel().
#'
#'   Note: In stats R, the function Box.test was built to compute the Box and
#'   Pierce (1970) and Ljung and Box (1978) test statistics only in the
#'   univariate case where we can not use more than one single lag value at a
#'   time. The functions BoxPierce and LjungBox are more accurate than Box.test
#'   function and can be used in the univariate or multivariate time series at
#'   vector of different lag values as well as they can be applied on an output
#'   object from a fitted model described in the description of the function
#'   BoxPierce.
#'
#' @return
#' The Ljung and Box test statistic with the associated p-values for different
#' lags based on the asymptotic chi-square distribution with `k^2(lags-order)`
#' degrees of freedom.
#'
#' @author
#' Esam Mahdi and A.I. McLeod
#'
#' @references
#' Ljung, G.M. and Box, G.E.P (1978). "On a Measure of Lack of Fit in Time
#' Series Models". Biometrika, 65, 297-303.
#'
#' @examples
#' x <- rnorm(100)
#' LjungBox(x) # univariate test
#'
#' x <- cbind(rnorm(100),rnorm(100))
#' LjungBox(x) # multivariate test
#'
#' @export

LjungBox <- function(
  obj,
  lags = seq(5, 30, 5),
  order = 0,
  season = 1,
  squared.residuals = FALSE
  ){

    class.obj <- class(obj)[1]

    TestType <- "0"

    if (class.obj == "ts" || class.obj == "numeric" || class.obj ==
        "matrix" || class.obj == "mts")

      TestType <- "1"

    if (class.obj == "ar" || class.obj == "arima0" || class.obj ==
        "Arima" || class.obj == "ARIMA" || class.obj == "varest" || class.obj == "lm"
        || class.obj == "glm" || class.obj == "list")

      TestType <- "2"

    if (TestType == "0")

      stop("obj must be class ar, arima0, Arima, (ARIMA forecast_ARIMA Arima), varest, lm, (glm lm), ts, numeric, matrix, (mts ts), or list")

    Maxlag <- max(lags)

    if (TestType == "1")

      res <- as.ts(obj)

    else {

      GetResid <- GetResiduals(obj)
      res <- GetResid$res
      order <- GetResid$order

    }

    if (squared.residuals){
      res <- res ^ 2
    }

    n <- NROW(res)
    k <- NCOL(res)

    if (Maxlag*season >= n){
      stop("Maximum value of arguments lags * season can't exceed n!")
    }

    df <- k^2*(lags-order)

    NegativeDF <- which(df<0)

    df[NegativeDF] <- 0

    Accmat <- stats::acf(res, lag.max = Maxlag*season, plot = FALSE, type = "correlation")$acf

    inveseR0 <- solve(Accmat[1,,])

    prodvec <- numeric(Maxlag*season)

    for(l in 1:Maxlag){
      tvecR <- t(as.vector(Accmat[l*season+1,,]))
      prodvec[l] <- 1/(n-l)*crossprod(t(tvecR),crossprod(t(kronecker(inveseR0,inveseR0)),t(tvecR)))
    }

    Q <- n*(n+2)*cumsum(prodvec)


    STATISTIC <- Q[lags]

    PVAL <- 1 - stats::pchisq(STATISTIC,df)

    PVAL[NegativeDF] <- NA

    summary <- matrix(c(lags,STATISTIC,df,PVAL),ncol=4)

    dimnames(summary) <-
      list(
        rep("", length(STATISTIC)),
        c("lags", "statistic", "df", "p-value")
        )

    return(summary)
  }

#' @title
#' Extract Residuals from ARIMA, VAR, or any Simulated Fitted Time Series Model
#'
#' @description
#' This utility function is useful to use in the portmanteau functions,
#' BoxPierce, MahdiMcLeod, Hosking, LiMcLeod, LjungBox, and portest.
#' GetResiduals() function takes a fitted time-series object with class "ar",
#' "arima0", "Arima", ("ARIMA forecast ARIMA Arima"), "lm", ("glm" "lm"),
#' "varest", or "list". and returns the residuals and the order from the fitted
#' object.
#'
#' This method and the bottom documentation is taken directly from the original
#' 'portes' package.
#'
#' @param obj a fitted time-series model with class "ar", "arima0", "Arima",
#'   ("ARIMA forecast ARIMA Arima"), "lm", ("glm" "lm"), "varest", or "list".
#'
#' @return
#' List of order of fitted time series model and residuals from this model.
#'
#' @author
#' Esam Mahdi and A.I. McLeod.
#'
#' @examples
#' fit <- arima(Nile, c(1, 0, 1))
#' GetResiduals(fit)
#'
#' @export

GetResiduals <- function(obj){

    class.obj = class(obj)[1]

    if (class.obj != "ar" && class.obj != "arima0" && class.obj != "Arima" && class.obj != "varest" &&
        class.obj != "ARIMA" && class.obj != "lm"
        && class.obj != "glm" && class.obj != "list" )
      stop("obj must be class ar, arima0, Arima, (ARIMA forecast_ARIMA Arima), varest, lm, (glm lm), or list")

    if (all(class.obj=="ar")){

      order <- obj$order
      res <- ts(as.matrix(obj$resid)[-(1:order),])

    } else if (all(class.obj == "arima0") || all(class.obj == "Arima")|| all (class.obj == "ARIMA")) {

      pdq <- obj$arma
      p <- pdq[1]
      q <- pdq[2]
      ps <- pdq[3]
      qs <- pdq[4]
      order <- p+q+ps+qs
      res <- ts(obj$residuals)

    } else if (all(class.obj=="varest")){

      order <- obj$p
      res <- resid(obj)

    } else if (all(class.obj == "list")){

      order <- obj$order

      if(is.null(order)){

        order <- 0

      }

      res <- obj$res
    }

    if (all(class.obj=="lm") || all(class.obj == "glm")){
      order <- 0
      res <- obj$residuals
    }

    list(order = order, res = res)
  }
