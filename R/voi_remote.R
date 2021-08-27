#' Calculate the expected value of perfect information from a decision model
#'
#' @param outputs This could take one of two forms
#'
#'   "net benefit" form: a matrix or data frame of samples from the uncertainty
#'   distribution of the expected net benefit.  The number of rows should equal
#'   the number of samples, and the number of columns should equal the number
#'   of decision options.
#'
#'   "cost-effectiveness analysis" form: a list with the following named
#'   components:
#'
#' @param nsim Number of simulations from the model to use for calculating
#'   EVPPI.  The first \code{nsim} rows of the objects in \code{inputs} and
#'   \code{outputs} are used.
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(1)
#' nsam <- 10000
#' inputs <- data.frame(p1 = rnorm(nsam, 1, 1),
#'                      p2 = rnorm(nsam, 0, 2))
#' outputs_nb <- data.frame(t1 = 0,
#' t2 = inputs$p1 - inputs$p2)
#' nsim <- NULL
#' evpi.remote(outputs_nb, nsim)
evpi.remote <- function(outputs, nsim) {
  model_input <- list(outputs=outputs, nsim=nsim, func='evpi')
  res <- peermodels::model_run(model_name='voi', model_input, api_key='aaHYJJb4hcrmBYY3')
  return(res)
}


#' Title
#'
#' @param outputs
#' @param inputs
#' @param pars
#' @param se
#' @param B
#' @param nsim
#' @param verbose
#' @param method
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
evppi.remote <- function(outputs, inputs, pars, se, B, nsim, verbose, method, ...) {
  model_input <- list(outputs=outputs, inputs=inputs, pars=pars, se=se, B=B, nsim=nsim, verbose=verbose, func='evppi', method=method, etc=...)
  res <- peermodels::model_run(model_name='voi', model_input, api_key='aaHYJJb4hcrmBYY3')
  return(res)
}


#' Title
#'
#' @param model_fn
#' @param par_fn
#' @param pars
#' @param nouter
#' @param ninner
#' @param wtp
#' @param mfargs
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
evppi_mc.remote <- function(model_fn, par_fn, pars, nouter, ninner, wtp, mfargs, verbose) {
  model_input <- list(model_fn=model_fn, par_fn=par_fn, pars=pars, nouter=nouter, ninner=ninner, wtp=wtp, mfargs=mfargs, verbose=verbose, func='evppi_mc')
  res <- peermodels::model_run(model_name='voi', model_input, api_key='aaHYJJb4hcrmBYY3')
  return(res)
}

#' Title
#'
#' @param outputs
#' @param inputs
#' @param study
#' @param datagen_fn
#' @param pars
#' @param n
#' @param likelihood
#' @param analysis_model
#' @param analysis_options
#' @param decision_model
#' @param Q
#' @param npreg_method
#' @param nsim
#' @param verbose
#' @param method
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
evsi.remote <- function(outputs, inputs, study, datagen_fn, pars, n, likelihood, analysis_model, analysis_options, decision_model, Q, npreg_method, nsim, verbose, method, ...) {
  model_input <- list(outputs=outputs, inputs=inputs, study=study, datagen_fn=datagen_fn, pars=pars, n=n, likelihood=likelihood, analysis_model=analysis_model, analysis_options=analysis_options, decision_model=decision_model, Q=Q, npreg_method=npreg_method, nsim=nsim, verbose=verbose, func='evsi', method=method, etc=...)
  res <- peermodels::model_run(model_name='voi', model_input, api_key='aaHYJJb4hcrmBYY3')
  return(res)
}

#' Title
#'
#' @param outputs
#' @param inputs
#' @param study
#' @param datagen_fn
#' @param pars
#' @param n
#' @param likelihood
#' @param analysis_model
#' @param analysis_options
#' @param decision_model
#' @param Q
#' @param npreg_method
#' @param nsim
#' @param verbose
#' @param method
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
evsivar.remote <- function(outputs, inputs, study, datagen_fn, pars, n, likelihood, analysis_model, analysis_options, decision_model, Q, npreg_method, nsim, verbose, method, ...) {
  model_input <- list(outputs=outputs, inputs=inputs, study=study, datagen_fn=datagen_fn, pars=pars, n=n, likelihood=likelihood, analysis_model=analysis_model, analysis_options=analysis_options, decision_model=decision_model, Q=Q, npreg_method=npreg_method, nsim=nsim, verbose=verbose, func='evsivar', method=method, etc=...)
  res <- peermodels::model_run(model_name='voi', model_input, api_key='aaHYJJb4hcrmBYY3')
  return(res)
}
