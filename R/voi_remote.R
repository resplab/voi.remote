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
#'   \code{"c"}: a matrix or data frame of samples from the distribution of
#'   costs.  There should be one column for each decision option.
#'
#'   \code{"e"}: a matrix or data frame of samples from the distribution of
#'   effects, likewise.
#'
#'   \code{"k"}: a vector of willingness-to-pay values.
#'
#'   Objects of class \code{"bcea"}, as created by the \pkg{BCEA} package, are in
#'   this "cost-effectiveness analysis" format, therefore they may be supplied as
#'   the \code{outputs} argument.
#'
#'   If \code{outputs} is a matrix or data frame it is assumed to be of "net
#'   benefit" form.  Otherwise if it is a list, it is assumed to be of "cost
#'   effectiveness analysis" form.
#'
#' @param nsim Number of simulations from the model to use for calculating
#'   EVPPI.  The first \code{nsim} rows of the objects in \code{inputs} and
#'   \code{outputs} are used.
#'
#' @return The expected value of perfect information, either as a single value, or a data frame indicating the value for each willingness-to-pay.
#' @export
#'
#' @examples
#' set.seed(1)
#' nsam <- 10000
#' inputs <- data.frame(
#'   p1 = rnorm(nsam, 1, 1),
#'   p2 = rnorm(nsam, 0, 2)
#' )
#' outputs_nb <- data.frame(
#'   t1 = 0,
#'   t2 = inputs$p1 - inputs$p2
#' )
#' nsim <- NULL
#' evpi.remote(outputs = outputs_nb, nsim = nsim)
evpi.remote <- function(outputs, nsim) {
  model_input <- list(outputs = outputs,
                      nsim = nsim,
                      func = "evpi")
  res <-
    peermodels::model_run(model_name = "voi", model_input, api_key = "aaHYJJb4hcrmBYY3")
  return(res)
}


#' Calculate the expected value of partial perfect information from a decision model
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
#'   \code{"c"}: a matrix or data frame of samples from the distribution of
#'   costs.  There should be one column for each decision option.
#'
#'   \code{"e"}: a matrix or data frame of samples from the distribution of
#'   effects, likewise.
#'
#'   \code{"k"}: a vector of willingness-to-pay values.
#'
#'   Objects of class \code{"bcea"}, as created by the \pkg{BCEA} package, are in
#'   this "cost-effectiveness analysis" format, therefore they may be supplied as
#'   the \code{outputs} argument.
#'
#'   If \code{outputs} is a matrix or data frame it is assumed to be of "net
#'   benefit" form.  Otherwise if it is a list, it is assumed to be of "cost
#'   effectiveness analysis" form.
#'
#' @param inputs Matrix or data frame of samples from the uncertainty
#'   distribution of the input parameters of the decision model.   The number
#'   of columns should equal the number of parameters, and the columns should
#'   be named.    This should have the same number of rows as there are samples
#'   in \code{outputs}, and each row of the samples in \code{outputs} should
#'   give the model output evaluated at the corresponding parameters.
#'
#' @param pars A character vector giving the parameters of interest, for which
#'   a single EVPPI calculation is required.  If the vector has multiple
#'   element, then the joint expected value of perfect information on all these
#'   parameters together is calculated.
#'
#'   Alternatively, \code{pars} may be a list.  Multiple EVPPI calculations are
#'   then performed, one for each component of \code{pars} defined in the above
#'   vector form.
#'
#'   \code{pars} must be specified if \code{inputs} is a matrix or data frame.
#'   This should then correspond to particular columns of \code{inputs}.    If
#'   \code{inputs} is a vector, this is assumed to define the single parameter
#'   of interest, then \code{pars} is not required.
#'
#' @param se If possible, calculate a standard error for the EVPPI.  Currently
#'   only supported for \code{method="gam"}.
#'
#' @param B Number of parameter replicates for calculating the standard error.
#'
#' @param nsim Number of simulations from the model to use for calculating
#'   EVPPI.  The first \code{nsim} rows of the objects in \code{inputs} and
#'   \code{outputs} are used.
#'
#' @param verbose If \code{TRUE}, then messages are printed describing each step of
#'   the calculation, if the method supplies these.  Useful to see the progress
#'   of slow calculations.
#'
#' @param method Character string indicating the calculation method. The default
#'   methods are based on nonparametric regression
#'
#' @param ... Other arguments to control specific methods.
#'
#' @return A data frame with a column \code{pars} indicating the parameter(s)
#'   and a column \code{evppi} giving the corresponding EVPPI.
#'
#'   If \code{outputs} is of "cost-effectiveness analysis" form so that there is
#'   one EVPPI per willingness-to-pay value, then a column \code{k} identifies the
#'   willingness-to-pay.
#'
#'   If standard errors are requested, then the standard errors are returned in
#'   the column \code{se}.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' nsam <- 10000
#' inputs <- data.frame(
#'   p1 = rnorm(nsam, 1, 1),
#'   p2 = rnorm(nsam, 0, 2)
#' )
#' outputs_nb <- data.frame(
#'   t1 = 0,
#'   t2 = inputs$p1 - inputs$p2
#' )
#' nsim <- NULL
#' pars <- "p1"
#' nsim <- NULL
#' method <- NULL
#' evppi.remote(outputs = outputs_nb, inputs = inputs, pars = pars, nsim = nsim, method = method)
evppi.remote <-
  function(outputs,
           inputs,
           pars,
           se,
           B,
           nsim,
           verbose,
           method,
           ...) {
    if (is.null(se)) {
      se <- FALSE
    }
    if (is.null(B)) {
      B <- 500
    }
    if (is.null(verbose)) {
      verbose <- FALSE
    }
    model_input <-
      list(
        outputs = outputs,
        inputs = inputs,
        pars = pars,
        se = se,
        B = B,
        nsim = nsim,
        verbose = verbose,
        func = "evppi",
        method = method,
        etc = ...
      )
    res <-
      peermodels::model_run(model_name = "voi",
                            model_input = model_input,
                            api_key = "aaHYJJb4hcrmBYY3")
    return(res)
  }



#' Traditional two-level Monte Carlo estimator of EVPPI.
#'
#' Traditional two-level Monte Carlo estimator of the expected value of partial
#' perfect information from a decision-analytic model.  Only useful in the
#' simplest of examples.  For realistically complex examples, the methods
#' implemented in the \code{\link{evppi}} function will usually be preferred.
#'
#' @param model_fn A function to evaluate a decision-analytic model at a given
#'   set of parameters. This should either return:
#'
#'   (net benefit format) a vector giving the net benefit for each decision
#'   option, or
#'
#'   (cost-effectiveness analysis format) a matrix or data frame with two rows,
#'   and one column for each decision option.  If the rows have names
#'   \code{"e"} and \code{"c"} then these are assumed to be the effects and
#'   costs respectively.
#'
#'   Otherwise, the first row is assumed to be the effects, and the second the
#'   costs.
#'
#' @param par_fn A function to generate a random sample of values for the
#'   parameters of \code{model_fn}. This should return a matrix or a data frame
#'   with named columns matching the arguments of \code{model_fn}.
#'
#'   If any required arguments to \code{model_fn} are not supplied in this
#'   return value, then \code{evppi_mc} looks for them in the list supplied as
#'   the \code{mfargs} argument.
#'
#'   If any required arguments are not found in the results of \code{par_fn} or
#'   \code{mfargs}, and if \code{model_fn} defines default values for those
#'   arguments, then those default values are used.
#'
#'   The first argument of \code{par_fn} should be an integer \code{n} denoting
#'   the number of random values to draw for each parameter.  The object
#'   returned by \code{par_fn} should then have \code{n} rows, and one column
#'   for each parameter. If one value is drawn, then \code{par_fn} is also
#'   allowed to return a vector, but this should still be named.
#'
#'   The parameters may be correlated.  If we wish to compute the EVPPI for a
#'   parameter which is correlated with a different parameter q, then `par_fn`
#'   must have an argument with the name of that parameter.  If that argument
#'   is set to a fixed value, then `par_fn` should return a sample drawn
#'   conditionally on that value.  If that argument is not supplied, then
#'   `par_fn` must return a sample drawn from the marginal distribution. See
#'   the vignette for an example.
#'
#' @param pars A character vector giving the parameters of interest, for which
#'   the EVPPI is required.   This should correspond to an explicit argument to
#'   \code{model_fn}.
#'
#'   The parameters of interest are assumed to have uncertainty distributions
#'   that are independent of those of the other parameters.
#'
#' @param nouter Number of outer samples
#'
#' @param ninner Number of inner samples
#'
#' @param wtp Vector of willingness-to-pay values.  Only used if
#'   \code{model_fn} is in cost-effectiveness analyis format.
#'
#' @param mfargs Named list of additional arguments to supply to
#'   \code{model_fn}.
#'
#' @param verbose Set to \code{TRUE} to print some additional messages to
#' help with debugging.
#'
#' @return
#' @export
#'
#' @examples
#' model_fn_nb <- function(p1, p2) {
#'   c(0, p1 - p2)
#' }
#' par_fn <- function(n) {
#'   data.frame(
#'     p1 = rnorm(n, 1, 1),
#'     p2 = rnorm(n, 0, 2)
#'   )
#' }
#' pars <- "p1"
#' ninner <- 1000
#' nouter <- 100
#' wtp <- NULL
#' mfargs <- NULL
#' evppi_mc.remote(model_fn = model_fn, par_fn = par_fn, pars = pars, nouter = nouter, ninner = ninner, wtp = wtp, mfargs = mfargs)
evppi_mc.remote <-
  function(model_fn,
           par_fn,
           pars,
           nouter,
           ninner,
           wtp,
           mfargs,
           verbose) {
    if (is.null(verbose)) {
      verbose <- FALSE
    }
    model_input <-
      list(
        model_fn = model_fn,
        par_fn = par_fn,
        pars = pars,
        nouter = nouter,
        ninner = ninner,
        wtp = wtp,
        mfargs = mfargs,
        verbose = verbose,
        func = "evppi_mc"
      )
    res <-
      peermodels::model_run(model_name = "voi", model_input, api_key = "aaHYJJb4hcrmBYY3")
    return(res)
  }

#' Calculate the expected value of sample information from a decision-analytic model
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
#'   \code{"c"}: a matrix or data frame of samples from the distribution of
#'   costs.  There should be one column for each decision option.
#'
#'   \code{"e"}: a matrix or data frame of samples from the distribution of
#'   effects, likewise.
#'
#'   \code{"k"}: a vector of willingness-to-pay values.
#'
#'   Objects of class \code{"bcea"}, as created by the \pkg{BCEA} package, are in
#'   this "cost-effectiveness analysis" format, therefore they may be supplied as
#'   the \code{outputs} argument.
#'
#'   If \code{outputs} is a matrix or data frame it is assumed to be of "net
#'   benefit" form.  Otherwise if it is a list, it is assumed to be of "cost
#'   effectiveness analysis" form.
#'
#' @param inputs Matrix or data frame of samples from the uncertainty
#'   distribution of the input parameters of the decision model.   The number
#'   of columns should equal the number of parameters, and the columns should
#'   be named.    This should have the same number of rows as there are samples
#'   in \code{outputs}, and each row of the samples in \code{outputs} should
#'   give the model output evaluated at the corresponding parameters.
#'
#' @param study Name of one of the built-in study types supported by this
#'   package for EVSI calculation.  If this is supplied, then the columns of
#'   \code{inputs} that correspond to the parameters governing the study data
#'   should be identified in \code{poi}.
#'
#'   Currently supported studies are
#'
#'   \code{"binary"} A study with a binary outcome observed on one sample of
#'   individuals.   Requires one parameter: the probability of the outcome.
#'   The sample size is specifed in the \code{n} argument to \code{evsi()}, and
#'   the binomially-distributed outcome is named \code{X1}.
#'
#' \code{"trial_binary"} Two-arm trial with a binary outcome.   Requires two
#' parameters: the probability of the outcome in arm 1 and 2 respectively.  The
#' sample size is the same in each arm, specifed in the \code{n} argument to
#' \code{evsi()}, and the binomial outcomes are named \code{X1} and \code{X2}
#' respectively.
#'
#' \code{"normal_known"} A study of a normally-distributed outcome, with a
#' known standard deviation, on one sample of individuals.  Likewise the sample
#' size is specified in the \code{n} argument to \code{evsi()}.  The standard
#' deviation defaults to 1, and can be changed by specifying \code{sd} as a
#' component of the \code{aux_pars} argument, e.g.
#' \code{evsi(..., aux_pars=list(sd=2))}.
#'
#' Either \code{study} or \code{datagen_fn} should be supplied to \code{evsi()}.
#'
#' @param datagen_fn  If the proposed study is not one of the built-in types
#' supported, it can be specified in this argument as an R function to sample
#' predicted data from the study.  This function should have the following specification:
#'
#' 1. the function's first argument should be a data frame of parameter
#' simulations, with one row per simulation and one column per parameter.  The
#' parameters in this data frame must all be found in \code{inputs}.
#'
#' 2. the function should return a data frame.
#'
#' 3. the returned data frame should have number of rows equal to the number of
#' parameter simulations in \code{inputs}.
#'
#' 4. if \code{inputs} is considered as a sample from the posterior, then
#' \code{datagen_fn(inputs)} returns a corresponding sample from the posterior
#' predictive distribution, which includes two sources of uncertainty: (a)
#' uncertainty about the parameters and (b) sampling variation in observed data
#' given fixed parameter values.
#'
#' 5. the function can optionally have more than one argument. If so, these
#' additional arguments should be given default values in the definition of
#' \code{datagen_fn}.  These arguments might be used to define sample sizes for
#' a proposed study.
#'
#' @param pars Character vector identifying which columns of \code{inputs} are
#' the parameters required to generate data from the proposed study.  Required
#' if the proposed study is specified through the \code{study} argument, but not
#' if it is specified through the \code{datagen_fn} argument.
#' For example, if \code{study = "trial_binary"} is specified, then \code{pars}
#' should be a vector of two elements naming the probability of the outcome in
#' arm 1 and arm 2 respectively.
#'
#' The \code{pars} argument is also required for the methods which involve an
#' intermediate EVPPI calculation, that is the \code{"is"} and \code{"mm"}.  It
#' should consist of the variables used in the definition of \code{datagen_fn}
#' (and \code{likelihood} if used TODO ALSO in \code{analysis_model} and
#' \code{model}?) and only these variables.
#'
#' @param n Sample size of future study - optional argument to datagen_fn -
#' facilitates calculating EVSI for multiple sample sizes.  TODO if we want to
#' design trials with multiple unbalanced arms, we'll need more than one argument.
#'
#' @param likelihood Likelihood function, required (and only required) for the
#' importance sampling method.  This should have two arguments as follows:
#'
#' 1. a data frame of predicted data. Columns are defined by the number of
#' outcomes in the data, and names matching the data frame returned by
#' \code{datagen_fn}.
#'
#' 2. a data frame of parameter values, whose names should all correspond to
#' variables in \code{inputs}.
#'
#' The function should return a vector whose length matches the number of rows of
#' the parameters data frame given as the second argument.   Each element of the
#' vector gives the likelihood of the corresponding set of parameters, given the
#' data in the first argument.
#'
#' Examples of this are currently in \code{tests/tests_slow} and \code{tests/testthat}
#' in the package directory.
#'
#' Note the definition of the likelihood should agree with the definition of
#' \code{datagen_fn} to define a consistent sampling distribution for the data.
#'
#' @param analysis_model Function which fits a Bayesian model to the generated
#' data.   Under development (need to decide format, output, JAGS dependencies,
#' etc.). Required for \code{method="mm"} (and Jalal method if n0 not given)
#'
#' @param analysis_options List of arguments required by \code{analysis_model}.
#' Under development - for \code{method="mm"} and Jalal method.
#'
#' @param decision_model Function which evaluates the decision-analytic model,
#' given parameter values.  Under development - for \code{method="mm"} and Jalal
#' method.  Need to decide the required format for nb, c, e output.
#'
#' @param Q Number of quantiles to use in \code{method="mm"} (under development).
#'
#' @param npreg_method Method to use to calculate the EVPPI, for those methods
#' that require it.  This is passed to \code{\link{evppi}} as the \code{method}
#' argument.
#'
#' @param nsim Number of simulations from the model to use for calculating EVPPI.
#' The first \code{nsim} rows of the objects in \code{inputs} and \code{outputs}
#'  are used.
#'
#' @param verbose Set to \code{TRUE} to print some additional messages to
#' help with debugging.
#'
#' @param method haracter string indicating the calculation method. The default
#'   methods are based on nonparametric regression

#'
#' @param ... Other arguments required by specific methods
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(1)
#' nsam <- 10000
#' inputs <- data.frame(
#'   p1 = rnorm(nsam, 1, 1),
#'   p2 = rnorm(nsam, 0, 2)
#' )
#' outputs_nb <- data.frame(
#'   t1 = 0,
#'   t2 = inputs$p1 - inputs$p2
#' )
#' datagen_normal <- function(inputs, n=100, sd=1){
#'    data.frame(xbar = rnorm(n = nrow(inputs),
#'                            mean = inputs[,"p1"],
#'                            sd = sd / sqrt(n)))
#'}
#' study <- NULL
#' pars <- "p1"
#' n <- c(10,100,1000)
#' aux_pars <- NULL
#' method <-  NULL
#' likelihood <- NULL
#' analysis_model <- NULL
#' analysis_options <- NULL
#' decision_model <- NULL
#' nsim <- NULL
#' evsi.remote(outputs = outputs_nb, inputs = inputs, study = study, datagen_fn = datagen_normal, pars = pars, n = n, method = method, likelihood = likelihood, analysis_model = analysis_model, analysis_options = analysis_options, decision_model = decision_model, nsim = nsim)
#'
evsi.remote <-
  function(outputs,
           inputs,
           study,
           datagen_fn,
           pars,
           n,
           likelihood,
           analysis_model,
           analysis_options,
           decision_model,
           Q,
           npreg_method,
           nsim,
           verbose,
           method,
           ...) {
    if (is.null(n)) {
      n <- 100
    }
    if (is.null(Q)) {
      Q <- 30
    }
    if (is.null(npreg_method)) {
      npreg_method <- "gam"
    }
    if (is.null(verbose)) {
      verbose <- FALSE
    }
    model_input <-
      list(
        outputs = outputs,
        inputs = inputs,
        study = study,
        datagen_fn = datagen_fn,
        pars = pars,
        n = n,
        likelihood = likelihood,
        analysis_model = analysis_model,
        analysis_options = analysis_options,
        decision_model = decision_model,
        Q = Q,
        npreg_method = npreg_method,
        nsim = nsim,
        verbose = verbose,
        func = "evsi",
        method = method,
        etc = ...
      )
    res <-
      peermodels::model_run(model_name = "voi", model_input, api_key = "aaHYJJb4hcrmBYY3")
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
evsivar.remote <-
  function(outputs,
           inputs,
           study,
           datagen_fn,
           pars,
           n,
           likelihood,
           analysis_model,
           analysis_options,
           decision_model,
           Q,
           npreg_method,
           nsim,
           verbose,
           method,
           ...) {
    model_input <-
      list(
        outputs = outputs,
        inputs = inputs,
        study = study,
        datagen_fn = datagen_fn,
        pars = pars,
        n = n,
        likelihood = likelihood,
        analysis_model = analysis_model,
        analysis_options = analysis_options,
        decision_model = decision_model,
        Q = Q,
        npreg_method = npreg_method,
        nsim = nsim,
        verbose = verbose,
        func = "evsivar",
        method = method,
        etc = ...
      )
    res <-
      peermodels::model_run(model_name = "voi", model_input, api_key = "aaHYJJb4hcrmBYY3")
    return(res)
  }
