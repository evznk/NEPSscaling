#' Function for estimating plausible values with NEPS competence data and NEPS
#' scaled item parameters
#'
#' @param SC numeric. The starting cohort used for the analysis is indicated by
#' an integer value (e.g., starting cohort one = 1).
#' @param domain character. The competence domain of interest is indicated by
#' the domain abbreviation as used in the variable names (see Fuß, D., Gnambs,
#' T., Lockl, K., & Attig, M., 2019).
#' @param wave numeric. The wave of competence testing is indicated by an
#' integer value (e.g., wave one = 1).
#' @param path character; file path leading to the location of the competence
#' data
#' @param bgdata data frame containing background variables. Categorical
#' variables have to be specified as factors. If \code{bgdata = NULL}, plausible
#' values are estimated without a background model. Missing data in the
#' covariates is imputed using sequential classification and regression trees.
#' If pre-imputed background data is to be used, it has to be formatted as a
#' list of \code{nmi} data.frames containing at least ID_t and 1 further variable;
#' categorical variables have to be specified as factors as well.
#' @param npv numeric; number of plausible values to be estimated; defaults to
#' 10.
#' @param nmi numeric; denotes the number of multiple imputations for missing
#' covariate data (defaults to 10).
#' @param min_valid numeric; minimum number of valid responses for a test
#' takers to be included in the estimation process. Defaults to 3 following NEPS
#' scaling standards (see Pohl & Carstensen, 2012).
#' @param longitudinal logical. TRUE indicating that a unidimensional model
#' per measurement points is to be estimated and subsequently linked to form
#' longitudinal estimates. Defaults to FALSE.
#' @param rotation logical. TRUE indicating that the competence scores should
#' be corrected for the rotation design of the booklets. Defaults to TRUE. If
#' both longitudinal and rotation are TRUE, test rotation is ignored and the
#' argument rotation is set to FALSE automatically.
#' @param include_nr logical; whether the number of not-reached items as a proxy
#' for processing speed should be included in the background model (the default
#' is TRUE)
#' @param adjust_school_context logical; whether the school context should be
#' included in the background models of SC3 and SC4 (the default is TRUE)
#' @param exclude vector (cross-sectional) or list (longitudinal). If
#' some variables shall be used for one time point, but not the other, the item
#' is listed in a character vector for the wave it should NOT be used. E.g.,
#' \code{list(w3 = "gender")} excludes the variable gender for wave 3. The
#' assessment waves can be looked up with \code{currently_implemented()}. Please
#' note that there has to be at least one background variable per specified
#' wave left. Otherwise, an error will be thrown.
#' @param verbose logical; whether progress should be displayed in the console
#' (the default is TRUE)
#' @param seed integer; seed for random number generators in plausible values
#' estimation
#' @param returnAll logical; if TRUE all estimated PVs during nested imputation
#' are returned, otherwise only \code{npv} PVs are returned. Defaults to FALSE.
#' @param control list of additional options. If \code{EAP = TRUE}, the EAPs
#' will be returned as well; for \code{WLE = TRUE} WLEs are returned.
#' Furthermore, additional control options for are collected in the list `ML`.
#' `minbucket` defines the minimum number of observations in any terminal CART
#' node (defaults to 5), `cp` determines the minimum decrease of overall lack of
#' fit by each CART split (defaults to 0.0001).
#'
#' @return \code{plausible_values()} returns an object of class \code{pv_obj}
#' containing:
#' \describe{
#' \item{SC}{Starting cohort that plausible values were estimated for}
#' \item{domain}{Competence domain that plausible values were estimated for}
#' \item{wave}{Wave(s) that plausible values were estimated for}
#' \item{type}{Whether cross-sectional ("cross") or longitudinal ("long")
#' plausible values were estimated}
#' \item{rotation}{In most assessments the position of the competence test was
#' rotated with another competence domain. If this was the case for the
#' specific estimation, this variable indicates whether the correction was
#' applied. Depending on the estimation context, this variable may have been
#' automatically set by the function and thus differ from user input}
#' \item{min_valid}{The minimum number of answers a test taker must have given}
#' \item{n_testtakers}{The number of persons for who plausible values are
#' estimated (per waves)}
#' \item{include_nr}{Whether the number of not-reached missing values per
#' person is to be used as a proxy for processing time}
#' \item{adjust_school_context}{Whether the WLE competence value mean per school
#' is to be used to approximate the multi-level structure of the data. Only
#' applies to cohorts/waves that were assessed in school}
#' \item{path}{The path leading to the SUF competence data}
#' \item{valid_responses_per_person}{A \code{data.frame} containing the
#' \code{ID_t} and the number of valid responses given by the respective
#' individual}
#' \item{npv}{The number of plausible values that are returned by the function}
#' \item{control}{The control variables that were applied to fine-tune the
#' estimation algorithms}
#' \item{position}{A \code{data.frame} containing the \code{ID_t} and the
#' position the respective individual received the testlet in (first or second).
#' Only applies if a rotated design has been estimated.}
#' \item{posterior_means}{The overall mean of all persons' abilities for the
#' EAPs and WLEs (if estimated) as well as across all PVs and per PV
#' imputation.}
#' \item{posterior_variances}{The overall variance of all persons' abilities for
#' the EAPs and WLEs (if estimated) as well as across all PVs and per PV
#' imputation.}
#' \item{seed}{Seed for random number generator; if supplied.}
#' \item{pv}{A list of \code{npv} \code{data.frame}s containing one plausible
#' value per wave each and the imputed data set that was used to estimate the
#' plausible value. The data sets are sampled randomly from \code{npv} *
#' \code{nmi} estimated data sets. Accordingly, some imputations might not be
#' present in the output data. This extends to reliability estimates and latent
#' regression coefficients that are also estimated per model / imputed data set.
#' Additionally, if \code{include_nr} was specified, the background model is
#' enriched by the number of not reached items (\code{not_reached}) per test
#' taker as a proxy for response times. Furthermore, if
#' \code{adjust_school_context} was specified, the background model is enriched
#' by the average competence per school.}
#' \item{eap}{A \code{data.frame} containing the \code{ID_t} and the ability
#' EAP value for the respective individual}
#' \item{treeplot}{A \code{list} containing the ggplot objects for the
#' constructed CART trees per imputation}
#' \item{variable_importance}{A \code{list} containing the variable importance
#' statistics for the predictor variables per imputation}
#' \item{EAP_rel}{The EAP reliability is returned for each sampled model}
#' \item{wle}{A \code{data.frame} containing the \code{ID_t} and the ability
#' WLE value for the respective individual}
#' \item{WLE_rel}{The WLE reliability is returned}
#' \item{regr_coeff}{The regression coefficients of the latent regression of
#' the ability are returned for each sampled model.}
#' \item{items}{The fixed item difficulty parameters and the SE per item are
#' returned as a `data.frame`. The SE and, if rotation == TRUE, position
#' effect (e.g., position1 signifies the effect of receiving the test in
#' first position) are estimated by the package.}
#' \item{comp_time}{The total computation time as well as computation times for
#' the various steps are returned}
#' }
#'
#' @references Albert, J. H. (1992). Bayesian estimation of normal ogive item
#' response curves using Gibbs sampling. \emph{Journal of Educational
#' Statistics}, \emph{17}(3), 251-269.
#' @references Azur, M. J., Stuart, E. A., Frangakis, C., & Leaf, P. J. (2011).
#' Multiple imputation by chained equations: what is it and how does it work?
#' \emph{International Journal of Methods in Psychiatric Research, 20}(1), 40-49.
#' @references Blossfeld, H.-P., Rossbach, H.-G., & von Maurice,  J. (Eds.)
#' (2011). Education as a Lifelong Process - The German National Educational
#' Panel Study (NEPS). \emph{Zeitschrift fuer Erziehungswissenschaft,
#' Sonderheft 14}.
#' @references Burgette, L. F., & Reiter, J. P. (2010). Multiple imputation for
#' missing data via sequential regression trees. \emph{American Journal of
#' Epidemiology}, \emph{172}(9), 1070-1076.
#' @references Mislevy, R. J. (1991). Randomization-based inference about latent
#' variables from complex samples. \emph{Psychometrika, 56}(2), 177-196.
#' @references Pohl, S., & Carstensen, C. H. (2012). NEPS technical report -
#' Scaling the data of the competence tests.
#' @references Scharl, A., Carstensen, C. H., & Gnambs, T. (2020).
#' \emph{Estimating Plausible Values with NEPS Data: An Example Using Reading
#' Competence in Starting Cohort 6 (NEPS Survey Paper No. 71)}. Leibniz
#' Institute for Educational Trajectories, National Educational Panel Study.
#' https://doi.org/10.5157/NEPS:SP71:1.0
#' @references Tanner, M. A., & Wong, W. H. (1987). The calculation of posterior
#' distributions by data augmentation. \emph{Journal of the American Statistical
#' Association}, \emph{82}(398), 528-549.
#' @references Weirich, S., Haag, N., Hecht, M., Boehme, K., Siegle, T., &
#' Luedtke, O. (2014). Nested multiple imputation in large-scale assessments.
#' \emph{Large-Scale Assessments in Education, 2}(1), 9.
#'
#' @examples
#' \dontrun{
#' rm(list = ls())
#' library(NEPSscaling)
#' library(foreign)
#' ## LONGITUDINAL ESTIMATION FOR "SC3"
#' ## read in data object for conditioning variables
#' data("bgdata_sc3")
#' ## define path to NEPS competence data
#' path <- "Usr/NEPS-data/"
#' setwd(path)
#' ## save simulated target competencies to path
#' data("xTargetCompetencies_sc3")
#' write.dta(
#'   dataframe = xTargetCompetencies_sc3,
#'   file = "SC3_xTargetCompetencies_sim.dta"
#' )
#' ## save simulated school ids to path
#' data("CohortProfile_sc3")
#' write.dta(
#'   dataframe = CohortProfile_sc3,
#'   file = "SC3_CohortProfile.dta"
#' )
#' ## estimate default number of 10 plausible values
#' ## note: the example background data is completely observed!
#' result <- plausible_values(
#'   SC = 3,
#'   domain = "MA",
#'   # longitudinal = TRUE,
#'   wave = 1,
#'   bgdata = bgdata_sc3,
#'   path = path
#' )
#'
#' ## CROSS-SECTIONal ESTIMATION FOR "SC6"
#' ## read in data object for conditioning variables
#' data("bgdata_sc6")
#' ## define path to NEPS competence data
#' path <- "Usr/NEPS-data/"
#' setwd(path)
#' ## save simulated target competencies to path
#' data("xTargetCompetencies_sc6")
#' write.dta(
#'   dataframe = xTargetCompetencies_sc6,
#'   file = "SC6_xTargetCompetencies_sim.dta"
#' )
#' ## estimate default number of 10 plausible values
#' ## note: the example background data is completely observed!
#' result <- plausible_values(
#'   SC = 6,
#'   domain = "RE",
#'   wave = 3,
#'   bgdata = bgdata_sc6,
#'   path = path
#' )
#' }
#'
#' @note For an extensive example, see the accompanying NEPS Survey Paper.
#' The function will only work with NEPS data. To access NEPS data see
#' https://www.neps-data.de/en-us/datacenter/dataaccess.aspx.
#'
#' @importFrom stats as.formula na.omit AIC BIC predict runif model.matrix sd var
#' @importFrom utils flush.console capture.output
#' @importFrom rlang .data
#'
#' @export

plausible_values <- function(SC,
                             domain = c(
                               "MA", "RE", "SC", "IC", "LI", "EF",
                               "NR", "NT", "ORA", "ORB", "ST", "BA", "CD",
                               "GR", "VO"
                             ),
                             wave,
                             path,
                             bgdata = NULL,
                             npv = 10L,
                             nmi = 10L,
                             longitudinal = FALSE,
                             rotation = TRUE,
                             min_valid = 3L,
                             include_nr = TRUE,
                             verbose = TRUE,
                             adjust_school_context = TRUE,
                             exclude = NULL,
                             seed = NULL,
                             returnAll = FALSE,
                             control = list(
                               EAP = FALSE, WLE = FALSE,
                               ML = list(
                                 ntheta = 2000,
                                 normal.approx = FALSE,
                                 samp.regr = FALSE,
                                 theta.model = FALSE,
                                 np.adj = 8, na.grid = 5,
                                 minbucket = 5,
                                 seed = 1234,
                                 cp = 0.0001
                               )
                             )) {
  t0 <- Sys.time()

  # check argument validity and apply necessary changes to arguments ----------

  if (missing(SC)) {
    stop("Starting cohort is missing, but must be provided.", call. = FALSE)
  }
  if (!is.numeric(SC)) {
    stop("Starting cohort must be numeric.", call. = FALSE)
  }
  if (missing(wave)) {
    stop(paste0(
      "Wave is missing, but must be provided.\n",
      "Please note that, for longitudinal estimation, you can use any of the ",
      "waves in which the competence was assessed.\n",
      "For example: if the competence was assessed in waves 1, 3, and 5, it ",
      "is fine to use either 1 or 3 or 5."
    ),
    call. = FALSE
    )
  }
  if (!is.numeric(wave)) {
    stop("Wave must be numeric.", call. = FALSE)
  }

  # convert SC and wave to character strings to index internal lists more
  # comfortably
  SC <- paste0("SC", SC)
  wave <- paste0("w", wave)

  domain <- toupper(domain)
  domain <- match.arg(domain)

  if (missing(path)) {
    stop("Path is missing, but must be provided.", call. = FALSE)
  }
  if (!is.character(path)) {
    stop("Path must be a character string.", call. = FALSE)
  }
  path <- gsub("\\\\", "/", path)
  if (!grepl("/$", path)) {
    path <- paste0(path, "/")
  }
  if (is.null(item_labels[[domain]][[SC]][[wave]])) {
    stop(paste0(
      "There were no competence tests for ", SC, " ", domain, " ",
      wave, ". Please check the NEPS documentation at https://neps-data.de."
    ), call. = FALSE)
  }
  if (longitudinal &&
      (
        #(SC == "SC6" & domain %in% c("IC", "SC")) ||
        (SC == "SC5" & domain %in% c("IC", "SC", "BA", "EF")) ||
        (SC == "SC3" & domain %in% c("ST", "LI")) ||
        (SC == "SC1" & domain %in% c("CD", "SC"))
      )
  ) {
    stop(paste0(
      SC, " ", domain, " was not tested longitudinally.\n",
      "If you wanted to estimate cross-sectional plausible values, ",
      "set longitudinal = FALSE."
    ), call. = FALSE)
  }
  if (longitudinal && SC == "SC1" && domain == "MA" && wave == "w9") {
    stop(paste0("Please note that Starting Cohort 1's maths tests can only ",
                "be linked longitudinally up to wave 7. Wave 9 test data does ",
                "not share any common test information and can, thus, not be ",
                "connected to previous assessment waves. It can only be ",
                "analysed cross-sectionally."), call. = FALSE)
  }
  if (longitudinal && SC == "SC2" && domain == "SC" && wave == "w9") {
    stop(paste0("Please note that Starting Cohort 2's science tests can only ",
                "be linked longitudinally up to wave 5. Wave 9 test data does ",
                "not share any common test information and can, thus, not be ",
                "connected to previous assessment waves. It can only be ",
                "analysed cross-sectionally."), call. = FALSE)
  }
  #if (longitudinal && SC == "SC2" && domain == "GR" && wave == "w3") {
  #  stop(paste0("Please note that Starting Cohort 2's grammar tests cannot ",
  #              "be linked in this NEPSscaling version (3.1.4) as it depends on SUF version SC2 10-0-0 ",
  #              "and an error was detected in this SUF version. Therefore, it is not possible to estimate longitudinal PVs using this NEPSscaling version.",
  #              "The error will be corrected in the next package version."), call. = FALSE)
  #}
  #if (longitudinal && SC == "SC2" && domain == "GR" && wave == "w1") {
  #  stop(paste0("Please note that Starting Cohort 2's grammar tests cannot ",
  #              "be linked in this NEPSscaling version (3.1.4) as it depends on SUF version SC2 10-0-0 ",
  #              "and an error was detected in this SUF version. Therefore, it is not possible to estimate longitudinal PVs using this NEPSscaling version.",
  #              "The error will be corrected in the next package version."), call. = FALSE)
  #}
  if (min_valid < 0) {
    stop("min_valid must be greater than or equal to 0.", call. = FALSE)
  }
  if (min_valid >= length(item_labels[[domain]][[SC]][[wave]])) {
    stop("min_valid is too high. It excludes all possible test takers.",
         call. = FALSE)
  }
  if (!is.null(bgdata)) {
    if (inherits(bgdata, "list") ) {
      if (length(bgdata) != nmi) {
        stop(paste0(
          "The number of imputed data sets does not equal the specified ",
          "number of imputations 'nmi'."
        ), call. = FALSE)
      }
      if (!all(sapply(bgdata, is.data.frame))) {
        stop(paste0(
          "All data sets in bgdata must be formatted as data.frames"
        ), call. = FALSE)
      }
      if (!(sum(unlist(lapply(bgdata, names)) %in% "ID_t") == nmi)) {
        stop(paste0(
          "ID_t must be included in all imputed bgdata sets."
        ), call. = FALSE)
      }
      if (any(sapply(bgdata, ncol)) < 2) {
        stop(paste0(
          "The list of imputed bgdata sets must include at least one variable ",
          "apart from ID_t."
        ), call. = FALSE)
      }
      if (any(sapply(bgdata, is.na))) {
        stop(paste0(
          "Pre-imputed bgdata must not contain any NA."
        ), call. = FALSE)
      }
    } else {
      if (!is.data.frame(bgdata)) {
        stop("bgdata must be a data.frame.")
      }
      if (is.null(bgdata[["ID_t"]])) {
        stop("ID_t must be included in bgdata.")
      }
    }
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed)) {
      stop("'seed' must be a positive integer.")
    }
    set.seed(seed)
  }

  # complement control lists
  res <- complement_control_lists(
    control[["EAP"]], control[["WLE"]], control[["ML"]]
  )
  # control$Bayes <- res$Bayes
  control[["ML"]] <- res[["ML"]]
  control[["EAP"]] <- res[["EAP"]]
  control[["WLE"]] <- res[["WLE"]]

  # create auxiliary waves variable for longitudinal estimation
  waves <- create_waves_vars(longitudinal, SC, domain, wave)
  type <- ifelse(longitudinal, "long", "cross")

  # school context only for SC2-4 while in school
  if (adjust_school_context) {
    adjust_school_context <- was_assessed_in_school(longitudinal, SC, wave)
  }

  # Begin data pre-processing -------------------------------------------------

  t1 <- Sys.time()
  if (verbose) {
    message("Begin pre-processing of data... ", paste(t1))
    flush.console()
  }

  # read in competence data specified by user
  data <- read_in_competence_data(path, SC, domain)

  # number of not-reached items as processing time proxy
  res <- not_reached_as_proxy(include_nr, longitudinal, data, SC, domain, wave,
                              waves)
  items_not_reached <- res[["nr"]]
  data <- res[["data"]]
  include_nr <- res[["include_nr"]]

  # test data and test taker selection
  res <- select_test_responses_and_test_takers(
    longitudinal, SC, domain, data, wave, waves_ = gsub("_", "", waves), min_valid
  )
  data <- res[["data"]]
  resp <- res[["resp"]]

  # check for Partial Credit Items
  PCM <- is_PCM(longitudinal, resp)

  # process background data ---------------------------------------------------

  res <- pre_process_background_data(bgdata, data, include_nr,
                                     items_not_reached, min_valid)
  if (!is.null(bgdata)) {
    data <- res[["data"]]
  }
  bgdata <- res[["bgdata"]]
  ID_t <- res[["ID_t"]]

  # number of valid responses per person (and wave)
  valid_responses_per_person <-
    calculate_number_of_valid_responses(longitudinal, resp, waves)

  # multi-level proxy for school starting cohorts (while still in school)
  if (adjust_school_context) {
    if (is.null(bgdata)) {
      bgdata <- ID_t
    }
    bgdata <- add_contextual_info(path, SC, domain, waves, bgdata, data)
  }

  # test rotation and changes thereof
  if (longitudinal) {
    rotation <- FALSE
    position <- get_rotation_change_info_longitudinal(SC, domain, data)
  } else {
    res <- get_test_rotation_info_cross_sec(rotation, data, SC, wave, domain,
                                            resp, bgdata, ID_t)
    position <- res[["position"]]
    rotation <- res[["rotation"]]
    data <- res[["data"]]
    resp <- res[["resp"]]
    ID_t <- res[["ID_t"]]
    bgdata <- res[["bgdata"]]
  }

  # split items if DIF was detected during original scaling procedure
  if (SC == "SC4" && domain == "MA") {
    resp <-
      split_SC4_math_items(
        get_item_split_info(SC, domain, data), resp, longitudinal, wave)
  }#else if (SC == "SC4" && domain == "SC") {
  #  resp <-
  #    split_SC4_science_items(
  #      get_item_split_info(SC, domain, data), resp, longitudinal, wave)
  #}
  else if (SC == "SC2" && domain == "RE") {
    resp <-
      split_SC2_reading_items(
        get_item_split_info(SC, domain, data), resp, longitudinal, wave)
  }#else if (SC == "SC6" && domain == "SC") {
  #  resp <-
  #    split_SC6_science_items(
  #      get_item_split_info(SC, domain, data), resp, longitudinal, wave)
  #}

  # multiple imputation of missing covariate data -----------------------------

  t2 <- Sys.time()
  res <- impute_missing_data(bgdata, verbose, control, nmi)
  imp <- res[["imp"]]
  frmY <- res[["frmY"]]
  bgdata <- res[["bgdata"]]
  variable_importance <- res[["variable_importance"]]
  treeplot <- res[["treeplot"]]
  indmis <- res[["indmis"]]

  # begin estimation of plausible values --------------------------------------

  t3 <- Sys.time()
  if (verbose) {
    message("\nBegin estimation... ", paste(t3), "\nThis might take some time.")
    flush.console()
  }

  if (longitudinal) {
    res <- estimate_longitudinal(
      bgdata, imp, frmY = frmY, resp, PCM, ID_t, waves, type, domain, SC,
      control, npv, nmi, exclude
    )
  } else {
    if (rotation) {
      if (PCM) {
        res <- estimate_cross_pcm_corrected_for_rotation(
          bgdata, imp, frmY = frmY, waves, ID_t, resp, type, domain, SC,
          control, npv, nmi, position, exclude
        )
      } else {
        res <- estimate_cross_rasch_corrected_for_rotation(
          bgdata, imp, frmY = frmY, resp, position, waves, ID_t, type, domain,
          SC, control, npv, nmi, exclude
        )
      }
    } else {
      if (PCM) {
        res <- estimate_cross_pcm_uncorrected(
          bgdata, imp, resp, waves, frmY = frmY, ID_t, type, domain, SC,
          control, npv, nmi, exclude
        )
      } else {
        res <- estimate_cross_rasch_uncorrected(
          bgdata, imp, resp, waves, frmY = frmY, ID_t, type, domain, SC,
          control, npv, nmi, exclude
        )
      }
    }
  }
  eap <- res[["eap"]]
  pvs <- res[["pvs"]]
  EAP.rel <- res[["EAP.rel"]]
  regr.coeff <- res[["regr.coeff"]]
  mod <- res[["mod"]]
  info_crit <- res[["info_crit"]]
  variance <- res[["variance"]]

  # Begin post-processing of estimated data -----------------------------------

  t4 <- Sys.time()
  if (verbose) {
    message("Finished estimation. Begin post-processing... ", paste(t4))
    flush.console()
  }

  # # assumption: eaps are equivalent over all estimations
  # eap <- eap[[1]]

  # estimate WLEs
  if (control[["WLE"]]) {
    res <- estimate_wles(longitudinal, waves, mod)
    wle <- res[["wle"]]
    WLE.rel <- res[["WLE.rel"]]
  }

  # extract correct number of plausible values from pvs object
  res <- extract_correct_number_of_pvs(bgdata, nmi, npv, pvs, returnAll)
  datalist <- res[["datalist"]]
  npv <- res[["npv"]]

  # keep only those regr. coefficients / EAP reliabilities of kept imputations
  res <- discard_not_used_imputations(datalist, regr.coeff, EAP.rel,
                                      longitudinal, info_crit, treeplot,
                                      variable_importance, variance, eap)
  regr.coeff <- res[["regr.coeff"]]
  EAP.rel <- res[["EAP.rel"]]
  eap <- res[["eap"]]
  info_crit <- res[["info_crit"]]
  treeplot <- res[["treeplot"]]
  variable_importance <- res[["variable_importance"]]
  variance <- res[["variance"]]

  # add standardized regression coefficients
  if (!is.null(bgdata)) {
    regr.coeff <- calculate_standardized_regr_coeff(regr.coeff, datalist,
                                                    longitudinal, waves, variance)
  }

  # linking of longitudinal plausible values ----------------------------------

  # linear transformation of longitudinal PVs to pre-defined scale
  if (longitudinal) {
    res <- link_longitudinal_plausible_values(
      datalist, npv, min_valid, valid_responses_per_person, waves, eap,
      wle = if (control[["WLE"]]) {wle} else {NULL},
      data, SC, domain
    )
    pv <- res[["pv"]]
    wle <- res[["wle"]]
    eap <- res[["eap"]]
    if (SC == "SC4" & domain %in% c("MA", "RE") ||
        (SC == "SC3" & domain == "RE") ||
        (SC == "SC2" & domain %in% c("VO", "MA"))) {
      res <- correct_for_changed_test_rotation(SC, domain, position,
                                               wle = if (control[["WLE"]]) {wle} else {NULL},
                                               eap, pv)
      pv <- res[["pv"]]
      wle <- res[["wle"]]
      eap <- res[["eap"]]
    }
  } else {
    res <- set_not_enough_valid_resp_NA(
      datalist, eap, wle = if (control[["WLE"]]) {wle} else {NULL},
      valid_responses_per_person, min_valid, npv
    )
    pv <- res[["pv"]]
    wle <- res[["wle"]]
    eap <- res[["eap"]]
  }

  # calculate posterior moments of estimated eaps/plausible values
  MEAN <- calculate_posterior_means(eap,
                                    wle = if (control[["WLE"]]) {wle} else {NULL},
                                    pv, waves, npv)
  # VAR <- calculate_posterior_variances(eap,
  #                                   wle = if (control[["WLE"]]) {wle} else {NULL},
  #                                   pv, waves, npv)

  t5 <- Sys.time()

  if (verbose) {
    message("Done!")
    flush.console()
  }

  # collect output object -----------------------------------------------------

  res <- list()
  res[["SC"]] <- as.numeric(gsub(pattern = "SC", replacement = "", x = SC))
  res[["domain"]] <- domain
  res[["wave"]] <- as.numeric(gsub("_w", "", waves))
  res[["type"]] <- type
  res[["rotation"]] <- ifelse(rotation, "Corrected For Test Position",
    "No Correction For Test Position"
  )
  res[["min_valid"]] <- min_valid
  res[["include_nr"]] <- include_nr
  res[["adjust_school_context"]] <- adjust_school_context
  res[["path"]] <- path
  res[["valid_responses_per_person"]] <- valid_responses_per_person
  res[["n_testtakers"]] <-
    colSums(!is.na(valid_responses_per_person[, -1, drop = FALSE]))
  res[["npv"]] <- npv
  res[["nmi"]] <- nmi
  if (!is.null(seed)) {
    res[["seed"]] <- seed
  }
  res[["control"]] <- control
  if (rotation) {
    res[["position"]] <- data.frame(ID_t, position)
  }
  res[["information_criteria"]] <- info_crit
  res[["posterior_means"]] <- MEAN
  res[["posterior_variances"]] <- variance #VAR
  res[["pv"]] <- pv
  if (control[["EAP"]]) {
    res[["eap"]] <- eap
  }
  if (control[["WLE"]]) {
    res[["wle"]] <- wle
    res[["WLE_rel"]] <- WLE.rel
  }
  res[["EAP_rel"]] <- EAP.rel
  res[["regr_coeff"]] <- regr.coeff
  res[["items"]] <- if (longitudinal) {
    xsi.fixed[["long"]][[domain]][[SC]][gsub("_", "", waves)]
  } else {
    mod[[1]][["xsi"]]
  }
  if (!is.null(treeplot)) {
    res[["treeplot"]] <- treeplot
  }
  if (!is.null(variable_importance)) {
    res[["variable_importance"]] <- variable_importance
  }
  if (!is.null(exclude)) {
    res[["exclude"]] <- exclude
  }
  res[["indmis"]] <- indmis
  res[["comp_time"]] <- list(
    initial_time = t0,
    input_check = t1 - t0,
    data_processing = t2 - t1,
    imputation = t3 - t2,
    estimation_time = t4 - t3,
    estimator_postprocessing = t5 - t4,
    total_comp_time = t5 - t0
  )
  class(res) <- "pv_obj"
  res
}


