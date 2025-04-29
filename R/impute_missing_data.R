#' perform multiple imputation via cart
#'
#' @param bgdata data.frame of background data (incl. ID_t) or NULL
#' @param verbose logical; whether stuff should be printed to the console
#' @param control list of arguments for mice algorithm
#' @param nmi numeric; denotes the number of multiple imputations for missing
#' covariate data (defaults to 10).
#'
#' @return list containing the imputations, the formula for the latent regression,
#' the incomplete background data, the list of imputation tree plots, the list
#' of variable importance statistics
#' @noRd

impute_missing_data <- function(bgdata, verbose, control, nmi) {
  imp <- frmY <- treeplot <- variable_importance <- indmis <- NULL
  if (!is.null(bgdata)) {
    if (inherits(bgdata, "list") ) {
      imp <- bgdata
      bgdata <- bgdata[[1]]
    } else {
      if (any(is.na(bgdata))) {
        if (verbose) {
          message(
            "Begin multiple imputation of missing background data... ",
            paste(Sys.time())
          )
          flush.console()
        }
        #_______________________Added by RaelK__________________________________
        if (!is.null(control$ML$seed)) {
          set.seed(control$ML$seed)
        }
        #_______________________________________________________________________
        
        res <- CART(X = bgdata, minbucket = control$ML$minbucket,
                    cp = control$ML$cp, nmi = nmi, verbose = verbose)
        imp <- res$imp
        treeplot <- res$treeplot
        variable_importance <- res$variable_importance
        indmis <- res$indmis
      } else {
        frmY <- create_formula(bgdata)
      }
    }
  }
  list(imp = imp, frmY = frmY, bgdata = bgdata, treeplot = treeplot,
       variable_importance = variable_importance, indmis = indmis)
}


#' #' perform multiple imputation via cart
#' #'
#' #' @param bgdata data.frame of background data (incl. ID_t) or NULL
#' #' @param verbose logical; whether stuff should be printed to the console
#' #' @param control list of arguments for mice algorithm
#' #'
#' #' @importFrom mice mice complete
#' #' @noRd
#'
#' impute_missing_data <- function(bgdata, verbose, control) {
#'   imp <- frmY <- loggedEvents <- NULL
#'   if (!is.null(bgdata)) {
#'     if (any(is.na(bgdata))) {
#'       if (verbose) {
#'         cat(
#'           "Begin multiple imputation of missing background data... ",
#'           paste(Sys.time()), "\n"
#'         )
#'         flush.console()
#'       }
#'       predictorMatrix <- matrix(1, ncol = ncol(bgdata), nrow = ncol(bgdata))
#'       diag(predictorMatrix) <- 0
#'       predictorMatrix[, which(names(bgdata) == "ID_t")] <- 0
#'       imp <- mice(data = bgdata, m = nmi, method = "cart",
#'                   minbucket = control$ML$minbucket, cp = control$ML$cp,
#'                   printFlag = verbose, predictorMatrix = predictorMatrix)
#'       loggedEvents <- imp$loggedEvents
#'       imp <- complete(imp, "all")
#'     } else {
#'       bgdata <- as.data.frame(lapply(bgdata, as.numeric))
#'       imp <- NULL
#'       frmY <-
#'         as.formula(
#'           paste("~", paste(colnames(bgdata[, -which(colnames(bgdata) == "ID_t"),
#'                                            drop = FALSE
#'           ]),
#'           collapse = "+"
#'           ))
#'         )
#'     }
#'   }
#'   list(imp = imp, frmY = frmY, bgdata = bgdata, loggedEvents = loggedEvents)
#' }
