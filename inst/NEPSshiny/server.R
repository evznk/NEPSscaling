
library(shiny)
library(xtable)




filter_data <- function(filter_op, filter_var, filter_val, out) {
  switch(filter_op,
    "<" = dplyr::filter(out, .data[[filter_var]] < filter_val),
    ">" = dplyr::filter(out, .data[[filter_var]] > filter_val),
    "<=" = dplyr::filter(out, .data[[filter_var]] <= filter_val),
    ">=" = dplyr::filter(out, .data[[filter_var]] >= filter_val),
    "==" = dplyr::filter(out, .data[[filter_var]] == filter_val),
    "!=" = dplyr::filter(out, .data[[filter_var]] != filter_val),
    showNotification(paste("Filter operator", filter_op, "not valid."),
                     type = "error")
  )
}

options(shiny.maxRequestSize = 30*1024^2)

export_files <- function(format, name) {
  switch(format,
         "SPSS" = paste0(name, "_", 1:10, ".sav"),
         "Stata" = paste0(name, "_", 1:10, ".dta"),
         "Mplus" = c(paste0(name, "_", 1:10, ".dat"), "content_file.dat"))
}

#https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode/8189441#8189441
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

shinyServer(function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })

  values <- reactiveValues(
    pv_obj = NULL,
    bgdata_raw = NULL,
    bgdata = NULL
  )



  ############################################################################
  #                                    UPLOAD
  ############################################################################

  # ------------------------------ UPLOAD STATE ------------------------------
  # previous state == pv_obj as rds
  
  #_______________________________Editted by RaelK______________________________
  #To support .RData or .rdata files for uploading pv_obj as well (not just .rds)
  observe({
    req(input$import_state)
    filetype <- tools::file_ext(input$import_state$datapath)
    
    out <- switch(
      filetype,
      rds = readRDS(file = input$import_state$datapath),
      sav = haven::read_spss(file = input$import_bgdata$datapath),
      dta = haven::read_dta(file = input$import_bgdata$datapath),
      RData = {
        e <- new.env()
        load(input$import_state$datapath, envir = e)
        obj_names <- ls(e)
        if (length(obj_names) > 1) {
          showNotification("Multiple objects found in RData; using the first.", type = "warning")
        }
        e[[obj_names[1]]]
      },
      rdata = {
        e <- new.env()
        load(input$import_state$datapath, envir = e)
        obj_names <- ls(e)
        if (length(obj_names) > 1) {
          showNotification("Multiple objects found in RData; using the first.", type = "warning")
        }
        e[[obj_names[1]]]
      },
      Rdata = {
        e <- new.env()
        load(input$import_state$datapath, envir = e)
        obj_names <- ls(e)
        if (length(obj_names) > 1) {
          showNotification("Multiple objects found in RData; using the first.", type = "warning")
        }
        e[[obj_names[1]]]
      },
      {
        showNotification(paste("Unsupported file type:", filetype), type = "error")
        return(NULL)
      }
    )
#_______________________________________________________________________________
    if (class(out) != "pv_obj") {
      showNotification("pv_obj must be of class 'pv_obj'.", type = "error")
    } else {
      values$pv_obj <- out
    }

    if (!input$metric & !isTruthy(input$nominal) & !isTruthy(input$ordinal)) {
      updateSelectInput(session = session, inputId = "ordinal",
                        label = "Select ordinal variables",
                        choices = names(out$pv[[1]]))
      updateSelectInput(session = session, inputId = "nominal",
                        label = "Select nominal variables",
                        choices = names(out$pv[[1]]))
      showNotification("Please specify the scale levels of the data under 'Manage'.",
                       type = "message")
    }

    updateSelectInput(session, inputId = "imputation",
                      choices = names(out$treeplot),
                      selected = "")
    updateSelectInput(session, inputId = "variable",
                      choices = names(out$treeplot[[1]]),
                      selected = "")
    updateSelectInput(session, inputId = "imputation_var_imp",
                      choices = names(out$treeplot),
                      selected = "")
    updateSelectInput(session, inputId = "variable_var_imp",
                      choices = names(out$treeplot[[1]]),
                      selected = "")
    updateSelectInput(session, inputId = "fill",
                      choices = unique(c(input$nominal, input$ordinal)),
                      selected = "")
    updateSelectInput(session, inputId = "x",
                      choices = names(out$pv[[1]]),
                      selected = "")
    updateSelectInput(session, inputId = "y",
                      choices = names(out$pv[[1]]),
                      selected = "")
  })

  # ---------------------------- UPLOAD BGDATA -------------------------------
  # https://mastering-shiny.org/action-transfer.html
  observeEvent(input$import_bgdata, {
    req(input$import_bgdata)
    filetype <- tools::file_ext(input$import_bgdata$datapath)

   # out <- switch(
    #  filetype,
     # rds = readRDS(file = input$import_bgdata$datapath),
      #sav = haven::read_spss(file = input$import_bgdata$datapath),
      #dta = haven::read_dta(file = input$import_bgdata$datapath),
      #validate(paste0(
       # "Format of bgdata (", filetype, ") not recognized.\n",
        #"Needs: R object (.rds), SPSS (.sav) or Stata (.dta) format."
      #))
    #)
    
    #____________________________Editted by RaelK_______________________________
    #To allow .RData file extentions
    out <- switch(
      filetype,
      rds = readRDS(file = input$import_bgdata$datapath),
      sav = haven::read_spss(file = input$import_bgdata$datapath),
      dta = haven::read_dta(file = input$import_bgdata$datapath),
      RData = {
        e <- new.env()
        load(input$import_bgdata$datapath, envir = e)
        obj_names <- ls(e)
        if (length(obj_names) > 1) {
          showNotification("Multiple objects found in RData; using the first.", type = "warning")
        }
        e[[obj_names[1]]]
      },
      rdata = {
        e <- new.env()
        load(input$import_bgdata$datapath, envir = e)
        obj_names <- ls(e)
        if (length(obj_names) > 1) {
          showNotification("Multiple objects found in RData; using the first.", type = "warning")
        }
        e[[obj_names[1]]]
      },
      Rdata = {
        e <- new.env()
        load(input$import_bgdata$datapath, envir = e)
        obj_names <- ls(e)
        if (length(obj_names) > 1) {
          showNotification("Multiple objects found in RData; using the first.", type = "warning")
        }
        e[[obj_names[1]]]
      },
      {
        validate(paste0(
          "Format of bgdata (", filetype, ") not recognized.\n",
          "Needs: R object (.rds), .RData, SPSS (.sav) or Stata (.dta) format."
        ))
      }
    )
    

    if (!is.data.frame(out)) {
      showNotification("bgdata must be a data.frame.", type = "error")
      return(NULL)
    }

    updateSelectInput(session = session, inputId = "ordinal",
                      label = "Select ordinal variables", choices = names(out))
    updateSelectInput(session = session, inputId = "nominal",
                      label = "Select nominal variables", choices = names(out))

    updateSelectInput(session = session, inputId = "bgdata_select_cols",
                      label = "Select columns", choices = names(out),
                      selected = "")
    updateSelectInput(session = session, inputId = "bgdata_sort_cases",
                      label = "Sort by", choices = names(out),
                      selected = "")

    values$bgdata_raw <- haven::zap_labels(out)
  })

  observe({
    req(values$bgdata_raw)
    nominal <- input$nominal
    ordinal <- input$ordinal
    out <- values$bgdata_raw

    if (!input$metric & is.null(nominal) & is.null(ordinal)) {
      showNotification("Please specify the scale levels of the data.",
                       type = "message")
      return(NULL)
    }

    if (!is.null(ordinal) | !is.null(nominal)) {
      sel <- unique(c(ordinal, nominal))
      if (length(sel) == 1) {
        out[[sel]] <- as.factor(out[[sel]])
      } else {
        out[, sel] <- lapply(out[, sel], as.factor)
      }
    }

    choices <- colnames(out[, -which(names(out) == "ID_t")])
    updateSelectInput(session = session, inputId = "exclude1",
                      label = "Variables to exclude from bg data",
                      choices = choices, selected = "")
    updateSelectInput(session = session, inputId = "exclude2",
                      label = "Variables to exclude (2nd wave)",
                      choices = choices, selected = "")
    updateSelectInput(session = session, inputId = "exclude3",
                      label = "Variables to exclude (3rd wave)",
                      choices = choices, selected = "")
    updateSelectInput(session = session, inputId = "exclude4",
                      label = "Variables to exclude (4th wave)",
                      choices = choices, selected = "")
    updateSelectInput(session = session, inputId = "exclude5",
                      label = "Variables to exclude (5th wave)",
                      choices = choices, selected = "")

    values$bgdata <- out
  })

  ############################################################################
  #                            MANIPULATE
  ############################################################################

  # ---------------------------- DISPLAY BGDATA ------------------------------
  # select columns, filter by values, select rows
  # paged table

  observeEvent(input$Display_Bgdata, {
    shinyjs::toggle('bgdata_select_cols')
    shinyjs::toggle('bgdata_filter_rows')
    shinyjs::toggle('bgdata_sort_cases')
    shinyjs::toggle('bgdata_ascending')
    output$text <- renderText({"ahh you pressed it"})
  })


  bgdata_display <- reactive({
    req(values$bgdata)
    out <- values$bgdata

    if (isTruthy(input$bgdata_select_cols)) { # variables for selection have been chosen
      sel <- names(out)[names(out) %in% c("ID_t", input$bgdata_select_cols)]
      out <- out[, sel, drop = FALSE]
    }

    # bildet nur &, nicht | ab, wobei man | auch als & umformulieren kann
    if (isTruthy(input$bgdata_filter_rows)) {
      for (f in input$bgdata_filter_rows) { # swap f for input$bgdata_filter_rows if no loop is required!
        filter_op <- stringr::str_extract(f, "[<>=!]+")
        filter_var <- stringr::word(f, 1, sep = "[ <>!=]")
        filter_val <- sub(".*[ <>!=]", "", f)
        out <- filter_data(filter_op, filter_var, filter_val, out)
      }
    }

    if (isTruthy(input$bgdata_sort_cases)) {
      if (input$bgdata_ascending) {
        out <- dplyr::arrange(out, .data[[input$bgdata_sort_cases]])
      } else {
        out <- dplyr::arrange(out, dplyr::desc(.data[[input$bgdata_sort_cases]]))
      }
    }

    updateSelectInput(session = session, inputId = "bgdata_sort_cases",
                      label = "Sort by", choices = names(out),
                      selected = "")

    out
  })
  output$bgdata_display <- renderDataTable(
    bgdata_display(),
    options = list(pageLength = 25)
  )

  observeEvent(input$estimate_pv_obj, {
    req(
      values$bgdata, input$select_starting_cohort, input$select_domain,
      input$select_wave, input$path_to_data
    )
    
    message("🟢 Start Estimation button pressed.")
    
    # Confirm 'ID_t' exists
    if (!"ID_t" %in% names(values$bgdata)) {
      showNotification("❌ Background data must include 'ID_t'.", type = "error")
      return(NULL)
    }
    
    exclude <- NULL
    if (isTruthy(input$longitudinal) & input$longitudinal) {
      exclude <- list(
        input$exclude1, input$exclude2, input$exclude3, input$exclude4,
        input$exclude5
      )
      names(exclude) <- gsub("_", "",
                             NEPSscaling:::create_waves_vars(
                               longitudinal = input$longitudinal,
                               SC = paste0("SC", input$select_starting_cohort),
                               domain = input$select_domain, wave = NULL
                             )
      )
    } else if (isTruthy(input$longitudinal) & !input$longitudinal) {
      exclude <- input$exclude1
    }
    
    shinyjs::html("plausible_values_progress", "🔍 Starting estimation...")
    
    tryCatch({
      # Trigger and trace estimation
      message("📂 Estimating PVs using path: ", input$path_to_data)
      
      out <- NEPSscaling::plausible_values(
        SC = as.numeric(input$select_starting_cohort),
        domain = input$select_domain,
        wave = as.numeric(input$select_wave),
        path = gsub("\\\\", "/", input$path_to_data),
        bgdata = values$bgdata,
        npv = as.numeric(input$npv),
        longitudinal = input$longitudinal,
        rotation = input$rotation,
        min_valid = as.numeric(input$min_valid),
        include_nr = input$include_nr,
        verbose = TRUE,
        adjust_school_context = input$adjust_school_context,
        exclude = exclude,
        seed = input$seed,
        control = list(WLE = input$WLE, EAP = input$EAP,
                       ML = list(nmi = input$nmi, seed=input$seed))
      )
      
      if (is.null(out)) {
        shinyjs::html("plausible_values_progress", "⚠️ Estimation returned NULL.")
        showNotification("⚠️ No output. Something went wrong inside plausible_values().", type = "error")
        return(NULL)
      }
      
      values$pv_obj <- out
      
      updateSelectInput(session, inputId = "imputation",
                        choices = names(out$treeplot), selected = "")
      updateSelectInput(session, inputId = "variable",
                        choices = names(out$treeplot[[1]]), selected = "")
      updateSelectInput(session, inputId = "imputation_var_imp",
                        choices = names(out$treeplot), selected = "")
      updateSelectInput(session, inputId = "variable_var_imp",
                        choices = names(out$treeplot[[1]]), selected = "")
      updateSelectInput(session, inputId = "fill",
                        choices = unique(c(input$nominal, input$ordinal)), selected = "")
      updateSelectInput(session, inputId = "x",
                        choices = names(out$pv[[1]]), selected = "")
      updateSelectInput(session, inputId = "y",
                        choices = names(out$pv[[1]]), selected = "")
      
      shinyjs::html("plausible_values_progress", "✅ Estimation completed successfully.")
      showNotification("✅ PV estimation finished!", type = "message")
      
    }, error = function(e) {
      msg <- paste("❌ Estimation error:", e$message)
      shinyjs::html("plausible_values_progress", msg)
      showNotification(msg, type = "error")
      message(msg)
    })
  })
  
  

  # ------------------------- SUMMARY OF PV_OBJ ------------------------------

  output$summary <- renderText({
    req(values$pv_obj)
    paste(capture.output(print(values$pv_obj)), collapse = "\n")
  })

  # ------------------------- ITEM DIFFICULTIES ------------------------------

  output$item_difficulties <- renderTable({
    req(values$pv_obj)
    if (NEPSscaling::get_type(pv_obj = values$pv_obj) == "longitudinal") {
      new_items <- paste0("items_w", NEPSscaling::get_wave(values$pv_obj))
      new_xsi <- paste0("xsi_w", NEPSscaling::get_wave(values$pv_obj))
      NEPSscaling::get_item_difficulties(values$pv_obj) %>%
        purrr::map(.f = function(mat) {
          colnames(mat) <- base::make.names(colnames(mat), unique = TRUE)
          mat}) %>%
        purrr::map(tibble::as_tibble, rownames = "items") %>%
        purrr::map(dplyr::rename, "pos" = "X") %>%
        purrr::map2(.y = new_items, ~dplyr::rename(.x, !!.y := "items")) %>%
        purrr::map2(.y = new_xsi, ~dplyr::rename(.x, !!.y := "xsi")) %>%
        purrr::reduce(dplyr::full_join, by = "pos") %>%
        dplyr::select(-.data$pos) %>%
        dplyr::mutate_if(.predicate = is.numeric,
                         .funs = round, digits = 3) %>%
        as.data.frame()
    } else {
      items <- data.frame(rownames(NEPSscaling::get_item_difficulties(values$pv_obj)),
                          round(NEPSscaling::get_item_difficulties(values$pv_obj), 3))
      names(items) <- c("Items", "xsi", "se")
      items
    }
  },
    caption = "Item Difficulty Parameters. SE of fixed parameters is set to 0. Position = Estimated rotation effect.",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
  )


  # ------------------------ AVERAGE IMPUTED DATA ----------------------------

  average_pvs <- reactive({
    req(values$pv_obj)

    out <- values$pv_obj$pv[[1]]

    tmp <- replicate(length(names(out)[-1]),
                     data.frame(ID_t = out[["ID_t"]]), simplify = FALSE)
    names(tmp) <- names(out)[-1]
    for (i in seq(NEPSscaling::get_npv(values$pv_obj))) {
      for (var in names(out)[-1]) {
        tmp[[var]] <- dplyr::left_join(tmp[[var]],
                                       values$pv_obj$pv[[i]][, c("ID_t", var)],
                                       by = "ID_t")
      }
    }
    tmp <- lapply(names(tmp), function(x) {
      if (x %in% input$ordinal) {
        as.factor(apply(tmp[[x]][, -1], 1, median, na.rm = TRUE))
      } else if (x %in% input$nominal) {
        as.factor(apply(tmp[[x]][, -1], 1, Mode))
      } else {
        apply(tmp[[x]][, -1], 1, mean, na.rm = TRUE)
      }
    })
    out[, -1] <- as.data.frame(tmp)
    out
  })

  # ----------------------- DISPLAY AVERAGE IMPUTATIONS ----------------------
  # select columns, filter by values, select rows
  # paged table
  imputations_display <- reactive({
    req(average_pvs())
    out <- average_pvs()

    if (isTruthy(input$imputations_select_cols)) { # variables for selection have been chosen
      sel <- names(out)[names(out) %in% c("ID_t", input$imputations_select_cols)]
      out <- out[, sel, drop = FALSE]
    }

    # bildet nur &, nicht | ab, wobei man | auch als & umformulieren kann
    if (isTruthy(input$imputations_filter_rows)) {
      for (f in input$imputations_filter_rows) { # swap f for input$imputations_filter_rows if no loop is required!
        filter_op <- stringr::str_extract(f, "[<>=!]+")
        filter_var <- stringr::word(f, 1, sep = "[ <>!=]")
        filter_val <- sub(".*[ <>!=]", "", f)
        out <- filter_data(filter_op, filter_var, filter_val, out)
      }
    }

    if (isTruthy(input$imputations_sort_cases)) {
      if (input$imputations_ascending) {
        out <- dplyr::arrange(out, .data[[input$imputations_sort_cases]])
      } else {
        out <- dplyr::arrange(out, dplyr::desc(.data[[input$imputations_sort_cases]]))
      }
    }

    updateSelectInput(session = session, inputId = "imputations_sort_cases",
                      label = "Sort by", choices = names(out),
                      selected = "")

    out
  })
  output$imputations_display <- renderDataTable(
    imputations_display(),
    options = list(pageLength = 50)
  )

  # --------------------------- CREATE CART PLOT -----------------------------

  cart_plot <- eventReactive(input$cart_plot, {
    req(values$pv_obj, input$imputation, input$variable)
    tryCatch(
      NEPSscaling::display_imputation_tree(values$pv_obj, input$imputation, input$variable),
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )
  })
  output$cart_plot <- renderPlot(cart_plot())

  # ------------------- CREATE VARIABLE IMPORTANCE PLOT ----------------------

  variable_importance_plot <- eventReactive(input$variable_importance_plot, {
    req(values$pv_obj, input$imputation_var_imp, input$variable_var_imp)
    NEPSscaling::display_variable_importance(values$pv_obj,
                                             input$imputation_var_imp,
                                             input$variable_var_imp)
  })
  output$variable_importance_plot <- renderPlot(variable_importance_plot())

  # -------------------- CREATE DISTR Plots for pv_obj$pvs -------------------

  plot_geom <- reactive({
    switch(input$geom,
           "Histogram" = ggplot2::geom_histogram(),
           "Density plot" = ggplot2::geom_density(),
           "Scatter plot" = ggplot2::geom_point()
    )
  })

  plot_theme <- reactive({
    switch(input$theme,
           "Gray" = ggplot2::theme_gray(),
           "Black and white" = ggplot2::theme_bw(),
           "Linedraw" = ggplot2::theme_linedraw(),
           "Light" = ggplot2::theme_light(),
           "Dark" = ggplot2::theme_dark(),
           "Minimal" = ggplot2::theme_minimal(),
           "Classic" = ggplot2::theme_classic(),
           "Void" = ggplot2::theme_void()
    )
  })

  plot_title <- reactive({ggplot2::ggtitle(input$title)})

  plot_xlab <- reactive({
    if (isTruthy(input$xlab)) {
      return(ggplot2::xlab(input$xlab))
    } else if (isTruthy(input$x)) {
      return(ggplot2::xlab(input$x))
    }
  })

  plot_ylab <- reactive({
    if (isTruthy(input$ylab)) {
      return(ggplot2::ylab(input$ylab))
    } else if (isTruthy(input$y)) {
      return(ggplot2::ylab(input$y))
    }
  })

  imputation_plot <- eventReactive(input$plot, {
    req(average_pvs(), input$geom, input$x)

    # aesthetics: x, y
    gplot <- ggplot2::ggplot(
      average_pvs(),
      ggplot2::aes(x = .data[[input$x]])
    )
    if (isTruthy(input$y) & input$geom == "Scatter plot") {
      gplot <- gplot + ggplot2::aes(y = .data[[input$y]])
    }

    # aesthetics: fill
    if (isTruthy(input$fill)) {
      gplot <- gplot + ggplot2::aes(fill = .data[[input$fill]],
                                    color = .data[[input$fill]])
    }

    # build plot
    gplot <- gplot +
      plot_geom() +
      plot_title() +
      plot_xlab() +
      plot_ylab() +
      plot_theme()

    gplot
  })
  output$plot <- renderPlot(imputation_plot())

  # --------------------------- CREATE REGRESSION TABLES ---------------------
  regression_table <- reactive({
    req(values$pv_obj)
    tmp <- NEPSscaling::get_regression_coefficients(values$pv_obj)
    if (NEPSscaling::get_type(values$pv_obj) == "longitudinal") {
      Variable <- tmp[[1]]$Variable
      tmp <- lapply(tmp, function(x) x[,-1]) %>%
        purrr::reduce(`+`) / length(tmp)
      tab <- data.frame(
        Variable = paste(Variable, "Wave",
                         rep(NEPSscaling::get_wave(values$pv_obj), each = nrow(tmp))),
        N = as.character(rep(NEPSscaling::get_n_testtakers(values$pv_obj), each = nrow(tmp))),
        b = unname(unlist(tmp[, seq(1, ncol(tmp), 3)])),
        beta = unname(unlist(tmp[, seq(2, ncol(tmp), 3)])),
        se = unname(unlist(tmp[, seq(3, ncol(tmp), 3)]))
      )
    } else {
      tab <- data.frame(
        Variable = tmp$Variable,
        N = as.character(NEPSscaling::get_n_testtakers(values$pv_obj)),
        b = rowMeans(as.matrix(tmp[, grepl("_coeff$", names(tmp))])),
        beta = rowMeans(as.matrix(tmp[, grepl("_std$", names(tmp))])),
        se = rowMeans(as.matrix(tmp[, grepl("_se$", names(tmp))]))
      )
    }
    tab[["95% CI of b"]] <- paste0("[", round(tab$b - 1.96 * tab$se, 3),"; ",
                                   round(tab$b + 1.96 * tab$se, 3), "]")
    tab$b <- as.character(round(tab$b, 3))
    tab$beta <- as.character(round(tab$beta, 3))
    tab$se <- as.character(round(tab$se, 3))
    tab
  })
  output$regression_table <- renderTable({
      regression_table()
    },
    caption = "Latent Regression Weights with 95% CI based on normal distribution",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
  )

  # --------------------------- CREATE SUMMARY STATISTICS --------------------
  imputation_table <- reactive({
    req(average_pvs())
    out <- psych::describe(average_pvs())
    out$vars <- rownames(out)
    out
  })
  output$imputation_table <- renderTable({
      imputation_table()
    },
    caption = "Descriptive Statistics of Average Imputed Data Sets. * Factor variables.",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
  )

  observeEvent(input$remove_pv_obj, {
    values$pv_obj <- NULL

    updateSelectInput(session, inputId = "imputation",
                      choices = "",
                      selected = "")
    updateSelectInput(session, inputId = "variable",
                      choices = "",
                      selected = "")
    updateSelectInput(session, inputId = "imputation_var_imp",
                      choices = "",
                      selected = "")
    updateSelectInput(session, inputId = "variable_var_imp",
                      choices = "",
                      selected = "")
    updateSelectInput(session, inputId = "fill",
                      choices = "",
                      selected = "")
    updateSelectInput(session, inputId = "x",
                      choices = "",
                      selected = "")
    updateSelectInput(session, inputId = "y",
                      choices = "",
                      selected = "")

    if (is.null(values$bgdata_raw)) {
      updateSelectInput(session = session, inputId = "ordinal",
                        label = "Select ordinal variables", choices = "")
      updateSelectInput(session = session, inputId = "nominal",
                        label = "Select nominal variables", choices = "")

      updateSelectInput(session = session, inputId = "bgdata_select_cols",
                        label = "Select columns", choices = "",
                        selected = "")
      updateSelectInput(session = session, inputId = "bgdata_sort_cases",
                        label = "Sort by", choices = "",
                        selected = "")
      updateSelectInput(session = session, inputId = "exclude1",
                        label = "Variables to exclude from bg data",
                        choices = "", selected = "")
      updateSelectInput(session = session, inputId = "exclude2",
                        label = "Variables to exclude (2nd wave)",
                        choices = "", selected = "")
      updateSelectInput(session = session, inputId = "exclude3",
                        label = "Variables to exclude (3rd wave)",
                        choices = "", selected = "")
      updateSelectInput(session = session, inputId = "exclude4",
                        label = "Variables to exclude (4th wave)",
                        choices = "", selected = "")
      updateSelectInput(session = session, inputId = "exclude5",
                        label = "Variables to exclude (5th wave)",
                        choices = "", selected = "")
    }
  })

  observeEvent(input$remove_bgdata, {
    values$bgdata_raw <- values$bgdata <- NULL

    if (is.null(values$pv_obj)) {
      updateSelectInput(session = session, inputId = "ordinal",
                        label = "Select ordinal variables", choices = "")
      updateSelectInput(session = session, inputId = "nominal",
                        label = "Select nominal variables", choices = "")

      updateSelectInput(session = session, inputId = "bgdata_select_cols",
                        label = "Select columns", choices = "",
                        selected = "")
      updateSelectInput(session = session, inputId = "bgdata_sort_cases",
                        label = "Sort by", choices = "",
                        selected = "")
      updateSelectInput(session = session, inputId = "exclude1",
                        label = "Variables to exclude from bg data",
                        choices = "", selected = "")
      updateSelectInput(session = session, inputId = "exclude2",
                        label = "Variables to exclude (2nd wave)",
                        choices = "", selected = "")
      updateSelectInput(session = session, inputId = "exclude3",
                        label = "Variables to exclude (3rd wave)",
                        choices = "", selected = "")
      updateSelectInput(session = session, inputId = "exclude4",
                        label = "Variables to exclude (4th wave)",
                        choices = "", selected = "")
      updateSelectInput(session = session, inputId = "exclude5",
                        label = "Variables to exclude (5th wave)",
                        choices = "", selected = "")
    }
  })


  ############################################################################
  #                            DOWNLOAD
  ############################################################################

  # ------------------------- DOWNLOAD PV_OBJ --------------------------------
  # == pv_obj as RDS
  output$download_pv_obj <- downloadHandler(
    filename = function() {
      req(input$pv_obj_name)
      paste0(input$pv_obj_name, ".rds")
    },
    content = function(file) {
      req(values$pv_obj)
      saveRDS(values$pv_obj, file = file)
    }
  )

  # --------------------------- EXPORT PV_OBJ --------------------------------
  # formats: spss, stata, mplus
  # https://stackoverflow.com/a/43939912
  output$export_pv_obj <- downloadHandler(
    filename = function() {
      rep(input$pv_obj_name)
      paste0(input$pv_obj_name, ".zip")
    },
    content = function(zipfile) {
      req(values$pv_obj, input$export_format)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      # vector of file names for pv_obj$pv data.frames
      files <- export_files(input$export_format, input$pv_obj_name)
      if (input$export_format == "SPSS") {
        for (i in seq(length(values$pv_obj$pv))) {
          haven::write_sav(values$pv_obj$pv[[i]], path = files[i])
        }
      } else if (input$export_format == "Stata") {
        for (i in seq(length(values$pv_obj$pv))) {
          colnames(values$pv_obj[["pv"]][[i]]) <-
            gsub("[[:punct:]]", "_", colnames(values$pv_obj[["pv"]][[i]]))
          haven::write_dta(values$pv_obj$pv[[i]], path = files[i])
        }
      } else if (input$export_format == "Mplus") {
        for (i in 1:length(values$pv_obj[["pv"]])) {
          write.table(values$pv_obj[["pv"]][[i]], file = files[i],
                      dec = ".", sep = ",", row.names = FALSE)
        }
        write(x = paste0(files[-length(files)], collapse = "\n"),
              file = files[length(files) - 1])
        write(names(values$pv_obj[["pv"]][[1]]), file = "variable_names.txt")

        write(x = paste0(files[-length(files)], collapse = "\n"),
              file = files[length(files)])
      }

      zip(zipfile = zipfile, files = files)
    },
    contentType = "application/zip"
  )


  # ------------------------ SAVE GGPLOTS ------------------------------------

  # distribution plots for imputations / plausible values
  output$download_plot <- downloadHandler(
    filename = function() {
      req(input$plot_name, input$plot_format)
      ext <- switch(input$plot_format,
                    "png" = ".png",
                    "RData" = ".RData")
      paste0(input$plot_name, ext)
    },
    content = function(file) {
      if (input$plot_format == "RData") {
        gplot <- imputation_plot()
        save(gplot, file = file)
      } else {
        ggplot2::ggsave(filename = file, plot = imputation_plot())
      }
    }
  )

  # cart plot for single variables' imputation
  output$download_cart <- downloadHandler(
    filename = function() {
      req(input$cart_name, input$cart_format)
      ext <- switch(input$cart_format,
                    "png" = ".png",
                    "RData" = ".RData")
      paste0(input$cart_name, ext)
    },
    content = function(file) {
      if (input$cart_format == "RData") {
        gplot <- cart_plot()
        save(gplot, file = file)
      } else {
        ggplot2::ggsave(filename = file, plot = cart_plot())
      }
    }
  )

  # variable_importance plot for single variables' imputation
  output$download_variable_importance <- downloadHandler(
    filename = function() {
      req(input$variable_importance_name, input$variable_importance_format)
      ext <- switch(input$variable_importance_format,
                    "png" = ".png",
                    "RData" = ".RData")
      paste0(input$variable_importance_name, ext)
    },
    content = function(file) {
      if (input$variable_importance_format == "RData") {
        gplot <- variable_importance_plot()
        save(gplot, file = file)
      } else {
        ggplot2::ggsave(filename = file, plot = variable_importance_plot())
      }
    }
  )

  # ------------------------ SAVE TABLES -------------------------------------

  output$download_descriptive <- downloadHandler(
    filename = function() {
      req(input$descriptive_name)
      paste0(input$descriptive_name, ".tsv")
    },
    content = function(file) {
      write.table(x = imputation_table(), file = file, sep = "\t",
                  quote = FALSE, row.names = FALSE)
    }
  )

  output$download_difficulties <- downloadHandler(
    filename = function() {
      req(input$difficulties_name)
      paste0(input$difficulties_name, ".tsv")
    },
    content = function(file) {
      write.table(x = difficulties_table(), file = file, sep = "\t",
                  quote = FALSE, row.names = FALSE)
    }
  )

  output$download_regression <- downloadHandler(
    filename = function() {
      req(input$regression_name)
      paste0(input$regression_name, ".tsv")
    },
    content = function(file) {
      write.table(x = regression_table(), file = file, sep = "\t",
                  quote = FALSE, row.names = FALSE)
    }
  )



  ############################################################################
  #                            UTILS
  ############################################################################

  # -------------------- Plots conditional panels ----------------------------

  observeEvent(input$plots_distribution_plot_state, {
    values$plots_conditional_visible <- 1
  })
  observeEvent(input$plots_tree_structure_state, {
    values$plots_conditional_visible <- 2
  })
  observeEvent(input$plots_variable_importance_state, {
    values$plots_conditional_visible <- 3
  })
  output$plots_conditional_visible <- renderText({
    values$plots_conditional_visible
  })
  outputOptions(output, "plots_conditional_visible", suspendWhenHidden = FALSE)

  # -------------------- Tables conditional panels ----------------------------

  observeEvent(input$plots_distribution_plot_state, {
    values$tables_conditional_visible <- 1
  })
  observeEvent(input$plots_tree_structure_state, {
    values$tables_conditional_visible <- 2
  })
  observeEvent(input$plots_variable_importance_state, {
    values$tables_conditional_visible <- 3
  })
  output$tables_conditional_visible <- renderText({
    values$tables_conditional_visible
  })
  outputOptions(output, "tables_conditional_visible", suspendWhenHidden = FALSE)

  # ---------- Hide-able exclude argument in longitudinal case ----------------

  observeEvent(input$longitudinal, {
    shinyjs::toggle('exclude2', condition = input$longitudinal)
    shinyjs::toggle('exclude3', condition = input$longitudinal)
    shinyjs::toggle('exclude4', condition = input$longitudinal)
    shinyjs::toggle('exclude5', condition = input$longitudinal)
  })

  # -------------------- select domain and wave input -----------------------

  observeEvent(input$select_starting_cohort, {
    values$domains_for_sc <-  if (input$select_starting_cohort == 1) {
      c("Mathematics" = "MA", "Cognitive Development" = "CD",
        "Science" = "SC")#, "Vocabulary" = "VO")
    } else if (input$select_starting_cohort == 2) {
      c("Mathematics" = "MA", "Reading" = "RE", "Science" = "SC", "ICT" = "IC",
        "Native Russian" = "NR", "Native Turkish" = "NT",
        "Orthography A" = "ORA", "Orthography B" = "ORB", "Vocabulatry" = "VO",
        "Grammar" = "GR")
    } else if (input$select_starting_cohort == 3) {
      c("Mathematics" = "MA", "Reading" = "RE", "Science" = "SC", "ICT" = "IC",
        "English as a foreign language" = "EF", "Native Russian" = "NR",
        "Native Turkish" = "NT", "Scientific Thinking" = "ST",
        "Orthography A" = "ORA", "Orthography B" = "ORB", "Listening" = "LI")
    } else if (input$select_starting_cohort == 4) {
      c("Mathematics" = "MA", "Reading" = "RE", "Science" = "SC", "ICT" = "IC",
        "English as a foreign language" = "EF", "Native Russian" = "NR",
        "Native Turkish" = "NT", "Scientific Thinking" = "ST")
    } else if (input$select_starting_cohort == 5) {
      c("Mathematics" = "MA", "Reading" = "RE", "Science" = "SC", "ICT" = "IC",
        "English as a foreign language" = "EF", "Business Administration" = "BA")
    } else if (input$select_starting_cohort == 6) {
      c("Mathematics" = "MA", "Reading" = "RE", "Science" = "SC", "ICT" = "IC")
    }
  })

  observeEvent(c(input$select_starting_cohort, input$select_domain), {
    values$waves_for_domain_and_sc <- if (input$select_starting_cohort == 1) {
      if (input$select_domain == "MA") {c(5, 7)#, 9)
      } else if (input$select_domain == "SC") {c(6, 8)
      } else if (input$select_domain == "CD") {c(1)
        #      } else if (domain == "VO") {c(4, 6, 8)
      }
    } else if (input$select_starting_cohort == 2) {
      if (input$select_domain == "RE") {c(6, 9)
      } else if (input$select_domain == "MA") {c(2, 3, 4, 6, 9)
      } else if (input$select_domain == "SC") {c(1, 3, 5, 9)
      } else if (input$select_domain %in% c("NR", "NT")) {c(4)
      } else if (input$select_domain == "IC") {c(5)
      } else if (input$select_domain == "VO") {c(1, 3, 5) # wles not yet in suf!
      } else if (input$select_domain %in% c("ORA", "ORB")) {c(6)
      } else if (input$select_domain == "GR") {c(3)# c(1, 3)
      }
    } else if (input$select_starting_cohort == 3) {
      if (input$select_domain == "RE") {c(1, 3, 6, 9)
      } else if (input$select_domain == "MA") {c(1, 3, 5, 9)
      } else if (input$select_domain == "SC") {c(2, 5, 8)
      } else if (input$select_domain %in% c("NR", "NT")) {c(3, 6)
      } else if (input$select_domain == "IC") {c(2, 5, 9)
      } else if (input$select_domain == "EF") {c(7, 9)
      } else if (input$select_domain %in% c("ORA", "ORB")) {c(1, 3, 5)
      } else if (input$select_domain == "ST") {9
      } else if (input$select_domain == "LI") {6}
    } else if (input$select_starting_cohort == 4) {
      if (input$select_domain == "RE") {c(2, 7, 10)
      } else if (input$select_domain == "MA") {c(1, 7, 10)
      } else if (input$select_domain == "SC") {c(1, 5)
      } else if (input$select_domain %in% c("NR", "NT")) {2
      } else if (input$select_domain == "IC") {c(1, 7)
      } else if (input$select_domain == "EF") {c(3, 7)
      } else if (input$select_domain == "ST") {7}
    } else if (input$select_starting_cohort == 5) {
      if (input$select_domain %in% c("MA", "RE")) {c(1, 12)
      } else if (input$select_domain %in% c("SC", "IC")) {5
      } else if (input$select_domain == "BA") {7
      } else if (input$select_domain == "EF") {12}
    } else if (input$select_starting_cohort == 6) {
      if (input$select_domain == "RE") {c(3, 5, 9)
      } else if (input$select_domain == "MA") {c(3, 9)
      } else if (input$select_domain %in% c("SC", "IC")) {5}
    }
  })

  observe({
    updateSelectInput(session = session, inputId = "select_domain",
                      choices = values$domains_for_sc)
  })

  observe({
    updateSelectInput(session = session, inputId = "select_wave",
                      choices = values$waves_for_domain_and_sc)
  })

})
