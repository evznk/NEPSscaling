
shinyUI(
  navbarPage(id = "navbar",
   theme = bslib::bs_theme(
      bg = "#E8E6F4", fg = "black", primary = "#24195D", secondary = "#1D0E46",
      "font-size-base" = "1.5rem",
      base_font = bslib::font_google("Open Sans"),
      code_font = bslib::font_google("Open Sans")
    ),
      tabPanel(
      fluidRow(
      column(2, offset = 1, img(height = 50, width = 100, src = "NEPSscaling_Logo_3.png")))
      ,
      tags$head(
        tags$style(
          HTML(
          ".shiny-notification {
             position: fixed;
             top: calc(50%);
             left: calc(50%);
          }
          .btn-block {
             display: block;
             width: 100%;
             color: #E8E6F4;
             background-color: #24195D;
             word-break: break-all;
             word-wrap: break-word;
             white-space: normal;
          }
          .btn-block:hover {
             color: #E8E6F4;
             background-color: #24195D;
          }
          .btn-group-container-sw {
             display: flex;
          }
          .radiobtn {
             margin-top: 30px;
             margin-bottom: 30px;
             flex: 1;
             color: #E8E6F4;
             background-color: #24195D;
          }
          .radiobtn:hover {
             color: #E8E6F4;
             background-color: #24195D;
          }"
          )
        )
      ),
      # print output to shiny:
      # https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui/30490698#3049069844446666666
      shinyjs::useShinyjs(),

      ## ---------------------------------sidebar----------------------------------
      sidebarLayout(
        sidebarPanel(
          # Conditional Panel for Manage
          ## https://shiny.rstudio.com/reference/shiny/1.6.0/conditionalPanel.html
          ## Conditional panel in conditional panel
          conditionalPanel(
            condition = "input.conditionedPanels== 1",
            shinyWidgets::dropdownButton(
              inputId = "input_bgdata",
              fileInput(inputId = "import_bgdata",
                        label = tags$strong("Import background data"),
                        multiple = FALSE, 
                        accept = c(".rds", ".sav", ".dta", ".RData", ".rdata", ".Rdata")),
              tags$span(style = "font-size: 0.75em;",
                        "Upload size up to 30MB. Accepts '.rds', '.sav', '.dta' and RData formats."),

              hr(),

              actionButton(inputId = "remove_bgdata",
                           label = "Remove background data"),

              hr(),

              actionButton(inputId = "Display_Bgdata",
                           label = "Inspect background data"),
              tags$span(style = "font-size: 0.75em;",
                        "The changes affect the display of the background data only."),
              shinyjs::hidden(
                selectInput("bgdata_select_cols", "Select columns", choices = "",
                            multiple = TRUE),
                textInput("bgdata_filter_rows", "Filter", placeholder = "e.g., var1 == 1"),
                selectInput("bgdata_sort_cases", "Sort by", choices = ""),
                shinyWidgets::prettyCheckbox(
                  inputId = "bgdata_ascending", label = "Ascending",
                  status = "primary", value = TRUE, shape = "curve", outline = TRUE
                )
              ),

              circle = FALSE, status = "block",
              width = "100%",
              label = "Manage background data"
            ),
            hr(),
            shinyWidgets::dropdownButton(

              tags$strong("Download PV object"),
              textInput(
                "pv_obj_name", label = "Choose file name",
                value = paste0("pv_obj_", gsub(":", "-", gsub(" ", "_", Sys.time())))
              ),
              downloadButton("download_pv_obj", label = "Download PV object(.rds)"),
              selectInput("export_format", label = "Select export format",
                          choices = c("SPSS", "Stata", "Mplus")),
              downloadButton("export_pv_obj", label = "Export PV object"),

              circle = FALSE, status = "block",
              width = "100%",
              label = "Manage plausible value object"
            ,
            hr(),
            inputId = "input_pv_obj",
            fileInput(inputId = "import_state",
                      label = tags$strong("Import PV object"),
                      multiple = FALSE,
                      accept = c(".rds", ".sav", ".dta", ".RData", ".rdata", ".Rdata")),
            tags$span(style = "font-size: 0.75em;",
                      "Upload size up to 30MB. Accepts '.rds', '.sav', '.dta' and RData formats."),

            hr(),

            actionButton(inputId = "remove_pv_obj", label =  "Remove PV object")),

            hr(),
            shinyWidgets::dropdownButton(
              inputId = "scale_level",
              shinyWidgets::prettyCheckbox(
                inputId = "metric", label = "All variables are metric.",
                status = "primary", value = FALSE, shape = "curve", outline = TRUE
              ),
              selectInput(inputId = "ordinal", label = "Select ordinal variables",
                          choices = "No data uploaded yet", multiple = TRUE),
              selectInput(inputId = "nominal", label = "Select nominal variables",
                          choices = "No data uploaded yet", multiple = TRUE),

              circle = FALSE, status = "block",
              width = "100%",
              label = "Set scale levels of background data"
            )
          ),

        # Conditional Panel for Input Parameter
          conditionalPanel(
            condition = "input.conditionedPanels==2",
            h4("Arguments for Plausible Values Estimation"),
            selectInput("select_starting_cohort",
                        label = "Starting cohort",
                        choices = 1:6,
                        selected = ""
            ),
            selectInput("select_domain",
                        label = "Competence domain",
                        choices = c(
                          "Mathematics" = "MA", "Reading" = "RE", "Science" = "SC",
                          "Information and Communication Technology" = "IC",
                          "Listening Comprehension" = "LI",
                          "English as a Foreign Language" = "EF",
                          "Native Russian" = "NR", "Native Turkish" = "NT",
                          "Orthography A" = "ORA","Orthography B" = "ORB",
                          "Scientific Thinking" = "ST", "Business Administration" = "BA",
                          "Cognitive Development" = "CD", "Grammar" = "GR",
                          "Vocabulary" = "VO"
                        ),
                        selected = ""
            ),
            selectInput("select_wave",
                        label = "Assessment wave",
                        choices = 1:12,
                        selected = ""
            ),
            textInput(inputId = "path_to_data", label = "Directory with competence data (SUFs)",
                      value = getwd()),

            shinyWidgets::dropdownButton(
              inputId = "output_parameters",
              numericInput("npv", label = "Number of plausible values",
                           value = 10, min = 1),
              numericInput("nmi", label = "Number of imputations",
                           value = 10, min = 1),
              shinyWidgets::prettyCheckbox(
                inputId = "WLE", label = "Return WLEs?",
                status = "primary", value = FALSE, shape = "curve", outline = TRUE
              ),
              shinyWidgets::prettyCheckbox(
                inputId = "EAP", label = "Return EAPs?",
                status = "primary", value = FALSE, shape = "curve", outline = TRUE
              ),

              circle = FALSE, status = "block",
              width = "100%",
              label = "Customize output parameters"
            ),
            tags$hr(),
            shinyWidgets::dropdownButton(
              inputId = "model_parameters",
              shinyWidgets::prettyCheckbox(
                inputId = "longitudinal", label = "Use of longitudinal competence tests?",
                status = "primary", value = FALSE, shape = "curve", outline = TRUE
              ),
              shinyWidgets::prettyCheckbox(
                inputId = "rotation",
                label = tags$text("Include position of", #br(),
                                  "competence test?"),
                status = "primary", value = TRUE, shape = "curve", outline = TRUE
              ),
              shinyWidgets::prettyCheckbox(
                inputId = "adjust_school_context",
                label = tags$text("Include proxy for school", #br(),
                                  "context?"),
                status = "primary", value = TRUE, shape = "curve", outline = TRUE
              ),
              shinyWidgets::prettyCheckbox(
                inputId = "include_nr",
                label = tags$text("Include proxy for processing", #br(),
                                  "speed?"),
                status = "primary", value = TRUE, shape = "curve", outline = TRUE
              ),
              numericInput("min_valid",
                           label = "Minimum number of valid answers to competence test(s)",
                           value = 3, min = 0),
              numericInput("seed",
                           label = "Seed for random number generator",
                           #value = sample(0:100000, 1),
                           value = 1234,
                           min = 0),
              selectInput(inputId = "exclude1", label = "Variables to exclude from background data (for estimation only)",
                          choices = "", multiple = TRUE),
              shinyjs::hidden(
                selectInput(inputId = "exclude2",
                            label = "Variables to exclude (2nd wave)",
                            choices = "", multiple = TRUE),
                selectInput(inputId = "exclude3",
                            label = "Variables to exclude (3rd wave)",
                            choices = "", multiple = TRUE),
                selectInput(inputId = "exclude4",
                            label = "Variables to exclude (4th wave)",
                            choices = "", multiple = TRUE),
                selectInput(inputId = "exclude5",
                            label = "Variables to exclude (5th wave)",
                            choices = "", multiple = TRUE)
              ),

              circle = FALSE, status = "block",
              width = "100%",
              label = "Customize model parameters"
            ),
            tags$hr(),
            shinyWidgets::prettyCheckbox(
              inputId = "verbose", label = "Progress reports?",
              status = "primary", value = TRUE, shape = "curve", outline = TRUE
            ),
            # other controls: not changeable!,
            hr(),
            #___________________________-Added by RaelK_________________________
            numericInput(
              inputId = "seed",
              label = "Set random seed (optional)",
              value = 1234,
              min = 1
            ),
            #___________________________________________________________________
            actionButton("estimate_pv_obj", label = "Start estimation")
          ),

          # Conditional Panel for Visualize Estimates
          conditionalPanel(
            condition = "input.conditionedPanels==3",
            shinyWidgets::dropdownButton(
              inputId = "plots_distribution_plot",
              selectInput(inputId = "geom", label = "Select plot type",
                          choices = c("Histogram", "Density plot", "Scatter plot")),
              selectInput("x", label = "Select variable on x-axis",
                          choices = ""),
              selectInput("y", label = "Select variable on y-axis",
                          choices = ""),
              selectInput("fill", label = "Select variable for color coding",
                          choices = ""),
              textInput(inputId = "title", label = "Plot title"),
              textInput(inputId = "xlab", label = "Label of x-axis"),
              textInput(inputId = "ylab", label = "Label of y-axis"),
              selectInput("theme", label = "Select plot theme",
                          choices = c("Gray", "Black and white", "Linedraw", "Light",
                                      "Dark", "Minimal", "Classic", "Void")),
              actionButton("plot", label = "Display plot"),

              circle = FALSE, status = "block",
              width = "100%",
              label = "Plots for plausible values and imputations"
            ),
            hr(),
            shinyWidgets::dropdownButton(
              inputId = "plots_tree_structure",
              selectInput(inputId = "imputation", label = "Select imputation",
                          choices = ""),
              selectInput(inputId = "variable", label = "Select variable",
                          choices = ""),
              actionButton(inputId = "cart_plot", label = "Display tree plot"),

              circle = FALSE, status = "block",
              width = "100%",
              label = "Imputation tree structures"
            ),
            hr(),
            shinyWidgets::dropdownButton(
              inputId = "plots_variable_importance",
              selectInput(inputId = "imputation_var_imp",
                          label = "Select imputation", choices = ""),
              selectInput(inputId = "variable_var_imp",
                          label = "Select variable", choices = ""),
              actionButton(inputId = "variable_importance_plot",
                           label = "Display variable importance plot"),

              circle = FALSE, status = "block",
              width = "100%",
              label = "Variable importance plots for imputations"
            )
          ),

          # Conditional Panel for Summary Statistics
          conditionalPanel(
            condition = "input.conditionedPanels==5",
            shinyWidgets::radioGroupButtons("checkGroup3",
                         choices = list(
                           "Descriptive tables for plausible values and imputations" = 1,
                           "Descriptive tables for item parameters" = 2,
                           "Regression weights" = 3
                         ),
                         direction = "vertical"#,
                         # individual = TRUE
            )
          )
        ),
        ## --------------------------------Main Panel-----------------------------------------------------------
        mainPanel(
          tabsetPanel(
            tabPanel("Manage", value = 1,
                     verbatimTextOutput("summary"),
                     dataTableOutput("bgdata_display")),
            tabPanel("Estimate Plausible Values", value = 2,
                     h3(textOutput("plausible_values_progress"))),
            tabPanel("Plots", value = 3,
                     conditionalPanel(
                       condition = "output.plots_conditional_visible==1",
                       tags$h3("Distribution plots"),
                       plotOutput("plot"),
                       textInput("plot_name", label = "Plot name",
                                 value = paste0("plot_",
                                                gsub(":", "-", gsub(" ", "_", Sys.time())))),
                       selectInput("plot_format",
                                   label = "Select export format",
                                   choices = c("png", "RData")),
                       downloadButton(outputId = "download_plot",
                                      label = "Download plot")
                     ),
                     conditionalPanel(
                       condition = 'output.plots_conditional_visible==2',
                       tags$h3("Imputation tree plots"),
                       plotOutput("cart_plot"),
                       textInput("cart_name", label = "Plot name",
                                 value = paste0("cart_",
                                                gsub(":", "-", gsub(" ", "_", Sys.time())))),
                       selectInput("cart_format",
                                   label = "Select export format",
                                   choices = c("png", "RData")),
                       downloadButton(outputId = "download_cart",
                                      label = "Download plot")
                     ),
                     conditionalPanel(
                       condition = "output.plots_conditional_visible==3",
                       tags$h3("Variable importance plots"),
                       plotOutput("variable_importance_plot"),
                       textInput("variable_importance_name", label = "Plot name",
                                 value = paste0("variable_importance_",
                                                gsub(":", "-", gsub(" ", "_", Sys.time())))),
                       selectInput("variable_importance_format",
                                   label = "Select export format",
                                   choices = c("png", "RData")),
                       downloadButton(outputId = "download_variable_importance",
                                      label = "Download plot")
                     )),
            tabPanel("Tables", value = 5,
                     conditionalPanel(
                       condition = "input.checkGroup3==1",
                       tags$h3("Descriptive table"),
                       tableOutput("imputation_table"),
                       textInput("descriptive_name", label = "Table name",
                                 value = paste0("descriptives_",
                                                gsub(":", "-", gsub(" ", "_", Sys.time())))),
                       downloadButton(outputId = "download_descriptive",
                                      label = "Download Descriptives")
                     ),
                     conditionalPanel(
                       condition = "input.checkGroup3==2",
                       tags$h3("Item parameters"),
                       tableOutput("item_difficulties"),
                       textInput("difficulties_name", label = "Table name",
                                 value = paste0("difficulties_",
                                                gsub(":", "-", gsub(" ", "_", Sys.time())))),
                       downloadButton(outputId = "download_difficulties",
                                      label = "Download table")
                     ),
                     conditionalPanel(
                       condition = "input.checkGroup3==3",
                       tags$h3("Regression weights"),
                       tableOutput("regression_table"),
                       textInput("regression_name", label = "Table name",
                                 value = paste0("regression_",
                                                gsub(":", "-", gsub(" ", "_", Sys.time())))),
                       downloadButton(outputId = "download_regression",
                                      label = "Download table")                     )),
            id = "conditionedPanels"
          )
        )
      )
    ),
    title = NULL,
    tabPanel(
      fluidRow(uiOutput("tab")),
      inverse = FALSE
    ),

    ## ------------------------------Header-----------------------------------------------------------------
      navbarMenu(
        tags$i(class = "far fa-question-circle", style="font-size: 36px"),
      tabPanel("NEPSscaling",
               fluidRow(
                 column(12,
                        tags$dl(
                          tags$dt("The NEPSscaling package"),
                          tags$dd(
                            tags$ul(
                              tags$li("Helps NEPS data users to estimate plausible values for the major competence domains"),
                              tags$li("The estimation by plausible_values() is based on the psychometric results described in the respective technical reports of the substudies."),
                              tags$li("To further ensure comparability between the plausible values and the WLEs, any corrections of the WLEs (e.g., for sample dropout, changes in the booklet rotation design, or linking) are acknowledged by the function (see the respective technical reports for potential corrections applied)")
 )
                            ))
                        )
                 )),
      tabPanel("Citation",
        fluidRow(
          column(8,
                 tags$dl(
                   tags$dt("Citing the NEPSscaling package"),
                   tags$dd("Scharl, A., Carstensen, C. H., & Gnambs, T. (2020). Estimating Plausible Values with NEPS Data: An Example Using Reading Competence in Starting Cohort 6. NEPS Survey Papers. https://doi.org/10.5157/NEPS:SP71:1.0"
                     )
                 )
        ))),

      tabPanel(
        "Contact",
        fluidRow(
          column(8,
                 HTML("If you have any questions or comments regarding NEPS<em>scaling</em>, please contact one of the following:"),
                 tags$ul(
                   tags$li("The project", tags$a(href="mailto:skalierung(at)lifbi.de", tags$strong("Scaling and Test Design")), " of the LIfBi, responsible for developing ", HTML("NEPS<em>scaling</em>")),
                   tags$li("The", tags$a(href="https://forum.lifbi.de/", tags$strong("NEPSforum")), " for general questions regarding the edition, dissemination and use of NEPS data"),
                   tags$li("The", tags$a(href="https://www.neps-data.de/Data-Center/Contact-Data-Center", tags$strong("Research Data Center")), " for further information on the NEPS data")
                 )
         )
        )
      ),
      tabPanel(
        "Plausible Values",
        fluidRow(
          column(12,
                 tags$dl(
                   tags$dt("Plausible Values"),
                   tags$dd(
                     tags$ul(
                       tags$li("Estimators for latent constructs such as competencies"),
                       tags$li("Set of random draws out of individual respondent's latent competence distribution"),
                       tags$li("Derived from competence test and respondent characteristics (e.g., gender, socio-economic status)"),
                       tags$li("Uncertainty in random draws reflects uncertainty in competence estimation"),
                       tags$li("Background variables should at least contain all variables used for later analysis"),
                       tags$li("Unbiased on a population level, but biased on the individual level because of respondent information (i.e., group-level information)"),
                       tags$li("Special case of multiple imputation: statistical analyses with plausible values have to be performed accordingly")
                     )
                   )
                 ),
                 tags$img(style="max-width: 500px; width: 40%; height: auto;",
                          src = "structural_model_pvs.png", alt = "Structural model"),
                 tags$dl(
                   tags$dt("Recommended Reading"),
                   tags$dd(
                     tags$ol(
                       tags$li("Scharl, A., Carstensen, C.H., & Gnambs, T. (2020). Estimating Plausible Values with NEPS Data: An Example Using Reading Competence in Starting Cohort 6 (NEPS Survey Paper No. 71). Bamberg: Leibniz Institute for Educational Trajectories, National Educational Panel Study. doi:10.5157/NEPS:SP71:1.0"),
                       tags$li("von Davier, M., Gonzalez, E., & Mislevy, R. (2009). What are plausible values and why are they useful. IERI Monograph Series, 2, 9–36."),
                       tags$li("Lüdtke, O., & Robitzsch, A. (2017). Eine Einführung in die Plausible-Values-Technik für die psychologische Forschung. Diagnostica, 63(3), 193–205. doi:10.1026/0012-1924/a000175"),
                       tags$li("Rubin, D. B. (1987). Multiple imputation for nonresponse in surveys. doi:10.1002/9780470316696"),
                       tags$li("Mislevy, R. J. (1991). Randomization-based inference about latent variables from complex samples. Psychometrika, 56(2), 177–196. doi:10.1007/BF02294457"),
                       tags$li("Meng, X.-L. (1994). Multiple-imputation inferences with uncongenial sources of input. Statistical Science, 538–558. doi:10.1214/ss/1177010269")
                     )
                   )
                 )
          )
        )
      ),
      tabPanel(
        "Classification and Regression Trees",
        fluidRow(
          column(12,
                 tags$dl(
                   tags$dt("Classification and Regression Trees"),
                   tags$dd(
                     tags$ul(
                       tags$li("Background variables for plausible values cannot contain missingness, but non-response is pervasive in large scale assessments and surveys"),
                       tags$li("Multiple imputation as an approach to fill in randomly missing data without introducing further bias"),
                       tags$li("Decision trees (e.g., classification and regression trees, CART) can be used to identify a set of plausible responses for the missing data"),
                       tags$li("Variable with missingness is recursively split into subsets; each subset has to be more homogenous than the superset"),
                       tags$li("Splits are made according to a value on a predictor variable (e.g., being female, being older than X years) until a node purity criterion is reached"),
                       tags$li("Prediction for missing values are drawn by following the tree's branches to its nodes and choosing a value from the node following an algorithm"),
                       tags$li("CART is a non-parametric approach and automatically incorporates non-linear relationships in the predicted and predictor variables")
                     )
                   )
                 ),
                 tags$img(style="max-width: 500px; width: 40%; height: auto;",
                          src = "binary_tree.png", alt = "Binary decision tree"),
                 tags$dl(
                   tags$dt("Recommended Reading"),
                   tags$dd(
                     tags$ol(
                       tags$li("Scharl, A., Carstensen, C.H., & Gnambs, T. (2020). Estimating Plausible Values with NEPS Data: An Example Using Reading Competence in Starting Cohort 6 (NEPS Survey Paper No. 71). Bamberg: Leibniz Institute for Educational Trajectories, National Educational Panel Study. doi:10.5157/NEPS:SP71:1.0"),
                       tags$li("Aßmann, C., Gaasch, C., Pohl, S., & Carstensen, C. H. (2016). Estimation of plausible values considering partially missing background information: A data augmented MCMC approach. In H.-P. Blossfeld, J. Skopek, J. Maurice, & M. Bayer (Eds.), Methodological Issues of Longitudinal Surveys (pp. 503–521). Springer."),
                       tags$li("Loh, W.-Y. (2011). Classification and regression trees. Wiley Interdisciplinary Reviews: Data Mining and Knowledge Discovery, 1(1), 14–23. doi:10.1002/widm.8"),
                       tags$li("Rubin, D. B. (1987). Multiple imputation for nonresponse in surveys. doi:10.1002/9780470316696")
                     )
                   )
                 )
          )
        )
      )),
 tabPanel(
        column(1, offset = 1, img(height = 40, width = 90, src = "NEPS_reduziert_RGB_v01.png")),
        tabPanel("National Educational Panel Study",
                 fluidRow(
                   column(12,
                          tags$dl(
                            tags$dt("For information about the NEPS, visit the website"),
                            tags$dd(
                              tags$ul(
                                tags$a(href="https://www.neps-data.de/",
                                       "German"),
                                tags$a(href="https://www.neps-data.de/Mainpage",
                                       "English")
                                )))))
                 )
     ),
 tabPanel(
   fluidRow(column(1, offset = 5, img(height = 40, width = 45, src = "LIfBi_Logo_solo_RZ.png"))),
   tabPanel("Leibnitz Institute for Educational Trajectories",
            fluidRow(
              column(12,
                     tags$dl(
                       tags$dt("For information about the LIfBi, visit the website"),
                       tags$dd(
                         tags$ul(
                           tags$a(href="https://www.lifbi.de/",
                                  "German"),
                           tags$a(href="https://www.lifbi.de/LIfBi-Home",
                                  "English")

                         )
                         )))))
   )  ))





