
#'Rforecasting
#' @param df A dataframe input
#' @param Measure Specify value measures such as Revenue and Shipments
#' @param Time Specify time variables such as Year, Quarter, and Month
#' @param Pred.time Specify a number of years or quarters to predict
#' @param Last.period Specify the last time value. Year, Quarter
#' @export

Rforecasting <- function(df=.Last.value,
                         Measure = "",
                         Time = "",
                         Pred.time = "",
                         Last.period = "",
                         launch.browser = False) {



  ################################################################################################################
  #Non-reactive functions
  ################################################################################################################


  options(scipen=999)


  #Combine Time columns to one - DF
  if(length(Time) > 1){

    df_Time <- df %>% dplyr::select(Time)
    df_excl_Time <- df %>% dplyr::select(-one_of(Time))
    colnames(df_Time) <- c("Time_1", "Time_2")

    df_Time <- df_Time %>%
      dplyr::mutate(Time_1 = as.numeric(gsub("[[:alpha:][:punct:]]","", Time_1)),
                    Time_2 = as.numeric(gsub("[[:alpha:][:punct:]]","", Time_2))) %>%
      dplyr::mutate(Time = paste(Time_1, Time_2, sep = "_")) %>%
      dplyr::select(Time)

    df <-cbind(df_Time, df_excl_Time)

  }else{

    df_Time <- df %>% dplyr::select(Time)
    df_excl_Time <- df %>% dplyr::select(-one_of(Time))
    colnames(df_Time) <- c("Time")

    df_Time <- df_Time %>%
      dplyr::mutate(Time = as.numeric(gsub("[[:alpha:][:punct:]]","", Time)))

    df <-cbind(df_Time, df_excl_Time)

  }



  #Gather values
  if(length(Measure) > 1){
    df0 <- df %>%
      tidyr::gather(Measures, Value, Measure) %>%
      dplyr::mutate_if(is.factor, as.character)
  }else{
    df0 <- df %>%
      dplyr::mutate_if(is.factor, as.character)

    names(df0)[names(df0) == Measure] <- "Value"
  }

  #Make names
  names(df0) <- make.names(names(df0))
  all.elements <- "Show All"

  #Remove variables with a unique value.
  unique.df0 <- df0 %>%
    summarise_all(funs(n_distinct(.))) %>%
    gather() %>%
    filter(value>1)


  dim_names <-unique.df0$key


  #select dimension to run the model
  df0a <- df0[, dim_names]
  dim_select <- names(df0a)[!(names(df0a) %in% c("Measures", "Value"))]

  if(length(Measure) > 1){
    df1 <- df0 %>% dplyr::select(Time, Measures, Value, dim_select)
  }else{
    df1 <- df0 %>% dplyr::select(Time, Value, dim_select)
  }


  dim_names_df <- df1[,unlist(!sapply(df1,is.numeric))]
  dim_names <- names(dim_names_df)

  xreg_names_df <- df1[,unlist(sapply(df1,is.numeric))] %>%
    select(-Value)
  xreg_names <- names(xreg_names_df)



  ################################################################################################################
  #UI.R
  ################################################################################################################


  ui <- dashboardPage(

    dashboardHeader(title = "IPRforecasting"),
    dashboardSidebar(
      br(),
      fluidRow(
        column(
          width = 12,
          uiOutput("ui_update_data")
        ),
        column(
          div(
            style = "padding: 15px",
            h4("Data Filter: ")
          ),
          width = 12,
          uiOutput("selects")
        )
      ),
      br()
    ),

    dashboardBody(
      tags$style(HTML(".box.box-info {background: #ffffff}
                      .box.box-primary {background: rgba(236,240,245,1)}")),
      tags$style(".span12 {color: black;}"),
      tags$head(tags$style(HTML(".skin-blue .main-sidebar label{color: white;}
                                .skin-blue .main-header .navbar {background-color: #006791;}
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #006791;}
                                .skin-blue .main-header .logo:hover {background-color: #006791;}
                                .skin-blue .main-header .logo {background-color: #006791;}
                                .skin-blue .main-sidebar {background-color:  #434b4e;}
                                "))),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),

      tabsetPanel(
        tabPanel("Rforecasting",
                 icon = shiny::icon("cog"),
                 br(),
                 box(status = "info", title = "Dashboard", height = "1400",width = "12",solidHeader = F,
                     fluidRow(
                       shinyjs::useShinyjs(),
                       box(height = "600",width = "2",solidHeader = T, title = 'Model options',
                           column(
                             width = 12,
                             checkboxInput("Model.options.arima", label = "ARIMA", value = FALSE),
                             checkboxInput("Model.options.arima.xreg", label = "ARIMA.XREG", value = TRUE),
                             checkboxInput("Model.options.bagged.ets", label = "BaggedETS", value = FALSE),
                             checkboxInput("Model.options.bagged.ets.xreg", label = "BaggedETS.XREG", value = FALSE),
                             checkboxInput("Model.options.bagged.arima", label = "BaggedARIMA", value = FALSE),
                             checkboxInput("Model.options.bagged.arima.xreg", label = "BaggedARIMA.XREG", value = FALSE),
                             checkboxInput("Model.options.tbats", label = "TBATS", value = FALSE),
                             checkboxInput("Model.options.tbats.xreg", label = "TBATS.XREG", value = FALSE),
                             checkboxInput("Model.options.nnar", label = "NNAR", value = FALSE),
                             checkboxInput("Model.options.nnar.xreg", label = "NNAR.XREG", value = FALSE),
                             checkboxInput("Model.options.mlp", label = "MLP", value = FALSE),
                             checkboxInput("Model.options.mlp.xreg", label = "MLP.XREG", value = FALSE),
                             checkboxInput("Model.options.bass.diffusion", label = "Bass.Diffusion", value = FALSE)

                           )
                       ),
                       box(height = "600px",width = "10",solidHeader = T,
                           column(width = 12,
                                  radioButtons("Chart.options", label = "Chart options",
                                               choices = c("Level" = "Level", "Y/Y" = "YoY", "Q/Q" = "QoQ"),
                                               selected = "Level",
                                               inline = TRUE),
                                  plotlyOutput("plotly", width = "100%", height = "450px")
                           )
                       )
                     ),
                     br(),
                     fluidRow(
                       box(title = "Model Evalutation", height = "620",width = "5",solidHeader = T,
                           column(
                             width = 12,
                             list(
                               div(
                                 p(strong(code("Best Accuracy:"))), textOutput("Best.Accuracy")
                               ),
                               br(),
                               div(DT::dataTableOutput("Accuracy",width = "100%"))
                             )
                           )
                       ),
                       box(title = "Cross Validation", height = "620px",width = "5",solidHeader = T,
                           column(
                             width = 12,
                             list(
                               div(
                                 p(strong(code("Best CV:"))), textOutput("Best.cv")
                               ),
                               br(),
                               div(style = 'overflow-x: scroll',DT::dataTableOutput("ts.cv",width = "100%"))
                             )
                           )
                       ),
                       box(height = "620",width = "2",solidHeader = T, title = 'Cross-Validation options',
                           column(
                             width = 12,
                             checkboxInput("CV.options.arima", label = "ARIMA", value = FALSE),
                             checkboxInput("CV.options.arima.xreg", label = "ARIMA.XREG", value = TRUE),
                             checkboxInput("CV.options.bagged.ets", label = "BaggedETS", value = FALSE),
                             checkboxInput("CV.options.bagged.ets.xreg", label = "BaggedETS.XREG", value = FALSE),
                             checkboxInput("CV.options.bagged.arima", label = "BaggedARIMA", value = FALSE),
                             checkboxInput("CV.options.bagged.arima.xreg", label = "BaggedARIMA.XREG", value = FALSE),
                             checkboxInput("CV.options.tbats", label = "TBATS", value = FALSE),
                             checkboxInput("CV.options.tbats.xreg", label = "TBATS.XREG", value = FALSE),
                             checkboxInput("CV.options.nnar", label = "NNAR", value = FALSE),
                             checkboxInput("CV.options.nnar.xreg", label = "NNAR.XREG", value = FALSE),
                             checkboxInput("CV.options.mlp", label = "MLP", value = FALSE),
                             checkboxInput("CV.options.mlp.xreg", label = "MLP.XREG", value = FALSE)

                           )
                       ),
                       #                       box(title = "Data Output", height = "20",width = "6",solidHeader = T,
                       #                           column(
                       #                             width = 12,
                       #                             list(
                       #                               div(style = 'overflow-x: scroll',DT::dataTableOutput("df.dt",width = "100%")),
                       #                             )
                       #                           )
                       #                      ),
                       br(),
                       div(
                         style = "padding: 15px",
                         downloadButton(outputId = "df.dt_mydownload", label = "Download Data Output")
                       )
                     )),
                 br(),
                 br(),
                 fluidPage(
                   tags$style(HTML(".tabbable > .nav > li > a                           {background-color: #006791; color: white}
                                   .tabbable > .nav > li > a[data-value='Time.Series'] {background-color: #006791; color: white}
                                   .tabbable > .nav > li > a[data-value='Bass.Diffusion'] {background-color: #006791; color: white}
                                   ")),
                   tabsetPanel(
                     tabPanel("ARIMA",
                              br(),
                              h4(strong("ARIMA: ")),
                              br(),
                              h4("1. Decomposition"),
                              br(),
                              box(status = "primary", height = "500px",width = "4",solidHeader = F,
                                  h5(strong("Decomposition")),
                                  p("The building blocks of a time series analysis are seasonality, trend, and cycle.
                                    These intuitive components capture the historical patterns in the series.
                                    Not every series will have all three (or any) of these components, but if they are present, deconstructing the series can help you understand its behavior and prepare a foundation for building a forecasting model.
                                    Seasonal component refers to fluctuations in the data related to calendar cycles.
                                    ")
                                  ),
                              box(height = "500px",width = "8",solidHeader = F,
                                  plotOutput("decomp.plot", width = "100%", height = "400px")
                              ),
                              br(),
                              h4("2. Stationarity"),
                              br(),
                              box(status = "primary", height = "750px",width = "6",solidHeader = F,
                                  p("Fitting an ARIMA model requires the series to be stationary.
                                    A series is said to be stationary when its mean, variance, and autocovariance are time invariant. "),
                                  br(),
                                  h5(strong("KPSS")),
                                  p("The Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test figures out if a time series is stationary around a mean or linear trend, or is non-stationary due to a unit root. A stationary time series is one where statistical properties - like the mean and variance - are constant over time.In this case, the null-hypothesis is that the data are stationary.  In this case, p-value less than 0.05 indicates non-stationary series and p-value greater than 0.05 indicates stationary series."),
                                  h5(strong(verbatimTextOutput("kpss.text"))),
                                  textOutput("kpss.i"),
                                  br(),
                                  h5(strong("BOX-Ljung Test")),
                                  p("The p-values for the modified Box-Ljung test all are well above .05, indicating that the residuals are independent (white noise)."),
                                  h5(strong(verbatimTextOutput("ARIMA.Box"))),
                                  br(),
                                  h5(strong("ACF/PACF")),
                                  p("Autocorrelation plots (also known as ACF or the auto correlation function) are a useful visual tool in determining whether a series is stationary.
                                    These plots can also help to choose the order parameters for ARIMA model.
                                    ACF plots display correlation between a series and its lags.
                                    In addition to suggesting the order of differencing, ACF plots can help in determining the order of the M A (q) model.
                                    Partial autocorrelation plots (PACF), as the name suggests, display correlation between a variable and its lags that is not explained by previous lags.
                                    PACF plots are useful when determining the order of the AR(p) model.")
                                  ),
                              box(height = "750px",width = "6",solidHeader = F,
                                  plotOutput("smr.plot", width = "100%", height = "650px")
                              ),
                              h4("3.1 Auto Arima"),
                              br(),
                              box(status = "primary", height = "500px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary - ARIMA")),
                                  h5(strong(verbatimTextOutput("ARIMA.model")))
                              ),
                              box(status = "primary", height = "500px",width = "4",solidHeader = F,
                                  h5(strong("Arima Model")),
                                  p("ARIMA stands for auto-regressive integrated moving average and is specified by these three order parameters: (p, d, q).
                                    The process of fitting an ARIMA model is sometimes referred to as the Box-Jenkins method. "),
                                  br(),
                                  h5(strong("Seasonal Arima")),
                                  p("The seasonal ARIMA model incorporates both non-seasonal and seasonal factors in a multiplicative model.  One shorthand notation for the model is"),
                                  h5(strong("ARIMA(p, d, q) x (P, D, Q)S,")),
                                  p("with p = non-seasonal AR order, d = non-seasonal differencing, q = non-seasonal MA order, P = seasonal AR order, D = seasonal differencing, Q = seasonal MA order, and S = time span of repeating seasonal pattern.")
                                  ),
                              box(status = "primary", height = "500px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary - ARIMA.XREG")),
                                  h5(strong(verbatimTextOutput("ARIMA.XREG.model"))),
                                  h5(strong(verbatimTextOutput("ARIMA.XREG.accuracy")))
                              ),
                              box(status = "primary", height = "500px",width = "4",solidHeader = F,
                                  h5(strong("Arima.XREG.Model"))
                              )),
                     tabPanel("Bagged Model",
                              br(),
                              h4(strong("BaggedETS: ")),
                              br(),
                              h4("1. BaggedETS Model"),
                              br(),
                              box(status = "primary", height = "400px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary - BaggedETS")),
                                  h5(strong(verbatimTextOutput("BaggedETS.model"))),
                                  h5(strong(verbatimTextOutput("BaggedETS.accuracy")))
                              ),
                              box(status = "primary", height = "500px",width = "4",solidHeader = F,
                                  h5(strong("BaggedETS")),
                                  p("This function implements the bagged model forecasting method described in Bergmeir et al. By default, the ets function is applied to all bootstrapped series. Base models other than ets can be given by the parameter fn. Using the default parameters, the function bld.mbb.bootstrap is used to calculate the bootstrapped series with the Box-Cox and Loess-based decomposition (BLD) bootstrap. The function forecast.baggedModel can then be used to calculate forecasts."),
                                  br(),
                                  p("baggedETS is a wrapper for baggedModel, setting fn to 'ets'. This function is included for backwards compatibility only, and may be deprecated in the future."),
                                  br(),
                                  h5(strong("Box-Cox and Loess-based decomposition bootstrap")),
                                  br(),
                                  p("The procedure is described in Bergmeir et al. Box-Cox decomposition is applied, together with STL or Loess (for non-seasonal time series), and the remainder is bootstrapped using a moving block bootstrap. In the moving block bootstrap, data is split into n-b+1 overlapping blocks of length b: Observation 1 to b will be block 1, observation 2 to b+1 will be block 2 etc. Then from these n-b+1 blocks, n/b blocks will be drawn at random with replacement. Then aligning these n/b blocks in the order they were picked, will give the bootstrap observations.")
                              ),
                              box(status = "primary", height = "400px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary - BaggedETS.XREG")),
                                  h5(strong(verbatimTextOutput("BaggedETS.XREG.model"))),
                                  h5(strong(verbatimTextOutput("BaggedETS.XREG.accuracy")))
                              ),
                              box(status = "primary", height = "400px",width = "4",solidHeader = F,
                                  h5(strong("BaggedETS.XREG.Model"))
                              ),
                              box(status = "primary", height = "500px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary - BaggedARIMA")),
                                  h5(strong(verbatimTextOutput("BaggedARIMA.model"))),
                                  h5(strong(verbatimTextOutput("BaggedARIMA.accuracy")))
                              ),
                              box(status = "primary", height = "500px",width = "4",solidHeader = F,
                                  h5(strong("BaggedARIMA.Model"))
                              ),
                              box(status = "primary", height = "400px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary - BaggedARIMA.XREG")),
                                  h5(strong(verbatimTextOutput("BaggedARIMA.XREG.model"))),
                                  h5(strong(verbatimTextOutput("BaggedARIMA.XREG.accuracy")))
                              ),
                              box(status = "primary", height = "400px",width = "4",solidHeader = F,
                                  h5(strong("BaggedARIMA.XREG.Model"))
                              )
                     ),
                     tabPanel("TBATS",
                              br(),
                              h4(strong("TBATS: ")),
                              br(),
                              h4("1. TBATS Model"),
                              br(),
                              box(status = "primary", height = "600px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary - TBATS")),
                                  h5(strong(verbatimTextOutput("TBATS.model"))),
                                  h5(strong(verbatimTextOutput("TBATS.accuracy")))
                              ),
                              box(status = "primary", height = "600px",width = "4",solidHeader = F,
                                  h5(strong("TBATS (Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components)")),
                                  p("The generic accessor functions fitted.values and residuals extract useful features of the value returned by bats and associated functions. The fitted model is designated TBATS(omega, p,q, phi, <m1,k1>,...,<mJ,kJ>) where omega is the Box-Cox parameter and phi is the damping parameter; the error is modelled as an ARMA(p,q) process and m1,...,mJ list the seasonal periods used in the model and k1,...,kJ are the corresponding number of Fourier terms used for each seasonality."),
                                  br()
                              ),
                              box(status = "primary", height = "600px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary - TBATS.XREG")),
                                  h5(strong(verbatimTextOutput("TBATS.XREG.model"))),
                                  h5(strong(verbatimTextOutput("TBATS.XREG.accuracy")))
                              ),
                              box(status = "primary", height = "500px",width = "4",solidHeader = F,
                                  h5(strong("TBATS.XREG.Model"))
                              )
                     ),
                     tabPanel("NNAR",
                              br(),
                              h4(strong("NNAR: ")),
                              br(),
                              h4("1. NNAR Model"),
                              br(),
                              box(status = "primary", height = "500px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary - NNAR")),
                                  h5(strong(verbatimTextOutput("NNAR.model"))),
                                  h5(strong(verbatimTextOutput("NNAR.accuracy")))
                              ),
                              box(status = "primary", height = "500px",width = "4",solidHeader = F,
                                  h5(strong("NNAR: Neural Network Autoregression")),
                                  p("A feed-forward neural network is fitted with lagged values of y as inputs and a single hidden layer with size nodes. The inputs are for lags 1 to p, and lags m to mP where m=frequency(y). If xreg is provided, its columns are also used as inputs. If there are missing values in y or xreg, the corresponding rows (and any others which depend on them as lags) are omitted from the fit. A total of repeats networks are fitted, each with random starting weights. These are then averaged when computing forecasts. The network is trained for one-step forecasting. Multi-step forecasts are computed recursively."),
                                  br(),
                                  p("For non-seasonal data, the fitted model is denoted as an NNAR(p,k) model, where k is the number of hidden nodes. This is analogous to an AR(p) model but with nonlinear functions. For seasonal data, the fitted model is called an NNAR(p,P,k)[m] model, which is analogous to an ARIMA(p,0,0)(P,0,0)[m] model but with nonlinear functions.")
                              ),
                              box(status = "primary", height = "500px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary - NNAR.XREG")),
                                  h5(strong(verbatimTextOutput("NNAR.XREG.model"))),
                                  h5(strong(verbatimTextOutput("NNAR.XREG.accuracy")))
                              ),
                              box(status = "primary", height = "500px",width = "4",solidHeader = F,
                                  h5(strong("NNAR.XREG.Model"))
                              )
                     ),
                     tabPanel("MLP",
                              br(),
                              h4(strong("MLP: ")),
                              br(),
                              h4("1. MLP Model"),
                              br(),
                              box(status = "primary", height = "600px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary - MLP")),
                                  h5(strong(verbatimTextOutput("MLP.model"))),
                                  h5(strong(verbatimTextOutput("MLP.accuracy")))
                              ),
                              box(status = "primary", height = "600px",width = "4",solidHeader = F,
                                  h5(strong("MLP (Multilayer Perceptron for time series forecasting)")),
                                  p("A multilayer perceptron (MLP) is a class of feedforward artificial neural network. An MLP consists of, at least, three layers of nodes: an input layer, a hidden layer and an output layer. Except for the input nodes, each node is a neuron that uses a nonlinear activation function."),
                                  br()
                              ),
                              box(status = "primary", height = "600px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary - MLP.XREG")),
                                  h5(strong(verbatimTextOutput("MLP.XREG.model"))),
                                  h5(strong(verbatimTextOutput("MLP.XREG.accuracy")))
                              ),
                              box(status = "primary", height = "500px",width = "4",solidHeader = F,
                                  h5(strong("MLP.XREG.Model"))
                              )
                     ),
                     tabPanel("Bass Diffusion",
                              br(),
                              h4(strong("Bass Diffusion: ")),
                              br(),
                              h4("1. Bass Diffusion Model"),
                              br(),
                              box(status = "primary", height = "800px",width = "8",solidHeader = F,
                                  h5(strong("Model Summary")),
                                  h5(strong(verbatimTextOutput("BD.model"))),
                                  h5(strong(verbatimTextOutput("BD.accuracy")))
                              ),
                              box(status = "primary", height = "800px",width = "4",solidHeader = F,
                                  h5(strong("Bass Diffusion")),
                                  p("The Bass Model or Bass Diffusion Model was developed by Frank Bass. It consists of a simple differential equation that describes the process of how new products get adopted in a population. The model presents a rationale of how current adopters and potential adopters of a new product interact. The basic premise of the model is that adopters can be classified as innovators or as imitators and the speed and timing of adoption depends on their degree of innovativeness and the degree of imitation among adopters. The Bass model has been widely used in forecasting, especially new products' sales forecasting and technology forecasting. Mathematically, the basic Bass diffusion is a Riccati equation with constant coefficients."),
                                  p("MPQ: M is the ultimate market potential. The coefficient P is called the coefficient of innovation, external influence or advertising effect.
                                    The coefficient Q is called the coefficient of imitation, internal influence or word-of-mouth effect. #Function nlsLM - estimates parameters using the method of nonlinear least squares."),
                                  br()
                                  )
                     )))),

        tabPanel("Link",
                 br(),
                 h4("Link"),
                 icon = icon("link"),
                 br(),
                 a(href ="https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials", "[1][1] Introduction to Forecasting with ARIMA in R"),
                 br(),
                 a(href ="https://onlinecourses.science.psu.edu/stat510/node/67", "[1][2] Seasonal ARIMA models "),
                 br(),
                 a(href ="https://www.otexts.org/fpp/8/7", "[1][3] ARIMA modelling in R "),
                 br(),
                 a(href ="https://robjhyndman.com/hyndsight/forecast3/", "[1][4] Major changes to the forecast package "),
                 br(),
                 a(href ="http://www.statisticshowto.com/kpss-test/", "[1][5] KPSS Test: Definition and Interpretation "),
                 br(),
                 a(href ="https://www.listendata.com/2015/10/arima-modeling-with-r.html", "[1][6] ARIMA MODELING WITH R "),
                 br(),
                 a(href ="https://www.quantstart.com/articles/Autoregressive-Integrated-Moving-Average-ARIMA-p-d-q-Models-for-Time-Series-Analysis", "[1][7] Autoregressive Integrated Moving Average ARIMA(p, d, q) Models for Time Series Analysis"),
                 br(),
                 a(href ="http://www.uvm.edu/pdodds/files/papers/others/everything/bass1969a.pdf", "[2][1] F. Bass, A New Product Growth for Model Consumer Durables, Management Science, Vol. 15 (January 1969)"),
                 br(),
                 a(href ="https://rpubs.com/chengjun/bass_model", "[2][2] Bass diffusion model chengjun"),
                 br(),
                 a(href ="https://www.r-bloggers.com/a-better-nls/", "[2][3] A better 'nls' (?)"),
                 br(),
                 a(href ="https://journal.r-project.org/archive/2017/RJ-2017-006/RJ-2017-006.pdf", "[2][4] Implementing a Metapopulation Bass Diffusion Model using the R Package deSolve"),
                 br(),
                 a(href ="http://www.iam.fmph.uniba.sk/institute/stehlikova/ts16/ex/ex02_bass.pdf", "[2][5] Application of nonlinear least squares: Estimating parameters of the Bass model"),
                 br(),
                 a(href ="http://people.duke.edu/~rnau/testing.htm", "[2][6] Regression diagnostics:  testing the assumptions of linear regression"),
                 br(),
                 a(href = "http://shishirshakya.blogspot.com/", "[3][1] Economic Modeling"),
                 br(),
                 a(href = "https://hbr.org/1971/07/how-to-choose-the-right-forecasting-technique", "[3][2] How to Choose the Right Forecasting Technique")

        ))))


  ################################################################################################################
  #Server.R
  ################################################################################################################



  server <- function(input, output, session) {

    dat0 <- reactive({
      dat <- df1
      return(dat)
    })


    pred.time.input <- reactive({
      h <- Pred.time
      return(h)
    })

    last.period <- reactive({
      lp <- paste(Last.period[1],Last.period[2], sep="_")
      return(lp)
    })



    #Series filtering
    output$selects <- renderUI({
      req(dat0())
      #Loop through each dimension to build a filter
      lapply(seq_along(dim_names), function(i){
        dat <- dat0()

        #Treat all series the same... time, nonagg, etc...
        choice.list <- c(all.elements, unique(dat0()[, dim_names[i]]))
        #Choose that total
        choice.selected <- choice.list[1]
        #Multiple allowed
        choice.mult <- TRUE


        # Build the Menu for each dimension
        selectInput(
          inputId = paste0("Sel", i),
          label = paste0(dim_names[i]),
          choices = choice.list,
          selected = choice.selected,
          multiple = choice.mult
        )
      })
    })

    ###
    # Action Button
    ###
    output$ui_update_data <- renderUI({
      actionButton("update_data", label = "Refresh Data", icon = shiny::icon("refresh"),
                   style = "background-color:#17B3D1; color:#ffffff;")
    })


    ###
    # Edit table
    ###


    #Filter
    dat.filtered <- eventReactive(input$update_data,
                                  ignoreNULL = FALSE, {
                                    req(dat0())


                                    dat <- dat0()
                                    datF <- dat

                                    for(i in seq_along(dim_names)){
                                      get_input <- eval(parse(text=paste0("input$Sel", i))) #Which filter to check


                                      #If no items are selected or the Select All is selected, show ALL items
                                      if(length(get_input) == 0 || all.elements %in% get_input){
                                        get_series <- tibble::as.tibble(dat[, dim_names[i]]) %>% dplyr::distinct() %>% dplyr::pull()

                                        filter_criteria_T <- lazyeval::interp( ~ which_column %in% get_series, which_column = as.name(dim_names[i])) #If a Filter is selected....
                                      } else {
                                        get_series <- as.character(get_input)
                                        filter_criteria_T <- lazyeval::interp( ~ which_column %in% get_series, which_column = as.name(dim_names[i])) #If a Filter is selected....
                                      }

                                      #.... Do this
                                      datF <- datF %>%
                                        dplyr::filter_(filter_criteria_T)

                                    } #End for

                                    return(as.data.frame(datF))
                                  })




    dat.value <- reactive({

      dat <- dat.filtered()
      lp <- last.period()

      dat <- dat %>%
        dplyr::select(Time, Value) %>%
        dplyr::group_by(Time) %>%
        dplyr::summarize(Value = sum(Value)) %>%
        dplyr::filter(!grepl("NA",Time))

      lp.n <- which(grepl(lp, dat$Time))

      dat <- dat[1:lp.n, ]

      return(as.data.frame(dat))
    })


    dat.CurrentView <- reactive({
      dat <- dat.filtered()

      CurrentView <- dat %>%
        dplyr::select(Time, Value) %>%
        dplyr::group_by(Time) %>%
        dplyr::summarize(Value = sum(Value)) %>%
        dplyr::filter(!grepl("NA",Time)) %>%
        dplyr::rename(Value.incl.Forecast = Value)

      return(as.data.frame(CurrentView))
    })



    xreg <- reactive({
      dat <- dat.filtered()
      dat.value <- dat.value()

      xreg <- dat %>%
        dplyr::select(Time, xreg_names) %>%  #XREG names
        dplyr::group_by(Time) %>%
        gather(xreg_var, Value, -Time) %>%
        group_by(xreg_var, Time) %>%
        dplyr::summarize(xreg = sum(Value)) %>%
        spread(xreg_var, xreg) %>%
        dplyr::select(-Time) %>%
        na.omit()


      lp.n <- nrow(dat.value)

      xreg <- xreg[1:lp.n,]


      return(data.matrix(xreg))
    })


    xreg.fc <- reactive({
      dat <- dat.filtered()
      dat.value <- dat.value()

      xreg.fc <- dat %>%
        dplyr::select(Time, xreg_names) %>%  #XREG names
        dplyr::group_by(Time) %>%
        gather(xreg_var, Value, -Time) %>%
        group_by(xreg_var, Time) %>%
        dplyr::summarize(xreg = sum(Value)) %>%
        spread(xreg_var, xreg) %>%
        dplyr::select(-Time) %>%
        na.omit()

        lp.n <- nrow(dat.value)

        xreg.fc <- xreg.fc[1:(lp.n+1),]
        xreg.fc <- xreg.fc %>%
          tail(1)


      return(data.matrix(xreg.fc))
    })


    xreg.fc.full <- reactive({
      dat <- dat.filtered()
      dat.value <- dat.value()
      xreg <- xreg()

      dim.num <- xreg/dat.value$Value
      last.dim <- last(dim.num)
      xreg.dim.num <- rbind(dim.num, last.dim)


      xreg.fc.full <- dat %>%
        dplyr::select(Time, xreg_names) %>%  #XREG names
        dplyr::group_by(Time) %>%
        gather(xreg_var, Value, -Time) %>%
        group_by(xreg_var, Time) %>%
        dplyr::summarize(xreg = sum(Value)) %>%
        spread(xreg_var, xreg) %>%
        dplyr::select(-Time) %>%
        na.omit()

      xreg.fc.full <- xreg.fc.full/xreg.dim.num

      lp.n <- nrow(dat.value)
      xreg.fc.full <- xreg.fc.full[1:(lp.n+1),]


      return(data.matrix(xreg.fc.full))
    })


    ###
    #Pred.time.frame
    ###

    #Time frame, Time_1: Year, Time_2: Quarter or Month
    frame.output <- reactive({

      dat <- dat.value()
      h <- pred.time.input()

      dat <- dat %>%
        tidyr::separate(Time, c("Time_1", "Time_2"), "_", remove = FALSE)

      this.year <- max(as.numeric(dat$Time_1))

      if(is.na(dat$Time_2)){

        pred.time <- as.data.frame(rep((this.year + 1):(this.year + h), each = 1))
        colnames(pred.time) <- "Time"

      }else{

        this.time_2 <- as.numeric(tail(dat$Time_2, n=1))
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        # For a full year or Non full year data
        if(as.numeric(max(dat$Time_1)) == this.year && as.numeric(max(dat$Time_2)) == this.time_2){
          pred.time <- as.data.frame(rep((this.year + 1):(this.year + h/Freq), each = Freq))
          colnames(pred.time) <- "Time_1"
          pred.time$Time_2 <- rep(1:Freq, h/Freq)
        }else{
          pred.time <- as.data.frame(rep((this.year):(this.year + h/Freq), each = Freq))
          colnames(pred.time) <- "Time_1"
          pred.time <- pred.time %>%
            slice(this.time_2+1:n())
          Time_2 <- rep(1:Freq, (h+Freq)/Freq)
          pred.time$Time_2 <- Time_2[-(1:this.time_2)]
        }


        pred.time <- as.data.frame(paste(pred.time$Time_1, pred.time$Time_2, sep="_"))
        colnames(pred.time) <- "Time"

      }

      df.time <- dat %>%
        dplyr::select(Time) %>%
        unique()

      frame.output <- rbind(df.time, pred.time)

      return(as.data.frame(frame.output))
    })

    ###
    #ts.data
    ###

    ts.data <- reactive({

      dat <- dat.value()
      frame <- frame.output()
      h <- nrow(frame)-nrow(dat)

      dat <- dat %>%
        tidyr::separate(Time, c("Time_1", "Time_2"), "_", remove = FALSE)

      if(is.na(dat$Time_2)){
        Freq <- 1
        dat$ts.time <- as.numeric(dat$Time_1)
      }else{
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time_2)))
        dat$ts.time <- as.numeric(dat$Time_1) + as.numeric(dat$Time_2)/Freq - 1/Freq
      }

      start.time = min(dat$ts.time)
      end.time = max(dat$ts.time)

      ts.data <- ts(data = dat$Value, start = c(start.time) , end = c(end.time), frequency = Freq)

      return(ts.data)
    })


    ###
    #Models
    ###

    df.NULL <- reactive({
      frame <- frame.output()
      df <- setNames(data.frame(matrix(ncol = 15, nrow = nrow(frame))), c("Time", "MLP", "MLP.XREG","ARIMA", "ARIMA.XREG", "NNAR", "NNAR.XREG", "BaggedETS", "BaggedETS.XREG", "BaggedARIMA", "BaggedARIMA.XREG", "TBATS", "TBATS.XREG", "Bass.Diffusion", "Linear.Regession"))
    })



    ARIMA <- reactive({
      df.NULL <- df.NULL()
      ARIMA <- df.NULL$ARIMA
    })

    ARIMA.XREG <- reactive({
      df.NULL <- df.NULL()
      ARIMA.XREG <- df.NULL$ARIMA.XREG
    })

    NNAR <- reactive({
      df.NULL <- df.NULL()
      NNAR <- df.NULL$NNAR
    })

    NNAR.XREG <- reactive({
      df.NULL <- df.NULL()
      NNAR.XREG <- df.NULL$NNAR.XREG
    })

    BaggedETS <- reactive({
      df.NULL <- df.NULL()
      BaggedETS <- df.NULL$BaggedETS
    })

    BaggedETS.XREG <- reactive({
      df.NULL <- df.NULL()
      BaggedETS.XREG <- df.NULL$BaggedETS.XREG
    })

    BaggedARIMA <- reactive({
      df.NULL <- df.NULL()
      BaggedARIMA <- df.NULL$BaggedARIMA
    })

    BaggedARIMA.XREG <- reactive({
      df.NULL <- df.NULL()
      BaggedARIMA.XREG <- df.NULL$BaggedARIMA.XREG
    })

    TBATS <- reactive({
      df.NULL <- df.NULL()
      TBATS <- df.NULL$TBATS
    })

    TBATS.XREG <- reactive({
      df.NULL <- df.NULL()
      TBATS.XREG <- df.NULL$TBATS.XREG
    })

    Bass.Diffusion <- reactive({
      df.NULL <- df.NULL()
      Bass.Diffusion <- df.NULL$Bass.Diffusion
    })

    Linear.Trend <- reactive({
      df.NULL <- df.NULL()
      Linear.Trend <- df.NULL$Linear.Trend
    })

    MLP <- reactive({
      df.NULL <- df.NULL()
      MLP <- df.NULL$MLP
    })

    MLP.XREG <- reactive({
      df.NULL <- df.NULL()
      MLP <- df.NULL$MLP.XREG
    })


    #observeEvent(input$Model.options.nnar.xreg, shinyjs::disable("Model.options.nnar.xreg"))
    observeEvent(input$Model.options.arima.xreg, shinyjs::disable("Model.options.arima.xreg"))
    observeEvent(input$Model.options.bass.diffusion, shinyjs::disable("Model.options.bass.diffusion"))




    #Arima with XREG ############################################################################################


    ARIMA.XREG.model.2 <- reactive({


      ts <- ts.data()
      dat <- dat.value()
      xreg_input <- xreg()
      xreg_input_fc <- xreg.fc()
      xreg_input_full <- xreg.fc.full()

      #h
      frame <- frame.output()
      h <- nrow(frame)-nrow(dat)
      Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))



      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        ARIMA.XREG.model <- auto.arima(ts, xreg = xreg_input, allowmean = FALSE, allowdrift = FALSE),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){

        new.ts <- ts(xreg_input_full, frequency = Freq)

        ARIMA.XREG.model.2 <- auto.arima(new.ts, allowmean = FALSE, allowdrift = FALSE)


      }else{

        ARIMA.XREG.model.1 <- auto.arima(ts, xreg = xreg_input, allowmean = FALSE, allowdrift = FALSE)

        #ARIMA.XREG.input
        ARIMA.XREG <- forecast(ARIMA.XREG.model.1, h=1, xreg = xreg_input_fc)


        ARIMA.XREG.x <- data.frame(ARIMA.XREG = c(ARIMA.XREG$x), time = c(time(ARIMA.XREG$x)))
        ARIMA.XREG.mean <-data.frame(ARIMA.XREG = c(ARIMA.XREG$mean), time = c(time(ARIMA.XREG$mean)))
        ARIMA.XREG.input <- rbind(ARIMA.XREG.x, ARIMA.XREG.mean) %>%
          dplyr::select(ARIMA.XREG)


        ARIMA.XREG.ts <- ts(ARIMA.XREG.input, frequency = Freq)

        ARIMA.XREG.model.2 <- auto.arima(ARIMA.XREG.ts, allowmean =  FALSE)

      }


      return(ARIMA.XREG.model.2)

    })


    ARIMA.XREG.Forecast <- reactive({


      ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()

      #h
      dat <- dat.value()
      frame <- frame.output()
      h <- nrow(frame)-nrow(dat)

      ARIMA.XREG <- forecast(ARIMA.XREG.model.2, h=h-1)


      return(ARIMA.XREG)
    })


    #Time Series Result
    ARIMA.XREG <- reactive({


      ARIMA.XREG <- ARIMA.XREG.Forecast()

      #ts.fitted with forecast
      ARIMA.XREG.fitted <- data.frame(ARIMA.XREG = c(ARIMA.XREG$fitted), time = c(time(ARIMA.XREG$fitted)))
      ARIMA.XREG.mean <-data.frame(ARIMA.XREG = c(ARIMA.XREG$mean), time = c(time(ARIMA.XREG$mean)))

      if(min(ARIMA.XREG.mean$ARIMA.XREG)<0){
        ARIMA.XREG.mean$ARIMA.XREG <- 0
      }else{
        ARIMA.XREG.mean <- ARIMA.XREG.mean
      }

      ARIMA.XREG <- rbind(ARIMA.XREG.fitted, ARIMA.XREG.mean) %>%
        dplyr::select(ARIMA.XREG)


      #merge
      return(as.data.frame(ARIMA.XREG))

    })



    #Model Summary
    output$ARIMA.XREG.model <- renderPrint({
      ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
      ARIMA.XREG.model.2
    })

    output$ARIMA.XREG.accuracy <- renderPrint({
      ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
      ARIMA.XREG.input <- ARIMA.XREG.model.2$x


      dat <- dat.value()
      Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))
      ARIMA.XREG.ts <- ts(ARIMA.XREG.input, frequency = Freq)


      accuracy(ARIMA.XREG.model.2$fitted, ARIMA.XREG.ts)
    })


    #NNAR with XREG ############################################################################################

    lambda <- reactive({
      ts <- ts.data()
      dat <- dat.value()
      frame <- frame.output()
      h <- nrow(frame)-nrow(dat)

      NNAR.model <- nnetar(ts, lambda = 0.5, scale.inputs = TRUE)

      set.seed(12345)
      NNAR <- forecast(NNAR.model, h=h, bootstrap = TRUE)
      NNAR.mean <-data.frame(NNAR = c(NNAR$mean), time = c(time(NNAR$mean)))

      if(min(NNAR.mean$NNAR)<0){
        lambda <- 0
      }else{
        lambda <- 0.5
      }

    })

    NNAR.XREG.model.on <- reactive({

      NNAR.XREG.model.on <- input$Model.options.nnar.xreg

      if(NNAR.XREG.model.on != 0){
        NNAR.XREG.model.on <- "NNAR.XREG.model.on"
      }else{
        NNAR.XREG.model.on <- "NNAR.XREG.model.off"
      }

      return(NNAR.XREG.model.on)

    })


    NNAR.XREG.model.1 <- reactive({

      NNAR.XREG.model.on <- NNAR.XREG.model.on()

      if(NNAR.XREG.model.on()  == "NNAR.XREG.model.on"){

      ts <- ts.data()
      xreg_input <- xreg()
      xreg_input <- as.data.frame(xreg_input)
      lambda <- lambda()

      NNAR.XREG.model.1 <- nnetar(ts, lambda = lambda, xreg = xreg_input, scale.inputs = TRUE)

      }

      return(NNAR.XREG.model.1)
    })

    #Add a new input
    NNAR.XREG.input <- reactive({

      NNAR.XREG.model.on <- NNAR.XREG.model.on()

      if(NNAR.XREG.model.on()  == "NNAR.XREG.model.on"){

      ts <- ts.data()
      xreg_input <- xreg()
      xreg_input_fc <- xreg.fc()


      NNAR.XREG.model.1 <- NNAR.XREG.model.1()

      #NNAR.XREG.input
      set.seed(12345)
      NNAR.XREG <- forecast(NNAR.XREG.model.1, h=1, xreg = xreg_input_fc, bootstrap = TRUE)

      #h
      dat <- dat.value()
      Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

      NNAR.XREG.x <- data.frame(NNAR.XREG = c(NNAR.XREG$x), time = c(time(NNAR.XREG$x)))
      NNAR.XREG.mean <-data.frame(NNAR.XREG = c(NNAR.XREG$mean), time = c(time(NNAR.XREG$mean)))
      NNAR.XREG.input <- rbind(NNAR.XREG.x, NNAR.XREG.mean) %>%
        dplyr::select(NNAR.XREG)

      }

      return(NNAR.XREG.input)

    })

    #build a model using a new input
    NNAR.XREG.model.2 <- reactive({

      NNAR.XREG.model.on <- NNAR.XREG.model.on()

      if(NNAR.XREG.model.on()  == "NNAR.XREG.model.on"){

        NNAR.XREG.input <- NNAR.XREG.input()

        dat <- dat.value()
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))
        lambda <- lambda()

        NNAR.XREG.ts <- ts(NNAR.XREG.input, frequency = Freq)

        NNAR.XREG.model.2 <- nnetar(NNAR.XREG.ts, lambda = lambda, scale.inputs = TRUE)
      }

      return(NNAR.XREG.model.2)
    })


    NNAR.XREG.Forecast <- reactive({

      NNAR.XREG.model.on <- NNAR.XREG.model.on()

      if(NNAR.XREG.model.on()  == "NNAR.XREG.model.on"){

        NNAR.XREG.model.2 <- NNAR.XREG.model.2()

        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)

        set.seed(12345)
        NNAR.XREG <- forecast(NNAR.XREG.model.2, h=h-1, bootstrap = TRUE)

      }

      return(NNAR.XREG)
    })



    #Time Series Result
    NNAR.XREG <- reactive({

      NNAR.XREG.model.on <- NNAR.XREG.model.on()
      df.NULL <- df.NULL()

      if(NNAR.XREG.model.on()  == "NNAR.XREG.model.on"){

          NNAR.XREG <- NNAR.XREG.Forecast()

          #ts.fitted with forecast
          NNAR.XREG.fitted <- data.frame(NNAR.XREG = c(NNAR.XREG$fitted), time = c(time(NNAR.XREG$fitted)))
          NNAR.XREG.mean <-data.frame(NNAR.XREG = c(NNAR.XREG$mean), time = c(time(NNAR.XREG$mean)))

          if(min(NNAR.XREG.mean$NNAR.XREG)<0){
            NNAR.XREG.mean$NNAR.XREG <- 0
          }else{
            NNAR.XREG.mean <- NNAR.XREG.mean
          }

          NNAR.XREG <- rbind(NNAR.XREG.fitted, NNAR.XREG.mean) %>%
            dplyr::select(NNAR.XREG)

      }else{
        NNAR.XREG <- df.NULL$NNAR.XREG
      }


      return(as.data.frame(NNAR.XREG))

    })



    #Model Summary
    output$NNAR.XREG.model <- renderPrint({
      NNAR.XREG.model.2 <- NNAR.XREG.model.2()
      NNAR.XREG.model.2
    })

    output$NNAR.XREG.accuracy <- renderPrint({
      NNAR.XREG.input <- NNAR.XREG.input()
      NNAR.XREG.model.2 <- NNAR.XREG.model.2()

      dat <- dat.value()
      Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

      NNAR.XREG.ts <- ts(NNAR.XREG.input, frequency = Freq)
      NNAR.XREG.accuracy <- accuracy(NNAR.XREG.model.2$fitted, NNAR.XREG.ts)

      return(NNAR.XREG.accuracy)
    })




    #Linear Trend ############################################################################################

    Linear.Trend <- reactive({

      #h
      dat <- dat.value()
      frame <- frame.output()
      h <- nrow(frame)-nrow(dat)

      lm.dat <- dat %>%
        dplyr::select(Value)
      lm.dat$x <- 1:nrow(lm.dat)


      possibleError <- tryCatch(
        lm <- glm(Value ~ x , data=lm.dat, family=gaussian(link = "identity")),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        pred.time <- data.frame(x = 1:(nrow(lm.dat)+h), Linear.Regression=0)
        Linear.Trend<- pred.time$Linear.Regression

      }else{

        lm <- glm(Value ~ x , data=lm.dat, family=gaussian(link = "identity"))
        pred.time <- data.frame(x = 1:(nrow(lm.dat)+h), Value=0)
        Linear.Trend <- predict.lm(lm, pred.time)

      }
      return(as.data.frame(Linear.Trend))

    })




    #ARIMA ############################################################################################

    ARIMA.model.on <- reactive({

      ARIMA.model.on <- input$Model.options.arima

      if(ARIMA.model.on != 0){
        ARIMA.model.on <- "ARIMA.model.on"
      }else{
        ARIMA.model.on <- "ARIMA.model.off"
      }

      return(ARIMA.model.on)

    })

    #Time Series Model
    ARIMA.model <- reactive({

      ARIMA.model.on <- ARIMA.model.on()

      if(ARIMA.model.on()  == "ARIMA.model.on"){

        ts <- ts.data()

        ARIMA.model <- auto.arima(ts, allowmean = FALSE, allowdrift = FALSE)

      }else{
        ARIMA.model <- NA
      }

      return(ARIMA.model)
    })

    #Model Summary
    output$ARIMA.model <- renderPrint({
      ARIMA.model <- ARIMA.model()
      summary(ARIMA.model)
    })

    ARIMA.Forecast <- reactive({

      ARIMA.model.on <- ARIMA.model.on()

      if(ARIMA.model.on()  == "ARIMA.model.on"){

        ts <- ts.data()

        #h
        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)

        #Fit AR(2) model
        ARIMA.model <- auto.arima(ts, allowmean = FALSE, allowdrift = FALSE)
        ARIMA.Forecast <- forecast(ARIMA.model, h=h)

      }

      return(ARIMA.Forecast)
    })


    #Time Series Result
    ARIMA <- reactive({

      ARIMA.model.on <- ARIMA.model.on()
      df.NULL <- df.NULL()

      if(ARIMA.model.on()  == "ARIMA.model.on"){

        ARIMA <- ARIMA.Forecast()

        #ts.fitted with forecast
        ts.fitted <- data.frame(ARIMA = c(ARIMA$fit), time = c(time(ARIMA$fitted)))
        ts.mean <-data.frame(ARIMA = c(ARIMA$mean), time = c(time(ARIMA$mean)))

        if(min(ts.mean$ARIMA)<0){
          ts.mean$ARIMA <- 0
        }else{
          ts.mean <- ts.mean
        }

        ARIMA <- rbind(ts.fitted, ts.mean) %>%
          dplyr::select(ARIMA)


      }else{
        ARIMA <- df.NULL$ARIMA
      }

      #merge
      return(as.data.frame(ARIMA))

    })


    ## Data Validation
    #Decomposition
    ARIMA.decomp <- reactive({

      ARIMA.model.on <- ARIMA.model.on()
      df.NULL <- df.NULL()

      if(ARIMA.model.on()  == "ARIMA.model.on"){

        ts <- ts.data()
        decomp = stl(ts, s.window = "periodic")

      }else{
        ARIMA.decomp <- NA
      }

      return(decomp)
    })

    output$decomp.plot <- renderPlot({

      ARIMA.model.on <- ARIMA.model.on()
      df.NULL <- df.NULL()

      if(ARIMA.model.on()  == "ARIMA.model.on"){

        decomp <- ARIMA.decomp()

      }

      plot(decomp)

    })



    #Stationary test: Augmented Dickey-Fuller Test

    kpss.i <- reactive({

      ARIMA.model.on <- ARIMA.model.on()

      if(ARIMA.model.on()  == "ARIMA.model.on"){

        ts <- ts.data()
        D <- nsdiffs(ts, test = c("ocsb"))

        if(D>0){
          ts <- diff(ts, lag = 4, D)
          d <- ndiffs(ts, test = "kpss")
        }else if(D==0){
          d <- ndiffs(ts, test = "kpss")
        }else{
          d <- NA
        }

        kpss.i <-rbind(d, D)
        kpss.i <- paste("(p,", d, ",q)(P,", D, ",Q)")

      }else{
        kpss.i <- NA
      }

      return(kpss.i)

    })


    kpss <- reactive({

      ARIMA.model.on <- ARIMA.model.on()

      if(ARIMA.model.on()  == "ARIMA.model.on"){

        ts <- ts.data()
        D <- nsdiffs(ts, test = c("ocsb"))

        if(D>0){
          ts <- diff(ts, lag = 4, D)
          d <- ndiffs(ts, test = "kpss")
        }else {
          d <- ndiffs(ts, test = "kpss")
        }



        if(D>0){
          diffed <- diff(ts, lag = 4, D)
          diffed <- diff(diffed, d)
          kpss.test <- kpss.test(diffed)
        }else{
          diffed <- diff(ts, d)
          kpss.test <- kpss.test(diffed)
        }

      }else{
        kpss.test <- NA
      }

      return(kpss.test)

    })


    output$kpss.i <- renderPrint({
      kpss <- kpss.i()
      kpss
    })

    output$kpss.text <- renderPrint({
      kpss <- kpss()
      kpss
    })


    #SMR tsdisplay
    ARIMA.SMR <- reactive({

      ARIMA.model.on <- ARIMA.model.on()

      if(ARIMA.model.on()  == "ARIMA.model.on"){

        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        ARIMA <- ARIMA()
        ARIMA.model <- ARIMA.model()

        smr <- tsdisplay(residuals(ARIMA.model), lag.max=length(ARIMA[[1]])-h, main='Model Residuals')

      }else{
        smr <- NA
      }
      return(smr)

    })

    output$smr.plot <- renderPlot({
      smr <- ARIMA.SMR()
      smr

    })

    #Box,test
    ARIMA.Box <- reactive({

      ARIMA.model.on <- ARIMA.model.on()

      if(ARIMA.model.on()  == "ARIMA.model.on"){

        ARIMA.model <- ARIMA.model()

        box <-Box.test(ARIMA.model$residuals, type = "Ljung-Box")

      }else{
        box <- NA
      }
      return(box)

    })

    output$ARIMA.Box <- renderPrint({
      box <- ARIMA.Box()
      box

    })



    #NNAR ############################################################################################



    NNAR.model.on <- reactive({

      NNAR.model.on <- input$Model.options.nnar

      if(NNAR.model.on != 0){
        NNAR.model.on <- "NNAR.model.on"
      }else{
        NNAR.model.on <- "NNAR.model.off"
      }

      return(NNAR.model.on)

    })

    NNAR.model <- reactive({

      NNAR.model.on <- NNAR.model.on()

      if(NNAR.model.on()  == "NNAR.model.on"){

        ts <- ts.data()
        lambda <- lambda()

        NNAR.model <- nnetar(ts, lambda = lambda, scale.inputs = TRUE)

      }else{
        NNAR.model <- NA
      }

      return(NNAR.model)
    })


    NNAR.Forecast <- reactive({

      NNAR.model.on <- NNAR.model.on()

      if(NNAR.model.on()  == "NNAR.model.on"){

        ts <- ts.data()
        NNAR.model <- NNAR.model()

        #h
        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)

        #fit NNAR
        set.seed(12345)
        NNAR.Forecast <- forecast(NNAR.model, h=h, bootstrap = TRUE)

      }

      return(NNAR.Forecast)
    })


    #Time Series Result
    NNAR <- reactive({

      NNAR.model.on <- NNAR.model.on()
      df.NULL <- df.NULL()


      if(NNAR.model.on()  == "NNAR.model.on"){

        NNAR <- NNAR.Forecast()

        #ts.fitted with forecast
        NNAR.fitted <- data.frame(NNAR = c(NNAR$fit), time = c(time(NNAR$fitted)))
        NNAR.mean <-data.frame(NNAR = c(NNAR$mean), time = c(time(NNAR$mean)))

        if(min(NNAR.mean$NNAR)<0){
          NNAR.mean$NNAR <- 0
        }else{
          NNAR.mean <- NNAR.mean
        }

        NNAR <- rbind(NNAR.fitted, NNAR.mean) %>%
          dplyr::select(NNAR)

      }else{
        NNAR <- df.NULL$NNAR
      }

      #merge
      return(as.data.frame(NNAR))

    })

    #Model Summary
    output$NNAR.model <- renderPrint({
      NNAR.model <- NNAR.model()
      NNAR.model
    })

    output$NNAR.accuracy <- renderPrint({
      ts <- ts.data()
      NNAR.model <- NNAR.model()
      accuracy(NNAR.model$fitted, ts)
    })


    #MLP ############################################################################################


    MLP.model.on <- reactive({

      MLP.model.on <- input$Model.options.mlp

      if(MLP.model.on != 0){
        MLP.model.on <- "MLP.model.on"
      }else{
        MLP.model.on <- "MLP.model.off"
      }

      return(MLP.model.on)

    })

    MLP.model <- reactive({

      MLP.model.on <- MLP.model.on()

      if(MLP.model.on()  == "MLP.model.on"){

        ts <- ts.data()

        set.seed(12345)
        MLP.model <- mlp(ts, hd.auto.type = "valid")

      }else{
        MLP.model <- NA
      }

      return(MLP.model)
    })


    MLP.Forecast <- reactive({

      MLP.model.on <- MLP.model.on()

      if(MLP.model.on()  == "MLP.model.on"){

        ts <- ts.data()
        MLP.model <- MLP.model()

        #h
        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)

        #fit MLP
        MLP.Forecast <- forecast(MLP.model, h=h)

      }

      return(MLP.Forecast)
    })


    #Time Series Result
    MLP <- reactive({

      MLP.model.on <- MLP.model.on()
      df.NULL <- df.NULL()
      dat <- dat.value()

      if(MLP.model.on()  == "MLP.model.on"){

        MLP <- MLP.Forecast()

        #ts.fitted with forecast
        MLP.fitted <- data.frame(MLP = c(MLP$fit), time = c(time(MLP$fitted)))
        MLP.mean <-data.frame(MLP = c(MLP$mean), time = c(time(MLP$mean)))

        if(min(MLP.mean$MLP)<0){
          MLP.mean$MLP <- 0
        }else{
          MLP.mean  <- MLP.mean
        }

        MLP <- rbind(MLP.fitted, MLP.mean) %>%
          dplyr::select(MLP)

        MLP.df <- setNames(data.frame(matrix(ncol = 2, nrow = nrow(dat))), c("MLP", "MLP.XREG"))
        MLP.na.df <- MLP.df[(1:(nrow(MLP.df)-nrow(MLP.fitted))), ]
        MLP.na <- as.data.frame(MLP.na.df$MLP) %>% rename(MLP = 'MLP.na.df$MLP')

        MLP <- rbind(MLP.na, MLP)

      }else{
        MLP <- df.NULL$MLP
      }

      #merge
      return(as.data.frame(MLP))

    })

    #Model Summary
    output$MLP.model <- renderPrint({
      MLP.model <- MLP.model()
      MLP.model
    })

    output$MLP.accuracy <- renderPrint({
      ts <- ts.data()
      MLP.model <- MLP.model()
      accuracy(MLP.model$fitted, ts)
    })

    #MLP with XREG ############################################################################################




    MLP.XREG.model.on <- reactive({

      MLP.XREG.model.on <- input$Model.options.mlp.xreg

      if(MLP.XREG.model.on != 0){
        MLP.XREG.model.on <- "MLP.XREG.model.on"
      }else{
        MLP.XREG.model.on <- "MLP.XREG.model.off"
      }

      return(MLP.XREG.model.on)

    })


    MLP.XREG.model <- reactive({

      MLP.XREG.model.on <- MLP.XREG.model.on()

      if(MLP.XREG.model.on()  == "MLP.XREG.model.on"){

        ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
        ARIMA.XREG.input <- ARIMA.XREG.model.2$x

        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))


        MLP.XREG.ts <- ARIMA.XREG.input[1:nrow(ARIMA.XREG.input),]
        MLP.XREG.ts <- ts(MLP.XREG.ts, frequency = Freq)
        MLP.XREG.model <- mlp(MLP.XREG.ts, hd.auto.type = "valid")

      }else{
        MLP.XREG.model <- NA
      }

      return(MLP.XREG.model)
    })



    MLP.XREG.Forecast <- reactive({

      MLP.XREG.model.on <- MLP.XREG.model.on()

      if(MLP.XREG.model.on()  == "MLP.XREG.model.on"){

        ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
        ARIMA.XREG.input <- ARIMA.XREG.model.2$x

        MLP.XREG.model <- MLP.XREG.model()

        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)

        MLP.XREG <- forecast(MLP.XREG.model, h=h-1)

      }

      return(MLP.XREG)
    })




    #Time Series Result
    MLP.XREG <- reactive({

      MLP.XREG.model.on <- MLP.XREG.model.on()
      df.NULL <- df.NULL()
      dat <- dat.value()

      if(MLP.XREG.model.on()  == "MLP.XREG.model.on"){


        MLP.XREG <- MLP.XREG.Forecast()

        #ts.fitted with forecast
        MLP.XREG.fitted <- data.frame(MLP.XREG = c(MLP.XREG$fit), time = c(time(MLP.XREG$fitted)))
        MLP.XREG.mean <-data.frame(MLP.XREG = c(MLP.XREG$mean), time = c(time(MLP.XREG$mean)))

        if(min(MLP.XREG.mean$MLP.XREG)<0){
          MLP.XREG.mean$MLP.XREG <- 0
        }else{
          MLP.XREG.mean <- MLP.XREG.mean
        }

        MLP.XREG <- rbind(MLP.XREG.fitted, MLP.XREG.mean) %>%
          dplyr::select(MLP.XREG)

        MLP.XREG.df <- setNames(data.frame(matrix(ncol = 2, nrow = nrow(dat))), c("MLP", "MLP.XREG"))
        MLP.XREG.na.df <- MLP.XREG.df[(1:(nrow(MLP.XREG.df)-nrow(MLP.XREG.fitted)+1)), ]
        MLP.XREG.na <- as.data.frame(MLP.XREG.na.df$MLP.XREG) %>% rename(MLP.XREG = 'MLP.XREG.na.df$MLP.XREG')

        MLP.XREG <- rbind(MLP.XREG.na, MLP.XREG)

      }else{
        MLP.XREG <- df.NULL$MLP.XREG
      }

      #merge
      return(as.data.frame(MLP.XREG))

    })



    #Model Summary
    output$MLP.XREG.model <- renderPrint({
      MLP.XREG.model <- MLP.XREG.model()
      MLP.XREG.model
    })

    output$MLP.XREG.accuracy <- renderPrint({

      ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
      ARIMA.XREG.input <- ARIMA.XREG.model.2$x
      MLP.XREG.model <- MLP.XREG.model()

      dat <- dat.value()
      Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

      MLP.XREG.ts <- ARIMA.XREG.input[1:nrow(ARIMA.XREG.input),]
      MLP.XREG.ts <- ts(MLP.XREG.ts, frequency = Freq)

      MLP.XREG.accuracy <- accuracy(MLP.XREG.model$fitted, MLP.XREG.ts)

      return(MLP.XREG.accuracy)
    })





    # BaggedARIMA ############################################################################################

    BaggedARIMA.model.on <- reactive({

      BaggedARIMA.model.on <- input$Model.options.bagged.arima

      if(BaggedARIMA.model.on != 0){
        BaggedARIMA.model.on <- "BaggedARIMA.model.on"
      }else{
        BaggedARIMA.model.on <- "BaggedARIMA.model.off"
      }

      return(BaggedARIMA.model.on)

    })

    #ETS Model
    BaggedARIMA.model <- reactive({

      BaggedARIMA.model.on <- BaggedARIMA.model.on()

      if(BaggedARIMA.model.on()  == "BaggedARIMA.model.on"){

        dat <- dat.value()
        ts <- ts.data()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        set.seed(12345)
        BaggedARIMA.model <- baggedModel(ts, bootstrapped_series = bld.mbb.bootstrap(ts, nrow(dat)-Freq*2 +1, block_size = Freq*2), fn = auto.arima)

      }else{
        BaggedARIMA.model <- NA
      }
      return(BaggedARIMA.model)
    })

    #Model Summary
    output$BaggedARIMA.model <- renderPrint({
      BaggedARIMA.model <- BaggedARIMA.model()
      BaggedARIMA.model
    })


    output$BaggedARIMA.accuracy <- renderPrint({
      ts <- ts.data()
      BaggedARIMA.model <- BaggedARIMA.model()
      accuracy(BaggedARIMA.model$fitted, ts)

    })

    #Forecast
    BaggedARIMA.Forecast <- reactive({

      BaggedARIMA.model.on <- BaggedARIMA.model.on()

      if(BaggedARIMA.model.on()  == "BaggedARIMA.model.on"){

        dat <- dat.value()
        ts <- ts.data()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        BaggedARIMA.model <- BaggedARIMA.model()

        #fit ETS
        BaggedARIMA.Forecast <- forecast(BaggedARIMA.model, h=h)

      }

      return(BaggedARIMA.Forecast)
    })


    #Time Series Result
    BaggedARIMA <- reactive({

      BaggedARIMA.model.on <- BaggedARIMA.model.on()
      df.NULL <- df.NULL()

      if(BaggedARIMA.model.on()  == "BaggedARIMA.model.on"){

        BaggedARIMA <- BaggedARIMA.Forecast()

        #ts.fitted with forecast
        BaggedARIMA.fitted <- data.frame(BaggedARIMA = c(BaggedARIMA$fit), time = c(time(BaggedARIMA$fitted)))
        BaggedARIMA.mean <-data.frame(BaggedARIMA = c(BaggedARIMA$mean), time = c(time(BaggedARIMA$mean)))

        if(min(BaggedARIMA.mean$BaggedARIMA)<0){
          BaggedARIMA.mean$BaggedARIMA <- 0
        }else{
          BaggedARIMA.mean <- BaggedARIMA.mean
        }

        BaggedARIMA <- rbind(BaggedARIMA.fitted, BaggedARIMA.mean) %>%
          dplyr::select(BaggedARIMA)

      }else{
        BaggedARIMA <- df.NULL$BaggedARIMA
      }
      #merge
      return(as.data.frame(BaggedARIMA))

    })


    #BaggedARIMA.XREG ############################################################################################


    BaggedARIMA.XREG.model.on <- reactive({

      BaggedARIMA.XREG.model.on <- input$Model.options.bagged.arima.xreg

      if(BaggedARIMA.XREG.model.on != 0){
        BaggedARIMA.XREG.model.on <- "BaggedARIMA.XREG.model.on"
      }else{
        BaggedARIMA.XREG.model.on <- "BaggedARIMA.XREG.model.off"
      }

      return(BaggedARIMA.XREG.model.on)

    })


    BaggedARIMA.XREG.model <- reactive({

      BaggedARIMA.XREG.model.on <- BaggedARIMA.XREG.model.on()

      if(BaggedARIMA.XREG.model.on()  == "BaggedARIMA.XREG.model.on"){

        ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
        ARIMA.XREG.input <- ARIMA.XREG.model.2$x

        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        set.seed(12345)
        BaggedARIMA.XREG.ts <- ARIMA.XREG.input[1:nrow(ARIMA.XREG.input),]
        BaggedARIMA.XREG.ts <- ts(BaggedARIMA.XREG.ts, frequency = Freq)
        BaggedARIMA.XREG.model <- baggedModel(BaggedARIMA.XREG.ts, bootstrapped_series = bld.mbb.bootstrap(BaggedARIMA.XREG.ts, nrow(dat)-Freq*2 +1, block_size = Freq*2), fn = auto.arima)

      }else{
        BaggedARIMA.XREG.model <- NA
      }

      return(BaggedARIMA.XREG.model)
    })



    BaggedARIMA.XREG.Forecast <- reactive({

      BaggedARIMA.XREG.model.on <- BaggedARIMA.XREG.model.on()

      if(BaggedARIMA.XREG.model.on()  == "BaggedARIMA.XREG.model.on"){

        ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
        ARIMA.XREG.input <- ARIMA.XREG.model.2$x

        BaggedARIMA.XREG.model <- BaggedARIMA.XREG.model()

        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)

        BaggedARIMA.XREG <- forecast(BaggedARIMA.XREG.model, h=h-1)

      }

      return(BaggedARIMA.XREG)
    })




    #Time Series Result
    BaggedARIMA.XREG <- reactive({

      BaggedARIMA.XREG.model.on <- BaggedARIMA.XREG.model.on()
      df.NULL <- df.NULL()

      if(BaggedARIMA.XREG.model.on()  == "BaggedARIMA.XREG.model.on"){


        BaggedARIMA.XREG <- BaggedARIMA.XREG.Forecast()

        #ts.fitted with forecast
        BaggedARIMA.XREG.fitted <- data.frame(BaggedARIMA.XREG = c(BaggedARIMA.XREG$fit), time = c(time(BaggedARIMA.XREG$fitted)))
        BaggedARIMA.XREG.mean <-data.frame(BaggedARIMA.XREG = c(BaggedARIMA.XREG$mean), time = c(time(BaggedARIMA.XREG$mean)))

        if(min(BaggedARIMA.XREG.mean$BaggedARIMA.XREG)<0){
          BaggedARIMA.XREG.mean$BaggedARIMA.XREG <- 0
        }else{
          BaggedARIMA.mean.XREG <- BaggedARIMA.XREG.mean
        }

        BaggedARIMA.XREG <- rbind(BaggedARIMA.XREG.fitted, BaggedARIMA.XREG.mean) %>%
          dplyr::select(BaggedARIMA.XREG)

      }else{
        BaggedARIMA.XREG <- df.NULL$BaggedARIMA.XREG
      }

      #merge
      return(as.data.frame(BaggedARIMA.XREG))

    })



    #Model Summary
    output$BaggedARIMA.XREG.model <- renderPrint({
      BaggedARIMA.XREG.model <- BaggedARIMA.XREG.model()
      BaggedARIMA.XREG.model
    })

    output$BaggedARIMA.XREG.accuracy <- renderPrint({

      ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
      ARIMA.XREG.input <- ARIMA.XREG.model.2$x
      BaggedARIMA.XREG.model <- BaggedARIMA.XREG.model()

      dat <- dat.value()
      Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

      BaggedARIMA.XREG.ts <- ARIMA.XREG.input[1:nrow(ARIMA.XREG.input),]
      BaggedARIMA.XREG.ts <- ts(BaggedARIMA.XREG.ts, frequency = Freq)

      BaggedARIMA.XREG.accuracy <- accuracy(BaggedARIMA.XREG.model$fitted, BaggedARIMA.XREG.ts)

      return(BaggedARIMA.XREG.accuracy)
    })


    #BaggedETS ############################################################################################

    BaggedETS.model.on <- reactive({

      BaggedETS.model.on <- input$Model.options.bagged.ets

      if(BaggedETS.model.on != 0){
        BaggedETS.model.on <- "BaggedETS.model.on"
      }else{
        BaggedETS.model.on <- "BaggedETS.model.off"
      }

      return(BaggedETS.model.on)

    })


    #ETS Model
    BaggedETS.model <- reactive({

      BaggedETS.model.on <- BaggedETS.model.on()

      if(BaggedETS.model.on()  == "BaggedETS.model.on"){

        dat <- dat.value()
        ts <- ts.data()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        set.seed(12345)
        BaggedETS.model <- baggedModel(ts, bootstrapped_series = bld.mbb.bootstrap(ts, nrow(dat)-Freq*2 +1, block_size = Freq*2), fn = ets)

      }else{
        BaggedETS.model <- NA
      }

      return(BaggedETS.model)
    })

    #Model Summary
    output$BaggedETS.model <- renderPrint({
      BaggedETS.model <- BaggedETS.model()
      BaggedETS.model
    })


    output$BaggedETS.accuracy <- renderPrint({
      ts <- ts.data()
      BaggedETS.model <- BaggedETS.model()
      accuracy(BaggedETS.model$fitted, ts)
    })

    #Forecast
    BaggedETS.Forecast <- reactive({

      BaggedETS.model.on <- BaggedETS.model.on()

      if(BaggedETS.model.on()  == "BaggedETS.model.on"){

        dat <- dat.value()
        ts <- ts.data()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        BaggedETS.model <- BaggedETS.model()

        #fit ETS
        BaggedETS.Forecast <- forecast(BaggedETS.model, h=h)

      }

      return(BaggedETS.Forecast)
    })


    #Time Series Result
    BaggedETS <- reactive({

      BaggedETS.model.on <- BaggedETS.model.on()
      df.NULL <- df.NULL()

      if(BaggedETS.model.on()  == "BaggedETS.model.on"){

        BaggedETS <- BaggedETS.Forecast()

        #ts.fitted with forecast
        BaggedETS.fitted <- data.frame(BaggedETS = c(BaggedETS$fit), time = c(time(BaggedETS$fitted)))
        BaggedETS.mean <-data.frame(BaggedETS = c(BaggedETS$mean), time = c(time(BaggedETS$mean)))

        if(min(BaggedETS.mean$BaggedETS)<0){
          BaggedETS.mean$BaggedETS <- 0
        }else{
          BaggedETS.mean <- BaggedETS.mean
        }

        BaggedETS <- rbind(BaggedETS.fitted, BaggedETS.mean) %>%
          dplyr::select(BaggedETS)

      }else{
        BaggedETS <- df.NULL$BaggedETS
      }

      #merge
      return(as.data.frame(BaggedETS))

    })



    #BaggedETS.XREG ########################################################################################


    BaggedETS.XREG.model.on <- reactive({

      BaggedETS.XREG.model.on <- input$Model.options.bagged.ets.xreg

      if(BaggedETS.XREG.model.on != 0){
        BaggedETS.XREG.model.on <- "BaggedETS.XREG.model.on"
      }else{
        BaggedETS.XREG.model.on <- "BaggedETS.XREG.model.off"
      }

      return(BaggedETS.XREG.model.on)

    })


    BaggedETS.XREG.model <- reactive({

      BaggedETS.XREG.model.on <- BaggedETS.XREG.model.on()

      if(BaggedETS.XREG.model.on()  == "BaggedETS.XREG.model.on"){

        ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
        ARIMA.XREG.input <- ARIMA.XREG.model.2$x

        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        set.seed(12345)
        BaggedETS.XREG.ts <- ARIMA.XREG.input[1:nrow(ARIMA.XREG.input),]
        BaggedETS.XREG.ts <- ts(BaggedETS.XREG.ts, frequency = Freq)
        BaggedETS.XREG.model <- baggedModel(BaggedETS.XREG.ts, bootstrapped_series = bld.mbb.bootstrap(BaggedETS.XREG.ts, nrow(dat)-Freq*2 +1, block_size = Freq*2), fn = ets)

      }else{
        BaggedETS.XREG.model <- NA
      }

      return(BaggedETS.XREG.model)
    })

    BaggedETS.XREG.Forecast <- reactive({

      BaggedETS.XREG.model.on <- BaggedETS.XREG.model.on()

      if(BaggedETS.XREG.model.on()  == "BaggedETS.XREG.model.on"){

        ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
        ARIMA.XREG.input <- ARIMA.XREG.model.2$x
        BaggedETS.XREG.model <- BaggedETS.XREG.model()

        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)

        BaggedETS.XREG <- forecast(BaggedETS.XREG.model, h=h-1)

      }

      return(BaggedETS.XREG)
    })


    #Time Series Result
    BaggedETS.XREG <- reactive({

      BaggedETS.XREG.model.on <- BaggedETS.XREG.model.on()
      df.NULL <- df.NULL()

      if(BaggedETS.XREG.model.on()  == "BaggedETS.XREG.model.on"){

        BaggedETS.XREG <- BaggedETS.XREG.Forecast()

        #ts.fitted with forecast
        BaggedETS.XREG.fitted <- data.frame(BaggedETS.XREG = c(BaggedETS.XREG$fit), time = c(time(BaggedETS.XREG$fitted)))
        BaggedETS.XREG.mean <-data.frame(BaggedETS.XREG = c(BaggedETS.XREG$mean), time = c(time(BaggedETS.XREG$mean)))

        if(min(BaggedETS.XREG.mean$BaggedETS.XREG)<0){
          BaggedETS.XREG.mean$BaggedETS.XREG <- 0
        }else{
          BaggedETS.XREG.mean <- BaggedETS.XREG.mean
        }

        BaggedETS.XREG <- rbind(BaggedETS.XREG.fitted, BaggedETS.XREG.mean) %>%
          dplyr::select(BaggedETS.XREG)


      }else{
        BaggedETS.XREG <- df.NULL$BaggedETS.XREG
      }

      #merge
      return(as.data.frame(BaggedETS.XREG))

    })



    #Model Summary
    output$BaggedETS.XREG.model <- renderPrint({
      BaggedETS.XREG.model <- BaggedETS.XREG.model()
      BaggedETS.XREG.model
    })

    output$BaggedETS.XREG.accuracy <- renderPrint({
      ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
      ARIMA.XREG.input <- ARIMA.XREG.model.2$x
      BaggedETS.XREG.model <- BaggedETS.XREG.model()

      dat <- dat.value()
      Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

      BaggedETS.XREG.ts <- ARIMA.XREG.input[1:nrow(ARIMA.XREG.input),]
      BaggedETS.XREG.ts <- ts(BaggedETS.XREG.ts, frequency = Freq)

      BaggedETS.XREG.accuracy <- accuracy(BaggedETS.XREG.model$fitted, BaggedETS.XREG.ts)

      return(BaggedETS.XREG.accuracy)
    })




    #TBATS ############################################################################################


    TBATS.model.on <- reactive({

      TBATS.model.on <- input$Model.options.tbats

      if(TBATS.model.on != 0){
        TBATS.model.on <- "TBATS.model.on"
      }else{
        TBATS.model.on <- "TBATS.model.off"
      }

      return(TBATS.model.on)

    })

    #TBATS Model
    TBATS.model <- reactive({

      TBATS.model.on <- TBATS.model.on()

      if(TBATS.model.on()  == "TBATS.model.on"){

        ts <- ts.data()

        TBATS.model <- tbats(ts)

      }else{
        TBATS.model <- NA
      }

      return(TBATS.model)
    })

    #Model Summary
    output$TBATS.model <- renderPrint({
      TBATS.model <- TBATS.model()
      TBATS.model
    })

    output$TBATS.accuracy <- renderPrint({
      ts <- ts.data()
      TBATS.model <- TBATS.model()
      accuracy(TBATS.model$fitted, ts)
    })

    TBATS.Forecast <- reactive({

      TBATS.model.on <- TBATS.model.on()

      if(TBATS.model.on()  == "TBATS.model.on"){

        ts <- ts.data()

        #h
        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)

        #fit TBATS
        TBATS.Forecast <- forecast(tbats(ts), h=h)

      }

      return(TBATS.Forecast)
    })


    #Time Series Result
    TBATS <- reactive({

      TBATS.model.on <- TBATS.model.on()
      df.NULL <- df.NULL()

      if(TBATS.model.on()  == "TBATS.model.on"){

        TBATS <- TBATS.Forecast()

        #ts.fitted with forecast
        TBATS.fitted <- data.frame(TBATS = c(TBATS$fit), time = c(time(TBATS$fitted)))
        TBATS.mean <-data.frame(TBATS = c(TBATS$mean), time = c(time(TBATS$mean)))

        if(min(TBATS.mean$TBATS)<0){
          TBATS.mean$TBATS <- 0
        }else{
          TBATS.mean <- TBATS.mean
        }

        TBATS <- rbind(TBATS.fitted, TBATS.mean) %>%
          dplyr::select(TBATS)

      }else{
        TBATS <- df.NULL$TBATS
      }

      #merge
      return(as.data.frame(TBATS))

    })


    #TBATS.XREG ########################################################################################


    TBATS.XREG.model.on <- reactive({

      TBATS.XREG.model.on <- input$Model.options.tbats.xreg

      if(TBATS.XREG.model.on != 0){
        TBATS.XREG.model.on <- "TBATS.XREG.model.on"
      }else{
        TBATS.XREG.model.on <- "TBATS.XREG.model.off"
      }

      return(TBATS.XREG.model.on)

    })



    TBATS.XREG.model <- reactive({

      TBATS.XREG.model.on <- TBATS.XREG.model.on()

      if(TBATS.XREG.model.on()  == "TBATS.XREG.model.on"){

        ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
        ARIMA.XREG.input <- ARIMA.XREG.model.2$x

        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        set.seed(12345)
        TBATS.XREG.ts <- ARIMA.XREG.input[1:nrow(ARIMA.XREG.input),]
        TBATS.XREG.ts <- ts(TBATS.XREG.ts, frequency = Freq)
        TBATS.XREG.model <- tbats(TBATS.XREG.ts)

      }else{
        TBATS.XREG.model <- NA
      }
      return(TBATS.XREG.model)
    })



    TBATS.XREG.Forecast <- reactive({

      TBATS.XREG.model.on <- TBATS.XREG.model.on()

      if(TBATS.XREG.model.on()  == "TBATS.XREG.model.on"){

        TBATS.XREG.model <- TBATS.XREG.model()

        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)

        TBATS.XREG <- forecast(TBATS.XREG.model, h=h-1)

      }

      return(TBATS.XREG)
    })



    #Time Series Result
    TBATS.XREG <- reactive({

      TBATS.XREG.model.on <- TBATS.XREG.model.on()
      df.NULL <- df.NULL()

      if(TBATS.XREG.model.on()  == "TBATS.XREG.model.on"){

        TBATS.XREG <- TBATS.XREG.Forecast()

        #ts.fitted with forecast
        TBATS.XREG.fitted <- data.frame(TBATS.XREG = c(TBATS.XREG$fit), time = c(time(TBATS.XREG$fitted)))
        TBATS.XREG.mean <-data.frame(TBATS.XREG = c(TBATS.XREG$mean), time = c(time(TBATS.XREG$mean)))

        if(min(TBATS.XREG.mean$TBATS.XREG)<0){
          TBATS.XREG.mean$TBATS.XREG <- 0
        }else{
          TBATS.XREG.mean <- TBATS.XREG.mean
        }

        TBATS.XREG <- rbind(TBATS.XREG.fitted, TBATS.XREG.mean) %>%
          dplyr::select(TBATS.XREG)


      }else{
        TBATS.XREG <- df.NULL$TBATS.XREG
      }
      #merge
      return(as.data.frame(TBATS.XREG))

    })


    #Model Summary
    output$TBATS.XREG.model <- renderPrint({
      TBATS.XREG.model <- TBATS.XREG.model()
      TBATS.XREG.model
    })

    output$TBATS.XREG.accuracy <- renderPrint({

      TBATS.XREG.model <- TBATS.XREG.model()
      ARIMA.XREG.model.2 <- ARIMA.XREG.model.2()
      ARIMA.XREG.input <- ARIMA.XREG.model.2$x


      dat <- dat.value()
      Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

      TBATS.XREG.ts <- ARIMA.XREG.input[1:nrow(ARIMA.XREG.input),]
      TBATS.XREG.ts <- ts(TBATS.XREG.ts, frequency = Freq)

      TBATS.XREG.accuracy <-accuracy(TBATS.XREG.model$fitted, TBATS.XREG.ts)

      return(TBATS.XREG.accuracy)

    })


    # Bass diffusion ########################################################################################



    Bass.Diffusion <- reactive({


      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))
      dm.dat <- rbind(0, dm.dat)
      t <- 1:length(dm.dat)

      #Cumulative data to set start M
      cu.dat <- cumsum(dm.dat)

      #Find Optimal Start MPQ
      #BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2


      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }


      #Bass.Diffision
      possibleError <- tryCatch(
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = NA)
        Bass.Diffusion <- Pred.time$Bass.Diffusion

      }else{
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]


        #When M is smaller than Max data, Bass.Diffusion fails.
        if(max(cu.dat)>M){

          df.NULL <- df.NULL()
          Bass.Diffusion <- df.NULL$Bass.Diffusion
          #source("Source/dm.cu.R", local = TRUE)

        }else{
          Bass.nls <- Bass.nls
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          #bdf <- M * ((P + Q)^2/P) * exp(-(P + Q) * Pred.time)/(1 + (Q/P) * exp(-(P + Q) * Pred.time))^2
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Bass.Diffusion <- cdf + dat$Value[[1]] - cdf[[1]][1]
          Bass.Diffusion <- as.numeric(unlist(Bass.Diffusion))
        }

      }

      return(as.data.frame(Bass.Diffusion))

    })

    #BD Model Summary
    Bass.Diffusion.Summary <- reactive({


      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))
      dm.dat <- rbind(0, dm.dat)
      t <- 1:length(dm.dat)

      #Cumulative data to set start M
      cu.dat <- cumsum(dm.dat)

      #Find Optimal Start MPQ
      #BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2


      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }


      #Bass.Diffision
      possibleError <- tryCatch(
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = NA)
        Bass.Diffusion <- Pred.time$Bass.Diffusion

      }else{
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]


        #When M is smaller than Max data, Bass.Diffusion fails.
        if(max(cu.dat)>M){

          df.NULL <- df.NULL()
          Bass.Diffusion <- df.NULL$Bass.Diffusion
          #source("Source/dm.cu.R", local = TRUE)

        }else{
          Bass.nls <- Bass.nls
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          #bdf <- M * ((P + Q)^2/P) * exp(-(P + Q) * Pred.time)/(1 + (Q/P) * exp(-(P + Q) * Pred.time))^2
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Bass.Diffusion <- cdf + dat$Value[[1]] - cdf[[1]][1]
          Bass.Diffusion <- as.numeric(unlist(Bass.Diffusion))
        }

      }

      overview(Bass.nls)

    })

    output$BD.model <- renderPrint({
      BD.model <- Bass.Diffusion.Summary()
      BD.model
    })




    #BD Accuracy
    Bass.Diffusion.Accuracy <- reactive({



      dat <- dat.value()
      frame.output <- frame.output()
      h <- nrow(frame.output)-nrow(dat)


      #Data processing for dm
      dm.dat <- as.numeric(as.matrix(dat$Value))
      dm.dat <- as.matrix(diff(dm.dat))
      dm.dat <- rbind(0, dm.dat)
      t <- 1:length(dm.dat)

      #Cumulative data to set start M
      cu.dat <- cumsum(dm.dat)

      #Find Optimal Start MPQ
      #BassFormula <- cu.dat ~ M * (1 - (exp(-(P + Q) * t)))/(1 + (Q/P) * (exp(-(P + Q) * t)))
      BassFormula <- dm.dat ~ M * (((P+Q)^2 / P) * exp(-(P+Q) * t) ) /(1+(Q/P)*exp(-(P+Q)*t))^2


      possibleError <- tryCatch(
        #https://www.r-bloggers.com/a-better-nls/ nlsLM can fit zero noise data, when nls fails
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M = max(cu.dat), P= 0.01, Q=0.1)),
        error=function(e) e
      )

      if(inherits(possibleError, "error")){
        mpq.start <- c(M = 0,
                       P = 0,
                       Q = 0)
      }else{
        Bass.nlsLM <- nlsLM(BassFormula, start = list( M=max(cu.dat), P= 0.01, Q=0.1))
        mpq.start <- update(Bass.nlsLM)

        mpq.start <- c(M = summary(mpq.start)$coefficients[1],
                       P = summary(mpq.start)$coefficients[2],
                       Q = summary(mpq.start)$coefficients[3])
      }


      #Bass.Diffision
      possibleError <- tryCatch(
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024)),
        error=function(e) e
      )


      if(inherits(possibleError, "error")){

        Pred.time<-data.frame(t = 1:(length(dm.dat) + h), Bass.Diffusion = NA)
        Bass.Diffusion <- Pred.time$Bass.Diffusion

      }else{
        Bass.nls <- nls(BassFormula, start = mpq.start,  trace=F, control = nls.control(maxiter = 1000, minFactor = 1/1024))
        Bass.nls.parameters=as.matrix(coef(Bass.nls))


        M = Bass.nls.parameters[1,]
        P = Bass.nls.parameters[2,]
        Q = Bass.nls.parameters[3,]


        #When M is smaller than Max data, Bass.Diffusion fails.
        if(max(cu.dat)>M){

          df.NULL <- df.NULL()
          Bass.Diffusion <- df.NULL$Bass.Diffusion
          #source("Source/dm.cu.R", local = TRUE)

        }else{
          Bass.nls <- Bass.nls
          Pred.time<-data.frame(t = 1:(length(dm.dat) + h))
          #bdf <- M * ((P + Q)^2/P) * exp(-(P + Q) * Pred.time)/(1 + (Q/P) * exp(-(P + Q) * Pred.time))^2
          cdf <- (M * (1 - (exp(-(P + Q) * Pred.time)))/(1 + (Q/P) * (exp(-(P + Q) * Pred.time))))

          Bass.Diffusion <- cdf + dat$Value[[1]] - cdf[[1]][1]
          Bass.Diffusion <- as.numeric(unlist(Bass.Diffusion))
        }

      }

      pred = predict(Bass.nls)
      real = as.vector(dm.dat)
      accuracy <- accuracy(real, pred)
      accuracy
    })

    output$BD.accuracy <- renderPrint({
      BD.accuracy <- Bass.Diffusion.Accuracy()
      BD.accuracy
    })



    ###
    #Output
    ###


    df <- reactive({

      dat.value <- dat.value() #Time, Value
      dat.CurrentView <- dat.CurrentView() #Time, CurrentView

      frame <- frame.output()
      df <- setNames(data.frame(matrix(ncol = 15, nrow = nrow(frame))), c("Time","ARIMA", "ARIMA.XREG", "BaggedETS", "BaggedETS.XREG", "BaggedARIMA", "BaggedARIMA.XREG", "TBATS", "TBATS.XREG", "NNAR", "NNAR.XREG", "MLP", "MLP.XREG", "Bass.Diffusion", "Linear.Regession"))


      df$Time <- as.data.frame(frame.output())
      df$ARIMA <- as.data.frame(ARIMA())
      df$ARIMA.XREG <- as.data.frame(ARIMA.XREG())
      df$BaggedETS <- as.data.frame(BaggedETS())
      df$BaggedETS.XREG <- as.data.frame(BaggedETS.XREG())
      df$BaggedARIMA <- as.data.frame(BaggedARIMA())
      df$BaggedARIMA.XREG <- as.data.frame(BaggedARIMA.XREG())
      df$TBATS <- as.data.frame(TBATS())
      df$TBATS.XREG <- as.data.frame(TBATS.XREG())

      df$NNAR <- as.data.frame(NNAR())
      df$NNAR.XREG <- as.data.frame(NNAR.XREG())
      df$MLP <- as.data.frame(MLP())
      df$MLP.XREG <- as.data.frame(MLP.XREG())

      df$Bass.Diffusion <- as.data.frame(Bass.Diffusion())
      df$Linear.Regession <- as.data.frame(Linear.Trend())

      not_all_na <- function(x) any(!is.na(x))

      df <- as.data.frame(df) %>% select_if(not_all_na)

      dat.value <- dat.value %>% dplyr::select(Time, Value)

      df <- as.data.frame(as.list(df)) %>%
        dplyr::left_join(dat.value) %>%
        dplyr::left_join(dat.CurrentView)


      return(df)

    })





    #df datatable
    df.dt <- reactive({
      df <- df()
      df <- df %>%
        select(-Value)

      df.dt <- setNames(as.data.frame(t(df[,-1])), df[,1])
      return(df.dt)

    })

    #Output DataTable
    output$df.dt <- renderDataTable({
      df.dt <- df.dt()

      datatable(round(df.dt, digits =2),
                options = list(searching = FALSE, paging = FALSE))


    })

    output$df.dt_mydownload <-downloadHandler(
      filename = "Data_Output.csv",
      content = function(file){
        write.csv(df.dt(), file)
      })


    ###
    #Cross Validation
    ###


    CV.NULL <- reactive({
      dat <- dat.value()
      Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))
      CV.NULL <- setNames(data.frame(matrix(ncol = 12, nrow = 3)), c("ARIMA", "ARIMA.XREG","BaggedETS", "BaggedETS.XREG","BaggedARIMA","BaggedARIMA.XREG","TBATS", "TBATS.XREG", "NNAR", "NNAR.XREG", "MLP","MLP.XREG"))

    })

    #ARIMA.CV
    ARIMA.CV.on <- reactive({

      ARIMA.CV.on <- input$CV.options.arima

      if(ARIMA.CV.on != 0){
        ARIMA.CV.on <- "ARIMA.CV.on"
      }else{
        ARIMA.CV.on <- "ARIMA.CV.off"
      }

      return(ARIMA.CV.on)

    })


    ARIMA.CV <- reactive({

      ARIMA.CV.on <- ARIMA.CV.on()
      CV.NULL <- CV.NULL()

      if(ARIMA.CV.on()  == "ARIMA.CV.on"){

        ts <- ts.data()
        dat <- dat.value()
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))


        arima.function <- function(x, h){forecast(auto.arima(x, allowmean = FALSE, allowdrift = FALSE), h=h)}
        arima.e <- tsCV(ts, arima.function, h = Freq)

        MAPE <- round(colMeans(abs((arima.e/ts) *100), na.rm = TRUE), 2)
        MAE <- round(colMeans(abs(arima.e), na.rm = TRUE), 2)
        RMSE <- round(sqrt(colMeans((arima.e)^2, na.rm = TRUE)), 2)

        ARIMA <- apply(cbind(MAE, RMSE, MAPE),2,min)


      }else{
        ARIMA <- CV.NULL$ARIMA
      }

      return(ARIMA)
    })


    #ARIMA.XREG.CV
    ARIMA.XREG.CV.on <- reactive({

      ARIMA.XREG.CV.on <- input$CV.options.arima.xreg

      if(ARIMA.XREG.CV.on != 0){
        ARIMA.XREG.CV.on <- "ARIMA.XREG.CV.on"
      }else{
        ARIMA.XREG.CV.on <- "ARIMA.XREG.CV.off"
      }

      return(ARIMA.XREG.CV.on)

    })


    ARIMA.XREG.CV <- reactive({

      ARIMA.XREG.CV.on <- ARIMA.XREG.CV.on()
      CV.NULL <- CV.NULL()

      if(ARIMA.XREG.CV.on()  == "ARIMA.XREG.CV.on"){

        dat <- dat.value()
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        ARIMA.XREG.model.2<- ARIMA.XREG.model.2()
        ARIMA.XREG.input <- ARIMA.XREG.model.2$x
        ARIMA.XREG.ts <- ARIMA.XREG.input[1:nrow(ARIMA.XREG.input),]

        ARIMA.XREG.function <- function(x, h){forecast(auto.arima(x, allowmean = FALSE, allowdrift = FALSE), h=h)}
        ARIMA.XREG.e <- tsCV(ARIMA.XREG.ts, ARIMA.XREG.function, h = Freq)

        MAPE <- round(colMeans(abs((ARIMA.XREG.e/ARIMA.XREG.ts) *100), na.rm = TRUE), 2)
        MAE <- round(colMeans(abs(ARIMA.XREG.e), na.rm = TRUE), 2)
        RMSE <- round(sqrt(colMeans((ARIMA.XREG.e)^2, na.rm = TRUE)), 2)

        ARIMA.XREG <- apply(cbind(MAE, RMSE, MAPE),2,min)


      }else{
        ARIMA.XREG <- CV.NULL$ARIMA.XREG
      }

      return(ARIMA.XREG)
    })



    #NNAR.CV
    NNAR.CV.on <- reactive({

      NNAR.CV.on <- input$CV.options.nnar

      if(NNAR.CV.on != 0){
        NNAR.CV.on <- "NNAR.CV.on"
      }else{
        NNAR.CV.on <- "NNAR.CV.off"
      }

      return(NNAR.CV.on)

    })


    NNAR.CV <- reactive({

      NNAR.CV.on <- NNAR.CV.on()
      CV.NULL <- CV.NULL()

      if(NNAR.CV.on()  == "NNAR.CV.on"){

        ts <- ts.data()
        dat <- dat.value()
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))
        lambda <- lambda()

        set.seed(12345)
        NNAR.function <- function(x, h){forecast(nnetar(x,  lambda = lambda, scale.inputs = TRUE), h=h, bootstrap = TRUE)}
        NNAR.e <- tsCV(ts, NNAR.function, h = Freq)

        MAPE <- round(colMeans(abs((NNAR.e/ts) *100), na.rm = TRUE), 2)
        MAE <- round(colMeans(abs(NNAR.e), na.rm = TRUE), 2)
        RMSE <- round(sqrt(colMeans((NNAR.e)^2, na.rm = TRUE)), 2)

        NNAR <- apply(cbind(MAE, RMSE, MAPE),2,min)


      }else{
        NNAR <- CV.NULL$NNAR
      }

      return(NNAR)
    })



    #NNAR.XREG.on

    NNAR.XREG.CV.on <- reactive({

      NNAR.XREG.CV.on <- input$CV.options.nnar.xreg

      if(NNAR.XREG.CV.on != 0){
        NNAR.XREG.CV.on <- "NNAR.XREG.CV.on"
      }else{
        NNAR.XREG.CV.on <- "NNAR.XREG.CV.off"
      }

      return(NNAR.XREG.CV.on)

    })

    #NNAR.XREG
    NNAR.XREG.CV <- reactive({

      NNAR.XREG.CV.on <- NNAR.XREG.CV.on()
      CV.NULL <- CV.NULL()

      if(NNAR.XREG.CV.on()  == "NNAR.XREG.CV.on"){

        dat <- dat.value()
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))
        lambda <- lambda()

        NNAR.XREG.model.2<- NNAR.XREG.model.2()
        NNAR.XREG.input <- NNAR.XREG.model.2$x
        NNAR.XREG.ts <- NNAR.XREG.input[1:nrow(NNAR.XREG.input),]

        set.seed(12345)
        NNAR.XREG.function <- function(x, h){forecast(nnetar(x,  lambda = lambda, scale.inputs = TRUE), h=h, bootstrap = TRUE)}
        NNAR.XREG.e <- tsCV(NNAR.XREG.ts, NNAR.XREG.function, h = Freq)

        MAPE <- round(colMeans(abs((NNAR.XREG.e/NNAR.XREG.ts) *100), na.rm = TRUE), 2)
        MAE <- round(colMeans(abs(NNAR.XREG.e), na.rm = TRUE), 2)
        RMSE <- round(sqrt(colMeans((NNAR.XREG.e)^2, na.rm = TRUE)), 2)

        NNAR.XREG <- apply(cbind(MAE, RMSE, MAPE),2,min)


      }else{
        NNAR.XREG <- CV.NULL$NNAR.XREG
      }

      return(NNAR.XREG)
    })





    #BaggedETS.CV
    BaggedETS.CV.on <- reactive({

      BaggedETS.CV.on <- input$CV.options.bagged.ets

      if(BaggedETS.CV.on != 0){
        BaggedETS.CV.on <- "BaggedETS.CV.on"
      }else{
        BaggedETS.CV.on <- "BaggedETS.CV.off"
      }

      return(BaggedETS.CV.on)

    })


    BaggedETS.CV <- reactive({

      BaggedETS.CV.on <- BaggedETS.CV.on()
      CV.NULL <- CV.NULL()

      if(BaggedETS.CV.on()  == "BaggedETS.CV.on"){

        ts <- ts.data()
        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        set.seed(12345)
        BaggedETS.function <- function(x, h){forecast(baggedModel(x, bootstrapped_series = bld.mbb.bootstrap(x, nrow(dat)-Freq*2 +1, block_size = Freq*2), fn = ets), h=h)}
        BaggedETS.e <- tsCV(ts, BaggedETS.function, h = Freq)

        MAPE <- round(colMeans(abs((BaggedETS.e/ts) *100), na.rm = TRUE), 2)
        MAE <- round(colMeans(abs(BaggedETS.e), na.rm = TRUE), 2)
        RMSE <- round(sqrt(colMeans((BaggedETS.e)^2, na.rm = TRUE)), 2)

        BaggedETS <- apply(cbind(MAE, RMSE, MAPE),2,min)


      }else{
        BaggedETS <- CV.NULL$BaggedETS
      }

      return(BaggedETS)
    })



    #BaggedETS.XREG.CV
    BaggedETS.XREG.CV.on <- reactive({

      BaggedETS.XREG.CV.on <- input$CV.options.bagged.ets.xreg

      if(BaggedETS.XREG.CV.on != 0){
        BaggedETS.XREG.CV.on <- "BaggedETS.XREG.CV.on"
      }else{
        BaggedETS.XREG.CV.on <- "BaggedETS.XREG.CV.off"
      }

      return(BaggedETS.XREG.CV.on)

    })


    BaggedETS.XREG.CV <- reactive({

      BaggedETS.XREG.CV.on <- BaggedETS.XREG.CV.on()
      CV.NULL <- CV.NULL()

      if(BaggedETS.XREG.CV.on()  == "BaggedETS.XREG.CV.on"){

        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        ARIMA.XREG.model.2<- ARIMA.XREG.model.2()
        ARIMA.XREG.input <- ARIMA.XREG.model.2$x
        BaggedETS.XREG.ts <- ARIMA.XREG.input[1:nrow(ARIMA.XREG.input),]

        set.seed(12345)
        BaggedETS.XREG.function <- function(x, h){forecast(baggedModel(x, bootstrapped_series = bld.mbb.bootstrap(x, nrow(dat)-Freq*2 +1, block_size = Freq*2), fn = ets), h=h)}
        BaggedETS.XREG.e <- tsCV(BaggedETS.XREG.ts, BaggedETS.XREG.function, h = Freq)

        MAPE <- round(colMeans(abs((BaggedETS.XREG.e/BaggedETS.XREG.ts) *100), na.rm = TRUE), 2)
        MAE <- round(colMeans(abs(BaggedETS.XREG.e), na.rm = TRUE), 2)
        RMSE <- round(sqrt(colMeans((BaggedETS.XREG.e)^2, na.rm = TRUE)), 2)

        BaggedETS.XREG <- apply(cbind(MAE, RMSE, MAPE),2,min)


      }else{
        BaggedETS.XREG <- CV.NULL$BaggedETS.XREG
      }

      return(BaggedETS.XREG)
    })



    #BaggedARIMA.CV
    BaggedARIMA.CV.on <- reactive({

      BaggedARIMA.CV.on <- input$CV.options.bagged.arima

      if(BaggedARIMA.CV.on != 0){
        BaggedARIMA.CV.on <- "BaggedARIMA.CV.on"
      }else{
        BaggedARIMA.CV.on <- "BaggedARIMA.CV.off"
      }

      return(BaggedARIMA.CV.on)

    })


    BaggedARIMA.CV <- reactive({

      BaggedARIMA.CV.on <- BaggedARIMA.CV.on()
      CV.NULL <- CV.NULL()

      if(BaggedARIMA.CV.on()  == "BaggedARIMA.CV.on"){

        ts <- ts.data()
        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        set.seed(12345)
        BaggedARIMA.function <- function(x, h){forecast(baggedModel(x, bootstrapped_series = bld.mbb.bootstrap(x, nrow(dat)-Freq*2 +1, block_size = Freq*2), fn = auto.arima), h=h)}
        BaggedARIMA.e <- tsCV(ts, BaggedARIMA.function, h = Freq)

        MAPE <- round(colMeans(abs((BaggedARIMA.e/ts) *100), na.rm = TRUE), 2)
        MAE <- round(colMeans(abs(BaggedARIMA.e), na.rm = TRUE), 2)
        RMSE <- round(sqrt(colMeans((BaggedARIMA.e)^2, na.rm = TRUE)), 2)

        BaggedARIMA <- apply(cbind(MAE, RMSE, MAPE),2,min)


      }else{
        BaggedARIMA <- CV.NULL$BaggedARIMA
      }

      return(BaggedARIMA)
    })

    #BaggedARIMA.XREG.CV
    BaggedARIMA.XREG.CV.on <- reactive({

      BaggedARIMA.XREG.CV.on <- input$CV.options.bagged.arima.xreg

      if(BaggedARIMA.XREG.CV.on != 0){
        BaggedARIMA.XREG.CV.on <- "BaggedARIMA.XREG.CV.on"
      }else{
        BaggedARIMA.XREG.CV.on <- "BaggedARIMA.XREG.CV.off"
      }

      return(BaggedARIMA.XREG.CV.on)

    })


    BaggedARIMA.XREG.CV <- reactive({

      BaggedARIMA.XREG.CV.on <- BaggedARIMA.XREG.CV.on()
      CV.NULL <- CV.NULL()

      if(BaggedARIMA.XREG.CV.on()  == "BaggedARIMA.XREG.CV.on"){

        dat <- dat.value()
        frame <- frame.output()
        h <- nrow(frame)-nrow(dat)
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        ARIMA.XREG.model.2<- ARIMA.XREG.model.2()
        ARIMA.XREG.input <- ARIMA.XREG.model.2$x
        BaggedARIMA.XREG.ts <- ARIMA.XREG.input[1:nrow(ARIMA.XREG.input),]

        set.seed(12345)
        BaggedARIMA.XREG.function <- function(x, h){forecast(baggedModel(x, bootstrapped_series = bld.mbb.bootstrap(x, nrow(dat)-Freq*2 +1, block_size = Freq*2), fn = auto.arima), h=h)}
        BaggedARIMA.XREG.e <- tsCV(BaggedARIMA.XREG.ts, BaggedARIMA.XREG.function, h = Freq)

        MAPE <- round(colMeans(abs((BaggedARIMA.XREG.e/BaggedARIMA.XREG.ts) *100), na.rm = TRUE), 2)
        MAE <- round(colMeans(abs(BaggedARIMA.XREG.e), na.rm = TRUE), 2)
        RMSE <- round(sqrt(colMeans((BaggedARIMA.XREG.e)^2, na.rm = TRUE)), 2)

        BaggedARIMA.XREG <- apply(cbind(MAE, RMSE, MAPE),2,min)


      }else{
        BaggedARIMA.XREG <- CV.NULL$BaggedARIMA.XREG
      }

      return(BaggedARIMA.XREG)
    })




    #TBATS.CV
    TBATS.CV.on <- reactive({

      TBATS.CV.on <- input$CV.options.tbats

      if(TBATS.CV.on != 0){
        TBATS.CV.on <- "TBATS.CV.on"
      }else{
        TBATS.CV.on <- "TBATS.CV.off"
      }

      return(TBATS.CV.on)

    })


    TBATS.CV <- reactive({

      TBATS.CV.on <- TBATS.CV.on()
      CV.NULL <- CV.NULL()

      if(TBATS.CV.on()  == "TBATS.CV.on"){

        ts <- ts.data()
        dat <- dat.value()
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))


        TBATS.function <- function(x, h){forecast(tbats(x), h=h)}
        TBATS.e <- tsCV(ts, TBATS.function, h = Freq)

        MAPE <- round(colMeans(abs((TBATS.e/ts) *100), na.rm = TRUE), 2)
        MAE <- round(colMeans(abs(TBATS.e), na.rm = TRUE), 2)
        RMSE <- round(sqrt(colMeans((TBATS.e)^2, na.rm = TRUE)), 2)

        TBATS <- apply(cbind(MAE, RMSE, MAPE),2,min)


      }else{
        TBATS <- CV.NULL$TBATS
      }

      return(TBATS)
    })


    #TBATS.XREG.CV
    TBATS.XREG.CV.on <- reactive({

      TBATS.XREG.CV.on <- input$CV.options.tbats.xreg

      if(TBATS.XREG.CV.on != 0){
        TBATS.XREG.CV.on <- "TBATS.XREG.CV.on"
      }else{
        TBATS.XREG.CV.on <- "TBATS.XREG.CV.off"
      }

      return(TBATS.XREG.CV.on)

    })


    TBATS.XREG.CV <- reactive({

      TBATS.XREG.CV.on <- TBATS.XREG.CV.on()
      CV.NULL <- CV.NULL()

      if(TBATS.XREG.CV.on()  == "TBATS.XREG.CV.on"){

        dat <- dat.value()
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

        ARIMA.XREG.model.2<- ARIMA.XREG.model.2()
        ARIMA.XREG.input <- ARIMA.XREG.model.2$x
        TBATS.XREG.ts <- ARIMA.XREG.input[1:nrow(ARIMA.XREG.input),]

        TBATS.XREG.function <- function(x, h){forecast(tbats(x), h=h)}
        TBATS.XREG.e <- tsCV(TBATS.XREG.ts, TBATS.XREG.function, h = Freq)

        MAPE <- round(colMeans(abs((TBATS.XREG.e/TBATS.XREG.ts) *100), na.rm = TRUE), 2)
        MAE <- round(colMeans(abs(TBATS.XREG.e), na.rm = TRUE), 2)
        RMSE <- round(sqrt(colMeans((TBATS.XREG.e)^2, na.rm = TRUE)), 2)

        TBATS.XREG <- apply(cbind(MAE, RMSE, MAPE),2,min)


      }else{
        TBATS.XREG <- CV.NULL$TBATS.XREG
      }

      return(TBATS.XREG)
    })


    #MLP.CV
    MLP.CV.on <- reactive({

      MLP.CV.on <- input$CV.options.mlp

      if(MLP.CV.on != 0){
        MLP.CV.on <- "MLP.CV.on"
      }else{
        MLP.CV.on <- "MLP.CV.off"
      }

      return(MLP.CV.on)

    })



    MLP.CV <- reactive({

      MLP.CV.on <- MLP.CV.on()
      CV.NULL <- CV.NULL()

      if(MLP.CV.on()  == "MLP.CV.on"){

        ts <- ts.data()
        dat <- dat.value()
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))


        MLP.function <- function(x, h){forecast(mlp(x, hd.auto.type = "valid"), h=h)}
        MLP.e <- tsCV(ts, MLP.function, h = Freq)

        MAPE <- round(colMeans(abs((MLP.e/ts) *100), na.rm = TRUE), 2)
        MAE <- round(colMeans(abs(MLP.e), na.rm = TRUE), 2)
        RMSE <- round(sqrt(colMeans((MLP.e)^2, na.rm = TRUE)), 2)

        MLP <- apply(cbind(MAE, RMSE, MAPE),2,min)


      }else{
        MLP <- CV.NULL$MLP
      }

      return(MLP)
    })

    #MLP.CV
    MLP.XREG.CV.on <- reactive({

      MLP.XREG.CV.on <- input$CV.options.mlp.xreg

      if(MLP.XREG.CV.on != 0){
        MLP.XREG.CV.on <- "MLP.XREG.CV.on"
      }else{
        MLP.XREG.CV.on <- "MLP.XREG.CV.off"
      }

      return(MLP.XREG.CV.on)

    })

    MLP.XREG.CV <- reactive({

      MLP.XREG.CV.on <- MLP.XREG.CV.on()
      CV.NULL <- CV.NULL()

      if(MLP.XREG.CV.on()  == "MLP.XREG.CV.on"){

        ts <- ts.data()
        dat <- dat.value()
        Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))


        MLP.XREG.function <- function(x, h){forecast(mlp(x,  hd.auto.type = "valid"), h=h)}
        MLP.XREG.e <- tsCV(ts, MLP.XREG.function, h = Freq)

        MAPE <- round(colMeans(abs((MLP.XREG.e/ts) *100), na.rm = TRUE), 2)
        MAE <- round(colMeans(abs(MLP.XREG.e), na.rm = TRUE), 2)
        RMSE <- round(sqrt(colMeans((MLP.XREG.e)^2, na.rm = TRUE)), 2)

        MLP.XREG <- apply(cbind(MAE, RMSE, MAPE),2,min)


      }else{
        MLP.XREG <- CV.NULL$MLP.XREG
      }

      return(MLP.XREG)
    })


    ts.cv <- reactive({

      dat <- dat.value()
      Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))
      ts.cv <- setNames(data.frame(matrix(ncol = 12, nrow = 3)), c("ARIMA", "ARIMA.XREG", "BaggedETS","BaggedETS.XREG","BaggedARIMA","BaggedARIMA.XREG", "TBATS", "TBATS.XREG", "NNAR", "NNAR.XREG","MLP", "MLP.XREG"))


      ts.cv$ARIMA <- as.data.frame(ARIMA.CV())
      ts.cv$ARIMA.XREG <- as.data.frame(ARIMA.XREG.CV())
      ts.cv$MLP.XREG <- as.data.frame(MLP.XREG.CV())
      ts.cv$BaggedETS <- as.data.frame(BaggedETS.CV())
      ts.cv$BaggedETS.XREG <- as.data.frame(BaggedETS.XREG.CV())
      ts.cv$BaggedARIMA <- as.data.frame(BaggedARIMA.CV())
      ts.cv$BaggedARIMA.XREG <- as.data.frame(BaggedARIMA.XREG.CV())
      ts.cv$TBATS <- as.data.frame(TBATS.CV())
      ts.cv$TBATS.XREG <- as.data.frame(TBATS.XREG.CV())
      ts.cv$NNAR <- as.data.frame(NNAR.CV())
      ts.cv$NNAR.XREG <- as.data.frame(NNAR.XREG.CV())
      ts.cv$MLP <- as.data.frame(MLP.CV())


      not_all_na <- function(x) any(!is.na(x))

      ts.cv <- as.data.frame(ts.cv) %>% select_if(not_all_na)
      ts.cv.names <- names(ts.cv)

      ts.cv <- as.data.frame(as.list(ts.cv))

      ts.cv <- data.frame(t(ts.cv))
      rownames(ts.cv) <- ts.cv.names

      return(ts.cv)

    })

    output$ts.cv <- DT::renderDataTable({
      ts.cv <- ts.cv()

      formattable(ts.cv, list(
        MAE = formatter("span", style = ~ style(color = ifelse(MAE == min(MAE), "blue", NA))),
        RMSE =  formatter("span", style = ~ style(color = ifelse(RMSE == min(RMSE), "blue", NA))),
        MAPE =  formatter("span", style = ~ style(color = ifelse(MAPE == min(MAPE), "blue", NA)))
      )) %>%
        as.datatable(.,
                     options = list(searching = FALSE, paging = FALSE), rownames = TRUE)

    })




    #Output - Best.cv
    Best.cv <-reactive({


      ts.cv <- ts.cv()

      Best.MAE <- row.names(ts.cv)[(which(ts.cv$MAE==min(ts.cv$MAE)))]
      Best.RMSE <- row.names(ts.cv)[(which(ts.cv$RMSE==min(ts.cv$RMSE)))]
      Best.MAPE <- row.names(ts.cv)[(which(ts.cv$MAPE==min(ts.cv$MAPE)))]

      Best.cv <- rbind(Best.MAE, Best.RMSE, Best.MAPE)
      Best.cv <- unique(Best.cv[,1])

      return(Best.cv)

    })

    output$Best.cv <- renderText({
      Best.cv <- Best.cv()
    })



    ###
    #Accuracy Table
    ###



    Accuracy <-reactive({

      df <-df()
      dat.value <- dat.value()

      df.Measure <- df[1:(nrow(dat.value)+1),]
      df.Measure <- df.Measure %>% dplyr::select(-Time, -Value.incl.Forecast)
      df.Pred <- df.Measure %>% dplyr::select(-Value) %>% as.list()
      df.Value <- df.Measure$Value

      ARIMA.XREG.model.2<- ARIMA.XREG.model.2()
      ARIMA.XREG.input <- ARIMA.XREG.model.2$x

      df.Value.ARIMA.XREG <- as.data.frame(ARIMA.XREG.input)
      df.Value.ARIMA.XREG <- df.Value.ARIMA.XREG[,1]


      NNAR.XREG.model.on <- NNAR.XREG.model.on()

      if(NNAR.XREG.model.on()  == "NNAR.XREG.model.on"){

      NNAR.XREG.model.2<- NNAR.XREG.model.2()
      NNAR.XREG.input <- NNAR.XREG.model.2$x

      df.Value.NNAR.XREG <- as.data.frame(NNAR.XREG.input)
      df.Value.NNAR.XREG <- df.Value.NNAR.XREG[,1]

      }


      df.Names <- names(df.Pred)

      # Error
      Error <- NULL

      for(i in 1:length(df.Pred)) {

        if(grepl("XREG", names(df.Pred[i])) == TRUE){
          Error[[i]] <- as.numeric(unlist(df.Pred[i]))-as.numeric(df.Value.ARIMA.XREG)
        }else if(grepl("NNAR.XREG", names(df.Pred[i])) == TRUE){
          Error[[i]] <- as.numeric(unlist(df.Pred[i]))-as.numeric(df.Value.NNAR.XREG)
        }else{
          Error[[i]] <- as.numeric(unlist(df.Pred[i]))-as.numeric(df.Value)
        }

      }

      # MAE/RMSE
      mae <- function(error) { mean(abs(error), na.rm = TRUE) }
      rmse <- function(error) { sqrt(mean(error^2, na.rm = TRUE)) }

      MAE <- sapply(Error, mae)
      RMSE <- sapply(Error, rmse)

      # MAPE
      MAPE <- NULL

      for(i in 1:length(df.Pred)) {
        if(grepl("XREG", names(df.Pred[i])) == TRUE){
          MAPE[[i]] <- mean(abs(((as.numeric(unlist(df.Pred[i])-as.numeric(df.Value.ARIMA.XREG)))/df.Value.ARIMA.XREG)*100), na.rm = TRUE)
        }else if(grepl("NNAR.XREG", names(df.Pred[i])) == TRUE){
          MAPE[[i]] <- mean(abs(((as.numeric(unlist(df.Pred[i])-as.numeric(df.Value.ARIMA.XREG)))/df.Value.NNAR.XREG)*100), na.rm = TRUE)
        }else{
          MAPE[[i]] <- mean(abs(((as.numeric(unlist(df.Pred[i])-as.numeric(df.Value)))/df.Value)*100), na.rm = TRUE)
        }

      }

      # Accuracy
      Accuracy <- data.frame(t(rbind(MAE, RMSE, MAPE)))
      row.names(Accuracy) <- df.Names

      return(Accuracy)

    })



    output$Accuracy <- renderDataTable({

      Accuracy <- Accuracy()
      Accuracy <- round(Accuracy, 2)


      formattable(Accuracy, list(
        MAE = formatter("span", style = ~ style(color = ifelse(MAE == min(MAE), "blue", NA))),
        RMSE =  formatter("span", style = ~ style(color = ifelse(RMSE == min(RMSE), "blue", NA))),
        MAPE =  formatter("span", style = ~ style(color = ifelse(MAPE == min(MAPE), "blue", NA)))
      )) %>%
        as.datatable(.,
                     options = list(searching = FALSE, paging = FALSE), rownames = TRUE)
    })


    #Output - Best.Accuracy
    Best.Accuracy <-reactive({
      Accuracy <- Accuracy()
      Accuracy

      Best.MAE <- row.names(Accuracy)[(which(Accuracy$MAE==min(Accuracy$MAE)))]
      Best.RMSE <- row.names(Accuracy)[(which(Accuracy$RMSE==min(Accuracy$RMSE)))]
      Best.MAPE <- row.names(Accuracy)[(which(Accuracy$MAPE==min(Accuracy$MAPE)))]

      Best.Accuracy <- rbind(Best.MAE, Best.RMSE, Best.MAPE)
      Best.Accuracy <- unique(Best.Accuracy[,1])

      return(Best.Accuracy)

    })



    output$Best.Accuracy <- renderText({
      Best.Accuracy <- Best.Accuracy()
    })



    ###
    #Plotly
    ###


    #plotly select default
    plotly.defaults <- reactiveValues(part = "Level")

    #plotly Output
    output$plotly <-renderPlotly({

      dat <- dat.value()
      df <- df()
      df$Time <- as.character(df$Time)

      Freq <- max(as.numeric(gsub(".*_", "", dat$Time)))

      #Color Select
      tableau10 <- c('#4e79a7', '#f28e2b', '#e15759', '#76b7b2', '#59a14f', '#edc948', '#b07aa1', '#ff9da7', '#9c775f', '#bab0ac')
      manual6 <- c('#1f78b4', '#919EAD','#ffc425', '#00b159','#00aedb','brown')


      #Customized for Quarterly data display
      #Quarterly data
      df$Time <- str_replace_all(df$Time, "_", "Q")

      #Auto plotly
      plotly.df <- df %>% group_by(Time) %>% gather(key = model, value = value, -Time) %>% as.data.frame()
      plotly <- plot_ly(plotly.df, x=~Time, y = ~value, color = ~model, type = 'scatter', mode = 'lines',
                        colors = tableau10)


      ##Chart Options
      #QOQ YOY Growth rate functions
      QoQ_growth_rate <- function(x)(x/lag(x)-1)*100
      YoY_growth_rate <-function (x,periodsPerYear){
        if(NROW(x)<=periodsPerYear){
          stop("too few rows")
        }
        else{
          indexes<-1:(NROW(x)-periodsPerYear)
          return(c(rep(NA,periodsPerYear),((x[indexes+periodsPerYear]-x[indexes])/x[indexes])*100))
        }
      }

      plotly.df$QoQ <- round(QoQ_growth_rate(plotly.df$value),2)
      plotly.df$YoY <- round(YoY_growth_rate(plotly.df$value, 4),2)


      ##Plotly graph select
      #Auto select

      if (input$Chart.options =="Level"){
        plotly.defaults$part = "Level"

        plotly <- plot_ly(plotly.df, x=~Time, y = ~value, color = ~model, type = 'scatter', mode = 'lines',
                          colors = tableau10)

      } else if (input$Chart.options =="QoQ"){
        plotly.defaults$part = "QoQ"

        plotly <- plot_ly(plotly.df, x=~Time, y = ~YoY, color = ~model, type = 'scatter', mode = 'lines',
                          colors = tableau10)
      } else {
        plotly.defaults$part = "YoY"

        plotly <- plot_ly(plotly.df, x=~Time, y = ~QoQ, color = ~model, type = 'scatter', mode = 'lines',
                          colors = tableau10)
      }


      ##Manual select


      if (input$Chart.options =="Level"){
        plotly.defaults$part = "Level"

        annot.level <- df %>% select(Time, Value) %>% na.omit(.) %>% tail(1)

        annot.level <- list(
          x = annot.level$Time ,
          y = annot.level$Value,
          text = paste('Last Actual: ',annot.level$Time, sep = ""),
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = 20,
          ay = -40
        )


       #plotly level
        plotly <- plot_ly(df, x=~Time) %>%
          add_trace(y= ~Value, name = 'IPR', type = 'scatter', mode = 'lines+markers', line = list(color = '#1f78b4'), marker = list(color = '#1f78b4', width = 3))%>%
          add_trace(y= ~Value.incl.Forecast, name = 'IPR.incl.Forecast', type = 'scatter', mode = 'lines+markers', line = list(color = '#1f78b4' ), marker = list(color = '#1f78b4', width = 3))%>%
          add_trace(y = ~ARIMA, name = 'ARIMA', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'brown'), marker = list(color = 'brown', width = 3)) %>%
          add_trace(y = ~ARIMA.XREG, name = 'ARIMA.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'brown'), marker = list(color = 'brown', width = 3)) %>%
          add_trace(y = ~BaggedETS, name = 'BaggedETS', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#00aedb'), marker = list(color = '#00aedb', width = 3)) %>%
          add_trace(y = ~BaggedETS.XREG, name = 'BaggedETS.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#00aedb'), marker = list(color = '#00aedb', width = 3)) %>%
          add_trace(y = ~BaggedARIMA, name = 'BaggedARIMA', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'purple'), marker = list(color = 'purple', width = 3)) %>%
          add_trace(y = ~BaggedARIMA.XREG, name = 'BaggedARIMA.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'purple'), marker = list(color = 'purple', width = 3)) %>%
          add_trace(y = ~TBATS, name = 'TBATS', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'grey'), marker = list(color = 'grey', width = 3)) %>%
          add_trace(y = ~TBATS.XREG, name = 'TBATS.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'grey'), marker = list(color = 'grey', width = 3)) %>%
          add_trace(y = ~NNAR, name = 'NNAR', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#00b159'), marker = list(color = '#00b159', width = 3)) %>%
          add_trace(y = ~NNAR.XREG, name = 'NNAR.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#00b159'), marker = list(color = '#00b159', width = 3)) %>%
          add_trace(y = ~MLP, name = 'MLP', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'orange'), marker = list(color = 'orange', width = 3)) %>%
          add_trace(y = ~MLP.XREG, name = 'MLP.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'orange'), marker = list(color = 'orange', width = 3)) %>%
          add_trace(y = ~Bass.Diffusion, name = 'Bass Diffusion', type = 'scatter', mode = 'lines+markers', line = list(color = '#ffc425'), marker = list(color = '#ffc425', width = 3))%>%
          add_trace(y = ~Linear.Trend, name = 'Linear Trend', type = 'scatter', mode = 'lines', width = 2, line = list(dash = 3, color = '#919EAD'))%>%
          layout(title = "",
                 xaxis = list(title = ""), yaxis = list (title = ""), annotations = annot.level)

      } else if (input$Chart.options =="QoQ"){
        plotly.defaults$part = "QoQ"


        plotly.df.qoq <- plotly.df %>% select(Time, model, QoQ) %>% group_by(Time, model) %>% spread(model, -Time) %>% as.data.frame() %>% slice(2:n())

        annot.qoq <- plotly.df.qoq %>% select(Time, Value) %>% na.omit(.) %>% tail(1)

        annot.qoq <- list(
          x = annot.qoq$Time ,
          y = annot.qoq$Value,
          text = paste('Last Actual: ',annot.qoq$Time, sep = ""),
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = 20,
          ay = -40
        )

        #plotly Q/Q
        plotly <- plot_ly(plotly.df.qoq, x=~Time) %>%
          add_trace(y= ~Value, name = 'IPR', type = 'scatter', mode = 'lines+markers', line = list(color = '#1f78b4'), marker = list(color = '#1f78b4', width = 3))%>%
          add_trace(y= ~Value.incl.Forecast, name = 'IPR.incl.Forecast', type = 'scatter', mode = 'lines+markers', line = list(color = '#1f78b4' ), marker = list(color = '#1f78b4', width = 3))%>%
          add_trace(y = ~ARIMA, name = 'ARIMA', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'brown'), marker = list(color = 'brown', width = 3)) %>%
          add_trace(y = ~ARIMA.XREG, name = 'ARIMA.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'brown'), marker = list(color = 'brown', width = 3)) %>%
          add_trace(y = ~BaggedETS, name = 'BaggedETS', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#00aedb'), marker = list(color = '#00aedb', width = 3)) %>%
          add_trace(y = ~BaggedETS.XREG, name = 'BaggedETS.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#00aedb'), marker = list(color = '#00aedb', width = 3)) %>%
          add_trace(y = ~BaggedARIMA, name = 'BaggedARIMA', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'purple'), marker = list(color = 'purple', width = 3)) %>%
          add_trace(y = ~BaggedARIMA.XREG, name = 'BaggedARIMA.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'purple'), marker = list(color = 'purple', width = 3)) %>%
          add_trace(y = ~TBATS, name = 'TBATS', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'grey'), marker = list(color = 'grey', width = 3)) %>%
          add_trace(y = ~TBATS.XREG, name = 'TBATS.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'grey'), marker = list(color = 'grey', width = 3)) %>%
          add_trace(y = ~NNAR, name = 'NNAR', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#00b159'), marker = list(color = '#00b159', width = 3)) %>%
          add_trace(y = ~NNAR.XREG, name = 'NNAR.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#00b159'), marker = list(color = '#00b159', width = 3)) %>%
          add_trace(y = ~MLP, name = 'MLP', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'orange'), marker = list(color = 'orange', width = 3)) %>%
          add_trace(y = ~MLP.XREG, name = 'MLP.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'orange'), marker = list(color = 'orange', width = 3)) %>%
          add_trace(y = ~Bass.Diffusion, name = 'Bass Diffusion', type = 'scatter', mode = 'lines+markers', line = list(color = '#ffc425'), marker = list(color = '#ffc425', width = 3))%>%
          add_trace(y = ~Linear.Trend, name = 'Linear Trend', type = 'scatter', mode = 'lines', width = 2, line = list(dash = 3, color = '#919EAD'))%>%
          layout(title = "",
                 xaxis = list(title = ""), yaxis = list(title = "", ticksuffix = '%'), annotations = annot.qoq)

      } else {
        plotly.defaults$part = "YoY"


        plotly.df.yoy <- plotly.df %>% select(Time, model, YoY) %>% group_by(Time, model) %>% spread(model, -Time) %>% as.data.frame() %>% slice((Freq + 1):n())

        annot.yoy <- plotly.df.yoy %>% select(Time, Value) %>% na.omit(.) %>% tail(1)

        annot.yoy<- list(
          x = annot.yoy$Time ,
          y = annot.yoy$Value,
          text = paste('Last Actual:',annot.yoy$Time, sep = ""),
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = 20,
          ay = -40
        )

        #plotly Y/Y
        plotly <- plot_ly(plotly.df.yoy, x=~Time) %>%
          add_trace(y= ~Value, name = 'IPR', type = 'scatter', mode = 'lines+markers', line = list(color = '#1f78b4'), marker = list(color = '#1f78b4', width = 3))%>%
          add_trace(y= ~Value.incl.Forecast, name = 'IPR.incl.Forecast', type = 'scatter', mode = 'lines+markers', line = list(color = '#1f78b4' ), marker = list(color = '#1f78b4', width = 3))%>%
          add_trace(y = ~ARIMA, name = 'ARIMA', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'brown'), marker = list(color = 'brown', width = 3)) %>%
          add_trace(y = ~ARIMA.XREG, name = 'ARIMA.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'brown'), marker = list(color = 'brown', width = 3)) %>%
          add_trace(y = ~BaggedETS, name = 'BaggedETS', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#00aedb'), marker = list(color = '#00aedb', width = 3)) %>%
          add_trace(y = ~BaggedETS.XREG, name = 'BaggedETS.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#00aedb'), marker = list(color = '#00aedb', width = 3)) %>%
          add_trace(y = ~BaggedARIMA, name = 'BaggedARIMA', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'purple'), marker = list(color = 'purple', width = 3)) %>%
          add_trace(y = ~BaggedARIMA.XREG, name = 'BaggedARIMA.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'purple'), marker = list(color = 'purple', width = 3)) %>%
          add_trace(y = ~TBATS, name = 'TBATS', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'grey'), marker = list(color = 'grey', width = 3)) %>%
          add_trace(y = ~TBATS.XREG, name = 'TBATS.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'grey'), marker = list(color = 'grey', width = 3)) %>%
          add_trace(y = ~NNAR, name = 'NNAR', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#00b159'), marker = list(color = '#00b159', width = 3)) %>%
          add_trace(y = ~NNAR.XREG, name = 'NNAR.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = '#00b159'), marker = list(color = '#00b159', width = 3)) %>%
          add_trace(y = ~MLP, name = 'MLP', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'orange'), marker = list(color = 'orange', width = 3)) %>%
          add_trace(y = ~MLP.XREG, name = 'MLP.XREG', type = 'scatter', mode = 'lines+markers', width = 3, line = list(color = 'orange'), marker = list(color = 'orange', width = 3)) %>%
          add_trace(y = ~Bass.Diffusion, name = 'Bass Diffusion', type = 'scatter', mode = 'lines+markers', line = list(color = '#ffc425'), marker = list(color = '#ffc425', width = 3))%>%
          add_trace(y = ~Linear.Trend, name = 'Linear Trend', type = 'scatter', mode = 'lines', width = 2, line = list(dash = 3, color = '#919EAD'))%>%
          layout(title = "",
                 xaxis = list(title = ""), yaxis = list (title = "", ticksuffix = '%'), annotations = annot.yoy)
      }

      plotly

    })
  }
  runGadget(ui, server, viewer = browserViewer())

                                   }







