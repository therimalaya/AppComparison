library(shiny)
library(shinythemes)
library(tidyverse)
library(shinydashboard)
library(DT)

## Source Function ----
source("00-function.r", local = TRUE)

## Prepare Data ----
## Load Design ----
load("design.rdata")
pred_error <- readRDS("prediction-error.rds")
est_error <- readRDS("estimation-error.rds")

## Prediction Error Data ----
pred_data <- design_chr %>%
  mutate(Design = as.character(1:n())) %>%
  mutate_at(vars(p, gamma, R2, eta), as.factor) %>%
  right_join(pred_error, by = "Design") %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("p", as.factor) %>%
  mutate(Response = paste0("Y", Response))

pred_data_minimum <- pred_data %>%
  group_by(p, gamma, eta, R2, Replication, Method, Response) %>%
  summarize(Pred_Error = min(Pred_Error)) %>%
  spread(Response, Pred_Error)

## Estimation Error Data ----
est_data <- design_chr %>%
  mutate(Design = as.character(1:n())) %>%
  mutate_at(vars(p, gamma, eta, R2), as.factor) %>%
  right_join(est_error, by = "Design") %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("p", as.factor) %>%
  mutate(Response = paste0("Y", Response))

est_data_minimum <- est_data %>%
  group_by(p, gamma, eta, R2, Replication, Method, Response) %>%
  summarize(Est_Error = min(Est_Error)) %>%
  spread(Response, Est_Error)

## Some Functions ----
get_coef_error <- function(dgn, rep, mthd, mthd_idx) {
  dgn_chr <- formatC(dgn, digits = 0, width = 2, format = "f", flag = "0")
  mthd_chr <- formatC(mthd_idx, digits = 0, width = 2, format = "f", flag = "0")
  rep_chr <- formatC(rep, digits = 0, width = 2, format = "f", flag = "0")
  seed <- as.numeric(paste0(dgn_chr, mthd_chr, rep_chr))
  use_pc <- mthd %in% c("Xenv", "Yenv", "Senv")
  set.seed(seed)
  design %>%
    get_design(dgn) %>%
    simulate() %>%
    coef_errors(
      est_method = mthd, need_pc = use_pc,
      prop = 0.975, ncomp = 10
    )
}

## Some Constants ----
data_path <- "coef-error"

## For Estimator Selection ----
methods <- c(
  'Principal Component Regression (PCR)',
  'Partial Least Squares 1 (PLS1)',
  'Partial Least Squares 2 (PLS2)',
  'Envelope in predictor (Xenv)',
  'Simulteneous Envelope (Senv)',
  'Ridge Regression (Ridge)',
  'Lasso Regression (Lasso)'
)
names(mthds) <- methods

## ---- User Interface Start ----
ui <- dashboardPage(
  title = "Comparison of Models and Estimation Methods",
  ## Header ----
  header = dashboardHeader(
    titleWidth = 400,
    title = "Model Comparison"
  ),
  ## Sidebar----
  sidebar = dashboardSidebar(
    width = 400,
    sidebarMenu(
      ## Explore Menu ----
      menuItem(
        "Explore",
        startExpanded = TRUE,
        icon = icon("dashboard"),
        fluidPage(
          theme = shinytheme("yeti"),
          h3("Design Table"),
          dataTableOutput("design"),
          conditionalPanel(
            condition = 'output.has_selected',
            h3("Selected Design"),
            dataTableOutput("selected_design"),
            fluidRow(
              fillRow(
                textInput("rep", "Number of Replication", "1:2", width = '100%'),
                checkboxGroupInput(
                  inputId = "which_plot",
                  label = "Which Plot:",
                  choices = c("Prediction Error" = "pred",
                              "Estimation Error" = "est",
                              "Coefficients" = "coef"),
                  selected = c('pred', 'est')),
                height = '80px'
              )
            ),
            fluidRow(
              selectInput(
                inputId = "method",
                label = "Estimation Method",
                choices = mthds,
                multiple = TRUE,
                width = '100%'),
              actionButton("compare", "Compare", icon('random'))
            )
          )
        ),
        menuSubItem(
          "Exploration Results",
          tabName = "explore",
          selected = TRUE)
      ),
      ## Modelling Menu ----
      menuItem(
        "Modelling",
        icon = icon("puzzle-piece"),
        fluidPage(
          theme = shinytheme("yeti"),
          fluidRow(
            textAreaInput("model", "Model", width = '350px', height = '50px',
                          value = "Method * p * eta * gamma * R2",
                          resize = 'vertical'),
            textAreaInput("subset", "Subset Expression", width = '350px', height = '50px',
                          value = "", placeholder = "Subset expression for the model.",
                          resize = 'vertical'),
            uiOutput('eff_list'),
            actionButton("btn_model", "Model")
          )
        ),
        menuSubItem(
          "Modelling Result",
          tabName = "modelling",
          selected = TRUE
        )
      )
    )
  ),
  ## Body ----
  body = dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "custom.css"
      )
    ),
    tabItems(
      ## Explore Content ----
      tabItem(
        tabName = "explore",
        conditionalPanel(
          condition = 'input.compare && output.has_selected',
          uiOutput('my_ui')
        )
      ),
      ## Modelling Content ----
      tabItem(
        tabName = "modelling",
        fluidPage(
          theme = shinytheme("yeti"),
          fluidRow(
            conditionalPanel(
              condition = 'input.btn_model',
              # plotOutput("test_plot"),
              # tabBox(
              box(
                title = "Prediction Error Model",
                value = "pred_tab",
                verbatimTextOutput('pred_anova')
              ),
              box(
                title = "Estimation Error Model",
                value = "est_tab",
                verbatimTextOutput('est_anova')
              ),
              #   id = "model_panel",
              #   title = "Model Dashboard"
              # ),
              # tabBox(
              box(
                title = "EffectPrediction Model",
                value = "pred_tab",
                plotOutput('pred_eff_plot')
              ),
              box(
                title = "Estimation Error Model",
                value = "est_tab",
                plotOutput('est_eff_plot')
              )
              #   id = "effect_panel",
              #   title = "Effect Plots"
              # )
            )
          )
        )
      )
    )
  )
)


## ---- Server Start ----
server <- function(input, output) {
  ## ---- Exploration ----
  ## Design Table ----
  dta <- reactive({
    design_chr %>%
      mutate(Design = paste("Design", 1:n())) %>%
      select(Design, p, eta, gamma, R2)
  })
  output$design <- DT::renderDataTable({
    dta <- dta()
    out <- datatable(
      data = dta,
      rownames = FALSE,
      options = list(dom = "tp", pageLength = 8),
      class = "row-border"
    ) %>%
      formatStyle(
        columns = 1:ncol(dta),
        fontFamily = "monospace",
        fontSize = "larger",
        color = "black"
      )
    return(out)
  })

  ## Selected design ----
  selected_design <- reactive({
    input[["design_rows_selected"]]
  })

  ## output condition ----
  output$has_selected <- reactive(!is.null(selected_design()))
  outputOptions(output, "has_selected", suspendWhenHidden = FALSE)
  output$which_plot <- eventReactive(input$compare, input$which_plot)
  outputOptions(output, "which_plot", suspendWhenHidden = FALSE)

  ## Selectd Designs Output ----
  output$selected_design <- DT::renderDataTable({
    req(!is.null(selected_design()))
    idx <- selected_design()
    dta <- dta() %>% slice(idx)
    datatable(
      data = dta,
      rownames = FALSE,
      options = list(dom = "t", pageLength = 8),
      class = "row-border",
      selection = "none"
    ) %>%
      formatStyle(
        columns = 1:ncol(dta),
        fontFamily = "monospace",
        fontSize = "larger",
        color = "black"
      )
  })

  ## Isolate Inputes ----
  rep <- eventReactive(input$compare, {
    out <- try(eval(parse(text = input$rep)), TRUE)
    if ("try-error" %in% class(out)) return(1)
    return(out)
  })
  which_plot <- eventReactive(input$compare, input$which_plot)
  method <- eventReactive(input$compare, input$method)
  ## Design Grid ----
  design_grid <- eventReactive(input$compare, {
    # req(!is.null(selected_design()))
    # req(input$compare)
    dgn <- selected_design()
    method <- method()
    expand.grid(Design = dgn, Method = method, stringsAsFactors = F)
  })
  ## Design Name ----
  design_name <- reactive({
    design_grid() %>%
      pmap_chr(~paste('Design', ..1, ..2, sep = '-'))
  })
  ## Full File Path ----
  full_path <- reactive({
    fn <- function(dgn, mthd, path) {
      out <- paste("dgn", dgn, tolower(mthd), sep = "-")
      paste0(file.path(data_path, out), ".Rdata")
    }
    design_grid() %>%
      mutate_at(2, tolower) %>%
      pmap_chr(~fn(..1, ..2, data_path))
  })
  ## Loading the data ----
  load_data <- reactive({
    path <- full_path()
    out <- map(path, ~get(load(.x)))
    names(out) <- pmap_chr(design_grid(), ~paste("Design", ..1, ..2, sep = "-"))
    out
  })
  ## Filter the data based on replication ----
  filter_data <- reactive({
    dta <- load_data()
    lapply(dta, "[", rep())
  })

  ## Main Plot UI for prediction and estimation errors and coefficients ----
                    tab_ui <- eventReactive(input$compare, {
    mthds <- method()
    mdl_tabs <- lapply(mthds, function(mthd){
      dgn_tabs <- lapply(selected_design(), function(dgn){
        base_key <- paste("Design", dgn, mthd, sep = "-")
        pred_key <- paste("pred", base_key, sep = "-")
        est_key <- paste("est", base_key, sep = "-")
        coef_key <- paste("coef", base_key, sep = "-")
        tabPanel(
          paste("Design", dgn),
          value = dgn,
          fluidRow(
            conditionalPanel(
              condition = 'output.which_plot.indexOf("pred") != -1',
              div(
                class = "col-md-6",
                plotOutput(pred_key, width = '100%')
              )
            ),
            conditionalPanel(
              condition = 'output.which_plot.indexOf("est") != -1',
              div(
                class = "col-md-6",
                plotOutput(est_key, width = '100%')
              )
            )
          ),
          conditionalPanel(
            condition = 'output.which_plot.indexOf("coef") != -1',
            fluidPage(
              plotOutput(coef_key, width = '100%')
            )
          )
        )
      })
      dgn_tabset <- do.call(
        tabsetPanel,
        c(dgn_tabs, id = "dgn_tab")
      )
      tabPanel(mthd, value = mthd, dgn_tabset)
    })
    do.call(tabBox, c(mdl_tabs, id = "mdl_tab", width = '100%'))
  })

  ## The main UI output ----
  output$my_ui <- renderUI({
    req(input$compare)
    tab_ui()
  })

  ## Dynamically create plot based on selected criteria ----
  observeEvent(input$compare, {
    for (idx in design_name()) {
      local({
        my_i <- idx
        est_key <- paste0("est-", my_i)
        pred_key <- paste0("pred-", my_i)
        coef_key <- paste0("coef-", my_i)
        output[[est_key]] <- renderPlot({
          req('est' %in% which_plot())
          filter_data()[[my_i]] %>% err_plot("Estimation")
        })
        output[[pred_key]] <- renderPlot({
          req('pred' %in% which_plot())
          filter_data()[[my_i]] %>% err_plot("Prediction")
        })
        output[[coef_key]] <- renderPlot({
          req('coef' %in% which_plot())
          filter_data()[[my_i]] %>% coef_plot()
        })
      })
    }
  })
  ## ---- Modelling ----
  ## Selected Model ----
  ## Model Fitting ----
  model_formula <- reactive({
    as.formula(paste("cbind(Y1, Y2, Y3, Y4)", input$model, sep = "~"))
    # as.formula("cbind(mpg, hp) ~ am + carb + am:carb")
  })
  ## Effect term Input ----
  output$eff_list <- renderUI({
    textInput("eff", "Effect term",
              value = "Method:eta:gamma",
              width = '100%')
  })
  ## Subset expression input ----
  subset <- reactive({
    input$subset
  })
  ## Prediction Error Data ----
  pred_data <- reactive({
    if (subset() != "") {
      pred_data_minimum %>% filter_(subset)
    } else {
      pred_data_minimum
    }
  })
  ## Prediction Error Model ----
  pred_model <- eventReactive(input$btn_model, {
    ## lm(model_formula(), data = pred_data_minimum)
    lm(model_formula(), data = pred_data())
  })
  ## Pred Effect Plot ----
  output$pred_eff_plot <- renderPlot({
    req(!is.null(pred_model()))
    eff_plot(input$eff, pred_model())
  })
  ## Pred ANOVA ----
  output$pred_anova <- renderPrint({
    anova(pred_model())
  })

  ## Estimation Error Data ----
  est_data <- reactive({
    if (subset() != "") {
      est_data_minimum <- est_data_minimum %>% filter_(subset)
    } else {
      est_data_minimum
    }
  })

  ## Estimation Error Model ----
  est_model <- eventReactive(input$btn_model, {
    ## lm(model_formula(), data = est_data_minimum)
    lm(model_formula(), data = est_data())
  })
  ## Est Effect Plot ----
  output$est_eff_plot <- renderPlot({
    req(!is.null(est_model()))
    eff_plot(input$eff, est_model())
  })
  ## Est ANOVA ----
  output$est_anova <- renderPrint({
    anova(est_model())
  })
}

shinyApp(ui = ui, server = server)
