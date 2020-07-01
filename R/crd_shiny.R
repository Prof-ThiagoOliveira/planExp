#######################################################################
#                                                                     #
# Package: planExp                                                    #
#                                                                     #
# File: buildShiny.crdPlan.R                                          #
# Contains: buildShiny.crdPlan function                               #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2020, Thiago P. Oliveira                              #
#                                                                     #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Dashboard App for the completely randomized design (CRD)
##'
##' @description Constructs and starts a Shiny application from an object
##'   of class "crdPlan".
##'
##' @usage \method{buildShiny}{crdPlan}(obj)
##'
##' @aliases buildShiny.crdPlan
##'
##' @param obj an object inheriting from class "crdPlan" used to build
##'   the Shiny app.
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@usp.br}
##' @import shiny shinydashboard shinydashboardPlus shinyWidgets
##' @import ggplot2
##' @importFrom utils write.csv
##' @importFrom graphics layout
##' @importFrom ggfittext geom_fit_text
##' @importFrom DT formatRound
##' @examples
##' \dontrun{
##' treat <- LETTERS[seq( from = 1, to = 10 )]
##' rep <- rep(6, 10)
##' d1 <- crdPlan(treat, rep)
##' buildShiny(d1)
##'}
##'
##' @export

buildShiny.crdPlan <- function(obj) {
  if(class(obj)!="crdPlan") stop("Object must inherit from class \"crdPlan\"",
                                 call.=FALSE)
  #---------------------------------------------------------------------
  # Data
  #---------------------------------------------------------------------
  objData <- obj$Plan
  #---------------------------------------------------------------------
  # Shiny
  #---------------------------------------------------------------------
  Treatment <- SD <- Replicate <- NULL
  #---------------------------------------------------------------------
  # UI
  #---------------------------------------------------------------------
  body <- dashboardBody(
    #-------------------------------------------------------------------
    # Definitions
    #-------------------------------------------------------------------
    tags$head(
    tags$meta(charset="UTF-8"),
    tags$meta(name="description", content="..."),
    tags$meta(name="keywords", content="..."),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    #-------------------------------------------------------------------
    # Authors bar
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
                     width:450px;
                     background-color: #483C32;
                     color: #F0FFFF;}')),
    #-------------------------------------------------------------------
    # Shiny Color
    #-------------------------------------------------------------------
    tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #172F54;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #f4b943;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #483C32;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #483C32;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #3B2F2F;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #3B2F2F;
                              color: #F0FFFF;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ff69b4;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #ff69b4;
                              }
                              '
        )
        )
    ),
    setShadow("box"),
    setShadow("info-box"),
    setShadow("progress"),
    #-------------------------------------------------------------------
    withMathJax(),
    tabItems(
      #-----------------------------------------------------------------
      #=================================================================
      # Randomization
      #=================================================================
      tabItem(
        tabName = "plan",
        fluidPage(
        gradientBox(
          width = 12,
          icon = "fa fa-th",
          title = "Summary",
          boxToolSize = "lg",
          footer = column(
            width = 12,
            align = "center",
            fluidRow(
              column(
                width = 5,
                box(
                  title = "Randomization Table",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  HTML("<div style ='overflow:auto;  ' >"),
                  DT::dataTableOutput("plan_table"),
                  HTML("</div>"),
                  br(),
                  downloadButton('download',"Download the data")
                )
              ),
              column(
                width = 3,
                box(
                  title = "ANOVA Table",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  HTML("<div style ='overflow:auto;  ' >"),
                  tableOutput("summary_table"),
                  HTML("</div>"),
                  br(),
                  tags$div(
                    style="text-align:justified",
                    tags$p("SV - Source of Variation"),
                    tags$p("DF - Degree of Freedom"),
                    tags$p("DFv - Degree of Freedom (value)")
                  )
                ),
                box(
                  width = 12,
                  title = "Statistical Model",
                  solidHeader = TRUE,
                  status = "primary",
                  tags$style("#text_model {font-size:18px; text-align: left;}"),
                  uiOutput("text_model")
                )
              )
            )
          ),
          tags$div(
            tags$p("Proper randomization of the experiment is an important step in obtaining independence assumption on the errors. You can", tags$b("download resutls"), "as csv from the download button. Degree of freedom (DF) and the possible statistical model is also presented here.",
                   style = "font-size:15px", align="justify")
          )
        )
        )
      ),
      #=================================================================
      # Design sketch
      #=================================================================
      tabItem(
        tabName = "plot",
        fluidPage(
        gradientBox(
          width = 12,
          icon = "fa fa-th",
          title = "Graphical Parameters",
          boxToolSize = "lg",
          footer = column(
            width = 12,
            align = "center",
            #===========================================================
            navbarPage(
              "",
              tabPanel(
                "Plot Structure",
                icon = icon("fas fa-drafting-compass"),
                fluidPage(
                  fluidRow(
                    box(
                      title = "Structure",
                      width = 2,
                      solidHeader = TRUE,
                      status = "primary",
                      numericInput(
                        inputId = "rows",
                        label = "Number of rows:",
                        value = length(levels(objData$Treatment)),
                        min = 1,
                        max = 100
                      )
                    ),
                    box(
                      title = "Dimensions",
                      width = 2,
                      solidHeader = TRUE,
                      status = "primary",
                      numericInput(
                        inputId = "plot_height",
                        label = "Plot height (px):",
                        value = 500,
                        ),
                      numericInput(
                        inputId = "plot_width",
                        label = "Width (%):",
                        value = 70,
                        )
                    )
                  )
                )
              ),
              #---------------------------------------------------------
              tabPanel(
                "Title",
                icon = icon("fas fa-heading"),
                fluidPage(
                  fluidRow(
                    box(
                      title = "Titles",
                      width = 4,
                      solidHeader = TRUE,
                      status = "primary",
                      textInput(
                        inputId = "xaxis",
                        label = "X axis title:",
                        value = "Column"
                      ),
                      textInput(
                        inputId = "yaxis",
                        label = "Y axis title:",
                        value = "Row"
                      ),
                      textInput(
                        inputId = "Text",
                        label = "Main title:",
                        value = obj$Design
                      )
                    )
                  )
                )
              ),
              #---------------------------------------------------------
              tabPanel(
                "Size",
                icon = icon("fas fa-text-height"),
                fluidPage(
                  fluidRow(
                    box(
                      title = "Text",
                      width = 2,
                      solidHeader = TRUE,
                      status = "primary",
                      numericInput(
                        inputId = "title_size",
                        label = "Title text:",
                        value = 20,
                        min = 1,
                        max = 100
                      ),
                      numericInput(
                        inputId = "axis_size",
                        label = "Axis text:",
                        value = 20,
                        min = 1,
                        max = 100
                      ),
                      numericInput(
                        inputId = "treatment_size",
                        label = "Treatment text:",
                        value = 20,
                        min = 1,
                        max = 100
                      )
                    )
                  )
                )
              ),
              #---------------------------------------------------------
              tabPanel(
                "Colour",
                icon = icon("fas fa-tint"),
                fluidPage(
                  fluidRow(
                    box(
                      title = "Table",
                      width = 2,
                      solidHeader = TRUE,
                      status = "primary",
                      textInput(
                        inputId = "plotColour",
                        label = "Fill:",
                        value = "gray90"
                      ),
                      numericInput(
                        inputId = "alpha",
                        label = "Color opacity:",
                        value = 0.7,
                        min = 0,
                        max = 1
                      ),
                      textInput(
                        inputId = "textColour",
                        label = "Lines:",
                        value = "black"
                      )
                    ),
                    box(
                      title = "Treatment",
                      width = 2,
                      solidHeader = TRUE,
                      status = "primary",
                      textInput(
                        inputId = "treatColour",
                        label = "Text colour:",
                        value = "black"
                      ),
                      checkboxInput(
                        inputId = "bytreatColour",
                        label = "Color by treatment?",
                        value = FALSE
                      )
                    )
                  )
                )
              ),
              tabPanel(
                "Download Plot",
                icon = icon(""),
                fluidPage(
                  fluidRow(
                    box(
                      title = "Choose File type and Download",
                      width = 3,
                      solidHeader = TRUE,
                      status = "primary",
                      selectInput(
                        inputId = "plot_type",
                        label = "File type:",
                        choices = c("eps", "ps", "tex", "pdf",
                                    "jpeg", "tiff", "png", "bmp", "svg"),
                        selected = "pdf"
                      ),
                      downloadButton('download_plot',"Download here!",
                                     style="display: block; margin: 0 auto; color: black;")
                    )
                  )
                )
              )
              #---------------------------------------------------------
            )
            #===========================================================
          )
        ),
        gradientBox(
          width = 12,
          icon = "fa fa-th",
          title = "Design sketch",
          boxToolSize = "lg",
          footer = column(
            width = 12,
            align = "center",
            fluidPage(
              fluidRow(
                column(
                  width = 12,
                  actionButton(
                    "run_design", "Run Randomization Again!"
                  ),
                  br(),
                  br(),
                  uiOutput("plot_ui")
                )
              )
            )
          )
        )
      )
      )
    )
  )
  #=====================================================================
  sidebar <- dashboardSidebar(
    sidebarMenu(
      sidebarMenuOutput("Semi_collapsible_sidebar"),
      menuItem(text = "Summary", tabName = "plan",
               icon = icon("laptop")),
      menuItem(text = "Design sketch", tabName = "plot",
               icon = icon("fas fa-project-diagram")),
      br(),
      tags$div(
        HTML(paste("Press Quit to exit the application",
                   tags$span(style="color:white"), sep = "")
             )),
      actionButton("quit", "Quit")
    )
  )
  #=====================================================================
  header <- dashboardHeaderPlus(
    title = obj$Design,
    titleWidth = 350,
    enable_rightsidebar = FALSE,
    left_menu = tagList(
      dropdownBlock(
        id = "mydropdown",
        title = "Package planExp",
        icon = "fas fa-box-open",
        #---------------------------------------------------------------
        ## Box Header ##
        tags$li(
          class = "widget-user-header",
          tags$h3(class = "widget-user-username",
                  tags$p("Author:",
                  tags$a(href="https://prof-thiagooliveira.netlify.app",
                         "Thiago de Paula Oliveira",
                         style = "font-size:18px"),
                  style = "font-size:18px"),
                  tags$p("This package is powered by",
                         tags$a(href = "https://shiny.rstudio.com/",
                                "Shiny App", style = "font-size:18px"),
                         style = "font-size:18px")
                  )
        )
        #---------------------------------------------------------------
      )
    )
  )
  #=====================================================================
  ui <- dashboardPagePlus(header, sidebar, body)
  #=====================================================================
  # Server
  #=====================================================================
  server <- function(input, output) {
    #-------------------------------------------------------------------
    # Quit App
    #-------------------------------------------------------------------
    observe({
        if (input$quit == 1)
          stopApp()
    })
    #-------------------------------------------------------------------
    # Data
    #-------------------------------------------------------------------
    counter_d <- reactiveValues(cv = 0)
    observeEvent(input$run_design, {
      counter_d$cv <- counter_d$cv + 1
    })
    objData_r <- reactive({
      if (counter_d$cv == 0) {
        objData
      }else {
        data <- obj$initValues
        m1 <- crdPlan(treat = data$treat, rep = data$rep)
        m1$Plan
      }
    })
    #-------------------------------------------------------------------
    # Table - Randomization
    #-------------------------------------------------------------------
    output$plan_table <- DT::renderDataTable({
      DT::datatable(objData_r(), rownames = NULL,
                    options = list(lengthMenu = c(10, 15, 30, 50,  100),
                                   pageLength = 10,  dom = "tip",
                                   autoWidth = TRUE))
    })
    #-------------------------------------------------------------------
    # Table ANOVA
    #-------------------------------------------------------------------
    dataSummary <- function(obj) {
      if (class(obj) == "crdPlan") {
        df_trat <- length(levels(obj$Plan$Treatment)) - 1
        df_total <- nrow(obj$Plan) - 1
        df_res <- df_total - df_trat
        SV = c("Plots", "Treatment", "Residual")
        DF = c("n - 1", "t - 1", "n - t")
        newData <- data.frame(SV = SV, DF = DF,
                              DFv = c(df_total, df_trat, df_res))
      }
      return(newData)
    }
    output$summary_table <- renderTable(
      expr = dataSummary(obj),
      digits = 0,
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE
    )
    #-------------------------------------------------------------------
    # Plot
    #-------------------------------------------------------------------
    isValid_num <- reactive({
      !is.null(input$rows) && input$rows >= 1 &&
        is.numeric(input$rows)
    })
    data_plot <- reactive({
      if (isValid_num()) {
        rows. <- input$rows
      }else {
        rows. <- length(levels(objData_r()$Treatment))
      }
      a <- ceiling(nrow(objData_r())/rows.)
      grid.y <- rep(1:rows., a)[1:nrow(objData_r())]
      grid.x <- sort(rep(1:a, rows.))[1:nrow(objData_r())]
      data. <- objData_r()
      data.$grid.x <- grid.x
      data.$grid.y <- grid.y
      data.
    })
    #-------------------------------------------------------------------
    # Plot
    #-------------------------------------------------------------------
    isValid_alpha <- reactive({
      !is.null(input$alpha) && input$alpha >= 0 && input$alpha <= 1 &&
        is.numeric(input$alpha)
    })
    plot_design <- reactive({
      h <- input$plot_height
      if (isValid_alpha()) {
        alpha. <- input$alpha
      }else {
        alpha. <- 0.5
      }

      p1 <- ggplot(data_plot(),
                   aes(y = grid.y, x = grid.x,
                       label = Treatment,
                       text = paste("Row: ", grid.y,
                                    ", Column: ", grid.x,
                                    "<br>",
                                    "Treatment: ", Treatment))) +
        labs(x = input$xaxis, y = input$yaxis, title = input$title) +
                    scale_x_continuous(breaks = c(data_plot()$grid.x)) +
            scale_y_continuous(breaks = c(data_plot()$grid.y)) +
        theme(panel.background = element_blank(),
              legend.position = "none",
              title = element_text(size = input$title_size),
              axis.text = element_text(size = input$axis_size,
                                       face = input$fontfaceAxis),
              strip.background = element_blank(),
              strip.text = element_text(size = input$strip_size))

      p2 <- p1 + geom_rect(data=data_plot(),
                           mapping=aes(xmin=grid.x - 0.5,
                                       xmax=grid.x + 0.5,
                                       ymin=grid.y - 0.5,
                                       ymax=grid.y + 0.5),
                           color=input$textColour, alpha=alpha.,
                           fill = input$plotColour)
      if (input$bytreatColour == FALSE) {
        p3 <- p2 +  geom_fit_text(reflow = TRUE,
                            size = input$treatment_size,
                            colour = input$treatColour)
      }
      if (input$bytreatColour == TRUE) {
        p3 <- p2 +  geom_fit_text(aes(colour = Treatment),
                            reflow = TRUE,
                            size = input$treatment_size)
      }
      print(p3)
    })
    #-------------------------------------------------------------------
    output$plot <- renderPlot({
      plot_design()
    })
    #-------------------------------------------------------------------
    output$plot_ui <- renderUI({
      plotOutput("plot", height = paste0(input$plot_height, "px"),
                 width = paste0(input$plot_width, "%"))
    })
    #-------------------------------------------------------------------
    # Model
    #-------------------------------------------------------------------
    output$text_model <- renderUI({
      withMathJax(
        helpText(' $$y_{ij} = \\mu+\\tau_{i}+\\epsilon_{ij}$$
                  where \\(y_{ij}\\) is the response variable, \\(\\mu\\) is
                  the overall mean, \\(\\tau_{i}\\) is the \\(i\\)th effect
                  of treatment,  and \\(\\epsilon_{ij}\\) is the error
                  term, which is generally assumed to be
                  \\(\\epsilon_{ij} \\sim N \\left(0, \\sigma^2\\right)\\).')
      )
    })
    #===================================================================
    # Download
    #===================================================================
    output$download <- downloadHandler(
      filename = function() {
        paste0("plan-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(objData_r()[, c(2:3)], file)
      }
    )
    # Plot
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("planPlot-", Sys.Date(), ".", input$plot_type)
      },
      content = function(file) {
        ggsave(file, plot_design())
      }
    )
    #===================================================================
  }
  shinyApp(ui = ui, server = server)
}
