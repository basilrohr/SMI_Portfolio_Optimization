library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(timeSeries)
library(sortable)
library(DT)
library(R.utils)

# Load functions
dir = "."
sourceDirectory(paste0(dir, "/Functions"), modifiedOnly = F)

# Create user interface
ui = navbarPage("Robust Methods of Portfolio Optimization",
                tabPanel("Overview",
                         fluidRow(
                           column(2,
                                  radioButtons(
                                    "intervalButton",
                                    "Data",
                                    choices = c("Daily" = "1d",
                                                "Weekly" = "1wk",
                                                "Monthly" = "1mo")),
                                  textOutput("dataRange"),
                                  br(),
                                  strong("Return"),
                                  p("The return is calculated by log return minus risk-free rate."),
                                  uiOutput("return"),
                                  br(),
                                  selectInput(
                                    "stockSelector",
                                    "Plots",
                                    SMI_stocks)),
                           column(10, strong("Groups"),
                                  bucket_list(
                                    group_name = "stockGroups",
                                    header = "Use drag and drop to move stocks around.",
                                    orientation = "horizontal",
                                    add_rank_list(
                                      input_id = "rankList1",
                                      text = SMI_groups_names[1],
                                      labels = get(SMI_groups_names[1])),
                                    add_rank_list(
                                      input_id = "rankList2",
                                      text = SMI_groups_names[2],
                                      labels = get(SMI_groups_names[2])),
                                    add_rank_list(
                                      input_id = "rankList3",
                                      text = SMI_groups_names[3],
                                      labels = get(SMI_groups_names[3])),
                                    add_rank_list(
                                      input_id = "rankList4",
                                      text = SMI_groups_names[4],
                                      labels = get(SMI_groups_names[4])),
                                    add_rank_list(
                                      input_id = "rankList5",
                                      text = "Exclude",
                                      labels = c())))),
                         hr(),
                         fluidRow(
                           column(4,
                                  div(
                                    style = "position: relative",
                                    plotOutput("stockPlot",
                                               hover = hoverOpts("plot_hover2", delay = 10, delayType = "debounce"),
                                               height = "300px"),
                                    uiOutput("hover_info2"))),
                           column(4,
                                  div(
                                    style = "position: relative",
                                    plotOutput("returnPlot",
                                               hover = hoverOpts("plot_hover1", delay = 10, delayType = "debounce"),
                                               height = "300px"),
                                    uiOutput("hover_info1"))),
                           column(4,
                                  tabsetPanel(
                                    tabPanel("Stocks",
                                             dataTableOutput("SMI_mu_sd", width = "90%")),
                                    tabPanel("Groups",
                                             dataTableOutput("SMI_groups_mu_sd", width = "90%")),
                                    tabPanel("Risk-free rate",
                                             dataTableOutput("Rf", width = "90%")))))),
                tabPanel("Standard Errors"),
                tabPanel("Markovitz Optimization",
                         column(12,
                                verbatimTextOutput("test")
                                )),
                tabPanel("About the authors",
                         fluidRow(
                           column(12,
                                  img(src=paste0("Logo_ZHAW.jpg"), height = "200px")))))

# Define functionality
server = function(input, output) {
  
  df = reactive({
    load(paste0(dir, "/Data/data_", input$intervalButton, ".Rda"))
    df
  })
  
  SMI_returns = reactive({
    load(paste0(dir, "/Data/SMI_returns_", input$intervalButton, ".Rda"))
    SMI_returns
  })
  
  parameters = reactive({
    constructor(SMI_returns(), SMI_stocks, SMI_groups_names, list(input$rankList1, input$rankList2,
                                                                  input$rankList3, input$rankList4))
  })
  
  output$dataRange = renderText({
    paste0("Data ranges from ", format(df()[,1][1], "%d.%m.%Y"), " to ",
           format(df()[,1][nrow(df())], "%d.%m.%Y"), ".")
  })
  
  output$return = renderUI({
    withMathJax(helpText("$$R = R_{ln} - R_f = ln\\left(\\frac{x_t}{x_{t-1}}\\right) - R_f$$"))
  })
  
  output$stockPlot = renderPlot({
    ggplot(df()) +
      geom_line(aes_string(x = "Date", y = paste0("`", input$stockSelector, "`"))) +
      labs(x = "Year", y = "Price [CHF]", title = paste(input$stockSelector, "stock price")) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
      custom_theme_shiny
  })
  
  output$returnPlot = renderPlot({
    ggplot(parameters()$SMI_returns) +
      geom_line(aes_string(x = "Date", y = paste0("`", input$stockSelector, "`"))) +
      labs(x = "Year", y = "Log return [%]", title = paste(input$stockSelector, "log return")) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
      custom_theme_shiny
  })
  
  output$SMI_mu_sd = renderDataTable({
    parameters()$SMI_mu_sd %>%
      datatable(rownames = NULL, options = list(dom = "tip", pageLength = 4)) %>%
      formatRound(columns = c(2:3), digits = 3)
  })
  
  output$SMI_groups_mu_sd = renderDataTable({
    parameters()$SMI_groups_mu_sd %>%
      datatable(rownames = NULL, options = list(dom = "t")) %>%
      formatRound(columns = c(2:3), digits = 3)
  })
  
  output$Rf = renderDataTable({
    load(paste0(dir, "/Data/Rf_1d.Rda"))
    Rf = data.frame(names(Rf), Rf * 260)
    colnames(Rf) = c("Year", "Risk-free rate [%]")
    Rf %>% datatable(rownames = NULL, options = list(dom = "tip", pageLength = 4)) %>%
      formatRound(columns = 2, digits = 3)
  })
  
  slices = reactive({
    slicer(SMI_returns(), SMI_stocks, SMI_groups_names, list(input$rankList1, input$rankList2,
                                                             input$rankList3, input$rankList4))
  })
  
  sr_os = reactive({
    out_of_sample(slices())
  })
  
  sr_is = reactive({
    in_sample(parameters())
  })
  
  output$test = renderPrint({
    sr_os()
  })
  
  output$hover_info1 = renderUI({
    hover = input$plot_hover1
    point = nearPoints(parameters()$SMI_returns, hover, threshold = 5, maxpoints = 1, addDist = T)
    if (nrow(point) == 0) return(NULL)
    
    left_pct = (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct = (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    left_px = hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px = hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style = paste0("position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.95); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;", "padding: 10px 10px 0px 10px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0(point$Date, "<br/>", round(point[input$stockSelector], 3)))))
  })
  
  output$hover_info2 = renderUI({
    hover = input$plot_hover2
    point = nearPoints(df(), hover, threshold = 5, maxpoints = 1, addDist = T)
    if (nrow(point) == 0) return(NULL)
    
    left_pct = (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct = (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    left_px = hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px = hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style = paste0("position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.95); ",
                   "left:", left_px + 2, "px; top:", top_px + 2, "px;", "padding: 10px 10px 0px 10px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0(point$Date, "<br/>", round(point[input$stockSelector], 3)))))
  })
}

shinyApp(ui, server)
