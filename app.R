library(shiny)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(R.utils)
library(sortable)
library(DT)
library(shinycssloaders)
library(cowplot)

sourceDirectory("./Code", modifiedOnly = F)

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
                                  selectInput(
                                    "stockSelector",
                                    "Plots",
                                    stocks),
                                  strong("Return"),
                                  uiOutput("returnFormula")),
                           column(5,
                                  div(
                                    style = "position: relative",
                                    plotOutput("stockPlot", height = "300px",
                                               hover = hoverOpts("stockPlotHover",
                                                                 delay = 10, delayType = "debounce")),
                                    uiOutput("stockPlotHoverInfo")),
                                  br(),
                                  div(
                                    style = "position: relative",
                                    plotOutput("returnPlot", height = "300px",
                                               hover = hoverOpts("returnPlotHover",
                                                                 delay = 10, delayType = "debounce")),
                                    uiOutput("returnPlotHoverInfo"))),
                           column(5,
                                  tabsetPanel(
                                    tabPanel("Mean return and volatility",
                                             dataTableOutput("r_mr_vol", width = "90%")),
                                    tabPanel("Correlation", br(),
                                             plotOutput("r_cor", height = "600px")),
                                    tabPanel("Risk-free rate",
                                             dataTableOutput("rf", width = "90%")))))),
                tabPanel("Groups",
                         fluidRow(
                           column(8,
                                  strong("Groups"),
                                  bucket_list(
                                    group_name = "stockGroups",
                                    header = "Use drag and drop to move stocks around.",
                                    orientation = "horizontal",
                                    add_rank_list(
                                      input_id = "rankList1",
                                      text = textInput("group1Name", NULL, value = "Consumer", width = "100%"),
                                      labels = groups[[1]]),
                                    add_rank_list(
                                      input_id = "rankList2",
                                      text = textInput("group2Name", NULL, value = "Finance", width = "100%"),
                                      labels = groups[[2]]),
                                    add_rank_list(
                                      input_id = "rankList3",
                                      text = textInput("group3Name", NULL, value = "Industrial", width = "100%"),
                                      labels = groups[[3]]),
                                    add_rank_list(
                                      input_id = "rankList4",
                                      text = textInput("group4Name", NULL, value = "Pharma", width = "100%"),
                                      labels = groups[[4]]),
                                    add_rank_list(
                                      input_id = "rankList5",
                                      text = textInput("group5Name", NULL, value = "Group 5", width = "100%"),
                                      labels = c()),
                                    add_rank_list(
                                      input_id = "rankList6",
                                      text = textInput("group6Name", NULL, value = "Group 6", width = "100%"),
                                      labels = c()),
                                    add_rank_list(
                                      input_id = "rankList7",
                                      text = textInput("group7Name", NULL, value = "Group 7", width = "100%"),
                                      labels = c()),
                                    add_rank_list(
                                      input_id = "rankList8",
                                      text = textInput("group8Name", NULL, value = "Group 8", width = "100%"),
                                      labels = c()),
                                    add_rank_list(
                                      input_id = "rankList9",
                                      text = "Exclude",
                                      labels = c())),
                                  textOutput("errorGroupNames"),
                                  tags$head(tags$style("#errorGroupNames{color: red; font-size: 16px;
                                                       font-weight: bold;}"))),
                           column(4,
                                  tabsetPanel(
                                    tabPanel("Mean return and volatility",
                                             dataTableOutput("SMI_groups_mu_sd", width = "90%")),
                                    tabPanel("Correlation", br(),
                                             plotOutput("SMI_groups_cor", height = "500px")))))),
                tabPanel("Standard Errors",
                         fluidRow(
                           column(2,
                                  strong("Standard error of return"),
                                  uiOutput("sderrorreturnFormula"),
                                  strong("Standard error of volatility"),
                                  uiOutput("sderrorvolatilityFormula")),
                           column(10,
                                  tabsetPanel(
                                    tabPanel("Stocks", br(),
                                             fluidRow(
                                               column(5,plotOutput("sderrorPlot")),
                                               column(4,dataTableOutput("SMI_sd_se", width = "90%")))),
                                    tabPanel("Groups", br(),
                                             fluidRow(
                                               column(5,plotOutput("sderrorPlot_groups")),
                                               column(4,dataTableOutput("SMI_sd_se_groups", width = "90%"))))))
                           
                           )),
                tabPanel("Markovitz Optimization",
                         fluidRow(
                           column(4,
                                  plotOutput("effFrontierBootstrap") %>% withSpinner,
                                  actionButton("bootstrapButton", "Redo bootstrap")))),
                tabPanel("Shrinking",
                         fluidRow(
                           column(5,
                                  plotOutput("returnShrinking")),
                           column(5,
                                  plotOutput("correlationShrinking")))),
                tabPanel("About the authors",
                         fluidRow(
                           column(12,
                                  img(src=paste0("Logo_ZHAW.jpg"), height = "200px")))))

server = function(input, output, session) {
  
  df_react = reactive({
    load(paste0("./Data/data_", input$intervalButton, ".Rda"))
    df
  })
  
  r_react = reactive({
    load(paste0("./Data/returns_", input$intervalButton, ".Rda"))
    returns
  })
  
  gr_react = reactive({
    groups_returns(r_react())
  })
  
  g_react = reactive({
    req(input$stockGroups)
    SMI_groups_names = c(input$group1Name, input$group2Name, input$group3Name, input$group4Name,
                         input$group5Name, input$group6Name, input$group7Name, input$group8Name)
    SMI_groups_names = trimws(SMI_groups_names, which = "both")
    dupl = duplicated(SMI_groups_names)
    
    error_message = ""

    SMI_groups = list()
    indices = c()
    for (i in 1:(length(input$stockGroups)-1)) {
      if (length(input$stockGroups[[i]]) >= 1) {
        if (i %in% which(dupl)){
          error_message = "Error: Identical Group names are not allowed"
          next
        }
        SMI_groups[[SMI_groups_names[i]]] = input$stockGroups[[i]]
        indices = c(indices, i)
      }
    }
    output$errorGroupNames = renderText({error_message})
    list(SMI_groups, SMI_groups_names[indices])
  })
  
  output$dataRange = renderText({
    paste0("Data ranges from ", format(df_react()[,1][1], "%d.%m.%Y"), " to ",
           format(df_react()[,1][nrow(df_react())], "%d.%m.%Y"), ".")
  })
  
  output$returnFormula = renderUI({
    withMathJax(helpText("$$R = R_{ln} - R_f = ln\\left(\\frac{x_t}{x_{t-1}}\\right) - R_f$$"))
  })
  
  output$stockPlot = renderPlot({
    ggplot(df_react()) +
      geom_line(aes_string(x = "Date", y = paste0("`", input$stockSelector, "`"))) +
      labs(x = "Year", y = "Price [CHF]", title = paste(input$stockSelector, "stock price")) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
      custom_theme_shiny
  })
  
  output$stockPlotHoverInfo = renderUI({
    hover = input$stockPlotHover
    point = nearPoints(df_react(), hover, threshold = 5, maxpoints = 1, addDist = T)
    if (nrow(point) == 0) {return(NULL)}
    left_pct = (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct = (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    left_px = hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px = hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    style = paste0("position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.95);",
                   "left:", left_px + 2, "px; top:", top_px + 2, "px;", "padding: 10px 10px 0px 10px;")
    wellPanel(
      style = style,
      p(HTML(paste0(point$Date, "<br/>", round(point[input$stockSelector], 3), " CHF"))))
  })
  
  output$returnPlot = renderPlot({
    ggplot(r_react()) +
      geom_line(aes_string(x = "Date", y = paste0("`", input$stockSelector, "`"))) +
      labs(x = "Year", y = "Log return [%]", title = paste(input$stockSelector, "return")) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
      custom_theme_shiny
  })
  
  output$returnPlotHoverInfo = renderUI({
    hover = input$returnPlotHover
    point = nearPoints(r_react(), hover, threshold = 5, maxpoints = 1, addDist = T)
    if (nrow(point) == 0) {return(NULL)}
    left_pct = (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct = (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    left_px = hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px = hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    style = paste0("position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.95);",
                   "left:", left_px + 2, "px; top:", top_px + 2, "px;", "padding: 10px 10px 0px 10px;")
    wellPanel(
      style = style,
      p(HTML(paste0(point$Date, "<br/>", round(point[input$stockSelector], 3)), "%")))
  })
  
  output$r_mr_vol = renderDataTable({
    r = r_react()
    r_mr_vol = data.frame(colnames(r[-1]), mean_returns(r), volatilities(cov_mat(r)))
    r_mr_vol %>% datatable(colnames = c("Stock","Return [%]", "Volatility [%]"), rownames = NULL,
                           options = list(dom = "tip", pageLength = 10)) %>%
      formatRound(columns = c(2:3), digits = 3)
  })
  
  output$r_cor = renderPlot({
    gg_cor(cor_mat(r_react()), 3, 12, "Correlation", custom_theme_shiny)
  })
  
  output$rf = renderDataTable({
    load("./Data/rf_1d.Rda")
    ann_rf = data.frame(names(rf), round(rf * 252, 3))
    ann_rf %>% datatable(colnames = c("Year", "Rate [%]"), rownames = NULL,
                         options = list(dom = "tip", pageLength = 10))
  })
  
  output$gr_mr_vol = renderDataTable({
    gr = gr_react()
    gr_mr_vol = data.frame(colnames(gr[-1]), mean_returns(gr), volatilities(cov_mat(gr)))
    gr_mr_vol %>% datatable(colnames = c("Group","Return [%]", "Volatility [%]"), rownames = NULL,
                            options = list(dom = "t")) %>%
      formatRound(columns = c(2:3), digits = 3)
  })
  
  output$SMI_groups_cor = renderPlot({
    gg_cor(cor_mat(gr_react()), 5, 15, "Correlation", custom_theme_shiny)
  })
  
  output$sderrorreturnFormula = renderUI({
    withMathJax(helpText("$$\\sigma_{\\overline{R}} = \\frac{\\sigma}{\\sqrt{n}}$$"))
  })
  
  output$sderrorvolatilityFormula = renderUI({
    withMathJax(helpText("$$\\sigma_{\\sigma} = \\sigma * \\frac{1}{\\sqrt{2 * (n - 1)}}$$"))
  })
  
  sderrorplots = reactive({
    out = constructor_react()
    SMI_mu_sd = out$SMI_mu_sd
    returns = out$returns
    SMI_groups_returns = out$SMI_groups_returns
    SMI_groups_mu_sd = out$SMI_groups_mu_sd
    
    se_mean_SMI = apply(returns[,-1], MARGIN = 2, FUN = se_mean)
    se_mean_groups = apply(SMI_groups_returns[,-1], MARGIN = 2, FUN = se_mean)
    
    min = min(c(SMI_mu_sd$`Mean return [%]` - se_mean_SMI,
                SMI_groups_mu_sd$`Mean return [%]` - se_mean_groups))
    max = max(c(SMI_mu_sd$`Mean return [%]` + se_mean_SMI,
                SMI_groups_mu_sd$`Mean return [%]` + se_mean_groups))
    
    gg1 = gg_errorbar(SMI_mu_sd$Stock, SMI_mu_sd$`Mean return [%]`, se_mean_SMI, c(min, max), 90, "Return in [%]",
                title = "Mean return with standard error by stock",
                custom_theme_shiny)
    gg2 = gg_errorbar(SMI_groups_mu_sd$Group, SMI_groups_mu_sd$`Mean return [%]`, se_mean_groups, c(min, max), 90,
                "Return in [%]", title = "Mean return with standard error by group",
                custom_theme_shiny)
    align_plots(gg1, gg2, align="h")
  })
  
  output$sderrorPlot = renderPlot({
    ggdraw(sderrorplots()[[1]])
    
  })
  
  output$sderrorPlot_groups = renderPlot({
    ggdraw(sderrorplots()[[2]])
    
  })
  
  output$SMI_sd_se = renderDataTable({
    out = constructor_react()
    returns = out$returns
    SMI_mu_sd = out$SMI_mu_sd
    
    se_sd_SMI = as.vector(apply(returns[,-1], MARGIN = 2, FUN = se_sd))
    se_sd_SMI = data.frame(SMI_mu_sd$Stock, se_sd_SMI)
    colnames(se_sd_SMI) = c("Stock", "Standard Error in [%]")
    
    se_sd_SMI %>%
      datatable(rownames = NULL, options = list(dom = "tip", pageLength = 10)) %>%
      formatRound(columns = 2, digits = 3)
  })
  
  output$SMI_sd_se_groups = renderDataTable({
    out = constructor_react()
    SMI_groups_returns = out$SMI_groups_returns
    SMI_groups_mu_sd = out$SMI_groups_mu_sd
    
    se_sd_groups = as.vector(apply(SMI_groups_returns[,-1], MARGIN = 2, FUN = se_sd))
    se_sd_groups = data.frame(SMI_groups_mu_sd$Group, se_sd_groups)
    colnames(se_sd_groups) = c("Group", "Standard Error in [%]")
    
    se_sd_groups %>%
      datatable(rownames = NULL, options = list(dom = "tip", pageLength = 10)) %>%
      formatRound(columns = 2, digits = 3)
  })
  
  bootstrap_react_SMI = reactive({
    input$bootstrapButton
    bootstrap(constructor_react()$returns)
  })
  
  bootstrap_react_SMI_groups = reactive({
    input$bootstrapButton
    bootstrap(constructor_react()$SMI_groups_returns)
  })
  
  output$effFrontierBootstrap = renderPlot({
    bs_out_SMI = bootstrap_react_SMI()
    if (input$intervalButton == "1d") {xlim_sf = 1; ylim_sf = 1}
    if (input$intervalButton == "1wk") {xlim_sf = 2; ylim_sf = 4}
    if (input$intervalButton == "1mo") {xlim_sf = 3; ylim_sf = 15}
    suppressMessages(
      suppressWarnings(print(
        gg_bs_eff_frontier(bs_out_SMI, xlim_sf, ylim_sf, 1.5, "Bootstrap samples efficiency frontier",
                           custom_theme_shiny))))
  })
  
  slicer_react = reactive({
    SMI_groups = SMI_groups()
    slicer(returns(), SMI_stocks, SMI_groups[[2]], SMI_groups[[1]])
  })
  
  out_of_sample_react = reactive({
    out_of_sample(slicer_react(), input$returnShrinkingSlider, input$correlationShrinkingSlider,
                  interval = input$intervalButton)
  })
  
  output$sharpeRatioSlider = renderText({
    line1 = paste0("Out of sample SMI sharpe ratio: ",
                   round(out_of_sample_react()$sr_os_SMI, 3))
    line2 = paste0("Out of sample SMI groups sharpe ratio: ",
                   round(out_of_sample_react()$sr_os_SMI_groups, 3))
    HTML(paste(line1, line2, sep = "<br/>"))
  })
  
  output$returnShrinking = renderPlot({
    gg_shrinking2D(slicer_react(), r = T, title = "Shrinking of return",
                   theme = custom_theme_shiny, interval = input$intervalButton)
  })
  
  output$correlationShrinking = renderPlot({
    gg_shrinking2D(slicer_react(), cor = T, title = "Shrinking of correlation",
                   theme = custom_theme_shiny, interval = input$intervalButton)
  })
  
  sr_os = reactive({
    out_of_sample(slicer_react())
  })
  
  sr_is = reactive({
    in_sample(constructor_react())
  })
  
}

shinyApp(ui, server)
