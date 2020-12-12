library(shiny)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(R.utils)
library(sortable)
library(DT)
library(shinycssloaders)
library(cowplot)
library(ggrepel)
library(heatmap3)

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
                                    tabPanel("Return and volatility",
                                             dataTableOutput("r_mr_vol", width = "90%")),
                                    tabPanel("Correlation", br(),
                                             plotOutput("r_cor", height = "600px")),
                                    tabPanel("Standard error", br(),
                                             plotOutput("seReturnPlot", width = "90%"),
                                             dataTableOutput("seVolatility", width = "90%")),
                                    tabPanel("Risk-free rate",
                                             dataTableOutput("rf", width = "90%")))))),
                tabPanel("Groups",
                         fluidRow(
                           column(7,
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
                                  tags$style("#errorGroupNames{color: red; font-size: 16px; font-weight: bold;}")),
                           column(5,
                                  tabsetPanel(
                                    tabPanel("Return and volatility",
                                             dataTableOutput("gr_mr_vol", width = "90%")),
                                    tabPanel("Correlation", br(),
                                             plotOutput("gr_cor", height = "600px")),
                                    tabPanel("Standard error", br(),
                                             plotOutput("seGroupsReturnPlot", width = "90%"),
                                             dataTableOutput("seGroupsVolatility", width = "90%")),
                                    tabPanel("Cluster", br(),
                                             tags$div(style="font-size: 17px; font-weight: bold; text-align: center;",
                                                      "Grouped by euclidean distance of correlation coefficients"),
                                             plotOutput("distHeatmap", height = "600px")))))),
                tabPanel("Markovitz Optimization",
                         fluidRow(
                           column(6,
                                  plotOutput("effFrontierPlot", height = "600px",
                                             hover = hoverOpts("effFrontierPlotHover",
                                                               delay = 10, delayType = "debounce")),
                                  uiOutput("effFrontierPlotHoverInfo"),
                                  tabsetPanel(
                                    tabPanel("Stocks", br(),
                                             fluidRow(
                                               column(6, plotOutput("sdMVPWeightsPlot") %>% withSpinner),
                                               column(6, plotOutput("sdTPWeightsPlot") %>% withSpinner))),
                                    tabPanel("Groups", br(),
                                             fluidRow(
                                               column(6, plotOutput("sdMVPGroupsWeightsPlot") %>% withSpinner),
                                               column(6, plotOutput("sdTPGroupsWeightsPlot") %>% withSpinner))))),
                           column(4,
                                  plotOutput("effFrontierBootstrapPlot") %>% withSpinner,
                                  actionButton("bootstrapButton", "Redo bootstrap")))),
                tabPanel("Sharpe ratios",
                         fluidRow(
                           column(4,
                                  plotOutput("returnShrinking")),
                           column(4,
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
  
  g_react = reactive({
    req(input$stockGroups)
    groups_names = c(input$group1Name, input$group2Name, input$group3Name, input$group4Name,
                     input$group5Name, input$group6Name, input$group7Name, input$group8Name)
    dupl = duplicated(groups_names); error_message = ""
    groups = list()
    for (i in 1:(length(input$stockGroups)-1)) {
      if (length(input$stockGroups[[i]]) >= 1) {
        if (i %in% which(dupl)) {
          error_message = "Error: Identical group names are not allowed."
          break
        }
        groups[[groups_names[i]]] = input$stockGroups[[i]]
      }
    }
    output$errorGroupNames = renderText({error_message})
    groups
  })
  
  gr_react = reactive({
    groups_returns(r_react(), g_react())
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
      labs(x = "Year", y = "Return [%]", title = paste(input$stockSelector, "return")) +
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
                            options = list(dom = "tip", pageLength = 10)) %>%
      formatRound(columns = c(2:3), digits = 3)
  })
  
  output$gr_cor = renderPlot({
    gg_cor(cor_mat(gr_react()), 5, 15, "Correlation", custom_theme_shiny)
  })
  
  seReturnPlot_react = reactive({
    r = r_react(); gr = gr_react()
    mr = mean_returns(r); gmr = mean_returns(gr)
    se_r = apply(r[,-1], 2, se_mean); se_gr = apply(gr[,-1], 2, se_mean)
    min = min(c(mr - se_r, gmr - se_gr)); max = max(c(mr + se_r, gmr + se_gr))
    gg1 = gg_errorbar(colnames(r[-1]), mr, se_r, c(min, max), "Return [%]", 
                      "Standard error of return", custom_theme_shiny)
    gg2 = gg_errorbar(colnames(gr[-1]), gmr, se_gr, c(min, max), "Return [%]",
                      "Standard error of return", custom_theme_shiny)
    align_plots(gg1, gg2, align = "h")
  })
  
  output$seReturnPlot = renderPlot({
    ggdraw(seReturnPlot_react()[[1]])
  })
  
  output$seGroupsReturnPlot = renderPlot({
    ggdraw(seReturnPlot_react()[[2]])
  })
  
  output$seVolatility = renderDataTable({
    r = r_react()
    se_volr = data.frame(colnames(r[-1]), apply(r[,-1], 2, se_sd))
    se_volr %>% datatable(colnames = c("Stock", "Standard error of volatility [%]"), rownames = NULL,
                          options = list(dom = "tip", pageLength = 5)) %>%
      formatRound(columns = 2, digits = 5)
  })
  
  output$seGroupsVolatility = renderDataTable({
    gr = gr_react()
    se_volgr = data.frame(colnames(gr[-1]), apply(gr[,-1], 2, se_sd))
    se_volgr %>% datatable(colnames = c("Group", "Standard error of volatility [%]"), rownames = NULL,
                           options = list(dom = "tip", pageLength = 5)) %>%
      formatRound(columns = 2, digits = 5)
  })
  
  output$distHeatmap = renderPlot({
    cor = cor_mat(r_react())
    heatmap3(as.matrix(dist(cor)), method = "complete", symm = T, cexRow = 1.2, cexCol = 1.2, margins = c(8, 8),
             col = colorRampPalette(c("orangered4", "orangered2", "honeydew"))(1000))
  })
  
  ef_react = reactive({
    r = r_react(); gr = gr_react()
    cm = cov_mat(r); mr = mean_returns(r)
    gcm = cov_mat(gr); gmr = mean_returns(gr)
    efw_r = ef_weights(mvp_weights(cm), tp_weights(cm, mr), seq(-3, 3, 0.1))
    efp_r = ef_points(efw_r, cm, mr)
    efw_gr = ef_weights(mvp_weights(gcm), tp_weights(gcm, gmr), seq(-3, 3, 0.1))
    efp_gr = ef_points(efw_gr, gcm, gmr)
    efp = data.frame(n = c(colnames(r[-1]), colnames(gr[-1])), vol = c(volatilities(cm), volatilities(gcm)),
                     mr = c(mr, gmr), row.names = NULL)
    g = c()
    for (i in efp$n[1:length(colnames(r[-1]))]) {
      g = c(g, ifelse(identical(names(which(setNames(unlist(g_react(), use.names = F),
                                                     rep(names(g_react()), lengths(g_react()))) == i)),
                                character(0)),
                      "Excluded", names(which(setNames(unlist(g_react(), use.names = F),
                                                       rep(names(g_react()), lengths(g_react()))) == i))))
    }
    efp$g = c(g, colnames(gr[-1]))
    efp
  })
  
  output$effFrontierPlot = renderPlot({
    r = r_react(); gr = gr_react()
    cm = cov_mat(r); mr = mean_returns(r)
    gcm = cov_mat(gr); gmr = mean_returns(gr)
    efw_r = ef_weights(mvp_weights(cm), tp_weights(cm, mr), seq(-3, 3, 0.1))
    efp_r = as.data.frame(ef_points(efw_r, cm, mr))
    efw_gr = ef_weights(mvp_weights(gcm), tp_weights(gcm, gmr), seq(-3, 3, 0.1))
    efp_gr = as.data.frame(ef_points(efw_gr, gcm, gmr))
    g = c()
    if (input$intervalButton == "1d") {fxlim = 1; fylim = 1}
    if (input$intervalButton == "1wk") {fxlim = 2.5; fylim = 4.5}
    if (input$intervalButton == "1mo") {fxlim = 5; fylim = 15}
    ggplot() +
      geom_path(aes(x = efp_r[,1], y = efp_r[,2]), alpha = 0.5) +
      geom_point(aes(x = mvp_point(efp_r)[1], y = mvp_point(efp_r)[2], color = "MVP"), size = 2) +
      geom_point(aes(x = tp_point(efp_r)[1], y = tp_point(efp_r)[2], color = "TP"), size = 2) +
      geom_text(aes(x = tp_point(efp_r)[1], y = tp_point(efp_r)[2], label = "SMI"),
                vjust = -2) +
      geom_path(data = efp_gr, aes(x = efp_gr[,1], y = efp_gr[,2]), alpha = 0.5) +
      geom_point(aes(x = mvp_point(efp_gr)[1], y = mvp_point(efp_gr)[2], color = "MVP"), size = 2) +
      geom_point(aes(x = tp_point(efp_gr)[1], y = tp_point(efp_gr)[2], color = "TP"), size = 2) +
      geom_text(aes(x = tp_point(efp_gr)[1], y = tp_point(efp_gr)[2], label = "Groups"),
                vjust = -2) +
      geom_point(data = ef_react(), aes(x = vol, y = mr, color = g), key_glyph = draw_key_point) +
      geom_text_repel(data = ef_react(), aes(x = vol, y = mr, label = n, color = g),
                      key_glyph = draw_key_point, size = 4) +
      lims(x = fxlim * c(0, 3), y = fylim * c(-0.1, 0.2)) +
      labs(x = "Volatility [%]", y = "Expected return [%]", color = NULL) +
      guides(fill = guide_legend(ncol = 2)) +
      theme(legend.position = "bottom") +
      custom_theme_shiny
  })
  
  output$effFrontierPlotHoverInfo = renderUI({
    hover = input$effFrontierPlotHover
    point = nearPoints(ef_react(), hover, xvar = "vol", yvar = "mr", threshold = 5, maxpoints = 1, addDist = T)
    if (nrow(point) == 0) {return(NULL)}
    left_pct = (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct = (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    left_px = hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px = hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    style = paste0("position: absolute; z-index: 100; background-color: rgba(245, 245, 245, 0.95);",
                   "left:", left_px + 2, "px; top:", top_px + 2, "px;", "padding: 10px 10px 0px 10px;")
    wellPanel(
      style = style,
      p(HTML(paste0(point$n, "<br/>Vol = ", round(point$vol, 3), "%", "<br/>R = ", round(point$mr, 3), "%"))))
  })
  
  bootstrap_r_react = reactive({
    input$bootstrapButton
    bootstrap(r_react())
  })
  
  bootstrap_gr_react = reactive({
    input$bootstrapButton
    bootstrap(gr_react())
  })
  
  sdMVPWeightsPlot_react = reactive({
    r = r_react(); gr = gr_react()
    mvpw_r = mvp_weights(cov_mat(r)); mvpw_gr = mvp_weights(cov_mat(gr))
    mvpw_sd_r = bootstrap_r_react()$mvp_weights_sd; mvpw_sd_gr = bootstrap_gr_react()$mvp_weights_sd
    min = min(c(mvpw_r - mvpw_sd_r, mvpw_gr - mvpw_sd_gr))
    max = max(c(mvpw_r + mvpw_sd_r, mvpw_gr + mvpw_sd_gr))
    gg1 = gg_errorbar(colnames(r[-1]), mvpw_r, mvpw_sd_r, c(min, max), "Weights",
                      "Standard deviation MVP weights", custom_theme_shiny)
    gg2 = gg_errorbar(colnames(gr[-1]), mvpw_gr, mvpw_sd_gr, c(min, max), "Weights",
                      "Standard deviation MVP weights", custom_theme_shiny)
    align_plots(gg1, gg2, align = "h")
  })
  
  output$sdMVPWeightsPlot = renderPlot({
    ggdraw(sdMVPWeightsPlot_react()[[1]])
  })
  
  output$sdMVPGroupsWeightsPlot = renderPlot({
    ggdraw(sdMVPWeightsPlot_react()[[2]])
  })
  
  sdTPWeightsPlot_react = reactive({
    r = r_react(); gr = gr_react()
    tpw_r = tp_weights(cov_mat(r), mean_returns(r)); tpw_gr = tp_weights(cov_mat(gr), mean_returns(gr))
    tpw_sd_r = bootstrap_r_react()$tp_weights_sd; tpw_sd_gr = bootstrap_gr_react()$tp_weights_sd
    min = min(c(tpw_r - tpw_sd_r, tpw_gr - tpw_sd_gr))
    max = max(c(tpw_r + tpw_sd_r, tpw_gr + tpw_sd_gr))
    gg1 = gg_errorbar(colnames(r[-1]), tpw_r, tpw_sd_r, c(min, max), "Weights",
                      "Standard deviation TP weights", custom_theme_shiny)
    gg2 = gg_errorbar(colnames(gr[-1]), tpw_gr, tpw_sd_gr, c(min, max), "Weights",
                      "Standard deviation TP weights", custom_theme_shiny)
    align_plots(gg1, gg2, align = "h")
  })
  
  output$sdTPWeightsPlot = renderPlot({
    ggdraw(sdTPWeightsPlot_react()[[1]])
  })
  
  output$sdTPGroupsWeightsPlot = renderPlot({
    ggdraw(sdTPWeightsPlot_react()[[2]])
  })
  
  output$effFrontierBootstrapPlot = renderPlot({
    if (input$intervalButton == "1d") {fxlim = 1; fylim = 1}
    if (input$intervalButton == "1wk") {fxlim = 2; fylim = 4}
    if (input$intervalButton == "1mo") {fxlim = 3; fylim = 15}
    suppressMessages(
      suppressWarnings(print(
        gg_bootstrap_ef(bootstrap_r_react()$samples_ef_points, fxlim, fylim, 1.5,
                        "Bootstrap efficient frontiers", custom_theme_shiny))))
  })
  
  cross_validation_sets_r_react = reactive({
    cross_validation_sets(r_react())
  })
  
  cross_validation_sets_gr_react = reactive({
    cross_validation_sets(gr_react())
  })
  
  output$returnShrinking = renderPlot({
    os_r_sr = unlist(out_of_sample_vec(cross_validation_sets_r_react(), seq(0, 1, 0.01),
                                       interval = input$intervalButton))
    os_gr_sr = unlist(out_of_sample_vec(cross_validation_sets_gr_react(), seq(0, 1, 0.01),
                                        interval = input$intervalButton))
    gg_shrink2D(list(os_r_sr, os_gr_sr), c("SMI", "Groups"), "Return",
                "Sharpe ratio as a function of return shrinkage factor", custom_theme_shiny)
  })
  
  output$correlationShrinking = renderPlot({
    os_r_scor = unlist(out_of_sample_vec(cross_validation_sets_r_react(), 1, seq(0, 1, 0.01),
                                         interval = input$intervalButton))
    os_gr_scor = unlist(out_of_sample_vec(cross_validation_sets_gr_react(), 1, seq(0, 1, 0.01),
                                          interval = input$intervalButton))
    gg_shrink2D(list(os_r_scor, os_gr_scor), c("SMI", "Groups"), "Correlation",
                "Sharpe ratio as a function of correlation shrinkage factor", custom_theme_shiny)
  })
  
}

shinyApp(ui, server)
