#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr)
source("tools.R")
options(shiny.maxRequestSize=30*1024^2) 

# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Model Validation"),
   
   mainPanel(
     tabsetPanel(
       tabPanel("Model Information",
          column(4,
            helpText("Validation points"),
            fileInput("val_points_file", "Validation Points"),
            uiOutput("val_points_file_options")
          ),
          column(4,
            helpText("Model options"),
            fluidRow(
              column(8, sliderInput("num_exp_vars", "Number of explanatory variables", min = 0, max=8, step=1, value=0)),
              column(4, textInput("rmse", "RMSE")),
              uiOutput("exp_vars_options")
            ),
            fluidRow(
              column(6,
                helpText("Variable"),
                selectInput("intercept_dummy", label=NULL, choices="Intercept", selected="Intercept"),
                uiOutput("exp_vars_names_options")
              ),
              column(6,
                helpText("Coefficient"),
                textInput("intercept_value", label=NULL),
                uiOutput("exp_vars_values_options")
              )
            )
          ),
          column(4,
            helpText("Continuous data"),
            checkboxInput("include_cont", "Include continuous data for validation", value = FALSE),
            uiOutput("continuous_file_options"),
            uiOutput("continuous_file_datetime_options"),
            uiOutput("same_headings_option"),
            uiOutput("cont_heading_options")
          )
       ),
       tabPanel("Validation",
          uiOutput("warnings"),
          uiOutput("ts_plot_output"),
          uiOutput("ts_plot_output_zoomed"),
          uiOutput("scatter_plot_choices"),
          plotOutput("scatter_plot"),
          fluidRow(
            column(6, plotOutput("year_boxplot")),
            column(6, plotOutput("season_boxplot"))
          ),
          dataTableOutput("data_table")
       )
     )
   )

)

# Define server logic 
server <- function(input, output) {
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Get the raw content of the validation points file
  val_points_file_raw <- reactive({
    
    file <- input$val_points_file
    
    if(is.null(file)) 
      return(NULL)
      
    raw <- read.csv(file$datapath)
    
    
  })
  
  # Get the column headings of the validation points file
  val_points_headings <- reactive({
    
    raw <- val_points_file_raw()
    
    if(is.null(raw))
      return(NULL)
    
    headings <- names(raw)
    
  })

  # UI Options for the validation points file
  output$val_points_file_options <- renderUI({
    
    headings <- val_points_headings()
    
    if(is.null(headings))
      return(NULL)
    
    tagList(  
      # Ask which column has the date time
      selectInput("val_datetime_column", "Date/time column heading", choices = headings),
      
      # Ask what format the datetime is in
      selectInput("val_dateformat","Date/time format:", c(
        "mm/dd/yyyy hh:mm" = "%m/%d/%Y %H:%M",
        "yyyy-mm-dd hh:mm" = "%Y-%m-%d %H:%M",
        "yyyymmdd hhmm" = "%Y%m%d %H%M",
        "mm/dd/yy hh:mm" = "%m/%d/%y %H:%M",
        "yyyymmddhhmm" = "%Y%m%d%H%M",
        "yyyymmddhhmmss" = "%Y%m%d%H%M%S")),
      
      # Ask for the column heading of the modelled variable
      selectInput("val_mod_column", "Modelled variable column heading", choices = headings)
    )
      
    
  })
  
  output$exp_vars_names_options <- renderUI({
    
    headings <- val_points_headings()
    if(is.null(headings))
      return(NULL)
    
    headings <- headings[headings != input$val_datetime_column]
    
    nvars <- input$num_exp_vars
    if(nvars==0)
      return(NULL)
    
    lapply(1:nvars, function(i) {
      selectInput(paste0("var", i), label=NULL, choices=headings)
    })
    
    
  })
  
  output$exp_vars_values_options <- renderUI({
    
    headings <- val_points_headings()
    if(is.null(headings))
      return(NULL)

    headings <- headings[headings != input$val_datetime_column]

    nvars <- input$num_exp_vars
    if(nvars==0)
      return(NULL)
    
    lapply(1:nvars, function(i) {
      textInput(paste0("var", i, "value"), label=NULL)
    })
    
  })
  
  exp_var_names <- reactive({
    
    vars <- vector()
    nvars <- input$num_exp_vars
    for(i in 1:nvars) {
      vars[length(vars) + 1] <- input[[paste0("var", i)]]
    }
    vars
    
  })
  
  exp_var_coefs <- reactive({
    
    coefs <- vector()
    nvars <- input$num_exp_vars
    for(i in 1:nvars) {
      coefs[length(coefs) + 1] <- as.numeric(input[[paste0("var", i, "value")]])
    }
    coefs
    
  })
  
  output$continuous_file_options <- renderUI({
    
    if(!(input$include_cont))
      return(NULL)
    
    tagList(
      
      fileInput("cont_file", "Continuous data"),
      checkboxInput("same_datetime", "Same datetime options as validation data", value=TRUE)
      
    )
    
  })
  
  # Get the raw content of the validation points file
  cont_file_raw <- reactive({
    
    file <- input$cont_file
    
    if(is.null(file)) 
      return(NULL)
    
    raw <- read.csv(file$datapath)
    
    
  })
  
  # Get the column headings of the validation points file
  cont_headings <- reactive({
    
    raw <- cont_file_raw()
    
    if(is.null(raw))
      return(NULL)
    
    headings <- names(raw)
    
  })
  
  output$continuous_file_datetime_options <- renderUI({
    
    if(is.null(input$same_datetime))
      return(NULL)
    if(input$same_datetime)
      return(NULL)
    if(is.null(input$cont_file))
      return(NULL)
    
    headings <- cont_headings()
    
    tagList(
      
      selectInput("cont_datetime_column", "Date/time column heading", choices = headings),
      
      # Ask what format the datetime is in
      selectInput("cont_dateformat","Date/time format:", c(
        "mm/dd/yyyy hh:mm" = "%m/%d/%Y %H:%M",
        "yyyy-mm-dd hh:mm" = "%Y-%m-%d %H:%M",
        "yyyymmdd hhmm" = "%Y%m%d %H%M",
        "mm/dd/yy hh:mm" = "%m/%d/%y %H:%M",
        "yyyymmddhhmm" = "%Y%m%d%H%M",
        "yyyymmddhhmmss" = "%Y%m%d%H%M%S"))
    )
    
  })
  
  cont_datetime_heading <- reactive({
    
    if(is.null(input$same_datetime))
      return(NULL)
    
    if(input$same_datetime) {
      datetime_heading <- input$val_datetime_column
    } else {
      datetime_heading <- input$cont_datetime_column
    }
    
    datetime_heading
    
  })
  
  
  cont_datetime_format <- reactive({
    
    if(is.null(input$same_datetime))
      return(NULL)
    
    if(input$same_datetime) {
      datetime_format <- input$val_dateformat
    } else {
      datetime_heading <- input$cont_dateformat
    }
    
    datetime_format
    
  })
  
  output$same_headings_option <- renderUI({
    
    if(is.null(input$cont_file))
      return(NULL)
    
    c_headings <- cont_headings()
    c_headings <- c_headings[c_headings != cont_datetime_heading()]
    vars <- exp_var_names()
    if(all(vars %in% c_headings)) {
      check <- TRUE
    } else {
      check <- FALSE
    }
    
    checkboxInput("same_headings", "Same headings as validation data", value=check) 
    
  })
  
  output$cont_heading_options <- renderUI({
    
    if(is.null(input$cont_file))
      return(NULL)
    if(is.null(input$same_headings))
      return(NULL)
    if(input$same_headings)
      return(NULL)
    
    c_headings <- cont_headings()
    c_headings <- c_headings[c_headings != cont_datetime_heading()]
    vars <- exp_var_names()
    
    lapply(vars, function(i) {
      selectInput(paste0(i,"_cont_heading"), label=paste("Column corresponding to", i), choices=c_headings)
    })
    
    
  })
  
  exp_vars_cont_names <- reactive({
    
    if(is.null(input$cont_file))
      return(NULL)
    if(is.null(input$same_headings))
      return(NULL)
    
    if(input$same_headings) {
      vars_cont <- exp_var_names()
    } else {
      nms <- exp_var_names()
      vars_cont <- vector()
      for(i in 1:length(nms)) {
        vars_cont[length(vars_cont) + 1] <- input[[paste0(nms[i],"_cont_heading")]]
      }
    }
    
    vars_cont
    
  })
  
  rmse <- reactive({
    as.numeric(input$rmse)
  })
  
  val_points_data <- reactive({
    
    raw <- val_points_file_raw()
    sigma <- rmse()
    
    raw[,input$val_datetime_column] <- as.POSIXct(raw[,input$val_datetime_column], format=input$val_dateformat)
    
    raw %>% 
      predict_series(exp_var_names(), exp_var_coefs(), as.numeric(input$intercept_value)) %>%
      rename_("Measured" = input$val_mod_column, "datetime" = input$val_datetime_column) %>%
      mutate(Residual = Measured - Predicted, 
             Sigma_Distance = Residual / sigma,
             Year = year(datetime),
             Season = season(datetime))
    
    
  })
  
  cont_data <- reactive({
    
    if(!(input$include_cont) )
      return(NULL)
    
    raw <- cont_file_raw()
    sigma <- rmse()
    
    raw[,cont_datetime_heading()] <- as.POSIXct(raw[,cont_datetime_heading()], format=cont_datetime_format())
    
    raw %>%
      predict_series(exp_vars_cont_names(), exp_var_coefs(), as.numeric(input$intercept_value)) %>%
      mutate(pUp1 = Predicted + sigma,
             pUp2 = Predicted + 2 * sigma,
             pUp3 = Predicted + 3 * sigma,
             pDown1 = Predicted - sigma,
             pDown2 = Predicted - 2 * sigma,
             pDown3 = Predicted - 3 * sigma)
    
  })
  
  text_warnings <- reactive({
    
    data <- val_points_data() %>%
      mutate(High_Neg_Residual = as.numeric(Sigma_Distance <= -2),
             High_Pos_Residual = as.numeric(Sigma_Distance >= 2),
             Very_High_Residual = as.numeric(abs(Sigma_Distance) >= 3))
    print(data)
    
    #Check for high residuals
    if(any(data$High_Neg_Residual == 1) | any(data$High_Pos_Residual == 1)) {
      high_res_1 <- TRUE
    } else {
      high_res_1 <- FALSE
    }
    
    #Check for very high residuals
    if(any(data$Very_High_Residual == 1)) {
      very_high_res_1 <- TRUE
    } else {
      very_high_res_1 <- FALSE
    }
    
    #Check for two consecutive positive or negative high residuals ()
    if(high_res_1) { #Only need to do this check if there's at least 1 high residual
      high_neg_2_sum <- vector()
      high_pos_2_sum <- vector()
      for(i in 2:nrow(data)) {
        high_neg_2_sum[length(high_neg_2_sum) + 1] <- sum(data$High_Neg_Residual[(i-1):i])
        high_pos_2_sum[length(high_pos_2_sum) + 1] <- sum(data$High_Pos_Residual[(i-1):i])
      }
      
      if(any(high_neg_2_sum == 2)) {
        high_neg_2 <- TRUE
      } else {
        high_neg_2 <- FALSE
      }
      
      if(any(high_pos_2_sum == 2)) {
        high_pos_2 <- TRUE
      } else {
        high_pos_2 <- FALSE
      }
    } else {
      #There can't be two in a row if there isn't even one
      high_pos_2 <- FALSE
      high_neg_2 <- FALSE
    }
    
    #Check for three consecutive positive or negative high residuals
    if(high_pos_2 || high_neg_2) {
      high_neg_3_sum <- vector()
      high_pos_3_sum <- vector()
      for(i in 3:nrow(data)) {
        high_neg_3_sum[length(high_neg_3_sum) + 1] <- sum(data$High_Neg_Residual[(i-2):i])
        high_pos_3_sum[length(high_pos_3_sum) + 1] <- sum(data$High_Pos_Residual[(i-2):i])
      }
      
      if(any(high_neg_3_sum == 3)) {
        high_neg_3 <- TRUE
      } else {
        high_neg_3 <- FALSE
      }
      
      if(any(high_pos_3_sum == 3)) {
        high_pos_3 <- TRUE
      } else {
        high_pos_3 <- FALSE
      }
    } else {
      high_neg_3 <- FALSE
      high_pos_3 <- FALSE
    }
    
    if(very_high_res_1) {
      #Check for 2 consecutive very high residuals, regardless of sign
      very_high_2_sum <- vector()
      for(i in 2:nrow(data)) {
        very_high_2_sum[length(very_high_2_sum) + 1] <- sum(data$Very_High_Residual[(i-1):i])
      }
      if(any(very_high_2_sum == 2)) {
        very_high_2 <- TRUE
      } else {
        very_high_2 <- FALSE
      }
    } else {
      very_high_2 <- FALSE
    }
    
    warnings <- vector()
    if(high_res_1) {
      warnings[length(warnings) + 1] <- "At least one sample has a residual more than 2 * RMSE"
    }
    if(high_pos_2) {
      warnings[length(warnings) + 1] <- "Two samples in a row have a positive residual more than 2 * RMSE"
    } 
    if(high_neg_2) {
      warnings[length(warnings) + 1] <- "Two samples in a row have a negative residual mroe than 2 * RMSE"
    }
    if(high_pos_3) {
      warnings[length(warnings) + 1] <- "Three samples in a row have a positive residual more than 2 * RMSE"
    }
    if(very_high_res_1) {
      warnings[length(warnings) + 1] <- "At least one sample has a very high residual, more than 3 * RMSE"
    }
    if(very_high_2) {
      warnings[length(warnings) + 1] <- "Two samples in a row have a very high residual, more than 3 * RMSE"
    }
    
    warnings
    
    
  })
  
  output$warnings <- renderUI({
    
    print(text_warnings())
    
    if(length(text_warnings()) != 0) {
      txt <- paste(text_warnings(), collapse="<br/>")
    } else {
      return(NULL)
    }
    txt <- paste("<b>Warnings</b><br/>", txt, sep="")
    HTML(txt)
    
  })
  
  output$time_series <- renderPlot({

    if(!(input$include_cont))
      return(NULL)

    data <- cont_data()
    points <- val_points_data() 
    points <- points %>%
      mutate(High_Residual = abs(Sigma_Distance) > 2) %>%
      select(datetime, Measured, High_Residual) %>%
      na.omit()

    p <- ggplot(data = data) +
      geom_ribbon(aes(x=datetime, ymin=pDown3, ymax=pUp3), fill="#BFDBFF") +
      geom_ribbon(aes(x=datetime, ymin=pDown2, ymax=pUp2), fill="#CCE1FF") +
      geom_ribbon(aes(x=datetime, ymin=pDown1, ymax=pUp1), fill="#F0F4FC") +
      geom_line(aes(x = datetime, y = Predicted), color="steelblue3") +
      geom_point(data=points, aes(x=datetime, y=Measured, color=High_Residual), size=3) +
      scale_color_manual(values = c("black", "orangered1")) +
      scale_x_datetime() +
      xlab("Datetime") + ylab("Modeled Value")
    p


  })

  output$ts_plot_output <- renderUI({

    if(!(input$include_cont))
      return(NULL)

    plotOutput("time_series", width="100%", height="400px",
      brush = brushOpts(id = "time_series_brush", resetOnNew = FALSE, direction="x"))

  })

  #Observer for the brush on valPlot
  observe({
    brush <- input$time_series_brush
    if (!is.null(brush)) {
      ranges$x <- c(as.POSIXct(brush$xmin, origin="1970-01-01"), as.POSIXct(brush$xmax, origin="1970-01-01"))
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  output$zoomed_series <- renderPlot({

    if(!(input$include_cont))
      return(NULL)

    #Get the continuous data for the plot
    data <- cont_data()
    if(!is.null(ranges$x)) {
      data <- data[data$datetime > ranges$x[1] & data$datetime < ranges$x[2], ]
    }

    #Get the points, if any for the plot
    points <- val_points_data()
    if(!is.null(ranges$x)) {
      points <- points[points$datetime > ranges$x[1] & points$datetime < ranges$x[2], ]
    }
    points <- points %>%
      mutate(High_Residual = abs(Sigma_Distance) > 2) %>%
      select(datetime, Measured, High_Residual) %>%
      na.omit()

    p <- ggplot(data = data) +
      geom_ribbon(aes(x=datetime, ymin=pDown3, ymax=pUp3), fill="#BFDBFF") +
      geom_ribbon(aes(x=datetime, ymin=pDown2, ymax=pUp2), fill="#CCE1FF") +
      geom_ribbon(aes(x=datetime, ymin=pDown1, ymax=pUp1), fill="#F0F4FC") +
      geom_line(aes(x = datetime, y = Predicted), color="steelblue3") +
      geom_point(data=points, aes(x=datetime, y=Measured, color=High_Residual), size=3) +
      scale_color_manual(values = c("black", "orangered1")) +
      scale_x_datetime() +
      xlab("Datetime") + ylab("Modeled Value")
    p

  })

  output$ts_plot_output_zoomed <- renderUI({

    if(!(input$include_cont))
      return(NULL)

    plotOutput("zoomed_series")

  })
  
  output$data_table <- renderDataTable({
    
    val_points_data() %>% 
      select(one_of(c("datetime", "Measured", "Predicted", "Residual", "Sigma_Distance", exp_var_names())))
    
  })
  
  output$scatter_plot_choices <- renderUI({
    
    ch <- c("datetime", "Predicted", exp_var_names())
    selectInput("scatter_x", "Plot residual against", choices=ch, selected="datetime")
    
  })
  
  output$scatter_plot <- renderPlot({

    data <- val_points_data()
    x_var <- input$scatter_x
    
    ggplot(data = data) +
      geom_point(aes_string(x = x_var, y = "Residual"), color="blue") +
      geom_hline(yintercept=0, color="black") +
      xlab(x_var) + ylab("Residual") + ggtitle(paste("Residual by", x_var))
    
  })
  
  output$year_boxplot <- renderPlot({
    
    data <- val_points_data()
    
    #Find the annotation text for number of smaples for each year
    y_coord <- vector()
    n <- vector()
    range <- max(data$Residual) - min(data$Residual)
    for(i in unique(data$Year)[order(unique(data$Year))]) {
      y_coord[length(y_coord) + 1] <- max(data[data$Year==i,"Residual"]) + range * 0.05
      n[length(n) + 1] <- nrow(data[data$Year==i,])
    }
    
    #Find the y range to expand it a bit to make room for the annotation
    y_upper <- max(data$Residual) + range * 0.1
    y_lower <- min(data$Residual) - range * 0.1
    
    boxplot(Residual ~ Year, data = data, boxwex=0.3, las = 1, ylim=c(y_lower, y_upper),
            main = "Residual by year", xlab="Year", ylab = "Residual")
    abline(h = 0, col="blue")
    text(x = 1:length(unique(data$Year)), y = y_coord, labels = n)
    
  })
  
  output$season_boxplot <- renderPlot({
    
    data <- val_points_data()
    
    #Get the annotation text for the count of each season
    y_coord <- vector()
    n <- vector()
    range <- max(data$Residual) - min(data$Residual)
    for(i in levels(data$Season)) {
      y_coord[length(y_coord) + 1] <- max(data[data$Season == i, "Residual"]) + range * 0.05
      n[length(n) + 1] <- nrow(data[data$Season == i,])
    }
    
    #Find the y range to expand it a bit to make room for the annotation
    y_upper <- max(data$Residual) + range * 0.1
    y_lower <- min(data$Residual) - range * 0.1
    
    boxplot(Residual ~ Season, data=data, boxwex = 0.3, las=1, ylim = c(y_lower, y_upper),
            main = "Residual by season", xlab = "Season", ylab="Residual")
    abline(h = 0, col="blue")
    text(x = 1:length(unique(data$Season)), y = y_coord, labels = n)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

