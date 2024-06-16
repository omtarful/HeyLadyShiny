library(shiny)
library(extrafontdb)
library(extrafont)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggtext)
fluency_stages <- data.frame(
  Stage = 3:9,
  Label = c("I can understand but I'm afraid to speak",
            "I can speak but I make mistakes & get stuck",
            "I can speak freely as long as the topic is familiar",
            "I speak quite well but don't feel like myself",
            "I feel at ease speaking but can't think in English",
            "I'm fluent enough to handle most situations",
            "I'm fluent and feel like myself when I speak"),
  GraphLabel = c("Starting to speak", "Speak without fear", "Starting to speak freely",
                 "Speak with confidence", "Sound natural and at ease", 
                 "Mastering nuance", "Speak fluently & effortlessly"),
  weeks_between_stages = c(40, 50, 60, 90, 100, 120, 130)
)
calculate_progress <- function(stage, weekly_hours) {
  # Define the fluency stages, cumulative hours, and labels
  # Generate data starting from today
  start_date <- Sys.Date()
  current_hours <- fluency_stages$CumulativeHours[fluency_stages$Stage == stage]
  day_increment <- day(1)
  
 
  
  # Loop through each week and assign the correct stage and date
  current_stage_index <- which(fluency_stages$Stage == stage)
  current_date <- start_date
  multipliers <- c(1, 1.2, 1.25, 1.31, 1.38, 1.46, 1.55, 1.65, 1.73, 1.8, 1.86, 1.91, 1.95, 2)
  fluency_stages$weeks_between_stages <- round(fluency_stages$weeks_between_stages /(multipliers[weekly_hours]*weekly_hours))
  fluency_stages["CumulativeHours"] <- cumsum(fluency_stages$weeks_between_stages)*weekly_hours
  total_weeks <- sum(fluency_stages$weeks_between_stages) - sum(fluency_stages$weeks_between_stages[1:(stage - 2)])
  end_date <- start_date + weeks(total_weeks)
  daily_hours = weekly_hours/7
  current_hours = fluency_stages$CumulativeHours[fluency_stages$Stage == stage]
  # Initialize an empty list to store the rows
  estimated_iterations <- as.integer((end_date - current_date) / day_increment) + 1
  
  rows <- vector("list", estimated_iterations)
  while(current_stage_index <= nrow(fluency_stages) && current_date <= end_date) {
    current_hours <- current_hours + daily_hours
    if (current_stage_index < nrow(fluency_stages) && current_hours > fluency_stages$CumulativeHours[current_stage_index + 1]) {
      current_stage_index <- current_stage_index + 1
    }
    
    # Append the weekly data
    rows[[length(rows) + 1]] <- list(
      "HoursSpent" = current_hours,
      "Stage" = fluency_stages$Stage[current_stage_index],
      "Label" = fluency_stages$Label[current_stage_index],
      "Date" = format(current_date, "%Y-%m-%d"),
      "GraphLabel" =  fluency_stages$GraphLabel[current_stage_index]
    )
    
    # Move to the next Monday
    current_date <- current_date + day_increment
  }
  
  # Create a DataFrame for the results
  weekly_data <- do.call(rbind, lapply(rows, as.data.frame))
  
  # Calculate progress
  progress <- numeric(nrow(weekly_data))
  # Extract the relevant columns and precompute indices
  stage_indices <- weekly_data$Stage - 2
  next_stage_indices <- weekly_data$Stage - 1
  
  # Vectorize access to the 'CumulativeHours' for current and next stages
  current_stage_cum_hours <- fluency_stages$CumulativeHours[stage_indices]
  next_stage_cum_hours <- fluency_stages$CumulativeHours[next_stage_indices]
  
  # Compute 'p' for all rows without the loop
  p <- ((weekly_data$HoursSpent - current_stage_cum_hours) / 
          (next_stage_cum_hours - current_stage_cum_hours)) + weekly_data$Stage
  
  # If 'progress' needs to be updated or stored
  weekly_data$progress <- p
  weekly_data$Progress <- progress
  weekly_data$Date = as.Date(weekly_data$Date)
  return(list(weekly_data, fluency_stages))
}

library("memoise")

calculate_progress = memoise(calculate_progress)
label_colors = c(
  "Starting to speak" = "#D8E8E9",
  "Speak without fear" = "#B0D0D3",
  "Starting to speak freely" = "#8AB3AB",
  "Speak with confidence" = "#72C0B1",
  "Sound natural and at ease" = "#048F63",
  "Mastering nuance" = "#027450",
  "Speak fluently & effortlessly" = "#027450"
)

# Define UI for the application
ui <- fluidPage(
  # Custom CSS to increase the font size and width of selectInput elements
  tags$head(
    tags$style(HTML("
     
      .select-input-lg .selectize-input {
        font-size: 24px;

      }
      
      h1 {
        font-weight: bold;
        font-size: 50px;
      }
      
      .plot-container {
        margin-top: 20px;
        padding-top: 10px;
      }
     
      body {
        font-family: Helvetica;
        margin-left: 50px;
      }
    
      .select-input-lg .selectize-dropdown {
        font-size: 18px;
        line-height: 40px; /* Ensure text is vertically centered */
        padding: 0 12px; /* Adjust padding to vertically center the text */

      }
   
      .select-input-wide .selectize-input {
        width: 560px !important;  /* Adjust the width as needed */
        border-radius: 100px;
        border: 2px solid #D17329;
        color: #D17329;
        background: url('triangle.svg') no-repeat 98% center !important; /* Adjust background position */
        background-size: 13px 13px !important; /* Specify a size for the background image */
        background-color: white !important; /* Ensure background color is set */
        height: 40px; /* Set a fixed height for the input box */
        line-height: 40px; /* Ensure text is vertically centered */
        padding: 0 12px; /* Adjust padding to vertically center the text */



      }
      
       .selectize-control.single .selectize-input:not(.no-arrow):after {
        content: ' ';
        display: inline;
        position: absolute;
        top: 50%;
        right: 17px;
        margin-top: -3px;
        height: 0px;
        border-style: solid;
        border-width: 5px 5px 0 5px;
        border-color: transparent transparent transparent transparent;
        appearance: none;

       }
      
      .selectize-control.single .selectize-input:not(.no-arrow):after {
        content: ' ';
        display: inline;
        position: absolute;
        top: 50%;
        right: 17px;
        margin-top: -3px;
        width: 0px;
        height: 0px;
        border-style: solid;
        border-width: 5px 5px 0 5px;
        border-color: transparent transparent transparent transparent;
        appearance: none;
      }
      
     .select-input-wide-small .selectize-input {
        width: 140px !important;
        height: 40px; /* Ensure there's enough height */
        border-radius: 100px !important;
        border: 2px solid #D17329 !important;
        color: #D17329 !important;
        background: url('triangle.svg') no-repeat 90% center !important; /* Adjust background position */
        background-size: 12px 12px !important; /* Specify a size for the background image */
        background-color: white !important; /* Ensure background color is set */
        line-height: 40px; /* Ensure text is vertically centered */
        padding: 0 12px; /* Adjust padding to vertically center the text */
        box-sizing: border-box; /* Ensure padding does 
      }

   
   
     .selectize-control.single .selectize-input:not(.no-arrow):after {
        content: ' ';
        display: inline;
        position: absolute;
        top: 50%;
        right: 17px;
        margin-top: -3px;
        width: 0px;
        height: 0px;
        border-style: solid;
        border-width: 5px 5px 0 5px;
        border-color: transparent transparent transparent transparent;
        appearance: none;
     }
      
      .selectize-control.single .selectize-input:not(.no-arrow).dropdown-active:after {
        content: ' ';
        display: inline;
        position: absolute;
        top: 50%;
        right: 17px;
        margin-top: -3px;
        width: 0px;
        height: 0px;
        border-style: solid;
        border-width: 5px 5px 0 5px;
        border-color: transparent transparent transparent transparent;
        appearance: none;
        background: url('inverted-triangle.svg') no-repeat 90% center !important; /* Adjust background position */

      }
      
      h3 {
        margin-right: 5px;
      }
      
            
    "))
  ),
  
  # Header section
  fluidRow( column(12, style = "height: 30px;") ),
  fluidRow(
    column(12, 
           h1("How long until I get results?", align = "left" )
    )
  ),
  
  # First Row
  fluidRow(
    # Upper Left Section (8 columns)
    column(8, 
           # First line with ability input
           tags$div(
             style = "display: flex; align-items: center; margin-bottom: -30px;",
             h3("If your current ability is"),
             tags$div(
               style = "margin-left: 10px;",
               tags$div(
                 class = "select-input-lg select-input-wide",
                 style ="font-size: 24px; width: 400px",
                 selectInput("ability", "", 
                             choices = fluency_stages$Label)
               )
             )
           ),
           # Second line with hours input
           tags$div(
             style = "display: flex; align-items: center; margin-bottom: -20px;",
             h3("and you spend"),
             tags$div(
               style = "margin-left: 10px; display: flex; align-items: center;",
               tags$div(
                 class = "select-input-lg select-input-wide-small",
                 style="width: 130px; font-size:24px",
                 selectInput("hours", "", 
                             choices = c("1 hours", "2 hours", "3 hours",
                                         "4 hours", "5 hours", "6 hours",
                                         "7 hours", "8 hours", "9 hours",
                                         "10 hours", "11 hours", "12 hours",
                                         "13 hours", "14 hours"))
               ),
               tags$div(
                 style = "margin-left: 15px",
                 h3(" every week practising speaking, it will take")
               )
             )
           ),
           # Third line with final message
           tags$div(
             style = "display: flex; align-items: center; margin-bottom: 50px; margin-right: 20px;",
             h3(htmlOutput("monthsText")),
             h3(" to speak English fluently & effortlessly.")
           )
    ),
    # Upper Right Section (4 columns)
    column(4, 
           # Using HTML to apply specific styles to parts of the text
           h3(htmlOutput("savingsText")),
           # CSS for increasing line spacing globally within this column
           tags$head(
             tags$style(HTML(".shiny-text-output { line-height: 2; }"))  # Increase line height for better spacing
           )

        
    )
  ),
  
  # Second Row
  fluidRow(
    # Bottom Left Section (8 columns)
    column(8, 
           tags$div(
           card(plotOutput("fluencyGraph", width = "90%"), full_screen = TRUE)
           )
    ),
    # Bottom Right Section (4 columns)
    column(4, 
           tags$div(
           style = "margin-right: 50px;",
           card(plotOutput("costComparison", width = "100%"), full_screen = TRUE)
           )
    )
  )
)

# Define server logic required to draw the plots
server <- function(input, output) {
  # Reactive values to cache the plots
  cached_fluency_graph <- reactiveVal()
  cached_cost_comparison <- reactiveVal()
  toListen <- reactive({
    list(input$ability,input$hours)
  })
  
  observeEvent( list(input$ability,input$hours), {
    # Convert ability input to corresponding stage
    stage <- switch(input$ability,
                    "I can understand but I'm afraid to speak" = 3,
                    "I can speak but I make mistakes & get stuck" = 4,
                    "I can speak freely as long as the topic is familiar" = 5,
                    "I speak quite well but don't feel like myself" = 6,
                    "I feel at ease speaking but can't think in English"= 7,
                    "I'm fluent enough to handle most situations"=8,
                    "I'm fluent and feel like myself when I speak"=9)
    
    # Convert hours input to numeric
    weekly_hours <- switch(input$hours,
                           "1 hours" = 1,
                           "2 hours" = 2,
                           "3 hours" = 3,
                           "4 hours" = 4,
                           "5 hours" = 5,
                           "6 hours" = 6,
                           "7 hours" = 7,
                           "8 hours" = 8,
                           "9 hours" = 9,
                           "10 hours" = 10,
                           "11 hours" = 11,
                           "12 hours" = 12,
                           "13 hours" = 13,
                           "14 hours" = 14)

    # Calculate progress data
    data <- calculate_progress(stage, weekly_hours)
    progress_data = data[[1]]
    fluency_stages = data[[2]]
    # Define y-axis breaks based on your analysis
    y_breaks <- fluency_stages$CumulativeHours
    y_labels <- fluency_stages$GraphLabel
    integer_points <- progress_data %>%
      group_by(GraphLabel) %>%
      dplyr::filter(HoursSpent == min(HoursSpent)) %>%
      ungroup()  
    integer_points$HoursSpent = round(integer_points$HoursSpent)
    # Placeholder for fluency prediction graph
    
      fluency_plot = ggplot(progress_data, aes(x = Date, y = HoursSpent)) +
        geom_line(color = "black", size = 0.5) +  # Or geom_smooth, depending on your preference
        scale_y_continuous(
          breaks = y_breaks[(stage-2):nrow(fluency_stages)],  # Your custom breaks
          labels = y_labels[(stage-2):nrow(fluency_stages)],  # Corresponding labels,
          expand = expansion(mult = c(0, 0.1)),
          limits = c( min(y_breaks[(stage-2):nrow(fluency_stages)]), NA) 
        ) +
        geom_ribbon(aes( ymin = min(HoursSpent), ymax = HoursSpent, fill = factor(GraphLabel)), alpha = 0.6, show.legend = FALSE, outline.type =  "lower") +
        scale_fill_manual(values = label_colors[(stage-2):7]) +
        geom_point(data = integer_points[-1,], shape =21, fill = "white", color = "black", size = 5) +
        scale_x_date(
          breaks = c(min(progress_data$Date), max(progress_data$Date)),  # Set breaks to only the first and last date
          labels = function(x) ifelse(x == min(progress_data$Date), "Today", format(x, "%b\n%Y"))
        ) + 
        labs(y = "FLUENCY", x="TIME") +
        theme_minimal() +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray"),
          panel.grid.minor.y = element_blank(),
          axis.title =element_text(size = 20),
          axis.title.y = element_text(margin = margin(t = 0, r = -2, b = 0, l = 0), face = "bold"), # Adjust r (right) margin to bring the title closer to the axis
          axis.title.x = element_text(face = "bold") ,
          axis.text= element_text(size = 15),
          axis.line.x = element_line(size = 1, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
          plot.margin = margin(t = 20, r = 20, b = 20, l = 20) # Adjust plot margins if needed
          
        ) 
      cached_fluency_graph(fluency_plot)
    output$fluencyGraph <- renderPlot({
      cached_fluency_graph()


    })
    min_date <- min(progress_data$Date)
    max_date <- max(progress_data$Date)
    
    # Using interval and %/% to calculate full months
    months_difference <- as.numeric(interval(min_date, max_date) %/% months(1))
    years = months_difference/12
    months = months_difference %% 12
    cost = 0
    if(months_difference < 12){
      cost = months_difference*39
    } else{
      cost =  months_difference*24.916667
    }
    hours = max(progress_data$HoursSpent) - min(progress_data$HoursSpent)
    tutor_cost = hours*18
    # Placeholder for cost comparison plot
    data <- data.frame(
      Category = c("ENGLISH TUTOR", "HEY LADY!"),
      Cost = c(round(tutor_cost), round(cost)),
      Save = c(0, tutor_cost - cost) # Savings to display alongside the bars
    )
    
    
    data$Category <- factor(data$Category, levels = data$Category[order(-data$Cost)])
    min_cost = 800
    data$PlotCost <- ifelse(data$Cost < min_cost, min_cost, data$Cost)
    # Example data for plotting
    cost_plot = ggplot(data, aes(x = Category, y = PlotCost, fill = Category)) +
      geom_col(show.legend = FALSE, width = 0.8, position = position_dodge(width = 0.9)) +  # Adjust width for spacing
      geom_text(aes(label = scales::dollar(Cost)), vjust = 1.2, family = "Helvetica", size = 8, color = "white", face = "bold", position = position_dodge(width = 0.5)) +  # Adjusted vjust and position dodge
      scale_fill_manual(values = c("#D17329", "#048F63")) +
      scale_x_discrete(expand = expansion(mult = c(0, 0))) +  # Remove expansion on x-axis
      labs(
        title = "Cost comparison", 
        subtitle = paste("<span style='color:#048F63;'>", round(hours), " hours", "</span><span style='color:black;'> over</span><span style='color:#048F63;'>", round(months_difference), " months</span>")
      ) +
      theme_minimal() +
      theme(
        plot.margin = margin(30, 80, 30, 80),
        text = element_text(family = "Helvetica"),  # Set general text properties
        axis.text.x = element_text(face = "bold", size = 17),
        axis.text.y = element_blank(),  # Hide y-axis text
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family = "Helvetica", face = "bold", size = 40, hjust = 0.5, vjust = 1.5),  # Make title bold and larger
        plot.subtitle = element_markdown(family = "Helvetica", face = "bold", size = 25, hjust = 0.5, vjust = 0.5), # Make subtitle bold and slightly smaller than title
        axis.line.x = element_blank()  # Remove the default x-axis line
      ) +
      geom_segment(aes(x = 0.5, xend = length(unique(data$Category)) + 0.5, y = 0, yend = 0), size = 1, color = "black") +  # Add custom x-axis line
      geom_rect(aes(xmin = 1.6, xmax = 2.4, ymin = PlotCost[1], ymax = PlotCost[2]), fill = "white", colour = "black", linetype = "dashed") +
      geom_text(aes(x = 2, y = Cost[1]/2, label = paste("SAVE\n", scales::dollar(Cost[[1]] - Cost[[2]]))), color = "black", family = "Helvetica", size = 8, fontface = "bold")
    cached_cost_comparison(cost_plot)
    output$costComparison <- renderPlot({
      
     cached_cost_comparison()
    })
    output$savingsText <- renderUI({
    HTML(paste0("Compared to a pay-per-hour tutor, you would <span style='color: #D17329; font-weight: bold; line-height: 3; '>save around $", round(tutor_cost - cost), "</span> by practising with Hey Lady!"))
    })
    output$monthsText <- renderUI({
      HTML(paste0("around <span style='color:#048F63;'>", round(hours), " hours", "</span><span style='color:black;'> over </span><span style='color:#048F63;'>", round(months_difference), " months</span>"))
    })
  })
 
}

runApp(shinyApp(ui, server))

