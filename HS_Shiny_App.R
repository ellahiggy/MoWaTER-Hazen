#--------------------------------------------------------------
#- Filename: Shiny App Hazen & Sawyer Trains
#- Author: PJ Williams, Ella Higginbotham, Lauren Walker, Cyril Pillai, Henry Burch
#- Date: 06/07/22
#- Description: Creation of a shiny app to observe the plot trends within each train of the water treatment system.
#--------------------------------------------------------------

library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)

# Load Data Below & then run app #----------------------------------------------

load("HSMaster - Hazen and Sawyer.rda")

Train_1 <- select(HSMaster, starts_with("Date") | ends_with("TR1"))

Train_2 <- select(HSMaster, starts_with("Date") | ends_with("TR2"))

Train_3 <- select(HSMaster, starts_with("Date") | ends_with("TR3"))

Train_4 <- select(HSMaster, starts_with("Date") | ends_with("TR4"))

Train_5 <- select(HSMaster, starts_with("Date") | ends_with("TR5"))

# Separating Trains into useful variables
Train_1_Good_Variables <- Train_1[,c("Date_Time", "P_f_TR1", 
                                     "Per_R_a_TR1","Nt_DP_TR1", "Sp_Fl_TR1",
                                     "DP_n_TR1", "NCp_TR1")]

Train_2_Good_Variables <- Train_2[,c("Date_Time", "P_f_TR2", 
                                     "Per_R_a_TR2","Nt_DP_TR2", "Sp_Fl_TR2",
                                     "DP_n_TR2", "NCp_TR2")]

Train_3_Good_Variables <- Train_3[,c("Date_Time","P_f_TR3", 
                                     "Per_R_a_TR3","Nt_DP_TR3", "Sp_Fl_TR3",
                                     "DP_n_TR3", "NCp_TR3")]

Train_4_Good_Variables <- Train_4[,c("Date_Time","P_f_TR4", 
                                     "Per_R_a_TR4","Nt_DP_TR4", "Sp_Fl_TR4",
                                     "DP_n_TR4", "NCp_TR4")]

Train_5_Good_Variables <- Train_5[,c("Date_Time","P_f_TR5", 
                                     "Per_R_a_TR5","Nt_DP_TR5", "Sp_Fl_TR5",
                                     "DP_n_TR5", "NCp_TR5")]

# Rename the Variables to more fitting names for each train
colnames(Train_1_Good_Variables) <- c("Date", "Feedwater_Pressure", "Percent_Recovery", 
                                      "Net_Driving_Pressure", "Specific_Flux",
                                      "Normalized_Differential_Pressure",
                                      "Normalized_Permeate_Conductivity")

colnames(Train_2_Good_Variables) <- c("Date", "Feedwater_Pressure", "Percent_Recovery", 
                                      "Net_Driving_Pressure", "Specific_Flux",
                                      "Normalized_Differential_Pressure",
                                      "Normalized_Permeate_Conductivity")

colnames(Train_3_Good_Variables) <- c("Date", "Feedwater_Pressure", "Percent_Recovery", 
                                      "Net_Driving_Pressure", "Specific_Flux",
                                      "Normalized_Differential_Pressure",
                                      "Normalized_Permeate_Conductivity")

colnames(Train_4_Good_Variables) <- c("Date", "Feedwater_Pressure", "Percent_Recovery", 
                                      "Net_Driving_Pressure", "Specific_Flux",
                                      "Normalized_Differential_Pressure",
                                      "Normalized_Permeate_Conductivity")

colnames(Train_5_Good_Variables) <- c("Date", "Feedwater_Pressure", "Percent_Recovery", 
                                      "Net_Driving_Pressure", "Specific_Flux",
                                      "Normalized_Differential_Pressure",
                                      "Normalized_Permeate_Conductivity")
#-------------------------------------------------------------------------------

############################
### Labels
# "Recovery at Actual Conditions" = "R_a_TR1", "Recovery (percentage)" = "Per_R_a_TR1", 
# "Feed/Brine Conductivity" = "C_fb_a_TR1",
# "Feed/Brine Osmotic Pressure" = "OP_fb_a_TR1", 
# "Permeate Osmotic Pressure" = "OP_p_a_TR1",
# "Percent Salt Passage" = "SP_a_TR1", "Normalized Salt Passage" = "SP_n_TR1", 
# "Net Driving Pressure at Actual Conditions" = "NetDp_a_TR1", 
# "Normalized Permeate Flow" = "F_p_n_TR1", 
# "Normalized Differential Pressure" = "DP_n_TR1", 
# "Normalized Differential Pressure Stage 1" = "DP_n_s1_TR1", 
# "Normalized Differential Pressure Stage 2" = "DP_n_s2_TR1", 
# "Average Osmotic Pressure Gradient" = "OP_TR1",
# "Net Driving Pressure" = "Nt_DP_TR1", "Flux" = "Flux_TR1",
# "Specific Flux" = "Sp_Fl_TR1", "Normalized Permeate Conductivity" = "NCp_TR1"
############################
  
# Define UI ----
ui <- fluidPage(
  theme = shinytheme("darkly"),

  # App title ----
  titlePanel("Hazen & Sawyer Train Data"),
  
  # Sidebar layout with input and output definitions ----
  navbarPage(
    "Trains:",
    
    # Tab for Train 1
    tabPanel("Train 1",
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 selectInput(
                   inputId = "x1",
                   label = h3('X-axis'),
                   choices = c("Date"),
                   selected = "Date"
                 ),
                 
                 selectInput(
                   inputId = "y1",
                   label = h3("Y-axis"),
                   choices = c(
                     "Feedwater_Pressure", "Percent_Recovery", 
                     "Net_Driving_Pressure", "Specific_Flux",
                     "Normalized_Differential_Pressure",
                     "Normalized_Permeate_Conductivity"
                   ),
                   selected = "Feedwater_Pressure"
                 ),
                 
                 dateRangeInput('date',
                           label = h4('Date Range'),
                           start = min(Train_1_Good_Variables$Date),
                           end =  max(Train_1_Good_Variables$Date)
                 )
               ),
               
               # Main panel for displaying Train 1 Plot ----
               mainPanel(
                 #plotOutput(outputId = "plot1")
                 plotlyOutput(outputId = "plotly")
                 )
             )
             ),
    
    # Tab for Train 2 ----
    tabPanel("Train 2",
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 selectInput(
                   inputId = "x2",
                   label = h3('X-axis'),
                   choices = c("Date"),
                   selected = "Date"
                 ),
                
                  selectInput(
                   inputId = "y2",
                   label = h3("Y-axis"),
                   choices = c(
                     "Feedwater_Pressure", "Percent_Recovery", 
                     "Net_Driving_Pressure", "Specific_Flux",
                     "Normalized_Differential_Pressure",
                     "Normalized_Permeate_Conductivity"
                   ),
                   selected = "Feedwater_Pressure"
                 ),
                 
                 dateRangeInput('date2',
                                label = h4('Date Range'),
                                start = min(Train_2_Good_Variables$Date),
                                end =  max(Train_2_Good_Variables$Date)
                 )
               ),
              
                # Main panel for displaying Train 2 Plot ----
               mainPanel(
                 #plotOutput(outputId = "plot2"),
                 plotlyOutput(outputId = "plotly2"))
             )),
    
    # Tab for Train 3 ----
    tabPanel("Train 3",
              sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 selectInput(
                   inputId = "x3",
                   label = h3('X-axis'),
                   choices = c("Date"),
                   selected = "Date"
                 ),
                
                  selectInput(
                   inputId = "y3",
                   label = h3("Y-axis"),
                   choices = c(
                     "Feedwater_Pressure", "Percent_Recovery", 
                     "Net_Driving_Pressure", "Specific_Flux",
                     "Normalized_Differential_Pressure",
                     "Normalized_Permeate_Conductivity"
                   ),
                   selected = "Feedwater_Pressure"
                  ),
                 
                 dateRangeInput('date3',
                                label = h4('Date Range'),
                                start = min(Train_3_Good_Variables$Date),
                                end =  max(Train_3_Good_Variables$Date)
                 )
               ),
              
                # Main panel for displaying Train 3 Plot ----
               mainPanel(
                 #plotOutput(outputId = "plot3"),
                 plotlyOutput(outputId = "plotly3"))
             )),
    
    # Tab for Train 4
    tabPanel("Train 4",
             sidebarLayout(
              
                # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 selectInput(
                   inputId = "x4",
                   label = h3('X-axis'),
                   choices = c("Date"),
                   selected = "Date"
                 ),
                 
                 selectInput(
                   inputId = "y4",
                   label = h3("Y-axis"),
                   choices = c(
                     "Feedwater_Pressure", "Percent_Recovery", 
                     "Net_Driving_Pressure", "Specific_Flux",
                     "Normalized_Differential_Pressure",
                     "Normalized_Permeate_Conductivity"
                   ),
                   selected = "Feedwater_Pressure"
                 ),
                
                  dateRangeInput('date4',
                                label = h4('Date Range'),
                                start = min(Train_4_Good_Variables$Date),
                                end =  max(Train_4_Good_Variables$Date)
                 )
               ),
               
               # Main panel for displaying Train 4 Plot  ----
               mainPanel(
                 #plotOutput(outputId = "plot4"),
                 plotlyOutput(outputId = "plotly4"))
             )),
   
     # Tab for Train 5
    tabPanel("Train 5",
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 selectInput(
                   inputId = "x5",
                   label = h3('X-axis'),
                   choices = c("Date"),
                   selected = "Date"
                 ),
                
                  selectInput(
                   inputId = "y5",
                   label = h3("Y-axis"),
                   choices = c(
                     "Feedwater_Pressure", "Percent_Recovery", 
                     "Net_Driving_Pressure", "Specific_Flux",
                     "Normalized_Differential_Pressure",
                     "Normalized_Permeate_Conductivity"
                   ),
                   selected = "Feedwater_Pressure"
                  ),
                 
                 dateRangeInput('date5',
                                label = h4('Date Range'),
                                start = min(Train_5_Good_Variables$Date),
                                end =  max(Train_5_Good_Variables$Date)
                 )
               ),
               
               # Main panel for displaying Train 5 Plot  ----
               mainPanel(
                 #plotOutput(outputId = "plot5"),
                 plotlyOutput(outputId = "plotly5"))
             )
             )
  )
)


# Define server logic ----
server <- function(input, output) {
  
  # Train 1 Plot
  # output$plot1 <- renderPlot({
  # 
  #   Train1 <-  Train_1_Good_Variables
  #   x <- Train1[ ,input$x1]
  #   y <- Train1[ ,input$y1]
  #   title <- "Train 1"
  # 
  #   plot(x, y, main = title, xlab = "Date", ylab = input$y1,
  #        ylim = input$yAxisRange)
  # 
  # 
  # })
  
  # For making more interactive plots based on date ranges
  filtered_data <- reactive({
    subset(Train_1_Good_Variables,
           Date >= input$date[1] & Date <= input$date[2]
    )
    })

  output$plotly <- renderPlotly({
    ggplotly({
      p <- ggplot(filtered_data(), aes_string(x = input$x1, y = input$y1)) +
        geom_point(alpha = 1) +
        ggtitle("Train 1")

      p
      
      p + theme(plot.title = element_text(face = "bold", hjust = 0.5))
    })
  })
  
  
  # Train 2 Plot
  # output$plot2 <- renderPlot({
  #  
  #   Train2 <-  Train_2_Good_Variables
  #   x <- Train2[ ,input$x2]
  #   y <- Train2[ ,input$y2]
  #   title <- "Train 2"
  #   plot(x, y, main = title, xlab = "Date", ylab = input$y2)  
  #   
  #   
  # })
  
  # For making more interactive plots based on date ranges
  filtered_data2 <- reactive({
    subset(Train_2_Good_Variables,
           Date >= input$date2[1] & Date <= input$date2[2]
    )}
  )
  
  output$plotly2 <- renderPlotly({
    ggplotly({
      p <- ggplot(filtered_data2(), aes_string(x = input$x2, y = input$y2)) +
        geom_point(alpha = 1) +
        ggtitle("Train 2")
      
      p
      
      p + theme(plot.title = element_text(face = "bold", hjust = 0.5))
    })
  })
  
  
  # Train 3 Plot 
  # output$plot3 <- renderPlot({
    
  #   Train3 <-  Train_3_Good_Variables
  #   x <- Train3[ ,input$x3]
  #   y <- Train3[ ,input$y3]
  #   title <- "Train 3"
  #   plot(x, y, main = title, xlab = "Date", ylab = input$y3)  
  #   
  # })
  
  # For making more interactive plots based on date ranges
    filtered_data3 <- reactive({
      subset(Train_3_Good_Variables,
             Date >= input$date3[1] & Date <= input$date3[2]
      )}
    )
    
    output$plotly3 <- renderPlotly({
      ggplotly({
        p <- ggplot(filtered_data3(), aes_string(x = input$x3, y = input$y3)) +
          geom_point(alpha = 1) +
          ggtitle("Train 3")
        
        p
        
        p + theme(plot.title = element_text(face = "bold", hjust = 0.5))
      })
    })
    
    
  # Train 4 Plot
  # output$plot4 <- renderPlot({
    
  #   Train4 <-  Train_4_Good_Variables
  #   x <- Train4[ ,input$x4]
  #   y <- Train4[ ,input$y4]
  #   title <- "Train 4"
  #   plot(x, y, main = title, xlab = "Date", ylab = input$y4)  
  # 
  # })
  
  # For making more interactive plots based on date ranges
    filtered_data4 <- reactive({
      subset(Train_4_Good_Variables,
             Date >= input$date4[1] & Date <= input$date4[2]
      )}
    )
    output$plotly4 <- renderPlotly({
      ggplotly({
        p <- ggplot(filtered_data4(), aes_string(x = input$x4, y = input$y4)) +
          geom_point(alpha = 1) +
          ggtitle("Train 4")
        
        p
        
        p + theme(plot.title = element_text(face = "bold", hjust = 0.5))
      })
    })
    
    
  # Train 5 Plot
  # output$plot5 <- renderPlot({
  #   
  #   Train5 <-  Train_5_Good_Variables
  #   x <- Train5[ ,input$x5]
  #   y <- Train5[ ,input$y5]
  #   title <- "Train 5"
  #   plot(x, y, main = title, xlab = "Date", ylab = input$y5)  
  #   
  # })
   
    # For making more interactive plots based on date ranges 
    filtered_data5 <- reactive({
      subset(Train_5_Good_Variables,
             Date >= input$date5[1] & Date <= input$date5[2]
      )}
    )
    
    output$plotly5 <- renderPlotly({
      ggplotly({
        p <- ggplot(filtered_data5(), aes_string(x = input$x5, y = input$y5)) +
          geom_point(alpha = 1) +
          ggtitle("Train 5")
        
        p
        
        p + theme(plot.title = element_text(face = "bold", hjust = 0.5))
      })
    })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)