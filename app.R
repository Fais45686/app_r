library(shiny)
library(ggplot2)
library(dplyr)
library(flexdashboard)
#install.packages("plotly")
library(plotly)
#install.packages("visNetwork")
library(visNetwork)
#install.packages("shinyBS")
library(shinyBS)
library(fmsb)
library(tidyverse)

ui <- fluidPage(
  # ใส่ CSS เพื่อกำหนด Background สี
  tags$style(
    HTML("
      body {
        background-color: #ffe082;  /* สีฟ้าอ่อน */
      }
    ")
  ),
  titlePanel("ผลวิเคราะห์ความเสี่ยง CMER"),
  sidebarLayout(
    sidebarPanel(
      selectInput("province", "เลือกจังหวัด:", 
                  choices = unique(data_shiny$province), 
                  selected = unique(data_shiny$province)[1]),
      width = 3,
      uiOutput("district_ui")
    ),
    
    mainPanel(
      # ใช้ fluidRow สำหรับจัดแถวแรกและแถวสอง
      fluidRow(
        # แถวที่ 1: Correlation Heatmap
        column(12, plotOutput("correlation_heatmap") , height = "200px")
      ),
      br(),
      fluidRow(
        # แถวที่ 1: Correlation Heatmap
        column(6, plotOutput("percent_vip_plot"),height = "250px"), 
        column(6, plotOutput("scatter_plot"),height = "250px")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$province, {
    districts <- unique(data_shiny$affiliation[data_shiny$province == input$province])
    updateSelectInput(session, "district", choices = districts, selected = districts[1])
  })
  
  output$district_ui <- renderUI({
    selectInput("district", "เลือกเขต:", choices = NULL)
  })
  
  output$percent_vip_plot <- renderPlot({
    req(input$district)
    selected_data <- data_shiny %>% filter(province == input$province & affiliation == input$district)
    ggplot(selected_data, aes(x = term, y = percent_vip, fill = term)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = round(percent_vip, 2)), vjust = -0.5) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "สัดส่วนของ ความสำคัญเชิงสัมพัทธ์ของปัจจัยด้านภูมิหลังโรงเรียนและนักเรียน",
           x = "Term", y = "Percent VIP") +
      theme_minimal()
  })
  
  output$scatter_plot <- renderPlot({
    req(input$district)
    ggplot(data_shiny, aes(x = percent_vip, y = r_squared)) +
      geom_point(aes(color = ifelse(affiliation == input$district, "Selected", "Others"),
                     size = ifelse(affiliation == input$district, 3, 1)), alpha = 0.7) +
      scale_color_manual(values = c("Selected" = "red", "Others" = "lightgray")) +
      scale_size_identity() +
      labs(title = "ความสัมพันธ์ระหว่าง Percent_vip กับ R-squared",
           x = "Percent VIP", y = "R-squared") +
      theme_minimal() +
      theme(legend.position = "top") +
      facet_wrap(~term)
  })
  
  output$correlation_heatmap <- renderPlot({
    correlation_long <- as.data.frame(as.table(correlation_matrix))
    ggplot(correlation_long, aes(Var1, Var2, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = round(Freq, 2)), color = "black", size = 3) +
      scale_fill_gradient2(low = "red", high = "steelblue", mid = "white", midpoint = 0, limit = c(-1, 1)) +
      theme_minimal() +
      labs(title = "Correlation แสดงค่าสัมประสิทธ์สหสัมพันธ์ของ percent_vip และ R-squared", x = "", y = "")+
      theme(text = element_text(family = "ChulaCharasNew"))
  })
}

shinyApp(ui = ui, server = server)

rsconnect::deployApp('/Users/faisdueramae/Desktop/CU/Data visualization/Final/app_R.R')

