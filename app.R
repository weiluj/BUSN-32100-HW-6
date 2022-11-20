#--------------------
# Objective: PPHA 30536 HW2
# Date: 23rd Oct, 2022
#--------------------

# Access Shiny App Here: https://weiluj-uchicago.shinyapps.io/chciago-city-dashboard-theft-rodent/

# Clear Global Environment
rm(list = ls())
options(
  scipen = 999,
  digits = 3
)

# Load packages
library(scales)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(sf)
library(spData)
library(shinyWidgets)
library(plotly)

# User Interface
ui <- fluidPage(
  setBackgroundColor(color = "#FFF5EE"),
  titlePanel(div("Seattle Housing Dashboard", style = "color: #00839A")),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", plotOutput("heatmap", height = 650))
    )
  )
)

# Server
server <- function(input, output) {
  df <- read_csv("kc_house_data.csv")
  
  data <- reactive({
    df <- df %>%
      select(price:sqft_lot, grade, yr_built)
    df <- round(cor(df), 2) %>%
      melt()
  })
  
  output$heatmap <- renderPlot({
    # Plot
    plt <- ggplot(data()) +
      geom_tile(aes(Var1, Var2, fill = value)) +
      scale_fill_distiller(
        name = "Value",
        palette = "RdPu",
        direction = 1,
        breaks = seq(0, 1, 0.2),
        limits = c(0,1)
      ) +
      labs(
        title = "Correlation of House Index in Seattle",
        subtitle = "Year 2014-2015",
        caption = "Source: City of Seattle") +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
        axis.title = element_blank(),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.key.width = unit(1.5, "cm"),
        panel.background = element_blank(),
        plot.caption = element_text(
          size = 10, face = "italic",
          vjust = 10, hjust = 1.5
        )
      )
    ggplotly(plt)
  })
}

# Run Shiny App
shinyApp(ui = ui, server = server)