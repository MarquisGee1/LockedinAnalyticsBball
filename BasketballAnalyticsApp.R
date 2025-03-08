library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(plotly)
library(shinyjs)  # For transitions

# Load player data
player_data <- HawksVSKnicks <- read_excel("/Users/marquisgee/Desktop/NBA 2_24_25.xlsx")%>%
  mutate(
    PTS = as.numeric(PTS),
    TRB = as.numeric(TRB),
    AST = as.numeric(AST),
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Opp = as.character(Opp) 
  )

# UI
ui <- fluidPage(
  useShinyjs(),  # Enable JavaScript for transitions
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Oswald:wght@400;700&display=swap"),
    tags$style(HTML("
      body { background-color: #121212; color: white; font-family: 'Oswald', sans-serif; }
      .title-panel { text-align: center; font-size: 24px; font-weight: bold; padding: 10px; color: #E63946; }
      .sidebar { background-color: #1E1E1E; padding: 15px; border-radius: 10px; }
      .main { padding: 15px; border: 2px solid #E63946; border-radius: 10px; }
      .shiny-input-container { color: white; }
      .avg-container { text-align: center; background-color: #1E1E1E; padding: 10px; border-radius: 10px; margin-top: 15px; }
      .avg-text { font-size: 16px; font-weight: bold; display: flex; justify-content: center; gap: 20px; }
      .sports-pick-box { text-align: center; background-color: #1E1E1E; color: white; padding: 10px; font-size: 16px; font-weight: bold; border-radius: 10px; margin-top: 10px; }
    "))
  ),
  
  titlePanel(div("Locked in Analytics: NBA", class = "title-panel")),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar",
          selectInput("selected_player", "Choose a Player:", 
                      choices = unique(player_data$Player),
                      selected = unique(player_data$Player)[1]),
          
          checkboxGroupInput("selected_stats", "Select Stats to Show:", 
                             choices = list("Points (PTS)" = "PTS", 
                                            "Assists (AST)" = "AST",
                                            "Rebounds (TRB)" = "TRB"),
                             selected = c("PTS", "AST", "TRB")),
          
          sliderInput("num_games", "Number of Recent Games to Show:", min = 5, max = 182, value = 10, step = 1),
          
          textInput("daily_pick", "Your Sports Pick of the Day:", placeholder = "Enter your pick..."),  
          
          downloadButton("downloadPlot", "Download Graph"),
          downloadButton("downloadData", "Download Data (CSV)")
      )
    ),
    
    mainPanel(
      div(class = "sports-pick-box", textOutput("display_pick")),  
      div(class = "main", id = "plot_container", 
          plotlyOutput("player_plot")
      ),
      div(class = "avg-container", id = "avg_stats", 
          uiOutput("stat_averages")  # ✅ Averages now BELOW the plot
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$selected_player)
    
    data <- player_data %>%
      filter(Player == input$selected_player) %>%
      arrange(desc(Date)) %>%
      head(input$num_games)
    
    # ✅ Fix: Create tooltip column before passing it to `aes_string()`
    data <- data %>%
      mutate(Tooltip = paste(
        "<b>Opponent:</b>", Opp, "<br>",
        if ("PTS" %in% input$selected_stats) paste("<b><span style='color:#E63946;'>PTS:</span></b>", PTS, "<br>") else "",
        if ("AST" %in% input$selected_stats) paste("<b><span style='color:#2A9D8F;'>AST:</span></b>", AST, "<br>") else "",
        if ("TRB" %in% input$selected_stats) paste("<b><span style='color:#1E88E5;'>TRB:</span></b>", TRB) else ""
      ))
    
    return(data)
  })
  
  output$display_pick <- renderText({
    if (input$daily_pick == "") {
      return("Enter your Sports Pick above! 🏀")
    } else {
      return(paste("🔥 Locked In Pick:", input$daily_pick))
    }
  })
  
  output$stat_averages <- renderUI({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    stat_avg <- data %>%
      summarise(avg_PTS = mean(PTS, na.rm = TRUE),
                avg_AST = mean(AST, na.rm = TRUE),
                avg_TRB = mean(TRB, na.rm = TRUE))
    
    stat_colors <- list("PTS" = "red", "AST" = "green", "TRB" = "blue")
    
    div(
      class = "avg-text",
      lapply(input$selected_stats, function(stat) {
        span(style = paste("color:", stat_colors[[stat]], ";"), 
             paste("Avg", stat, ":", round(stat_avg[[paste0("avg_", stat)]], 1)))
      })
    )
  })
  
  output$player_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    stat_avg <- data %>%
      summarise(avg_PTS = mean(PTS, na.rm = TRUE),
                avg_AST = mean(AST, na.rm = TRUE),
                avg_TRB = mean(TRB, na.rm = TRUE))
    
    stat_colors <- list("PTS" = "red", "AST" = "green", "TRB" = "blue")
    
    plot <- ggplot(data, aes(x = Date)) +
      labs(title = paste("Now Viewing:", input$selected_player),
           x = "Date of Game", y = "Stat Total") +
      theme_minimal() +
      theme(plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5),
            panel.border = element_rect(color = "darkgrey", fill = NA, size = 1),
            legend.position = "top")
    
    for (stat in input$selected_stats) {
      plot <- plot +
        geom_line(aes_string(y = stat, color = shQuote(stat)), size = 1.2) +
        geom_point(aes_string(
          y = stat, 
          color = shQuote(stat), 
          text = "Tooltip"  
        ), size = 4) +
        geom_hline(yintercept = switch(stat,
                                       "PTS" = stat_avg$avg_PTS,
                                       "AST" = stat_avg$avg_AST,
                                       "TRB" = stat_avg$avg_TRB),
                   linetype = "dashed", color = stat_colors[[stat]], size = 1)
    }
    
    plot <- plot + scale_color_manual(values = setNames(unlist(stat_colors[input$selected_stats]), input$selected_stats))
    
    runjs("$('#plot_container').fadeOut(200).fadeIn(500);")
    
    ggplotly(plot, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "#121212",  
          font = list(color = "#FFFFFF", size = 12)  
        )
      )
  })
}



# Create the Shiny app
shinyApp(ui = ui, server = server)

rsconnect::writeManifest("/Users/marquisgee/BasketballAnalyticsApp.R")


