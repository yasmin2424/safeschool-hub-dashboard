# library(shiny)
# library(tidyverse)
# library(shinydashboard)
# 
# # Load data
# df <- read_csv("data/abr_cleaned_scored_unfiltered.csv")
# 
# ui <- dashboardPage(
#   dashboardHeader(title = "Safe School Hub Dashboard"),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
#       menuItem("School Safety", tabName = "safety", icon = icon("shield-alt")),
#       menuItem("Support & Reporting", tabName = "support", icon = icon("life-ring")),
#       menuItem("Advocacy & DEI", tabName = "advocacy", icon = icon("users"))
#     )
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem("overview",
#               fluidRow(
#                 valueBoxOutput("numResponses"),
#                 valueBoxOutput("avgSafety"),
#                 valueBoxOutput("avgSupport")
#               ),
#               plotOutput("heatmap")
#       ),
#       tabItem("safety",
#               plotOutput("safetyDist"),
#               plotOutput("safetyByDistrict")
#       ),
#       tabItem("support",
#               plotOutput("supportVsComfort"),
#               plotOutput("reportingBarriers")
#       ),
#       tabItem("advocacy",
#               plotOutput("deiScores"),
#               plotOutput("advocacyInterest")
#       )
#     )
#   )
# )
# 
# server <- function(input, output) {
#   df_num <- df %>%
#     mutate(across(where(is.character), as.factor))
#   
#   output$numResponses <- renderValueBox({
#     valueBox(nrow(df), "Valid Responses", icon = icon("user-check"), color = "green")
#   })
#   
#   output$avgSafety <- renderValueBox({
#     avg <- mean(df$feel_safe, na.rm = TRUE)
#     valueBox(round(avg, 2), "Avg. Safety Score", icon = icon("shield-alt"), color = "aqua")
#   })
#   
#   output$avgSupport <- renderValueBox({
#     avg <- mean(df$support_satisfaction, na.rm = TRUE)
#     valueBox(round(avg, 2), "Avg. Support Score", icon = icon("life-ring"), color = "orange")
#   })
#   
#   output$heatmap <- renderPlot({
#     df %>%
#       select(feel_safe, support_satisfaction, dei_effectiveness, comfort_reporting) %>%
#       mutate_all(as.numeric) %>%
#       cor(use = "complete.obs") %>%
#       ggcorrplot::ggcorrplot(lab = TRUE)
#   })
#   
#   output$safetyDist <- renderPlot({
#     ggplot(df, aes(x = feel_safe)) +
#       geom_bar(fill = "steelblue") +
#       labs(title = "How Safe Do Students Feel?", x = "Safety Score", y = "Count")
#   })
#   
#   output$safetyByDistrict <- renderPlot({
#     df %>%
#       group_by(school_district) %>%
#       summarise(avg_safety = mean(as.numeric(feel_safe), na.rm = TRUE)) %>%
#       arrange(desc(avg_safety)) %>%
#       top_n(10) %>%
#       ggplot(aes(x = reorder(school_district, avg_safety), y = avg_safety)) +
#       geom_col(fill = "darkgreen") +
#       coord_flip() +
#       labs(title = "Avg. Safety by District", x = "District", y = "Avg Safety")
#   })
#   
#   # Add other plots here...
# }
# 
# shinyApp(ui, server)


# safe_school_dashboard.R
# R Shiny Dashboard: Safe Schools - Racism & Support Analysis







library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
#
#--------------- Load data -----------------
df <- read.csv("data/abr_cleaned_scored_unfiltered.csv", stringsAsFactors = FALSE)
# Clean filters to avoid NA and trailing spaces


# Clean and simplify key filter columns
df <- df %>%
  mutate(
    racial_background = as.character(trimws(replace_na(racial_background, "Unknown"))),
    school_type = as.character(trimws(replace_na(school_type, "Unknown"))),
    school_district = as.character(trimws(replace_na(school_district, "Unknown"))) %>%
      str_replace_all("School District [0-9]+ ", "") # remove long prefixes like "School District 39 "
  )



# Timestamp to POSIXct (works with fractional seconds)
df$Timestamp <- suppressWarnings(ymd_hms(df$Timestamp, quiet = TRUE))

#--------------- Helpers -------------------
nznum <- function(x) suppressWarnings(as.numeric(x))

# Score columns to numeric
score_cols <- c(
  "feel_safe_score","support_score","school_response_score","dei_score",
  "resources_sufficiency_score","comfort_discussing_race_score",
  "comfort_reporting_score","comfort_advocating_score",
  "interest_training_score","interest_certification_score"
)
present_scores <- intersect(score_cols, names(df))
df[present_scores] <- lapply(df[present_scores], nznum)

# Experienced & witnessed flags
exp_wit_cols <- intersect(
  c("experienced_name_calling","witnessed_exclusion","witnessed_bullying"),
  names(df)
)
if (length(exp_wit_cols) == 0) {
  df$experienced_racism_flag <- 0L
  df$witnessed_racism_flag   <- 0L
} else {
  df$experienced_racism_flag <- df %>%
    select(all_of(exp_wit_cols)) %>%
    mutate(across(everything(),
                  ~ str_detect(.x %||% "", regex("experienced this personally", ignore_case = TRUE)))) %>%
    apply(1, any, na.rm = TRUE) %>% as.integer()

  df$witnessed_racism_flag <- df %>%
    select(all_of(exp_wit_cols)) %>%
    mutate(across(everything(),
                  ~ str_detect(.x %||% "", regex("seen it happening", ignore_case = TRUE)))) %>%
    apply(1, any, na.rm = TRUE) %>% as.integer()
}

# Default values
df$school_type     <- df$school_type     %||% "Unknown"
df$school_district <- df$school_district %||% "Unknown"

#--------------- UI ------------------------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Safe School Hub — Anti-Black Racism Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      br(),
      dateRangeInput(
        "daterange", "Date Range:",
        start = min(df$Timestamp, na.rm = TRUE),
        end   = max(df$Timestamp, na.rm = TRUE)
      ),
      selectInput(
        "race_filter", "Filter by Race:",
        choices  = sort(unique(df$racial_background), na.last = NA),
        selected = unique(df$racial_background),
        multiple = TRUE
      ),
      selectInput(
        "stype", "School Type:",
        choices  = sort(unique(df$school_type), na.last = NA),
        selected = unique(df$school_type),
        multiple = TRUE
      ),
      selectInput(
        "district_filter", "School District:",
        choices  = sort(unique(df$school_district), na.last = NA),
        selected = unique(df$school_district),
        multiple = TRUE
      )
      
  )),
  dashboardBody(
    tags$head(
      tags$style(HTML("
    /* ==============================
       SAFE SCHOOL HUB THEME COLORS
       Red: #F54D54
       Black: #000000
       Dark Gray: #1A1A1A
       Light Gray: #F4F4F4
       ============================== */

    /* ----- Header ----- */
    .skin-black .main-header .logo {
      background-color: #F54D54 !important;
      color: white !important;
      font-weight: bold;
    }
    .skin-black .main-header .navbar {
      background-color: #000000 !important;
    }

    /* ----- Sidebar ----- */
    .skin-black .main-sidebar {
      background-color: #1A1A1A !important;
    }
    .skin-black .sidebar a {
      color: #FFFFFF !important;
    }
    .skin-black .sidebar-menu>li.active>a {
      background-color: #F54D54 !important;
      color: #FFFFFF !important;
    }

    /* ----- Boxes (Plots & Sections) ----- */
    .box {
      border-top: 3px solid #F54D54 !important;
      border-radius: 10px;
    }

    /* Custom header colors for all boxes */
    .box.box-solid.box-primary>.box-header,
    .box.box-solid.box-warning>.box-header,
    .box.box-solid.box-info>.box-header {
      background-color: #F54D54 !important;
      color: #fff !important;
      font-weight: 600;
    }

    /* Reinforce red border for all box types */
    .box.box-solid.box-primary,
    .box.box-solid.box-warning,
    .box.box-solid.box-info {
      border-top: 3px solid #F54D54 !important;
    }

    /* Box titles and body text */
    .box-title {
      color: #000000 !important;
      font-weight: 600;
    }
    .box-body {
      color: #000000 !important;
    }

    /* ----- Value Boxes ----- */
    .small-box {
      border-radius: 14px !important;
    }
    .small-box.bg-aqua   { background-color: #F54D54 !important; color: #fff !important; }
    .small-box.bg-green  { background-color: #1A1A1A !important; color: #fff !important; }
    .small-box.bg-olive  { background-color: #000000 !important; color: #fff !important; }
    .small-box.bg-yellow { background-color: #F4F4F4 !important; color: #000 !important; }

    /* ----- Page Background ----- */
    .content-wrapper, .right-side {
      background-color: #F4F4F4 !important;
    }
  "))),

    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("totalResponses", width = 3),
                valueBoxOutput("avgSafety",       width = 3),
                valueBoxOutput("avgDEI",          width = 3),
                valueBoxOutput("pctExperienced",  width = 3)
              ),
              fluidRow(
                box(title = "Survey Submissions Over Time", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("timeTrend", height = 300)),
                box(title = "DEI vs Safety (with trend)", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("deiVsSafety", height = 300))
              ),
              fluidRow(
                box(title = "Comfort Reporting by School Type", status = "warning", solidHeader = TRUE,width = 6,
                    plotOutput("comfortByType", height = 300)),
                box(title = "Top 10 Districts by Support Score", status = "warning", solidHeader = TRUE, width = 6,
                    plotOutput("supportByDistrict", height = 300))
              ),
              fluidRow(
                box(title = "Experienced vs Witnessed Racism (Share of Respondents)", status = "info", solidHeader = TRUE, width = 12,
                    plotOutput("expVsWit", height = 260))
              ),
              fluidRow(
                box(title = "Average Scores by District (Support, Safety, DEI)",status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("districtScores", height = 500))

              ),
              fluidRow(
                box(title = "Survey Data Table", status = "primary", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("dataTable"))
              )
              
      )
    )
  )
)

 #--------------- Server --------------------
server <- function(input, output, session) {

  d_filt <- reactive({
    req(df)  # ensure df exists
    d <- df
    
    # Date filter
    if (!is.null(input$daterange) && length(input$daterange) == 2) {
      d <- d %>%
        filter(is.na(Timestamp) | (Timestamp >= input$daterange[1] & Timestamp <= input$daterange[2]))
    }
    
    # Apply filters only when selected
    if (!is.null(input$race_filter) && length(input$race_filter) > 0) {
      d <- d %>% filter(racial_background %in% input$race_filter)
    }
    
    if (!is.null(input$stype) && length(input$stype) > 0) {
      d <- d %>% filter(school_type %in% input$stype)
    }
    
    if (!is.null(input$district_filter) && length(input$district_filter) > 0) {
      d <- d %>% filter(school_district %in% input$district_filter)
    }
    
    return(d)
  })
  
  

  output$totalResponses <- renderValueBox({
    valueBox(
      value = nrow(d_filt()),
      subtitle = "Total Responses",
      icon = icon("users"),
      color = "aqua"
    )
  })

  output$avgSafety <- renderValueBox({
    avg <- d_filt() %>% summarize(x = mean(feel_safe_score, na.rm = TRUE)) %>% pull(x)
    valueBox(
      value = ifelse(is.finite(avg), round(avg, 2), "—"),
      subtitle = "Average Safety (1–5)",
      icon = icon("shield"),
      color = "green"
    )
  })

  output$avgDEI <- renderValueBox({
    avg <- d_filt() %>% summarize(x = mean(dei_score, na.rm = TRUE)) %>% pull(x)
    valueBox(
      value = ifelse(is.finite(avg), round(avg, 2), "—"),
      subtitle = "Average DEI (1–5)",
      icon = icon("balance-scale"),
      color = "olive"
    )
  })

  output$pctExperienced <- renderValueBox({
    p <- d_filt() %>% summarize(x = mean(experienced_racism_flag, na.rm = TRUE)) %>% pull(x)
    valueBox(
      value = ifelse(is.finite(p), paste0(round(100*p), "%"), "—"),
      subtitle = "% Experienced Racism",
      icon = icon("exclamation-triangle"),
      color = "aqua"
    )
  })

  output$timeTrend <- renderPlot({
    d_filt() %>%
      mutate(date = as.Date(Timestamp)) %>%
      filter(!is.na(date)) %>%
      count(date) %>%
      ggplot(aes(x = date, y = n)) +
      geom_line(size = 1, color = "#F54D54") +
      geom_point(size = 2, color = "#F54D54") +
      labs(x = NULL, y = "Submissions", title = NULL) +
      theme_minimal(base_size = 13)
  })

  output$deiVsSafety <- renderPlot({
    d <- d_filt()
    ggplot(d, aes(x = dei_score, y = feel_safe_score)) +
      geom_jitter(width = 0.15, height = 0.15, alpha = 0.5, color = "#F54D54") +
      geom_smooth(method = "lm", se = FALSE, color = "#000000") +
      labs(x = "DEI Score", y = "Safety Score", title = NULL) +
      theme_minimal(base_size = 13)
  })

  output$comfortByType <- renderPlot({
    d_filt() %>%
      group_by(school_type) %>%
      summarize(mean_comfort = mean(comfort_reporting_score, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = reorder(school_type, mean_comfort), y = mean_comfort, fill = school_type)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("Public"="#F54D54", "Private"="#1A1A1A")) +
      coord_flip() +
      labs(x = NULL, y = "Mean Comfort (1–5)", title = NULL) +
      theme_minimal(base_size = 13)
  })

  output$supportByDistrict <- renderPlot({
    d_filt() %>%
      group_by(school_district) %>%
      summarize(mean_support = mean(support_score, na.rm = TRUE), .groups = "drop") %>%
      slice_max(order_by = mean_support, n = 10, with_ties = FALSE) %>%
      ggplot(aes(x = reorder(school_district, mean_support), y = mean_support, fill = school_district)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = rep(c("#F54D54","#1A1A1A"), 5)) +
      coord_flip() +
      labs(x = NULL, y = "Support Score (1–5)", title = NULL) +
      theme_minimal(base_size = 13)
  })

  output$expVsWit <- renderPlot({
    d <- d_filt() %>%
      summarize(
        Experienced = mean(experienced_racism_flag, na.rm = TRUE),
        Witnessed   = mean(witnessed_racism_flag,   na.rm = TRUE)
      ) %>%
      pivot_longer(everything(), names_to = "type", values_to = "rate")
    ggplot(d, aes(x = type, y = 100*rate, fill = type)) +
      geom_col(show.legend = FALSE, width = 0.6) +
      geom_text(aes(label = paste0(round(100*rate), "%")), vjust = -0.2, color = "black") +
      scale_fill_manual(values = c("Experienced"="#F54D54", "Witnessed"="#1A1A1A")) +
      ylim(0, 100) +
      labs(x = NULL, y = "% of respondents", title = NULL) +
      theme_minimal(base_size = 13)
  })
  
  
  output$districtScores <- renderPlot({
    d_filt() %>%
      group_by(school_district) %>%
      summarise(
        support = mean(support_score, na.rm = TRUE),
        safety = mean(feel_safe_score, na.rm = TRUE),
        dei = mean(dei_score, na.rm = TRUE)
      ) %>%
      tidyr::pivot_longer(cols = c(support, safety, dei),
                          names_to = "score_type",
                          values_to = "value") %>%
      ggplot(aes(x = reorder(school_district, value), y = value, fill = score_type)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = c(
        "support" = "#0073e6",   # Blue
        "safety"  = "#ffa500",   # Orange
        "dei"     = "#69b3a2"    # Greenish teal
      )) +
      labs(x = "School District", y = "Average Score (1–5)",
           fill = "Score Type") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        plot.background = element_rect(fill = "#F4F4F4", color = NA),
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        axis.text.y = element_text(size = 11, color = "#000000"),
        axis.text.x = element_text(size = 11, color = "#000000"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)
      )
  })
  
 
  
  output$dataTable <- DT::renderDataTable({
    d_filt() %>%
      select(
        Timestamp, racial_background, residence_city, school_district, school_type,
        feel_safe_score, support_score, dei_score, comfort_reporting_score,
        comfort_advocating_score, resources_sufficiency_score
      ) %>%
      datatable(
        extensions = c("Buttons", "Scroller"),
        options = list(
          dom = "Bfrtip",
          scrollX = TRUE,
          scrollY = "400px",
          scroller = TRUE,
          pageLength = 10,
          buttons = c("copy", "csv", "excel", "pdf"),
          autoWidth = TRUE
        ),
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; font-weight: bold; color: #F54D54;',
          'Filtered Survey Responses'
        )
      )
  })
  

 }
#
shinyApp(ui, server)

