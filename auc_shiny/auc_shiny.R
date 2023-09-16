
# Libraries.
library(shiny); library(tidyverse); library(DT)
library(lubridate); library(collapse)

# Set up empty tibble for populating later. 
df <- dplyr::tibble(Date = NA, Fish = NA)


# -------------------------------------------------------------------------

# User interface.
ui <- fluidPage(
  
  # App title ----
  titlePanel("Salmon area-under-the-curve spawner abundance estimation"),
  
    fluidRow(
    # Make bigger and bold output value.
    # see https://stackoverflow.com/questions/39985307/shiny-r-rendertext-paste-new-line-and-bold
    column(width = 12,
           textOutput(outputId = "AUC")),
        
    column(width = 12,
           plotOutput("auc_plot")),
    
    # Frame is 12 pts wide. 
    # First 1/4 is input sliders. 
    column(width = 3, offset = 0.1,
           # Header.
           "Table 1. Input data",
           # Pad from top of frame only.
           style = 'padding:10px',
           # Date input - select from Calendar dropdown.
           shiny::dateInput(inputId    = "date",
                            label      = "Date:"),
           # How many fish? Pick a positive value.
           shiny::numericInput("fish",   "Fish:",
                               value   = 0, 
                               min     = 0),
           # Operators to add/remove rows. 
           shiny::actionButton(inputId = "add", 
                               label   = "Add"),
           shiny::selectInput(inputId  = "remove_row",
                              label    = "Remove row",
                              choices  = 1:100),
           shiny::actionButton(inputId = "remove",
                               label   = "Remove")),
    
    # Panel 2: Output table of enumerations.
    column(3, 'Table 2. Enumeration summary',
           style = 'padding:10px',
           DTOutput(outputId = "enum")),
    
    # Panel 3: ATU Calculations.
    # Slightly wider than other two panels. 
    column(4, 'Table 3. AUC Calculations',
           style = 'padding:10px',
           DTOutput(outputId = 'auc_calcs'))
    
  ))
  
  



# -------------------------------------------------------------------------


# Server - operations. 
server <- function(input, output, session) {
  
  # Table 2: Enumerations.
  # Start with blank DF defined above. 
  mod_df <- shiny::reactiveValues(x = df)
  
  # Render as DT without (show x entries).
  output$enum <- DT::renderDataTable(
    
    datatable(mod_df$x, options = list(dom = 't'))
    
    )
  
  # Operators for removing rows. 
  shiny::observe({
    shiny::updateSelectInput(session, 
                             inputId = "remove_row",
                             choices = 1:nrow(mod_df$x))
  })
  
  # For adding values and updating table without 
  # leaving original NA columns. 
  shiny::observeEvent(input$add, {
    
    mod_df$x <- mod_df$x %>%
      dplyr::bind_rows(
        dplyr::tibble(Date = input$date,
                      Fish = input$fish)
      ) %>% 
      filter(!is.na(Date))
    
  })
  
  shiny::observeEvent(input$remove, {
    
    mod_df$x <- mod_df$x[-as.integer(input$remove_row), ]
    
  })
  
  # For automatically updating enumerations table.
  proxy <- DT::dataTableProxy('enum')
  shiny::observe({
    
    DT::replaceData(proxy, mod_df$x)
    
  })
  
  
  aucdat <- reactive({
    
    dplyr::tibble(type = "Left tail",
                  doy  = yday(mod_df$x$Date[1]),
                  tdiff = NA,
                  xbar  = NA,
                  # Count on first day multiplied by residency time/2.
                  fishdays = as.numeric(mod_df$x[1,2])*(11/2)) %>% 
      # Now for the "center" of the trapezoid. 
      dplyr::bind_rows(
        mod_df$x %>% 
          # Take first dataframe and add components. 
          # Calculate Julian date.
          mutate(doy = lubridate::yday(Date),
                 # Difference between survey dates (in days).
                 tdiff = doy - lag(doy),
                 # Exclude negatives (shouldn't happen anyway).
                 tdiff = replace(tdiff, which(tdiff < 0), NA),
                 # Average fish count between dates.
                 xbar  = (Fish + lag(Fish))/2,
                 # Average fish count multiplied by time elapsed.
                 fishdays = case_when(
                   is.na(xbar) ~ Fish * (11/2),
                   !is.na(xbar) ~ tdiff*xbar
                 ),
                 # Specify these are the non-tail calculations.
                 type = "Trapezoid") %>% 
          # Remove NAs. Helps tidy DT.
          filter(!is.na(tdiff)) %>% 
          # Select relevant columns.
          select(c("type","doy", "tdiff", "xbar", "fishdays", "Fish"))) %>% 
      # Add terminal tail estimate. 
      dplyr::bind_rows(
        # Right-most tail.
        dplyr::tibble(type = "Right tail",
                      # Take the last date in the series. 
                      doy = yday(mod_df$x$Date[nrow(mod_df$x)]),
                      tdiff = NA, xbar = NA,
                      # Count on the last date multiplied by half of the residency time.
                      fishdays = as.numeric(mod_df$x[nrow(mod_df$x), 2])*(11/2))
      ) %>%  
      mutate(summ = fcumsum(fishdays))  
      # Renaming columns for presentation.
 
    
  })

  output$auc_calcs <- DT::renderDataTable(
    aucdat() %>% select(-c("summ", "Fish")) %>% 
      `colnames<-`(., c(" ", "Day\n(Julian)", 
                        "ΔTime\n(days)", "x̄", 
                        "Fish*days")), 
    options = list(dom = 't'))
  
  
  output$AUC <- renderText(paste("Area under the curve = ",
                                 max(aucdat()$summ )))
  
  
  
  counts <- reactive({
    dplyr::tibble(date = mod_df$x$Date,
                  fish = mod_df$x$Fish)
  })
  
  
  output$auc_plot <- renderPlot({
    ggplot(data = counts(),
           aes(x = date, y = fish)) +
      theme_bw() +
      labs(x = NULL,y = "Fish") +
      geom_area(aes(y = fish), 
                fill   = "gray",   alpha = 1/4) +
      geom_point(color = "black",  shape = 21, 
                 fill  = "gray99", size = 2) +
      geom_hline(yintercept = 0,   color = "deepskyblue3",
                 linewidth  = 1/2, linetype = "dashed") 
  })
  
  
  
  
  
  
}

shinyApp(ui, server)

# https://debruine.github.io/shinyintro/sharing.html

