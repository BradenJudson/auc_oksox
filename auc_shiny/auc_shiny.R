
# Libraries.
library(shiny); library(tidyverse); library(DT)
library(lubridate); library(collapse)

# Set up empty tibble for populating later. 
df <- dplyr::tibble(Date = NA, Fish = NA)


# -------------------------------------------------------------------------



ui <- fluidPage(
  
  # App title ----
  title = "title",
  
  fluidRow(
    # Panel 1: Input count data.
    
    # Would be nice to remove "Show # rows" thing.
    # Try to add border or better theme/style.
    
    column(width = 3, offset = 0.1,
           "Table 1. Input data",
           style = 'padding:10px',
           shiny::dateInput(inputId    = "date",
                            label      = "Date:"),
           shiny::numericInput("fish",   "Fish:",
                               value   = 0, 
                               min     = 0),
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
    column(4, 'Table 3.',
           style = 'padding:10px',
           DTOutput(outputId = 'aucs'))
    
  ))
  
  



# -------------------------------------------------------------------------



server <- function(input, output, session) {
  
  # Table 2: Enumerations.
  
  mod_df <- shiny::reactiveValues(x = df)
  
  output$enum <- DT::renderDataTable(
    
    datatable(mod_df$x, options = list(dom = 't'))
    
    )
  
  shiny::observe({
    shiny::updateSelectInput(session, 
                             inputId = "remove_row",
                             choices = 1:nrow(mod_df$x))
  })
  
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
  
  proxy <- DT::dataTableProxy('enum')
  shiny::observe({
    
    DT::replaceData(proxy, mod_df$x)
    
  })
 
  
  output$aucs <- DT::renderDataTable(
    
    datatable(
      dplyr::tibble(type = "Left tail",
                    doy = yday(mod_df$x$Date[1]),
                    tdiff = NA,
                    xbar = NA,
                    fishdays = as.numeric(mod_df$x[1,2])*(11/2)) %>% 
        dplyr::bind_rows(
                mod_df$x %>% 
                mutate(doy = lubridate::yday(Date),
                       tdiff = doy - lag(doy),
                       tdiff = replace(tdiff, which(tdiff <0), NA),
                       xbar  = (Fish + lag(Fish))/2,
                       fishdays = case_when(
                         is.na(xbar) ~ Fish * (11/2),
                         !is.na(xbar) ~ tdiff*xbar
                       ),
                       type = "Trapezoid") %>% 
                  filter(!is.na(tdiff)) %>% 
                select(c("type","doy", "tdiff", "xbar", "fishdays"))) %>% 
                dplyr::bind_rows(
                  dplyr::tibble(type = "Right tail",
                                doy = yday(mod_df$x$Date[nrow(mod_df$x)]),
                                tdiff = NA,
                                xbar = NA,
                                fishdays = as.numeric(mod_df$x[nrow(mod_df$x),2])*(11/2))
                ), 
              options = list(dom = 't'))
  )
  
  
  
}

shinyApp(ui, server)