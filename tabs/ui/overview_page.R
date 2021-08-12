
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(highcharter))

panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

css <- "
#large .selectize-input { line-height: 40px; }
#large .selectize-dropdown { line-height: 30px; }"

overviewpage <- tabPanel("Overview",
                         
  includeCSS("www/AdminLTE.css"), # for activating shinydashboard/plus widgets
  tags$head(tags$style(HTML('.modal-sm {width: 150%;}')),
            tags$style(HTML('.col-sm-2 {width: 14.0%;}'))
            ),
  fluidRow(
    div(id = "overview_controls",
        column(3, selectInput("site", "Select Site:", choices = NULL, multiple = T)),
        column(4, dateRangeInput("dateRange", "Select Date Range", min = "2015-01-01", max = Sys.Date(),  start = "2015-01-01", end = Sys.Date(),  
                                 startview = "year", separator = " to ", autoclose = TRUE, width = "100%"))
    ),
    column(1),
    
    column(4,
           shinyjs::useShinyjs(), 
           awesomeCheckbox(
             inputId = "follow_crosses",
             label = "Only crosses done between this date range", 
             value = FALSE,
             status = "info"
           ))
    
  ),
  fluidRow(
    
    tags$style("#n_crosses .small-box, #n_bc .small-box, #n_totalseeds .small-box, #n_rescued .small-box, #n_germination .small-box, #n_openfield_plantlets .small-box {cursor: pointer;}"),
    
    valueBoxOutput("n_crosses", width = 2),tags$style("#n_crosses"), # tags$style("#n_crosses {width:220px;}")
    bsModal("mod_crosses",tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Banana Crosses"),"btn",size = "large",
            column(12, offset = 10, 
                   downloadBttn("download_crosses",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_crosses"), br(), br()
    ),
    
    valueBoxOutput("n_bc", width = 2),
    tags$style("#n_bc"), 
    bsModal("mod_bc",
            tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Banana Back Crosses"),"btn",
            size = "large",
            column(12, 
                   offset = 10, 
                   downloadBttn("download_bc",
                                style = "jelly", 
                                size = "sm")
                   ),
            DT::dataTableOutput("list_bc"), 
            br(), br()
    ),
    
    valueBoxOutput("n_bunches", width = 2), tags$style("#n_bunches"),
    bsModal("modal_bunches", tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Banana Bunches Harvested"),"btn", size = "large",
            column(12, offset = 10,
                   downloadBttn("download_bunches",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_bunches"), br(), br()
    ),
    
    valueBoxOutput("n_totalseeds", width = 2), tags$style("#n_totalseeds"),
    bsModal("modal_totalseeds", tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Seed Extraction"), "btn", size = "large",
            column(12, offset = 10,
                   downloadBttn("download_totalseeds",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_totalseeds"), br(), br()
    ),
    
    valueBoxOutput("n_rescued", width = 2), tags$style("#n_rescued"), 
    bsModal("modal_rescued", tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Embryo Rescued"), "btn",size = "large",
            column(12, offset = 10,
                   downloadBttn("download_rescued",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_rescued"), br(), br()
    ),
    
    valueBoxOutput("n_germination", width = 2), tags$style("#n_germination"),
    bsModal("modal_germination", tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Embryo Germination"), "btn",size = "large",
            column(12, offset = 10,
                   downloadBttn("download_germination",style = "jelly", size = "sm")),
            DT::dataTableOutput("list_germination"), br(), br()
    ),
    
    valueBoxOutput("n_openfield_plantlets", width = 2), tags$style("#n_openfield_plantlets"),
    bsModal("modal_openfield", tags$p(style = "color: #FF8C00; font-size: 22px; text-align: center;","Plantlets in Openfield"), "btn",size = "large",
            column(12, offset = 10,
                   downloadBttn("download_openfield",style = "jelly",  size = "sm")),
            DT::dataTableOutput("list_openfield"), br(), br()
    )
    
  ), br(),br(),
  fluidRow(
    ######### CROSSES BAR PLOT
    column(width = 6,
           panel_div(class_type = "default",
                content = tags$div(
                     tags$div(align = 'right',
                              uiOutput("totalcrosses")
                     ),
                     
                     div(
                       conditionalPanel(
                         condition = "input.site == NULL", 
                         
                         highcharter::highchartOutput("totals_site", height = 350)  %>% 
                           withSpinner(type = 8, color = "#0dc5c1", size = 1)
                       ), br()
                     ))
                )),
    column(6,
      panel_div(class_type = "default",
          content = tags$div(
              column(width = 6,
                     tags$div(align = 'right', uiOutput("totalFemales")),
                     highchartOutput("mother", height = 367) %>% 
                       withSpinner(type = 8, color = "#0dc5c1", size = 1)
              ),
              column(width = 6,
                     tags$div(align = 'right', uiOutput("totalMales")),
                     highchartOutput("father", height = 367) %>% 
                       withSpinner(type = 8, color = "#0dc5c1", size = 1)
              )
          ))
      )
  ), br()
)
