
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(shinycssloaders))



panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

labelspage <- navbarMenu("Labels",
                 tabPanel("TC Label Management",
                          div(
                            includeCSS("abstyles.css"),
                            tags$p(style = "color: #FF8C00; font-size: 28px; text-align: center;","Tissue Culture Label Management"), br(),
                            
                            fluidRow(
                              column(2,
                                     radioGroupButtons(
                                       inputId = "tc_tabs",
                                       label = "",
                                       choices = c("Crosses (Embryo Rescue)", 
                                                   "Embryo Germinating", 
                                                   "Subcultures", 
                                                   "Rooting", 
                                                   "Weaning 1/ Sending Out",
                                                   "Weaning 2", 
                                                   "Screenhouse Transfer", 
                                                   "Hardening", 
                                                   "Openfield"),
                                       direction = "vertical",
                                       checkIcon = list(
                                         yes = icon("ok", 
                                                    lib = "glyphicon")
                                       ), size = "lg"
                                     )
                              ),
                              column(2,
                                     
                                     panel_div(class_type = "default",
                                               content = tags$div(  br(), br(),        
                                                                    
                                                                    selectInput(inputId = 'tc_site', 'SITE',choices = ""), br(),
                                                                    dateRangeInput("tcDateRange","Date Range"), br(),
                                                                    conditionalPanel(
                                                                      condition = "input.tc_tabs!='Embryo Germinating'",
                                                                      prettyRadioButtons(
                                                                        inputId = "number_per_tube",
                                                                        label = "Number of plants per test tube", 
                                                                        choices = c("single plant", "3 plants/test tube", "6 plants/test tube"),
                                                                        icon = icon("check"), inline = F, bigger = TRUE,
                                                                        status = "info",
                                                                        animation = "jelly"
                                                                      )
                                                                    ), br(),
                                                                    conditionalPanel(
                                                                      condition = "input.number_per_tube == 'single plant' || input.tc_tabs == 'Embryo Germinating'",
                                                                      numericInput("number_of_copies", "Number of labels (copies)", value = 1, min = 1, max = 10)
                                                                    ), br(), 
                                                                    downloadBttn("tc_download","Download", size = "sm",style = "fill")
                                               )
                                     )
                              ),
                              column(7,
                                     conditionalPanel(
                                       condition = "input.tc_tabs != 'How to do it'",
                                       selectizeInput("scan_ids", "Scan to select specific IDs",
                                                      choices = NULL, multiple = T, width = "100%"),
                                       
                                       DT::DTOutput("tc_dt")
                                     )
                              )
                            )
                          )
                          ),
                 
                 tabPanel("Download barcodes",
                          div(
                            tags$p(style = "color: black; font-size: 28px; text-align: center;","Barcode generator"),
                            
                            column(3, 
                                   wellPanel( br(),
                                              column(8,
                                                     selectInput('barcode_text', p('Text label'), c("FemalePlotName","MalePlotName","FemaleGenotype","MaleGenotype", "First Pollination Date"))
                                              ),
                                              column(4),
                                              verbatimTextOutput("tt34"),
                                              column(12,
                                                     downloadBttn("downloadlabels","Download",style = "bordered", size="sm",color = "primary"), br(),br(),
                                                     p("Note: QRCode generated is only for the crossnumber. Include more info by selecting text label above", br(),br(), "To download specific label IDs, select their respective rows in the table and click download button otherwise labels will be generated for all data in the table")
                                              )
                                   )),
                            column(8,
                                   wellPanel(
                                     div(style = 'overflow-x: scroll',
                                         DT::DTOutput("downloadlabelsDT")
                                     )
                                   )
                            )
                          )
                 )
)


