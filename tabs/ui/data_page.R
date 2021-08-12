
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(shinyalert))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(magrittr))

panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}


datapage <-
  tabPanel("Data",
           fluidRow(
             sidebarLayout(
               sidebarPanel(width = 3,
                            
                    # data tab
                      hidden(
                      selectInput("dataset","Select dataset:", 
                                  c("Flowering","Crosses","Plantlets", "Subcultures", 
                                    "Rooting", "Weaning 1", "Weaning 2", 
                                    "Screenhouse", "Hardening", "Open-field",
                                    "Status","Contamination"), 
                                  selected = 'Crosses')
                      ),
                    
                    # summary tab
                    hidden(
                    dateRangeInput("sum_dateRange", "First Pollination Date"),
                    selectInput('groupby', 'Aggregate by:', 
                                  c('Location','FemaleGenotype','MaleGenotype', 'CrossType', 'FemalePloidy', 'MalePloidy', 
                                    'Year_of_Pollination','Month_of_Pollination'), selected = "Crossnumber", 
                                  multiple=T, width="100%")
                    ), 
                    
                    # activity specific summaries
                    hidden(
                    selectInput("gen_activity","Select Activity", 
                                  c("First pollination","Bunches","Seed extraction", "Embryo rescue",
                                    "Embryo germination", "Subcultures", "Rooting", "Weaning 1", "Weaning 2", 
                                    "Screenhouse", "Hardening", "Open-field"), 
                                  selected = "First pollination", multiple = FALSE), 
                      
                    dateRangeInput("gen_dateRange", "Date Range"),
                    selectInput("gen_groupby", "Aggregate by", 
                                c("Location", "FemaleGenotype", "MaleGenotype", 'CrossType', 'FemalePloidy', 'MalePloidy'),
                                selected = "Location", multiple = T)
                    ),
                    verbatimTextOutput("ttx")
               ),
               
               mainPanel(
                 
                 tabsetPanel(
                   id = "datatabs",
                   
                   tabPanel("Data Table", br(),
                            
                            fluidRow(
                              panel_div(class_type = "default",
                                        content = tags$div(
                                          column(12,offset = 11, 
                                                 downloadBttn("downloadTbl", size = "xs")), br(), br(),
                                          column(12,
                                                 div(style = 'overflow-x: scroll',
                                                     DT::DTOutput("viewdt") %>%
                                                       shinycssloaders::withSpinner()
                                                     )
                                          )
                                        ))
                            )  
                   ),
                   tabPanel("Summary Table", br(),
                            fluidRow(
                              panel_div(class_type = "default",
                                        content = tags$div(
                                          column(12, offset = 11,
                                                 downloadBttn("downloadSummary","Download", size = "xs")), br(),br(),
                                          column(12,
                                                 div(style = 'overflow-x: scroll',
                                                    DT::DTOutput("summaryDT")%>%
                                                      shinycssloaders::withSpinner()
                                          ))
                                        )),
                              
                              div(id="sdrill",
                                  h5("Click any row in table above for more details")
                              ),br(),br(),
                              
                              shinyjs::hidden(
                                div(id="sDetails_id", br(),
                                    
                                    h4("Detailed Summary Table"),
                                    panel_div(class_type = "default",
                                              content = tags$div(
                                                column(12,offset = 11, 
                                                       downloadBttn("save_sum_details", size = "xs")), br(), br(),
                                                column(12,
                                                       div(style = 'overflow-x: scroll',
                                                          DT::DTOutput("sum_details")%>%
                                                            shinycssloaders::withSpinner()
                                                       )
                                                ), br()
                                              ))
                                ))
                            )
                   ),
                   tabPanel("Activity Specific Summaries",
                            
                            fluidRow( br(), 
                                      useShinyalert(),
                                      p("Here, you will be able to generate summaries of all activities by location and genotypes. 
                               Use the date range option to subset data for a specified period. 
                               You must select at least one variable under aggregate by box"),
                                      
                                      # hidden(
                                      div(id = "gsum_id",
                                          panel_div(class_type = "default",
                                                    content = tags$div(
                                                      column(12,offset = 11, 
                                                             downloadBttn("save_gen_summary", size = "xs")), br(), br(),
                                                      column(12,
                                                             div(style = 'overflow-x: scroll',
                                                                  DT::DTOutput("activityDT")%>%
                                                                    shinycssloaders::withSpinner()
                                                             )
                                                        ), br()
                                                    )), br(),
                                          
                                          div(id="drill",
                                              h5("Click any row in table above for more details")
                                          ),br(),
                                          #     )
                                      ),
                                      hidden(
                                        div(id="gdetail_id",
                                            panel_div(class_type = "default",
                                                      content = tags$div(
                                                        column(12,offset = 11, 
                                                               downloadBttn("save_gen_details", size = "xs")), br(), br(),
                                                        column(12,
                                                               div(style = 'overflow-x: scroll',
                                                                      DT::DTOutput("activity_details") %>%
                                                                        shinycssloaders::withSpinner()
                                                               )
                                                            ), br()
                                                      ))
                                        ))
                            )
                   )
                   
                 )
               )
             )
           )
  )


