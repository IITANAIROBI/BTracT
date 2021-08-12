
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic 
shinyServer(
  function(input, output, session) {
  
    source("tabs/server/overview_server.R", local=T)
    source("tabs/server/data_server.R", local=T)
    source("tabs/server/status_server.R", local=T)
    source("tabs/server/tc_server.R", local=T)
    source("tabs/server/labels_server.R", local=T)
    
})
    