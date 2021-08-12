#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
shinyUI(
      tagList(
        
        div(style="padding: 1px 0px; width: '100%'",
            titlePanel(
              title="", windowTitle = "BTracT"
            )
        ),
       navbarPage(title = "",
                 id = "navbar",
                 selected = "BTracT",
                 theme = shinythemes::shinytheme("readable"),
                 fluid = T,
                 
                 frontpage,
                 overviewpage,
                 datapage,
                 statuspage,
                 labelspage,
                 aboutpage
       )
      )
)