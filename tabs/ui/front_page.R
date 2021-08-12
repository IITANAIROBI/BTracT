BTracTFooter <- function() tagList(
  fluidRow(
    HTML('<footer align="center">
         <div class="row">
         <div class="col-sm-2 col-sm-offset-3">
         <a href = "http://breedingbetterbananas.org/">
         <img src = "bbb.png", height="65", width="120">
         </a>
         </div>
         <div class="col-sm-2">
         <a href = "http://www.iita.org/">
         <img src = "iita.png", height="65", width="120">
         </a>
         </div>
         <div class="col-sm-2">
         <a href = "https://btiscience.org/">
         <img src="bti.png", height="70", width="120">
         </a>
         </div>
         </div>
         </footer>')
  )
)


frontpage <- 
  tabPanel("BTracT",
    tags$style(".topimg {
          margin-left:-30px;
          margin-right:-30px;
          margin-top:-15px;
        }"), #br(),br(),br(),
  div(class="topimg", img(src="bunch.png", width="100%")),
  fluidRow(
    column(6, offset = 3,
      tags$p(class = "intro", style = "font-size: 20px; font-family: serif;","This is a real time application to monitor banana breeding activities. It brings together datasets produced by BTracT tool from various locations. Information is presented using dynamic graphs and data tables."),
      tags$hr(style="height:5px;border:1px;color:#000;background-color:#998900;"),
      tags$p(style="font-family: serif;","Main subject-area groupings of BTracT data are shown on the toolbar above. To navigate around this site, left-click one of the tabs and for tabs with categories choose the drop down list."),
      br(),
      tags$p(style="font-family: serif;","Graphs can be downloaded as png and tables can be downloaded to a csv file. Information on how to use the graphs and tables, along with an example, is available in the",
             tags$a("Help", title = "Help Tab", href = "#", id = "HelpTabLink"), "tab.",
             tags$script("$('#HelpTabLink').click(function(){$('a[data-value=\"Help\"]')[0].click();});")
        ),
      br(),
      tags$img(class = "rear-preview", src = "odk.png"), br(),
      
      tags$p(style="font-family: serif;","BTracT is an ",span(class = "bold", "automated"),
                  "system for tracking banana crosses and seeds. All accessions are designated with unique barcode identifier.
                   This system ensures reduced number of errors while also providing instant access to reports"),
                   
      tags$p(tags$span(class = "bold", "PLEASE NOTE:"), style="font-family:serif;",
                   "This webpage may time-out if left idle too long, which will cause the screen to grey-out.",
                   "To use the webpage again, refresh the page. This will reset all previously-selected input options."),
      br(), br(),br(), br(),
      BTracTFooter(),
      br()
      )
  )
)
  