library(shinydashboard)
load("AllReady.Rdata")
source("04-Functions.R")

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Plot", tabName="plot", icon=icon("line-chart"), selected=TRUE),
              menuItem("About", tabName = "about", icon = icon("question"))
              
  ),
  disable=TRUE
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "readme"
            # withMathJax(), 
            # includeMarkdown("readMe.Rmd")
    ),
    tabItem(tabName = "plot",
            fluidRow(
              column(width = 4, 
#                      actionButton("reset_reg", HTML(paste("Reset", "Registration", sep="<br/>"))),
#                      actionButton("reset_to", HTML(paste("Reset", "Turnout", sep="<br/>"))),
#                      actionButton("reset_vote", HTML(paste("Reset", "Vote", sep="<br/>"))),
#                      hr(),
                     
                     tabBox( width = NULL,
                             tabPanel("Registr.",
                                      actionButton("reset_reg", "Reset Registration"),
                                      uiOutput('resetable_registration')
                             ),
                             tabPanel("Turnout",
                                      actionButton("reset_to", "Reset Turnout"),
                                      uiOutput('resetable_turnout')
                             ),
                             tabPanel("Vote",
                                      actionButton("reset_vote", "Reset Vote"),
                                      uiOutput('resetable_voting')
                             )
                     ),
                     uiOutput('table')),
              column(width = 8,
                     #box(  width = NULL, plotOutput("plot2",height="100px"), collapsible = TRUE,
                     #      title = "Plot", status = "primary", solidHeader = TRUE),
                     box(  width = NULL, plotOutput("plot",height="400px"), collapsible = TRUE,
                           title = "Plot", status = "primary", solidHeader = TRUE),
                     radioButtons("base", "Vote weight", 
                                  c("One person" = "count",
                                    "One year of life remaining" ="lexp"),
                                  selected = NULL, inline = FALSE,
                                  width = NULL)
              ))
    )
  )
)

dashboardPage(
  dashboardHeader(title = "Brexit Vote Simulation"),
  sidebar,
  body
)
