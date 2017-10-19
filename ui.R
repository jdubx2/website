# .navbar-header {height: 0px; width: 0px;}
# .navbar-brand {display: none;}


library(shiny)
library(shinythemes)
library(highcharter)


shinyUI(navbarPage( "Jesse Weiss", theme = shinytheme("cyborg"),
                    
                    tabPanel("Welcome",
                             
                             
                             tags$head(
                               tags$link(rel = "stylesheet", type = "text/css", href = "os.css"),
                               tags$style(HTML("
                                               * {font-family: 'Open Sans', sans-serif;}
                                                .navbar-nav{font-size: 23px;}
                                                .navbar-brand {font-size: 26px;}
                                                li:hover{background-color: #121F1F;}
                                                li.active{background-color: #121F1F;}
                                                .jw {width: 60%; margin: 0 auto; text-align: center;}
                                               "))),
                             
                            fluidRow(tags$div(class = 'jw', tags$b("This text is bold."))),
                            fluidRow()
                             
                               ),
                    tabPanel("Projects"),
                    tabPanel("Resume"),
                    tabPanel("Contact")
                               )
        )
