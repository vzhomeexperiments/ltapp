#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
library("plotly")
library("DT")
path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
path_data <- file.path(path_user, "_DATA")
macd_ai <- readr::read_rds(file.path(path_data, 'macd_ai_classified.rds'))

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      
      tags$head(
        # Note the wrapping of the string in HTML()
        tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: #fffae8;
       
      }
     
      .shiny-input-container {
        color: #474747;
      }"))
      ),
      
      navbarPage("EA MANAGEMENT",
                 tabPanel("RESULT",
                          sidebarLayout(
                            sidebarPanel(fluidRow(
                              selectInput(inputId = "Terminal", label = "Select the terminal Number",choices = 1:5),
                              column(width = 12,fluidRow(
                                column(4,actionButton(inputId = "Refresh", label = "Refresh")),
                                column(8,helpText("Refresh the symbol and magic number selection")))),
                              column(width = 12,fluidRow( 
                                column(6, selectInput(inputId = "MagicNum", label = "Select Magic Number", choices = 1:10)),
                                column(6, selectInput(inputId = "Symbol", label = "Select the symbol",choices = 1:10)))),
                              column(width = 12,fluidRow(
                                column(6,dateInput(inputId = "From", label = "From", value = Sys.Date()-7)),
                                column(6,dateInput(inputId = "To", label = "To", value = Sys.Date())))),
                              column(width = 12,fluidRow(
                                column(6,radioButtons(inputId = "Time",label = "Select time filter", choices = c("Entry Time" , "Exit Time"),selected = "Exit Time")),
                                column(6,selectInput(inputId = "Sort", label = "Sort data by", choices = c("MagicNumber","Ticket","EntryTime", "ExitTime","Profit","Symbol")))))
                            )),
                            mainPanel(
                              tabsetPanel(type = "pills",
                                          tabPanel("Console",verbatimTextOutput("console")),
                                          tabPanel("Data",
                                                   tabsetPanel(
                                                     tabPanel("Data",DT::dataTableOutput("data")))),
                                          #tabPanel("Balance",tableOutput("balance"))
                                          
                                          tabPanel("Graph",
                                                   tabsetPanel(
                                                     tabPanel("Profit",plotlyOutput("profitGraph")),
                                                     tabPanel("Balance", plotlyOutput("balanceGraph")))),
                                          tabPanel("Account",
                                                   tabsetPanel(
                                                     tabPanel("Result",em("Click Refresh to update the data"),
                                                              br(),
                                                              DT::dataTableOutput("watchDogReport")),
                                                     tabPanel("Graph",plotlyOutput("equityGraph")))),
                                          
                                          tabPanel( 
                                            "Report",br(),
                                            
                                            column(width = 12,fluidRow(
                                              column(3,strong("Total Trades :", style = "text-decoration: underline;")),
                                              column(3,textOutput("totalTrade")))),
                                            column(width = 12, fluidRow(
                                              column(3,strong("Profit Factor :", style = "text-decoration: underline;")),
                                              column(3,textOutput("profitFactor")))),
                                            column(width = 12, fluidRow(
                                              column(3,strong("Maximum Profit :", style = "text-decoration: underline;")),
                                              column(3,textOutput("maxProfit")))),
                                            column(width = 12, fluidRow(
                                              column(3,strong("Minimum Profit :", style = "text-decoration: underline;")),
                                              column(3,textOutput("minProfit")))),
                                            column(width = 12, fluidRow(
                                              column(3,strong("Total Profit :", style = "text-decoration: underline;")),
                                              column(3,textOutput("totalProfit"))),
                                              p(),p(),br()),
                                            column(width = 12,fluidRow(
                                              column(12,DT::dataTableOutput("result")))),
                                          ))))),
                 tabPanel(
                   "MT INSPECTION",sidebarLayout(
                     sidebarPanel(
                       sliderInput("rows",
                                   "Number of rows:",
                                   min = 1,
                                   max = nrow(macd_ai),
                                   value = 1,
                                   step = 1),
                       actionButton(inputId = "BUN", label = "BUN"),
                       actionButton(inputId = "BUV", label = "BUV"),
                       actionButton(inputId = "BEN", label = "BEN"),
                       actionButton(inputId = "BEV", label = "BEV"),
                       actionButton(inputId = "RAN", label = "RAN"),
                       actionButton(inputId = "RAV", label = "RAV"),
                       actionButton(inputId ="BOOM", label = 'BOOM')
                     ),
                     mainPanel(
                       plotOutput("AI_Data")
                     ))
                 ),
                 tabPanel(
                   "MODEL INSPECTION", sidebarLayout(
                     sidebarPanel(actionButton(inputId = "RefreshM60", label = "Refresh"),
                                  helpText("Write the last model inspection result and Refresh the app")),
                     mainPanel(tabsetPanel(type = "pills",
                                           tabPanel("Result",
                                                    tabsetPanel(type = "tabs",
                                                                tabPanel("Simulation",DT::dataTableOutput("dataRes")),
                                                                tabPanel("Performance",DT::dataTableOutput("AnalyseResult")))),
                                           tabPanel("Graph",
                                                    tabsetPanel(type = "tabs",
                                                                tabPanel("Simulation",plotOutput("strategyTestResults")),
                                                                tabPanel("Performance",plotOutput(("modelPerformance"))))))),
                     position = "right")),
                 tabPanel(
                   "PERFORMANCE",sidebarLayout(
                     sidebarPanel(fluidRow(
                       column(width = 12,fluidRow(
                         column(6,dateInput(inputId = "FromCopy", label = "From", value = "2021-04-01")),
                         column(6,dateInput(inputId = "ToCopy", label = "To", value = Sys.Date()))))),
                       
                     ),
                     mainPanel(tabsetPanel( type = "pills",
                                            tabPanel("Log",DT::dataTableOutput("perfLog")),
                                            tabPanel("Graph",br(),plotlyOutput("perfGraph")))),
                     position = "right")
                 )
      )))
  }

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'MonitorGolem'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

