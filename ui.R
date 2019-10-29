####################################################
#      Network App    #
####################################################

library("shiny")
library("igraph")
library('visNetwork')
library('dplyr')
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  headerPanel("Network App"),
  # Input in sidepanel:
  sidebarPanel(

    h5(p("Data Input")),
    fileInput("file", "Upload Adjacency Matrix (csv file with header))"),
    fileInput("file1", "Upload Demographics data (csv file with header))"),
    selectInput("mode","Mode of Graph",c("directed", "undirected","max", "min", "upper",
                                         "lower", "plus"),"undirected"),
    # selectInput("comm","Find Communities",c("Yes", "No"),"No"),
    htmlOutput("yvarselect"),
    selectInput("cex2", "Vertex Size based on", c("Degree","Betweeness","Closeness"),"Degree"),
    sliderInput("cex", "Increase vertex size by", min = 20,  max = 100, value = 50,round = FALSE),
    
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                tabPanel('Network Plot',visNetworkOutput('int_net',width = '800px',height = '600px')),
                #tabPanel("Network Plot",plotOutput("graph1", height = 800, width = 840)),
                tabPanel("Communities Plot",plotOutput("graph2", height = 800, width = 840),uiOutput("graph3")),
                #visNetworkOutput('comm_plot')),
                #plotOutput("graph2", height = 800, width = 840),
                #uiOutput("graph3")), #, height = 800, width = 840
                tabPanel("Network Centralities",br(),
                         downloadButton('downloadData1', 'Download Centralities file (Works only in browser)'), br(),br(),
                         dataTableOutput("centdata"))
                )
            ) 
        ) 
    )
