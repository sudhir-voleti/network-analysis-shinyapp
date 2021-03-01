####################################################
#      Network App    #
####################################################

library("shiny")
library("igraph")
library('visNetwork')
library('dplyr')
library("tidyverse")
library('randomcoloR')
library("stringr")
#library("foreign")

fluidPage(
  
   # Header:
  #titlePanel(title="Network"),
  title = "Network Analysis",
  titlePanel(title=div(img(src="logo.png",align='right'),"Network App")),
  
  # Input in sidepanel:
  sidebarPanel(
    h4(p("Data Input")),
    fileInput("file", "Upload Adjacency Matrix (csv file with header)"),
    fileInput("file1", "Upload Demographics data (csv file with header)"),
    selectInput("mode","Mode of Graph",c("directed", "undirected","max", "min", "upper",
                                         "lower", "plus"),"undirected"),
    # selectInput("comm","Find Communities",c("Yes", "No"),"No"),
    htmlOutput("yvarselect"),
    selectInput("cex2", "Vertex Size based on", c("Degree","Betweeness","Closeness"),"Degree"),
    sliderInput("cex", "Increase vertex size by", min = 20,  max = 100, value = 50,round = FALSE),
    
    br(),
   # h5(p("Powered By:")),
    #img(src = "logo.png")
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                
                tabPanel("Overview",
                            tags$br(),
                            p("Network analysis is a set of integrated techniques to depict relations among nodes and to analyze the structures that emerge from the recurrence of these relations. This app will help you in analyzing such relationships.",align="justify"),
                            
                            h4(p(tags$b("Data Input"))),
                            
                            p("This shiny application requires following two different types of data input from the users"),
                            
                            tags$b("1. Adjacency Matrix"),
                            p("It represents the relationship between the nodes & input file looks like"),
                            
                            img(src = "input_adj.jpg", height = 180, width = 400),
                            tags$br(),
                            downloadButton('downloadData', 'Download Example adjacecny matrix file'),
                            
                            tags$br(),
                            tags$br(),
                            tags$b("2. Demographic Data"),
                            p("It represents demographic data of individual nodes & input file looks like"),
                            
                            img(src = "input_demo.jpg", height = 180, width = 400),
                            tags$br(),
                            downloadButton('downloadData2', 'Download Example demographic file'),
                            tags$br(),
                            tags$br(),
                         
                         
                         p("Once csv file is uploaded successfully, application will display a overall network, community plot and network of communities in  different tabs.",align="justify"),
                         tags$br(),
                        
                         tags$b(tags$i("Note:")),
                         tags$br(),
                         p(("1. Node names should not contain any special character.")),
                         p("2. Download will not work with RStudio interface. It will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         img(src = "example1.png") #, height = 280, width = 400
                         
                ),
                #
                tabPanel('Network Plot',
                         plotOutput("graph1", height = 800, width = 840),
                         visNetworkOutput('int_net',width = '800px',height = '600px')),
                #tabPanel("Network Plot",plotOutput("graph1", height = 800, width = 840)),
                tabPanel("Communities Plot",plotOutput("graph2", height = 800, width = 840),uiOutput("graph3"),helpText("Note: Seperate plot for community with size = 1 won't be shown")),
                #visNetworkOutput('comm_plot')),
                #plotOutput("graph2", height = 800, width = 840),
                #uiOutput("graph3")), #, height = 800, width = 840
                tabPanel("Network Centralities",br(),
                         downloadButton('downloadData1', 'Download Centralities file (Works only in browser)'), br(),br(),
                         dataTableOutput("centdata")),
                tabPanel("Network Structure",visNetworkOutput("com_net",height = 800, width = 840),dataTableOutput('com_cent'))
                )
            ) 
        ) 
    
