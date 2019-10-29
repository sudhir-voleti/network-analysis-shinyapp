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
                
                tabPanel("Overview",
                         h4(p("How to use this shiny application")),
                         p("This shiny application require two types of data input from the user.
                            one is adjacency Matrix which represents relationship between nodes & the other one is demographic data of individual node.
                            
                            Please note both the file should be 'csv' format.
                         
                            click on the Browse (in left side-bar panel) and upload the csv data input file.",align="justify"),
                         
                         p("Once csv file is uploaded successfully, application will display a overall network plot,community plot and summary statiscs of network in three different tabs.",align="justify"),
                         br(),
                         h4(p("Download Sample Adjacency Matrix")),
                         downloadButton('downloadData', 'Download Example adjacecny matrix file'),
                         h4(p('Download sample Demographic')),
                         downloadButton('downloadData2', 'Download Example demographic file'),
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         img(src = "example1.png") #, height = 280, width = 400
                         
                ),
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
