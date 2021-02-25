#################################################
#      Network App    #
#################################################


shinyServer(function(input, output,session) {
  
Dataset <- reactive({
  if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- read.csv(input$file$datapath ,header=TRUE, sep = ",",stringsAsFactors = FALSE)
    rownames(Dataset) <- make.names(Dataset[,1], unique=TRUE)

    colnames(Dataset) <- make.names(colnames(Dataset),unique=TRUE)
    #colnames(Dataset)<- str_replace_all(colnames(Dataset),pattern = "\\.","_")
    
   # rownames(Dataset) <- gsub("[[:punct:]]", "", rownames(Dataset))
    #colnames(Dataset) <- gsub("[[:punct:]]", "", colnames(Dataset))
    #row.names(Dataset) = Dataset[,1]
    Dataset = as.matrix(Dataset[,2:ncol(Dataset)])
    
    
    return(Dataset)
  }
})

Dataset2 <- reactive({
  if (is.null(input$file1)) { return(NULL) }
  else{
    Dataset <- read.csv(input$file1$datapath ,header=TRUE, sep = ",",stringsAsFactors = FALSE)
    colnames(Dataset) <- make.names(colnames(Dataset),unique=TRUE)
    #normalize categorical data
    for(cn in colnames(Dataset[,-1])) {
      Dataset[,cn] = apply(Dataset[,cn,drop=F],2, function(x)  tolower(x))
      Dataset[,cn] = apply(Dataset[,cn,drop=F],2 ,function(x) trimws(x))
      Dataset[cn] <- lapply(Dataset[cn], factor) 
      
    }
    # 
    #colnames(Dataset) <- gsub("[[:punct:]]", "", colnames(Dataset))
    #row.names(Dataset) = Dataset[,1]
    #Dataset = Dataset[,2:ncol(Dataset)]
    return(Dataset)
  }
})

# Select variables:
output$yvarselect <- renderUI({
  if (is.null(input$file1)) { return(NULL) }
  else{
  
  selectInput("colattr", "Select Color variable",
              colnames(Dataset2()[-1]), colnames(Dataset2())[1])
  }
})

graph = reactive({
  graph <- graph.adjacency(Dataset(), mode = input$mode, weighted=NULL)
  graph = igraph::simplify(graph)  
  col.names <- make.names(V(graph)$name, unique = TRUE)
  
  return(graph)
})

wc = reactive({
  wc = cluster_walktrap(graph())
})

centralities = reactive({
  graph = graph()
  metrics <- data.frame(Resp.Name = make.names(V(graph)$name, unique = TRUE),Community = wc()$membership,Degree=igraph::degree(graph), Out.Degree =igraph::degree(graph, v=V(graph), mode=c("out")),In.Degree =igraph::degree(graph, v=V(graph), mode=c("in")),
                        Betweenness=min_max_scaler(igraph::betweenness(graph)), 
                        Closeness = min_max_scaler(igraph::closeness(graph)),
                        Eigenvector.Centrality.Scores = min_max_scaler(eigen_centrality(graph)$vector),
                        Graph.Coreness = min_max_scaler(igraph::graph.coreness(graph)))
  # row.names(metrics) = V(graph)$name
  
  metrics = metrics[(order(metrics[,1],metrics[,2],metrics[,3],metrics[,4],metrics[,5],metrics[,6],metrics[,7], decreasing= T)),]
  
  return(metrics)
})

output$centdata = renderDataTable({
  if (is.null(input$file)) { return(NULL) }
  
  centralities() %>% mutate_if(is.numeric, round, digits=3)
  
}, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
  
output$graph1 = renderPlot({
  if (is.null(input$file)) { return(NULL) }
  else{
    
    colattr = Dataset2()[,input$colattr]
    
    if (is.null(input$file1)) {
      colattr   = 'lightskyblue'
      
      graph = graph()
      
      E(graph)$weight <- count.multiple(graph)
      
      egam = (log(E(graph)$weight)+.3)/max(log(E(graph)$weight)+.3)
      a0 = ((egam < 0 | egam >1)); egam[a0] = 0.5   # added later coz this parm was causing error
      E(graph)$color = rgb(.5,.5,0,egam)
      
      par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
      plot( graph,			#the graph to be plotted
            layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
            vertex.frame.color='lightskyblue', 		#the color of the border of the dots 
            vertex.color= colattr,
            vertex.label.color='black',		#the color of the name labels
            vertex.label.font=1,    			#the font of the name labels
            vertex.size = (input$cex)/10,     # size of the vertex
            vertex.label= make.names(V(graph)$name, unique = TRUE),	    	#specifies the lables of the vertices. in this case the 'name' attribute is used
            vertex.label.cex=(degree(graph)+1)/mean(degree(graph)) * (input$cex/50)		#specifies the size of the font of the labels. can also be made to vary
            
      ) 
      
    }else{
      graph <- graph()
      for(cn in colnames(Dataset2()[,-1])) {
        #print(cn)
        graph = set_vertex_attr(graph, cn,  1:nrow(Dataset2()), value=Dataset2()[,cn])
        n <- length(unique(Dataset2()[,input$colattr]))
        palette <- randomcoloR::distinctColorPalette(n)
        print(n)
        
        node_size <- case_when(input$cex2=="Degree" ~ degree(graph),
                               input$cex2=="Betweeness" ~ sqrt(betweenness(graph))+1,
                               input$cex2=="Closeness" ~ closeness(graph))
        E(graph)$weight <- count.multiple(graph)
        egam = (log(E(graph)$weight)+.3)/max(log(E(graph)$weight)+.3)
        a0 = ((egam < 0 | egam >1)); egam[a0] = 0.5   # added later coz this parm was causing error
        E(graph)$color = rgb(.5,.5,0,egam)
        par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
        plot( graph,			#the graph to be plotted
              layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
              vertex.frame.color='lightskyblue', 		#the color of the border of the dots 
              vertex.color= palette,
              vertex.label.color='black',		#the color of the name labels
              vertex.label.font=1,    			#the font of the name labels
              vertex.size = 5,#(node_size)/input$cex,     # size of the vertex
              vertex.label= make.names(V(graph)$name, unique = TRUE),	    	#specifies the lables of the vertices. in this case the 'name' attribute is used
              vertex.label.cex=(node_size+1)/mean(node_size)*(input$cex)/50		#specifies the size of the font of the labels. can also be made to vary
             )
        legend("topright", legend=levels(Dataset2()[,input$colattr]),horiz = FALSE,bty = "n", pch=19 ,  text.col=palette )
        
      }
     }
    }
    
})

output$graph2 = renderPlot({
  if (is.null(input$file)) { return(NULL) }
  
  else {
    par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
    plot(wc(),
         graph(),			#the graph to be plotted
         layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
         # vertex.frame.color='lightskyblue', 		#the color of the border of the dots
         # vertex.color= colattr,
         vertex.label.color='black',		#the color of the name labels
         vertex.label.font=1,    			#the font of the name labels
         vertex.size = (input$cex)/10,     # size of the vertex
         vertex.label= make.names(V(graph())$name, unique = TRUE)	    	#specifies the lables of the vertices. in this case the 'name' attribute is used
         # vertex.label.cex= input$cex		#specifies the size of the font of the labels. can also be made to vary
         
    ) 
  }
})

output$graph3 <- renderUI({
  plot_output_list <- lapply(1:max(wc()$membership), function(i) {
    plotname <- paste("plot", i, sep="")
    visNetworkOutput(plotname, height = '800px', width = '800px')
  })

  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plot_output_list)
})

a01 = reactive({
  if (is.null(input$file1)) { return(NULL) }
  else{
    
    a01<-put_title(Dataset2())
    a01$name = rownames(Dataset())
    return(a01)
      }
  })

for (i in 1:max_plots) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    
    my_i <- i 
    plotname <- paste("plot", my_i, sep="")
    
    output[[plotname]] <- renderVisNetwork({
      
      adj.mat = Dataset()
      wc = wc()
      a01 = a01()
      
      test = adj.mat[wc$membership == my_i,wc$membership == my_i]
      if(length(test)==1){
        return(NULL)
      }else{
      
      g = graph.adjacency(test, mode=input$mode)
      g = igraph::simplify(g)
      degree = igraph::degree(g)
      pct = round(length(wc$names[wc$membership == my_i])/length(wc$names)*100,2)
      
      if (is.null(input$file1)) {  
      
      graph = toVisNetworkData(g)
      degree = igraph::degree(g)
      graph$nodes = data.frame(graph$nodes, value=degree)
      
      visNetwork(nodes = graph$nodes, edges = graph$edges,
                 main =  paste("Community : ",my_i),
                 submain = paste("Population share - ",pct,"%") ) }
      else{
        graph = toVisNetworkData(g)
       degree = igraph::betweenness(g)
        
       # degree_value<-igraph::degree(g, mode = "in")
        #between_value <- igraph::betweenness(g)
       # closeness_value <- igraph::closeness(g)
        #temp_df <- data.frame(list(Degree = degree_value,Betweeness= between_value,Closeness=closeness_value))
       # centrality <- temp_df%>%select(input$cex2)
        #print(head(centrality))
        
        tempa01 = a01[a01$name %in% rownames(test),]
        graph$nodes = data.frame(graph$nodes, value=degree, title=tempa01$title0)
       # graph$nodes = data.frame(graph$nodes,title=a01$title0)
        # visNetwork(nodes = graph$nodes, edges = graph$edges,
        #            main =  paste("Community : ",my_i),
        #            submain = paste("Population share - ",pct,"%"))
        # 
       # visIgraph(g)%>%
        #visLegend(main = paste("Community : ",my_i),position = "left",zoom = FALSE)
        
        visNetwork(graph$nodes, 
                   graph$edges,
                   main =  paste("Community : ",my_i),
                   submain = paste("Population share - ",pct,"%") ) %>%
          visLayout(randomSeed = 123) # to always have the same network 
        
      }
      
      
     
      
      
      
      
      
      
      
      # plot(adj.mat)
      # par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
      # plot( graph,			#the graph to be plotted
      #       layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
      #       vertex.frame.color='lightskyblue', 		#the color of the border of the dots
      #       #vertex.color= colattr,
      #       vertex.label.color='black',		#the color of the name labels
      #       vertex.label.font=1,    			#the font of the name labels
      #       #vertex.size = input$cex2,     # size of the vertex
      #       vertex.label= make.names(V(graph)$name, unique = TRUE)    	#specifies the lables of the vertices. in this case the 'name' attribute is used
      #       #vertex.label.cex=(degree(graph)+1)/mean(degree(graph)) * input$cex		#specifies the size of the font of the labels. can also be made to vary
      # 
      # )

      
      
      #title(paste("Community : ",my_i), sub = paste("Population share - ",pct,"%"))
      }
      })
  })
}



output$int_net <- renderVisNetwork({
  if (is.null(input$file)) { return(NULL) }
  #if (is.null(input$file1)) { return(NULL) }
  adj_mat<-Dataset()
  demo_data <- Dataset2()
  G <- graph.adjacency(adj_mat,weighted = T,mode = input$mode)
  G<-igraph::simplify(G)
  G_vis <- toVisNetworkData(G)
  #- create nodes and edge df---#
  nodes <- data.frame(id = G_vis$nodes$id)
  edges <- data.frame(from = G_vis$edges$from, to = G_vis$edges$to)
  
  #-----setting nodes property----#
  degree_value<-igraph::degree(G, mode = "in")
  between_value <- igraph::betweenness(G)
  closeness_value <- igraph::closeness(G)
  nodes$size <- case_when(input$cex2=="Degree" ~ degree_value[match(nodes$id, names(degree_value))],
                          input$cex2=="Betweeness" ~ between_value[match(nodes$id, names(between_value))],
                          input$cex2=="Closeness" ~closeness_value[match(nodes$id, names(closeness_value))]
                          )
  nodes$size<- round((nodes$size/max(nodes$size))*input$cex,digits = 0)
  #colms<-colnames(demo_data)
  if (!is.null(input$file1)) { nodes$title <- do.call(paste, c(demo_data[,], sep = "<br>"))
                                              # paste("Name", demo_data[,1], "<br>",
                                              #      "Gender:", demo_data[,2], "<br>",
                                              #      "Yrs of Exp:", demo_data[,3],"<br>",
                                              #      "Native_Language:",demo_data[,4],"<br>")
                               nodes$group = demo_data[,which(colnames(demo_data) %in% input$colattr)]

  }
 # nodes$font.size <-input$cex
  
  
  #-------------------------------#
  
  if(input$mode=='directed'){
    visNetwork(nodes,edges)%>%
      visIgraphLayout('layout.fruchterman.reingold')%>%
      visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = T,selectedBy = 'group')%>%
    visEdges(arrows = 'from', scaling = list(min = 2, max = 2))
  }else{ 
    visNetwork(nodes,edges)%>%
      visIgraphLayout('layout.fruchterman.reingold')%>%
      visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = T,selectedBy = 'group')
  }
 

})

output$comm_plot <- renderVisNetwork({
  G<-graph()
  wc<-wc()
  demo_data<- Dataset2()
  V(G)$community <- wc$membership
  nodes <- data.frame(id = V(G)$name, title = V(G)$name, group = V(G)$community)
  nodes <- nodes[order(nodes$id, decreasing = F),]
  if (!is.null(input$file1)) { nodes$title <- do.call(paste, c(demo_data[,], sep = "<br>"))
                              # paste("Name", demo_data[,1], "<br>",
                              #      "Gender:", demo_data[,2], "<br>",
                              #      "Yrs of Exp:", demo_data[,3],"<br>",
                              #      "Native_Language:",demo_data[,4],"<br>")
                              }
  
  edges <- get.data.frame(G, what="edges")[1:2]
  visNetwork(nodes, edges) %>%
    visIgraphLayout('lay vout.fruchterman.reingold')%>%
    visOptions( highlightNearest = TRUE, nodesIdSelection = FALSE,selectedBy = list(variable="group",multiple=TRUE))
  
})



output$downloadData1 <- downloadHandler(
  filename = function() { paste(str_split(input$file$name,"\\.")[[1]][1],"_centralities.csv",collapse = "") },
  content = function(file) {
    write.csv(centralities(), file, row.names=F)
  }
)

output$downloadData <- downloadHandler(
  filename = function() { "friendship network adj mat mktr Co2017.csv" },
  content = function(file) {
    write.csv(read.csv("data/friendship network adj mat mktr Co2017.csv"), file, row.names=F, col.names=F)
  }
)

output$downloadData2 <- downloadHandler(
  filename = function() { "data/friendship network demographics mktr Co2017.csv" },
  content = function(file) {
    write.csv(read.csv("data/friendship network demographics mktr Co2017.csv"), file, row.names=F, col.names=F)
  }
)


output$com_net = renderVisNetwork({
  if (is.null(input$file)) { return(NULL) }
  
  com_el <- network_structure(centralities(),Dataset())
  com_graph <- graph.data.frame(as.matrix(com_el), directed = FALSE)
  com_graph <- igraph::simplify(com_graph)
  E(com_graph)$weight=as.numeric(com_el[,3]) 
  E(com_graph)$width <- E(com_graph)$weight
  cut.off <- mean(E(com_graph)$weight) 
  print(cut.off)
  com_graph_1<-delete.edges(com_graph, which(E(com_graph)$weight<=cut.off))
  visIgraph(com_graph_1)#,
       # layout=layout.fruchterman.reingold,
       # edge.width=E(com_graph)$weight/2,
       # vertex.size = (input$cex)/5)

})

com_cent = reactive({
  com_el <- network_structure(centralities(),Dataset())
  com_el_1 <- com_el[com_el[,3]!=0,]
  com_graph <- graph_from_data_frame(com_el_1)
  metrics <- data.frame(Community.Name = make.names(V(com_graph)$name, unique = TRUE),Degree=igraph::degree(com_graph), Out.Degree =igraph::degree(com_graph, v=V(com_graph), mode=c("out")),In.Degree =igraph::degree(com_graph, v=V(com_graph), mode=c("in")),
                        Betweenness=igraph::betweenness(com_graph), Closeness = igraph::closeness(com_graph), Eigenvector.Centrality.Scores = eigen_centrality(com_graph)$vector, Graph.Coreness = igraph::graph.coreness(com_graph))
  # row.names(metrics) = V(graph)$name
  
  metrics = metrics[(order(metrics[,1],metrics[,2],metrics[,3],metrics[,4],metrics[,5],metrics[,6],metrics[,7], decreasing= T)),]
  
  return(metrics)
})

output$com_cent = renderDataTable({
  if (is.null(input$file)) { return(NULL) }
  
  com_cent()
}, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))

})













#input = list(file = list(datapath = 'C:\\Users\\30773\\Desktop\\MKTR 2017\\friendship network adj mat mktr 2017.csv'),
#mode = 'directed')
