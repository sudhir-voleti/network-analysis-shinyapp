max_plots = 30
# wc1 = function(n,mat1,mat2){
#   for (i in 1:ncol(mat2)) {
#     freq = as.matrix(mat1[(match(rownames(mat2[(mat2[,i] > 1),]),rownames(mat1))),][,i])
#     freq = as.matrix(freq[(order(freq[,1], decreasing=T)),])
#     top_word = as.matrix(freq[1:n,])
#     #mypath <- (paste("WC_mat2_theta_topic",i,"_",n, ".jpeg", sep = ""))
#     #jpeg(file = mypath, pointsize = 12,  width = 800, height = 800, quality=200)
#     wordcloud(rownames(top_word), top_word, scale = c(8, 1), 1, , random.order=FALSE, random.color=FALSE, colors=c(1:4));
#     dev.off()
#   }
# }


# centralities <- read.csv("D://pgp 21 survey//network an data//Centralities (1).csv")
# adj_mat <- read.csv("D://pgp 21 survey//network an data//friendship_net_adj_mat_co21.csv")

#--------------code for network structure tab -----------------------#

network_structure <- function(centralities,adj_mat){
  # input:
  #-----------
  # centralities : centralities dataframe which contains community membership of each node
  # adj_mat : raw_adj matrix
  
  # output:
  #-----------
  # community_el : community edge list dataframe which shows how many member from one community is connected to others
  
  # Description:
  #-------------
  # step 1: filter data of each community (loop 1)
  # step 2: for each node in filtered df extract its connected node from adj_mat (loop 2)
  # step 3: group by and count how many members from one community is connected to others
  
  community_el_final <- data.frame()
  communities <- unique(centralities$Community)
  community_el_base <- data.frame(Community=communities)
  community_el_base$temp <- rep(0,nrow(community_el_base))
  
  for (i in communities){
    filtered_community <- centralities %>% filter(Community==i) #filter all the members of community i
    filtered_community_mat <- adj_mat[rownames(adj_mat) %in% filtered_community$Resp.Name,] # filter adjacency matrix of 
    final_df <- data.frame() #empty dataframe to store all results
    
    print(i)
    for(j in 1:nrow(filtered_community)){
      resp <- filtered_community$Resp.Name[j] 
      print(resp)
      respondent_network <- data.frame(adj_mat[rownames(adj_mat)==resp,])
      
      # respondent_network_T <- data.frame(t(respondent_network))
      colnames(respondent_network) <- c('friendship_score')
      temp <- respondent_network %>% filter(friendship_score!=0)
      temp$Resp.Name <- rownames(temp)
      #print(j)
      
      temp2 <- left_join(temp, centralities,by="Resp.Name") # get community membership of each friend
      final_df <- rbind(final_df,temp2) 
      
    }
    t <- final_df %>% group_by(Community)%>%summarise(n())
    #print(t)
    
    temp_df2 <- left_join(community_el_base,t,by="Community")
    temp_df2$outdegree <- temp_df2$temp +temp_df2$`n()`
    temp_df2$from <- i
    temp_df2$temp <- NULL
    temp_df2$`n()` <- NULL
    temp_df2$outdegree <- replace_na(temp_df2$outdegree,0)
    community_el_final <- rbind(community_el_final,temp_df2)
    
    
  }
  
  colnames(community_el_final)[1] <- "to"
  community_el_final <- community_el_final[, c(3,1,2)]
  community_el_final$from = paste0('community_', community_el_final$from)
  community_el_final$to = paste0('community_', community_el_final$to)
  community_el_final <- community_el_final[community_el_final$from!=community_el_final$to,]
  return(community_el_final)
  
  
  
}


put_title <- function(df0){
  colnames0 = colnames(df0)
  a01 = data.frame(title0=character())
  for (i1 in 1:nrow(df0)){
    a0 = NULL
    for (i0 in 1:ncol(df0)){ a0 = c(a0, paste0(colnames0[i0], ": ", df0[i1, i0], "<br>")) }
    a01[i1, 1] = str_c(a0, collapse="") }
  return(a01)}


min_max_scaler <- function(x){
  x <- (x-min(x))/(max(x)-min(x))
  return(x)
}
