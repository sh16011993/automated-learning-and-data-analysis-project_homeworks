########
# HW4 
# Instructor: Dr. Thomas Price
#
# Group 6 
# Shashank Shekhar - (sshekha4)
# Rahul Aettapu -(raettap)
# Purva Vasudeo - (ppvasude)
############################


library(dplyr)
library(ggplot2)

alda_calculate_sse <- function(data_df, cluster_assignments){
  # Calculate overall SSE
  # Input:
    # data_df: data frame that has been given to you by the TA (x,y attributes)
    # cluster_assignments: cluster assignments that have been generated using any of the clustering algorithms
  # Output:
    # A single value of type double, which is the total SSE of the clusters.
  
 #append cluster assignments to data frame
 data_df$cluster_col <- cluster_assignments
 #order data as per cluster assignment
 data_df <- data_df[order(data_df$cluster_col, decreasing=FALSE),]
 #get mean of each cluster
 mean_stats <- aggregate(data_df[, 1:2], list(data_df$cluster_col), mean)
 
 #initializations
 clust_num <- 0
 sum <- 0
 ans <- c()
 for(row in 1:nrow(data_df)){
   #get cluster assignment for data point
   self_clust_num <- data_df$cluster_col[row]
   x_val <- data_df$x[row]
   y_val <- data_df$y[row]
   #save SSE data for previous cluster number
   if(self_clust_num != clust_num){
     if(clust_num != 0){
       ans <- c(ans,as.double(sum))
     }
     
     #Re-initialize for new cluster
     sum <- 0
     clust_num <- clust_num + 1
     
   }
   avg_x <- 0
   avg_y <- 0
   
   #loop to get mean of cluster from cluster means table
   for(clust_avg in 1 :nrow(mean_stats)){
     clust_val <- mean_stats$Group.1[clust_avg]
    
     #if this is the mean for the cluster, set the same and break
     if(clust_val == clust_num){
       avg_x <- mean_stats$x[clust_avg]
       avg_y <- mean_stats$y[clust_avg]
       break
     }
   }
   
   #SSE calculation
   sum <- sum + (x_val - avg_x)*(x_val - avg_x) + (y_val - avg_y)*(y_val - avg_y)
 }
 #add final cluster SSE to vector
 ans <- c(ans,as.double(sum))

 return(sum(ans, na.rm = TRUE))
 
  
}



alda_kmeans_elbow_plot <- function(data_df, k_values){
  # ~ 8-10 lines
  # Input:
    # data_df: Original data frame supplied to you by the TA
    # k_values: A vector of values of k for k means clustering
  
  # General Information:
    # Run k means for all the values specified in k_values, generate elbow plot
    # Use alda_cluster with kmeans as your clustering type
    # (you can see an example this function call in hw4_checker.R for k = 2, now repeat it for all k_values)
  
  # Output:
    # Nothing, simply generate a plot and save it to disk as "GroupNumber_elbow.png"
  sum_sse <- c()
  
  #loop through k values for SSE calculations
  for(k_val in k_values){
    kmeans_result <- alda_cluster(data_df = clustering_data, n_clusters = k_val, clustering_type = "kmeans")
    #calculate SSE for this k value
    kmeans_sse <- alda_calculate_sse(data_df = clustering_data, cluster_assignments = kmeans_result)
    #add to vector of SSE
    sum_sse <- c(sum_sse,kmeans_sse)
  }
  png("G06_elbow.png")
  plot(x = k_values, y = sum_sse, type = "b",xlab = "Number of clusters", ylab = "Sum of squared errors")
  dev.off()
}


alda_cluster <- function(data_df, n_clusters, clustering_type){
  set.seed(100) # this is v. important from reproducibility point of view
  # Perform specified clustering
  
  # Inputs:
  # data_df: The dataset provided to you, 2-dimensional (x1,x2)
  # n_clusters: number of clusters to be created
  # clustering_type: can be one of "kmeans" or "single-link" or "complete-link"
  
  # Outputs:
  # Cluster assignments for the specified method (vector, with length = nrow(data_df) and values ranging from 1 to n_clusters)
  if(clustering_type == "kmeans"){
    # ~ 1-2 lines
    # allowed packages for kmeans: R-base, stats, dplyr
    # set the max number of iterations to 100, number of random restarts = 1 (let's not break the TA's computer! )
    # choose "Lloyd" as the algorithm 
    
    #create cluster model using kmeans Lloyd algorithm
    kmeans_ans <- kmeans(data_df, n_clusters, iter.max = 100, nstart = 1,algorithm = "Lloyd")
    
    return(c(kmeans_ans$cluster))
    
    
    
  }else if(clustering_type == "single-link"){
    # ~ 3-5 lines
    # Allowed packages for single-link: R-base, stats, dplyr
    # Use euclidean distance for distance calculation (Hint: Look at dist method from stats package)
    # Note 1: Can you use the data_df directly for hclust, or do you need to compute something first?
            # What does 'd' mean in hclust? 
    # Note 2: Does hclust return the clusters assignments directly, or does it return a dendrogram? 
            # Hint 2: Look up the stats package for a method to cut the tree at n_clusters
            # Visualize the dendrogram - paste this dendrogram in your PDF 
    
    #create distance matrix
    dist_data <- dist(method = "euclidean",x = data_df)
    hc <- hclust(d = dist_data,method = "single")
    
    #cut dendogram at n clusters
    single_link_ans <- cutree(tree = hc,k = n_clusters)
    
    return(c(single_link_ans))
    
    
    
    
  }else{ #complete link clustering is default
    # ~ 3-5 lines
    # Allowed packages for single-link: R-base, stats, dplyr
    # Use euclidean distance for distance calculation (Hint: Look at dist method from stats package)
    # Note 1: Can you use the data_df directly for hclust, or do you need to compute something first?
    # What does 'd' mean in hclust? 
    # Note 2: Does hclust return the clusters assignments directly, or does it return a dendrogram? 
    # Hint 2: Look up the stats package for a method to cut the dendrogram at n_clusters
    # Visualize the dendrogram - paste this dendrogram in your PDF 
    
    #create distance matrix using euclidean distance
    dist_data <- dist(method = "euclidean",x = data_df)
    #create model
    hc <- hclust(d = dist_data,method = "complete")
    
    #cut tree at n clusters
    complete_link_ans <- cutree(tree = hc,k = n_clusters)
    
    return(c(complete_link_ans))
    
    
      
  }
}



alda_nn <- function(x_train, x_test, y_train, parameter_grid){
  set.seed(100) # this is v. important from reproducibility point of view
  # ~4-7 lines
  # Perform classification using artificial neural networks using the nnet library
  
  # Inputs:
  # x_train: training data frame(4 variables, x1-x4)
  # x_test: test data frame(4 variables, x1-x4)
  # y_train: dependent variable for training data (can be one of the following classes: 0,1,2)
  # parameter_grid: grid of parameters has already been given to you in hw4_checker
  
  # General information
  # Both training data and test data have already been scaled - so you don't need to scale it once again.
  # 1. Use the nnet library 
  # 2. Perform 10 fold cross validation without replacement using caret and nnet
  # 3. Note that I am giving you x_train and x_test as separate variables - do you need to combine them like you did in the previous hws?
  # 4. Perform training using 10-fold cross validation without replacement:
    # 4.1  Use accuracy as your metric
    # 4.2  Use nnet as your method
    # 4.3  Use parameter_grid as your grid
  # 5. Predict using the trained model on the test dataset (x_test)
  
  # Output:
  # A list with two elements, first element = model generated, 
  # second element = predictions on test data (factor)
  
  # NOTE 1: doing as.vector(as.factor(...)) turns it into numeric datatype.
  # NOTE 2: Best way to ensure that your output is of type factor, with the same levels is factor(your_variable, levels=c(specify all your levels here))
  # NOTE 3: If you want to know the best parameters caret chose for you, you can just do print(your trained model using caret), which will print out the final values used for the model
  # NOTE 4: Setting trace = TRUE could help you get insight into how the procedure is done, but set it to FALSE when submitting to reduce clutter 
  # NOTE 5: Remember, there is a penalty for unnecessary print/View function calls in your code.
  # Methods you need to read about:
  # train() (from caret), predict(), nnet()
  
  # allowed packages: R-base, nnet, caret, dplyr
  
  # define training control with 10 fold CV
  train_control <- trainControl(method="cv", number=10)
  
  #combined dataset = training data + class
  train_set<-data.frame(x_train,y_train=y_train) 
  model <- train(y_train ~., data=train_set, metric = "Accuracy", trControl=train_control, method="nnet", tuneGrid=parameter_grid,trace="false")
  #give prediction according to model
  pred <- predict(model,x_test) 
  #generate factor
  factor_ans <- factor(pred)
  result <- list(model,factor_ans)
  
  return(result)
  
  
  
  
}


