#######################
# ALDA: hw0.R
# Instructor: Dr. Thomas Price
# Mention your team details here
# Shashank Shekhar - sshekha4
# Purva Vasudeo - ppvasude
# Rahul Aettapu - raettap
#########################
require(ggplot2)
set.seed(123)
# no install.packages or rm(list=ls(all=T)) in this function


intro_to_r <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: a  list:  [generated vector (type: double), mean of vector (type: double), median of the vector (type: double), max value of vector (type: double), min value of the vector (type: double)]
	
	new_vector <- runif(num_values)
  new_mean <- mean(new_vector)
  new_median <- median(new_vector)
  new_max <- max(new_vector)
  new_min <- min(new_vector)
  return(list(new_vector, new_mean, new_median, new_max, new_min))
}


intro_to_plotting <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: two plots (saved to disk, no return value), descriptions for which have been provided in the hw0 document. 
	
	new_vector <- runif(num_values)
	pdf("G06_plot01.pdf")
	plot(new_vector, new_vector, main="Plot 1", xlab="x-axis", ylab="y-axis")
	dev.off()
	pdf("G06_plot02.pdf")
	plot(new_vector, new_vector*new_vector, main="Plot 2", xlab="x-axis", ylab="y-axis")
	dev.off()
}
intro_to_plotting(10)
# do not call either function in this script 
# this script is only for function definitions
# if you wish to test your script, call your functions separately in a separate script.