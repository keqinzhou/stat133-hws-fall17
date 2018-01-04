#title: removing missing values 
#description: This function takes a vector and returns a vector without NAs.
#inputs: a vector 
#outputs: the vector with all missing values removed.
remove_missing <- function(v){
  if (sum(is.na(v))>0){
    idx <- which(is.na(v))
  v <- v[-idx]}
  v
}

#title: getting minimum values 
#description: This function takes a vector and if na.rm is true it returns the minimum with NAs removed.
#If na.rm is false, it returns the minimum with NAs. 
#inputs: a vector and na.rm
#outputs: the minimum of the input
get_minimum <- function(v,na.rm){
  if (is.numeric(v)==FALSE){
    stop('non-numeric argument')
  }
  if (na.rm==TRUE){
    v <- remove_missing(v)
  }
  v <- sort(v) 
  v[1]
}

#title: getting maximum values 
#description: This function takes a vector and if na.rm is true it returns the maximum with NAs removed.
#If na.rm is false, it returns the maximum with NAs. 
#inputs: a vector and na.rm
#outputs: the maximum of the input 
get_maximum <- function(v,na.rm){
  if (is.numeric(v)==FALSE){
    stop('non-numeric argument')
  }
  if (na.rm==TRUE){
    v <- remove_missing(v)
  }
  v <- sort(v) 
  v[length(v)]}

#title: getting the range  
#description: This function takes a vector and if na.rm is true it returns the range with NAs removed.
#If na.rm is false, it returns the range with NAs. 
#inputs: a vector and na.rm
#outputs: the range of the input 
get_range <- function(v,na.rm){
  if (is.numeric(v)==FALSE){
    stop('non-numeric argument')
  }
  if (na.rm==TRUE){
    v <- remove_missing(v)
  }
  get_maximum(v,na.rm=FALSE)-get_minimum(v,na.rm=FALSE)
}

#title: getting 10th percentile of the vector 
#description: This function takes a vector and if na.rm is true it returns the 10th percentile with NAs removed.
#If na.rm is false, it returns 10th percentile with NAs. 
#inputs: a vector and na.rm
#outputs: the 10th percentile of the input 
get_percentile10 <- function(v,na.rm){
  if (is.numeric(v)==FALSE){
    stop('non-numeric argument')
  }
  if (na.rm==TRUE){
    v <- remove_missing(v)
  }
  quantile(v,probs=c(0.1),names=F)
}

#title: getting 90th percentile of the vector 
#description: This function takes a vector and if na.rm is true it returns the 90th percentile with NAs removed.
#If na.rm is false, it returns 90th percentile with NAs. 
#inputs: a vector and na.rm
#outputs: the 90th percentile of the input 
get_percentile90 <- function(v,na.rm){
  if (is.numeric(v)==FALSE){
    stop('non-numeric argument')
  }
  if (na.rm==TRUE){
    v <- remove_missing(v)
  }
  quantile(v,probs=c(0.9),names=F)
}

#title: getting the median of a vector 
#description: This function takes a vector and if na.rm is true it returns the median with NAs removed.
#If na.rm is false, it returns the median with NAs. 
#inputs: a vector and na.rm
#outputs: the median of the input 
get_median <- function(v,na.rm){
  if (is.numeric(v)==FALSE){
    stop('non-numeric argument')
  }
  if (na.rm==TRUE){
    v <- remove_missing(v)
  }
  v <- sort(v)
  if(length(v) %% 2 == 0){
    median <- (v[length(v)/2]+v[length(v)/2+1])/2
  }
  else{
   median <-  v[(length(v)+1)/2]
  }
  median}

#title: getting the average of a vector 
#description: This function takes a vector and if na.rm is true it returns the average with NAs removed.
#If na.rm is false, it returns the average with NAs. 
#inputs: a vector and na.rm
#outputs: the average of the input 
get_average <- function(v,na.rm){
  if (is.numeric(v)==FALSE){
    stop('non-numeric argument')
  }
  if (na.rm==TRUE){
    v <- remove_missing(v)
  }
  avg <- 0
  for(i in 1:length(v)){
    avg <- avg + v[i]/length(v)
  }
  avg
}

#title: getting the standard deviation of a vector 
#description: This function takes a vector and if na.rm is true it returns the standard deviation with NAs removed.
#If na.rm is false, it returns the standard deviation with NAs. 
#inputs: a vector and na.rm
#outputs: the sd of the input 
get_stdev <- function(v,na.rm){
  if (is.numeric(v)==FALSE){
    stop('non-numeric argument')
  }
  if (na.rm==TRUE){
    v <- remove_missing(v)
  }
  sd <- 0
  avg <- get_average(v,na.rm=FALSE)
  for(i in 1:length(v)){
    sd <- sd + (v[i]-avg)^2
  }
  sqrt(1/(length(v)-1)*sd)
}

#title: getting the 1st quartile of a vector 
#description: This function takes a vector and if na.rm is true it returns the 1st quartile with NAs removed.
#If na.rm is false, it returns the 1st quartile with NAs. 
#inputs: a vector and na.rm
#outputs: the 1st quartile of the input 
get_quartile1 <- function(v,na.rm){
  if (is.numeric(v)==FALSE){
    stop('non-numeric argument')
  }
  if (na.rm==TRUE){
    v <- remove_missing(v)
  }
  quantile(v,probs=c(0.25),names=F)
}

#title: getting the 3rd quartile of a vector 
#description: This function takes a vector and if na.rm is true it returns the 3rd quartile with NAs removed.
#If na.rm is false, it returns the 3rd quartile with NAs. 
#inputs: a vector and na.rm
#outputs: the 3rd quartile of the input 
get_quartile3 <- function(v,na.rm){
  if (is.numeric(v)==FALSE){
    stop('non-numeric argument')
  }
  if (na.rm==TRUE){
    v <- remove_missing(v)
  }
  quantile(v,probs=c(0.75),names=F)
}

#title: counting the number of missing values 
#description: This function takes a vector and it returns the number of NAs in the input.
#inputs: a vector 
#outputs: the number of NAs
count_missing <- function(v){
 n <- length(v)
 n_2 <- length(remove_missing(v))
 n-n_2
}

#title: getting the summary statistics of a vector 
#description: This function takes a vector and returns the list of summary statistics with NAs removed.
#inputs: a vector
#outputs: a list of summary statistics
summary_stats <- function(v){
  list(minimum = get_minimum(v,na.rm=TRUE),
       percent10 = get_percentile10(v,na.rm=TRUE),
       quartile1 = get_quartile1(v,na.rm=TRUE),
       median = get_median(v,na.rm=TRUE),
       mean = get_average(v,na.rm=TRUE), 
       quartile3 = get_quartile3(v,na.rm=TRUE),  
       percent90 = get_percentile90(v,na.rm=TRUE), 
       maximum = get_maximum(v,na.rm=TRUE),
       range = get_range(v,na.rm=TRUE),
       stdev = get_stdev(v,na.rm=TRUE),
       missing = count_missing(v)
         )
}

#title: printing the statistics of the input 
#description: This function takes a list of summary statistics and prints the value in a nice format.
#inputs: a list
#outputs: print of the list 
print_stats <- function(stats){
  a <- data.frame(header=FALSE)
  for (i in 1:length(stats)){
    a[i,1] <- paste("## ", names(stats)[i], sep='')
    a[i,2] <- paste(": ",sprintf('%.4f',stats[[i]]), sep='')     
  }
  names(a) <- NULL
  print.data.frame(a,right=F)
}
  
#title: rescaling function
#description: This function takes a vector and rescale it with a scale from 0 to 100.
#inputs: a vector, xmin and xmax
#outputs: the rescaled vector 
rescale100  <- function(b,xmin,xmax){
  100*(b-xmin)/(xmax-xmin)
}

#title: dropping the lowest value
#description: This function takes a numeric vector and returns a vector by dropping the lowest value.
#inputs: a vector of length n
#outputs: a vector of length n-1
drop_lowest <- function(b){
  idx <- which.min(b)
  b[-idx]
}

#title: average score of homework 
#description: This function takes a numeric vector of homework scores and an optional logical 
#argument drop, to compute a single homework value. If drop is true, the function returns the average 
#value with the lowest homework score dropped. 
#inputs: a numeric vector
#outputs: average homework score 
score_homework <- function(hws,drop){
 if(drop==TRUE){
   hws <- drop_lowest(hws)
 }  
  avg <- 0
  for(i in 1:length(hws)){
    avg <- avg + hws[i]/length(hws)
  }
  avg
}

#title: average score of quizzes 
#description: This function takes a numeric vector of quiz scores and an optional logical 
#argument drop, to compute a single quiz value. If drop is true, the function returns the average 
#value with the lowest quiz score dropped. 
#inputs: a numeric vector
#outputs: average quiz score 
score_quiz <- function(quizzes,drop){
  if(drop==TRUE){
    quizzes <- drop_lowest(quizzes)
  }  
  avg <- 0
  for(i in 1:length(quizzes)){
    avg <- avg + quizzes[i]/length(quizzes)
  }
  avg
}

#title:lab score
#description: This function takes a numeric vector of lab attendance and returns the lab score. 
#inputs: number of lab attended
#outputs: the lab score 
score_lab <- function(a){
  if (a==11|a==12){
    100}
  else if (a==10){
    80}
  else if (a==9){
    60}
  else if (a==8){
    40}
  else if (a==7){
    20}
  else if (a <= 6){
    0}
}

#Comments and Reflections
#Yes,it is.
#testthat:1
#Yes,it is.
#ggvis:2
#Yes,it is.
#conditional panels:2
#ggplot. It's easier to use.
#Piazza.
#It took me about 7 hours.
#The shiny app part. 

