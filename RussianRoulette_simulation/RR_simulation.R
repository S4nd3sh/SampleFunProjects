#Shots fired
shots <- function(){
        
        first_shot <- sample(1:6,1)
        
        second_shot <- (first_shot+1) %% 6
        second_shot[second_shot == 0 ] <- 6
        
        third_shot <- (second_shot+1) %% 6
        third_shot[third_shot == 0 ] <- 6
        return(c(first_shot,second_shot,third_shot))
}

consecutive_shots <- function(attempts = 25){
        
        Bulletposition <- sample(1:6,replace = T,size = attempts)
        Shot <- sum(sapply(Bulletposition,function(x){ x %in% shots()})) > 1
        return(Shot)
}


simulate_Russian_Roulette <- function(number_of_individuals = 1000000,attempts = 25){
        
        alive <- replicate(number_of_individuals,consecutive_shots(attempts))
        return(alive)
}

library(tictoc)

tic()
parallel_output <- simulate_Russian_Roulette(1000000,10)
toc()


Survived <- length(which(!parallel_output))

print(Survived)

































