#Version 1
cointoss <- function(flips = 10000){
        seq_10 <- seq(0,flips,1000)
        sapply(1:length(seq_10),function(x){table(sample(0:1,size = seq_10[x],replace = TRUE))[1]/seq_10[x]})
}


Nflips = 1000000

prob_head <- cointoss(flips = Nflips)

plot(seq(0,Nflips,1000),prob_head,main = "Probability of head",
     xlab = "Number of Flips", ylab = "Probability",type ="l",lwd = 0.6)
abline(h = 0.5,col = "blue",lwd = 1.5)




##Version 2

#Coin toss function with number of flips
coin_toss <- function(s_size = 1000){
  
  flips <- table(sample(c(0,1),replace = T,size = s_size))
  flip_prob <- flips/sum(flips)
  return(flip_prob[1])     ##prob[1] mentions it is only for "0" assuming flip = heads
  
}


prob_heads <- sapply(1:10000,function(x){coin_toss(x)})

#Plotting actual versus theoretical probability 
plot(1:10000,prob_heads,xlab="Sample size",ylab = "Probability of heads",
     main = "Progression of prob of head with samples",type = "l")