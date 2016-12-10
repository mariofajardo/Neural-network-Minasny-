setwd("C:/Users/ifue3702/Documents/PTF budiman")
# read parameter files
Wr1<-read.table(file= 'Wr1_au3.txt', header=FALSE)
Wr2<-read.table(file= 'Wr2_au3.txt', header=FALSE)

ni=3 #no. inputs  sand,clay,bd
no=4 #no. outputs
nh=5 # no. hidden units
nbag = 100 # no. bootstrap

neuro <- function(W1,W2,X) {
  # evaluate neural networks
  #   feed forward nnet, with single hidden layer
  X=as.matrix(X)
  N=nrow(X)
  ni=ncol(X)
  M1=matrix(data=1, nrow=1,ncol=N)
  # from input layer to hidden layer
  h = W1 %*%  rbind(t(X),M1)
  y1 = tanh(h)       
  # from hidden layer to output layer
  h2 = W2 %*% rbind(y1,M1) 
  Y = t(h2)
  return(Y)
}

# store parameters as array
aW1<-array(data = NA, dim=c(nbag,nh,ni+1)) 
aW2<-array(data = NA, dim=c(nbag,no,nh+1)) 
for (ibag in 1:nbag) {    # loop though  n bootstrap
  aW1[ibag,,]=matrix(unlist(Wr1[ibag,]),nh,ni+1)
  aW2[ibag,,]=matrix(unlist(Wr2[ibag,]),no,nh+1)
}


# evaluate 
# input: sand,clay,bd
X=cbind(50.9,11,1.39)

pred=matrix(data=NA,nrow=nbag,ncol=no)
for (ibag in 1:nbag) {    # loop though  n bootstrap
  W1=aW1[ibag,,]
  W2=aW2[ibag,,]
  bp = neuro(W1,W2,X) # evaluate the nnet
  bp[,1] = bp[,1]^2  # back transform theta_r
  bp[,3] = exp(bp[,3])  # back transform alfa
  bp[,4] = exp(bp[,4])+1  # back transform n
  pred[ibag,]=bp     # predicted parameters
}
pred # output is a distribution of the van Genuchten parameters (theta_r, theta_s, alfa, n)


