library(ggplot2)
library(tidyverse)
library(deSolve)

state <- c(x=10,y=10)
parameters<-c(A=0.1,B=0.02,D=0.02,G=0.4) #A is growth rate of prey, B is effect of predator on prey growth rate, D is effect of prey on predator growth rate, G is predator death rate
t <- seq(0,100,by=0.01)
LG <- function(t,state,parameters){ 
  ##logistic grown function,that takes a set of parameter values, initial conditions and atime sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looksfor r, K and P in state and parameters
    dx <- x*A-B*x*y
    dy <- D*x*y-G*y ##this is our logistic equation governing the rate of change of P
    return(list(c(dx,dy)))
    ## return the rate of change - it needsto be a list
    }) # end with
}
out  <- ode(y=state, times=t,func=LG, parms= parameters)
#Transform the deSolve data into a data frame
out_dataframe  <- data.frame(out)
ggplot(data = out_dataframe)+
  geom_line(mapping=aes(x=time,y=x),color="blue")+ 
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")

ggplot(data = out_dataframe)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")

#Euqation but with logistic growth
state <- c(x=10,y=10)
parameters<-c(A=0.1,B=0.02,D=0.02,G=0.4,K=30) #A is growth rate of prey, B is effect of predator on prey growth rate, D is effect of prey on predator growth rate, G is predator death rate
t <- seq(0,500,by=0.01)

LG2 <- function(t,state,parameters){ 
  ##logistic grown function,that takes a set of parameter values, initial conditions and atime sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looksfor r, K and P in state and parameters
    dx <- x*A*(1-(x/K))-B*x*y
    dy <- D*x*y-G*y ##this is our logistic equation governing the rate of change of P
    return(list(c(dx,dy)))
    ## return the rate of change - it needsto be a list
  }) # end with
}
outLG  <- ode(y=state, times=t,func=LG2, parms= parameters)
#Transform the deSolve data into a data frame
outLG_dataframe  <- data.frame(outLG)
ggplot(data = outLG_dataframe)+
  geom_line(mapping=aes(x=time,y=x),color="blue")+ 
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")

ggplot(data = outLG_dataframe)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")
#Initially there is an increase of prey, followed by predators, but a decrease in prey number to the carrying capacity decreases predator numbers until they reach a stable equilibrium
#Functional response, prey consumption from predators is not always linear at some point there is so many preys that the predator cant hunt them all
x <- seq(0,50,0.1)
A <- 0.15 ###Slope of the functional response
y <- x/(1+A*x)
ggplot()+
  geom_line(mapping=aes(x=x,y=y),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
labs(x = "Prey population", y = "Prey consumed")
#Add Funtional REsponse to Loke-Volterra with exponential growth
#Add to model of logistic growth
state <- c(x=10,y=10)
parameters<-c(Alpha=0.1,B=0.02,D=0.02,G=0.4,A=0.15) #Alpha is growth rate of prey, B is effect of predator on prey growth rate, D is effect of prey on predator growth rate, G is predator death rate
t <- seq(0,500,by=0.01)

LG3 <- function(t,state,parameters){ 
  ##logistic grown function,that takes a set of parameter values, initial conditions and atime sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looksfor r, K and P in state and parameters
    dx <- x*Alpha-(B*x*y)/(1+A*x)
    dy <- (D*x*y)/(1+A*x)-G*y ##this is our logistic equation governing the rate of change of P
    return(list(c(dx,dy)))
    ## return the rate of change - it needsto be a list
  }) # end with
}
outLG3  <- ode(y=state, times=t,func=LG3, parms= parameters)
#Transform the deSolve data into a data frame
outLG3_dataframe  <- data.frame(outLG3)
ggplot(data = outLG3_dataframe)+
  geom_line(mapping=aes(x=time,y=x),color="blue")+ 
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")
#Predators go extinct with an A of 0.15
ggplot(data = outLG3_dataframe)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")
#Add Functional Response to model of logistic growth
state <- c(x=10,y=10)
parameters<-c(Alpha=0.1,B=0.02,D=0.02,G=0.4,K=30,A=0.001) #Alpha is growth rate of prey, B is effect of predator on prey growth rate, D is effect of prey on predator growth rate, G is predator death rate
t <- seq(0,500,by=0.01) #

LG4 <- function(t,state,parameters){ 
  ##logistic grown function,that takes a set of parameter values, initial conditions and atime sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looksfor r, K and P in state and parameters
    dx <- x*Alpha*(1-(x/K))-(B*x*y)/(1+A*x)
    dy <- (D*x*y)/(1+A*x)-G*y ##this is our logistic equation governing the rate of change of P
    return(list(c(dx,dy)))
    ## return the rate of change - it needsto be a list
  }) # end with
}
outLG4  <- ode(y=state, times=t,func=LG4, parms= parameters)
#Transform the deSolve data into a data frame
outLG4_dataframe  <- data.frame(outLG4)
ggplot(data = outLG4_dataframe)+
  geom_line(mapping=aes(x=time,y=x),color="blue")+ 
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")

#Predators go extinct with an A of 0.15, A needs to be very low 0.01 for the predator not to disappear. 
#For higher A predators are less efficient at hunting, so they would either need more prey, or greater reproductive rate not to go extinct
ggplot(data = outLG4_dataframe)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")

#Three species competition Lotka Volterra model
state <- c(x1=10,x2=10)
parameters<-c(A12=1,A21=0.9,K1=50,K2=50,r=0.3) #Alpha is growth rate of prey, B is effect of predator on prey growth rate, D is effect of prey on predator growth rate, G is predator death rate
t <- seq(0,500,by=0.01)
Competition <- function(t,state,parameters){ 
  ##logistic grown function,that takes a set of parameter values, initial conditions and atime sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looksfor r, K and P in state and parameters
    dx1 <- x1*r*(1-(x1/K1)-((A12*x2)/K1))
    dx2<- x2*r*(1-(x2/K2)-((A21*x1)/K2))
     ##this is our logistic equation governing the rate of change of P
    return(list(c(dx1,dx2)))
    ## return the rate of change - it needsto be a list
  }) # end with
}
Competition2Species  <- ode(y=state, times=t,func=Competition, parms= parameters)
C2S_dataframe  <- data.frame(Competition2Species)
ggplot(data = C2S_dataframe)+
  geom_line(mapping=aes(x=time,y=x1),color="blue")+ 
  geom_line(mapping=aes(x=time,y=x2),color="green") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")
#Add a third species and calculate alpha12/21/... using the integral equation
alpha.func <- function(mu1,sig1,mu2,sig2,start,end){
  ##this isthe function to compute the alpha coefficients from the mean and standard deviations of the Gaussian niches of the species
  #and the start and end values of the environment
  niche1 <-dnorm(seq(start,end,length.out=100),mean=mu1,sd=sig1)##dnorm() generates the values of the Gaussian. 
  niche2 <-dnorm(seq(start,end,length.out=100),mean=mu2,sd=sig2)
  a <- sum(niche1*niche2)/sum(niche1*niche1) ##because we have discrete values, we use a sum to approximate the integral
  return(a)}
##Let's try different parameter values
D <- 5 ##distance between the niche optima
mu1 <- 10 ##niche optima of species 1
mu2 <- mu1+D ##niche optima of species 2
mu3 <- mu1+2*D ##niche optima of species 3
sig1 <- sig2 <- sig3 <- 10 ##all species niches have the same standard deviation
start <- 0
end <- 100
a12 <- alpha.func(mu1,sig1,mu2,sig2,start,end)
a13 <- alpha.func(mu1,sig1,mu3,sig3,start,end)
a21 <- alpha.func(mu2,sig2,mu1,sig1,start,end)
a23 <- alpha.func(mu2,sig2,mu3,sig3,start,end)
a31 <- alpha.func(mu3,sig3,mu1,sig1,start,end)
a32 <- alpha.func(mu3,sig3,mu2,sig2,start,end)
K1 <- 200 ##carrying capacity species 1 and 3
K2 <- 250 ##carrying capacity species 2

#Plot the niches
resource <- seq(start,end,length.out=100)
niche1 <- dnorm(resource,mean=mu1,sd=sig1)*K1
niche2 <- dnorm(resource,mean=mu2,sd=sig2)*K2
niche3 <- dnorm(resource,mean=mu3,sd=sig3)*K1

ggplot()+
  geom_line(mapping=aes(x=resource,y=niche1),color="blue")+
  geom_line(mapping=aes(x=resource,y=niche2),color="red")+
  geom_line(mapping=aes(x=resource,y=niche3),color="darkgreen")
#Create the 3 species competition model
parameters <- c(a12=a12, a13=a13, a21=a21, a23=a23, a31=a31,a32=a32, r=0.3, K1 = K1, K2 = K2)
state <- c(x1=10, x2=10, x3=10)
Competition2 <- function(t,state,parameters){ 
  ##logistic grown function,that takes a set of parameter values, initial conditions and atime sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looksfor r, K and P in state and parameters
    dx1 <- x1*r*(1-(x1+a12*x2+a13*x3)/K1)
    dx2<- x2*r*(1-(x2+a21*x1+a23*x3)/K2)
    dx3<- x3*r*(1-(x3+a31*x1+a32*x2)/K1)
    ##this is our logistic equation governing the rate of change of P
    return(list(c(dx1,dx2,dx3)))
    ## return the rate of change - it needsto be a list
  }) # end with
}
Competition3Species  <- ode(y=state, times=t,func=Competition2, parms= parameters)
C3S_dataframe  <- data.frame(Competition3Species)
ggplot(data = C3S_dataframe)+
  geom_line(mapping=aes(x=time,y=x1),color="blue")+ 
  geom_line(mapping=aes(x=time,y=x2),color="red") +
  geom_line(mapping=aes(x=time,y=x3),color="green") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")
#Since the niches have a large overlap, the red prevails while the other two go extinct, since red has a higher carryign capacity and where it overlaps with the other two it does a better job
#Change parameters to see different outcomes depedning on how the niches overlap, results based on lectures
#If the carrying capacity of red is 190 (rather than 250) while the rest of the niche is maintained the blue/green increase in population to carrying capcity while the red disappears, this is the support for the biotic invasion resistance hypothesis
#If the carrying capacity is maintained at 250 and 200 but the distance between niches (D) is increased from 5 to 15 then the niches from the 3 species are different enough for eahc to survive and reach their carrying capacity
