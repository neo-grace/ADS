?distribution()

#1.Uniform 
#Could be used in a game to generate 100 random numbers between 1 to 10
n <- floor(runif(100,min=1,max=10))
t <- table(n)
barplot(t)

#2.Binomial
#To find the probability of biased coin given 6 tosses with a probability 
#of 0.3 to be heads

b <- dbinom(x=3, size=6, prob=0.3) #Probability of 3 heads
plot(b)

binom <- dbinom(x=0:6, size=7, prob=0.3)
plot(binom)

#3.Normal
#Weight with a mean of 166 pounds and standard deviation 20 pounds
xaxis <- seq(120,190,10)
normal <- rnorm(xaxis, 155, 20)
probdf <- dnorm(normal, 155, 20)
cumdf <- pnorm(normal, 155, 20)
plot(xaxis,normal)


#4.Bernoulli
#Probability of flipping a coin 8 times 
install.packages('Rlab')
library(Rlab)
rb <- rbern(8, 0.5)
prb <- pbern(rb, 0.5, lower.tail=TRUE, log.p=FALSE)
qrb <- qbern(rb, 0.5, lower.tail=TRUE, log.p=FALSE)
plot(rb)

#5.HyperGeometric
#Probability of drawing 20 white balls from 45 White,50 Red, 45 Black
rh <- rhyper(45, 45, 50, 20)
prh <- phyper(20, 45, 50, 45)
plot(rh)

#6.Geometric
#100 continuous flips of a coin where probability is 0.5 success of
#getting a head before a tail
rg <- rgeom(100, 0.5)
dg <- dgeom(rg, 0.4, log = FALSE)
pg <- pgeom(rg, 0.4, lower.tail = TRUE, log.p = FALSE)
qg <- qgeom(rg, 0.4, lower.tail = TRUE, log.p = FALSE)
hist(rg)

#7.Negative Binomial
#Getting a tail at the 10th toss before 3 heads with a probability of 0.3 as success
rnb <- rnbinom(10,3,0.3)
plot(rnb)

#8.Poisson
#5 flights take off in an hour. Probability of 10 or more flights to be taken off
ppois(10,5,lower.tail = FALSE, log.p = FALSE)
rps <- rpois(10,5)
plot(rps)

#9.Exponential
#Average Time taken by a marketing person for a call is 5 mins
#Probability of the person to complete the call in less than 3mins
rex <- rexp(3,rate = 0.2)
prex <- pexp(3,rate = 0.2)
plot(rex)
lines(rex)

#10.LogNormal
rln <- rlnorm(100, meanlog = 1.5, sdlog = 0.3)
plot(rln)

#11.Weibull
#In radar systems to model the dispersion of the received signals level 
#produced by some types of clutters
ran <- rweibull(100, shape = 0.5, scale = 1)
hist(ran)

#12.Chi-squared
#Application in Magnetic Resonance Imaging
#80th percentile with 5 as degree of freedom
rch <- rchisq(80, 5, ncp = 0)
plot(rch)

#13.Student-t
#Used In Bayesian Statistics
rst <- rt(10,5)
plot(rst)

#14.Beta
rbet <- rbeta(100,9,9,ncp=0)
dgma <- dbeta(rbet,9,9)
hist(rbet)
curve(dbeta(rbet, 9, 9, log = FALSE))

#15.Gamma
#In wireless communication, the gamma distribution is used to model the multi-path fading of signal power
rgma <- rgamma(100, 9, scale = 1)
dgma <- dgamma(rgma)
hist(rgma)
curve(dgamma(rgma, shape = 9, rate = 1, log = FALSE))



















