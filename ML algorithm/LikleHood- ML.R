library(MASS)

size<-70

x1<-1+runif(size)
y1<-5+runif(size)


x2<-2+runif(size)
y2<-6+runif(size)

xcor<-c(x1,x2)
ycor<-c(y1,y2)

df <- data.frame(xcor,ycor)
str(df)
plot(df)
plot(x1,y1)

lh <- function(input){
  output <- c()
  sd <- sd(input)
  mu <- mean(input)
  for(a in input){
    
    density <- ((1 / (sqrt(2*pi)*sd))*exp((-1*(a-mu)^2) / (2*(sd^2))))
    output <- c(output,density)
  }  
  return(output)
}

discriminant<- function(input){
  gx_output<- c()
  sd <- sd(input)
  mu <- mean(input)
  for(a in input){
    gx <- -.5* log(2*pi) - log(sd) - (((a-mu)^2)/2*(sd^2)) + log(0.5)
    gx_output<- c(gx_output,gx)
  }
  return(gx_output)
}

gx1 <- discriminant(xcor)
gx2 <-discriminant(ycor)
px1 <- lh(xcor)
px2 <- lh(ycor)

pC1x=px1*.5/(px1*.5+px2*.5)
pC2x=px2*.5/(px1*.5+px2*.5)

plot(xcor,px1,  type='p', col="green",  xlab ='xcor',ylab ='px1 and px2')
lines(xcor, px2, type='p',col ="red", xlab ='x', ylab ='y')

plot(ycor, px2, type='p', col="green", xlab ='xcor',ylab ='px1 and px2')
lines(ycor, px1, type ='p', col ="red", xlab ='x', ylab ='y')

plot(xcor, pC1x,  type='p',col ="green", xlab ='x',ylab ='pC1x and PC2x')
lines(xcor,pC2x,  type='p',col ="red", xlab='x', ylab ='y')

plot(ycor, pC1x, type ='p', xlim = range(ycor),col ="green", ylim = range(c(pC1x,pC1x)), xlab ='x',ylab ='pC1x and PC2x')
lines(ycor, pC2x, type ='p', col ="red", xlab='x', ylab ='y')

