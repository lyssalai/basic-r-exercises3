#This is Basic R Exercises 3. Functions

#(a) Write functions \texttt{tmpFn1} and \texttt{tmpFn2} such that
#if \texttt{xVec} is the vector $(x_1, x_2, ... ,x_n)$,
#then \texttt{tmpFn1(xVec)} returns vector $(x_1, x_2^2, ..., x_n^n)$
#and \texttt{tmpFn2(xVec)} returns the vector
#$(x_1, \frac{x_2^2}{2}, ..., \frac{x_n^n}{n})$.
tmpFn1 <- function(xVec){
  return(xVec^(1:length(xVec)))
}
tmpFn2 <- function(yVec){
  return(yVec^(1:length(yVec))/1:length(yVec))
}

#(b) Now write a fuction \texttt{tmpFn3}
#which takes 2 arguments $x$
#and $n$ where $x$ is a single number and $n$ is a strictly positive integer.
#The function should return the value of   
#$$1 + \frac{x}{1} + \frac{x^2}{2} + \frac{x^3}{3} + ... + \frac{x^n}{n}$$
tmpFn3 <- function(x, n){
  1+sum((x^(1:n))/(1:n))
}

#2) Write a funciton \texttt{tmpFn(xVec)} such that if \texttt{xVec} is the vector $x = (x_1, ...,x_n)$ then \texttt{tmpFn(xVec)} returns the vector of moving averages:
#$$\frac{x_1 + x_2 + x_3}{3}, \frac{x_2 + x_3 + x_4}{3}, ...,\frac{x_{n-2} + x_{n-1} + x_n}{3}$$
#Try out your function.  \texttt{tmpFn}(c(1:5,6:1))
tmpFn <- function(x){
  n <- length(x)
  (x[1:(n-2)]+x[2:(n-1)]+x[3:n])/3
}

#3) Consider the continuous function
#$$f(x) = \left\{\begin{matrix}
#x^2 + 2x + 3 & if  & x < 0 \\ 
#x + 3 & if & 0 \leq x < 2 \\ 
#x^2 + 4x - 7 & if & 2 \leq  x
#\end{matrix}\right.$$
#Write a function \texttt{tmpFn} which takes a single argument \texttt{xVec}.
#the function should return the vector
#the values of the function $f(x)$ evaluated at the values in \texttt{xVec}.  
#Hence plot the function $f(x)$ for $-3 < x <3$
tmpFn <- function(y){
  ifelse(y<0,y^2+2*y+3, ifelse(y<2,y+3,y^2+4*y-7))
}
tmp <- seq(-3,3,len=100)
plot(tmp,tmpFn(tmp),type="l")

