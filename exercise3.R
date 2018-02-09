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

#4) Write a function which takes a single argument which is a matrix.
#The function should return a matrix
#which is the same as the function argument but every odd number is doubled.  
#Hence the result of using the function on the matrix   
#$$\begin{bmatrix}
#1 & 1 & 3\\ 
#5 & 2 & 6\\ 
#-2 & -1 & -3
#\end{bmatrix}$$
#should be:
#$$\begin{bmatrix}
#2 & 2 & 6\\ 
#10 & 2 & 6\\ 
#-2 & -2 & -6
#\end{bmatrix}$$
func2 <- function(mat){
  mat[mat%%2==1] <- 2*mat[mat%%2==1]
  mat
}

#5) Write a function which takes 2 arguements $n$ and $k$
#which are positive integers.
#It should return the $\mathit{n} x \mathit{n}$ matrix:
#$$\begin{bmatrix}
#k & 1 & 0 & 0 & \cdots  & 0 & 0\\ 
#1 & k & 1 & 0 & \cdots  & 0 & 0\\
#0 & 1 & k & 1 & \cdots  & 0 & 0\\
#0 & 0 & 1 & k & \cdots  & 0 & 0\\
#\cdot & \cdot & \cdot & \cdot & \cdot & \cdot & \cdot\\ 
#0 & 0 & 0 & 0 & \cdots  & k & 1\\
#0 & 0 & 0 & 0 & \cdots  & 1 & k
#\end{bmatrix}$$
#First try to do it for a specific case such as n = 5 and k = 2 on the Command Line.
mat <- diag(2, nr=5)
mat[abs(row(mat)-col(mat))==1] <- 1
mat
#Now with k and n
func3 <- function(n,k){
  mat1 <- diag(k,nr=n)
  mat1[abs(row(mat1)-col(mat1))==1] <- 1
  mat1
}

#6) Suppose an angle $\alpha$ is given as a positive real number of degrees.  
#If $0 \leq \alpha < 90$ then it is quadrant 1.  If $90 \leq \alpha < 180$ then it is quadrant 2.  
#If $180 \leq \alpha < 270$ then it is quadrant3.  if $270 \leq \alpha < 360$ then it is quadrant 4.  
#If $360 \leq \alpha < 450$ then it is quadrant 1.  
#And so on ...
#Write a function \texttt{quadrant(alpha)} which returns the quadrant of the angle $\alpha$.

func4 <- function(alpha)
{
  floor(alpha/90)%%4+1
}

#7) Zeller's congruence is the formula:
#$$ f = ([2.6m - 0.2] + k + y + [y/4] + [c/4] - 2c)mod 7 $$
#where $[x]$ denotes the integer part of $x$; for example $[7.5] = 7$.
#Zeller's congruence returns the day of the week $f$ given:
#$k$ = the day of the month  
#$y$ = the year in the century  
#$c$ = the first 2 digits of the year (the century number)  
#$m$ = the month number (where January is month 11 of the preceding year, February is month 12 of the preceding year, March is month 1, etc.)   
#For example, the date 21/07/1`963 has $m = 5, k = 21, c = 19, y = 63$;  
#the date 21/2/63 has $m=12, k=21, c=19, and y=62$.  

#(a)Write a function \texttt{weekday(day,month,year)} which returns the day of the week when given the numerical inputs of the day, month and year.  
#Note that the value of 1 for $f$ denotes Sunday, 2 denotes Monday, etc.
weekday <- function(day,month,year){
  month <- month-2
  if(month<=0){
    month <- month+12
    year <- year-1
  }
  cc <- year%%100
  year <- year%%100
  a <- floor(2.6*month-0.2)+day+year+year%/%4+cc%/%4-2*cc
  c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[1+a%%7]
}
#(b) Does your function work if the input parameters \text{day, month, and year} are vectors with the same length and valid entries?
Yes, it still works.

#8) Suppose x0 = 1 and x1 = 2 and xj = xj−1 + 2 xj−1 for j = 1, 2, . . . .

#(a) Write a function testLoop which takes the single argument n and returns the first n − 1 values of the
#sequence {xj}j≥0: that means the values of x0, x1, x2, . . . , xn−2.
testLoop <- function(x){
  xVec <- rep(NA,x-1)
  xVec[1] <- 1
  xVec[2] <- 2
  for(j in 3:(x-1))
  xVec[j] <- xVec[j-1]+2/xVec[j-1]
  xVec
}

#(b) Now write a function testLoop2 which takes a single argument yVec which is a vector.
#The function should return ∑n j=1 e^j where n is the length of yVec.
testLoop2 <- function(yVec){
  n <-length(yVec)
  sum(exp(seq(along=yVec)))
}

#9) Solution of the difference equation xn = rxn−1(1 − xn−1), with starting value x1.

#(a) Write a function quadmap( start, rho, niter ) which returns the vector (x1, . . . , xn) where xk =
#rxk−1(1 − xk−1) and
#niter denotes n,
#start denotes x1, and
#rho denotes r.
quadmap <- function(start,rho,niter){
  xVec <- rep(NA,niter)
  xVec[1] <- start
  for(i in 1:(niter-1)){
    xVec[i +1] <- rho * xVec[i] * (1-xVec[i])
  }
  xVec
}
tmp3 <- quadmap(start=0.95,rho=2.99,niter=500)
tmp3
plot(tmp3, type="l")
plot(tmp3[300:500],type="l")

#(b) Now write a function which determines the number of iterations needed to get | xn − xn−1 |< 0.02.
#So this function has only 2 arguments: start and rho.
#(For start=0.95 and rho=2.99, the answer is 84.)
quadmap2 <- function(start,rho,max1=.02){
  x1 <- start
  x2 <- rho*x1*(1-x1)
  x3 <- 1
  while(abs(x1-x2)>=max1){
    x1 <- x2
    x2 <- rho*x1*(1-x1)
    x3 <- x3+1
  }
  x3
}
quadmap2(0.95,2.99)

#10) Given a vector (x1, . . . , xn), the sample autocorrelation of lag k
#is defined to be.

#(a) Write a function tmpFn(xVec) which takes a single argument xVec
#which is a vector and returns a list of two values: r1 and r2.
#In particular, find r1 and r2 for the vector (2, 5, 8, . . . , 53, 56).
tmpFn4 <- function(xVec){
  x1 <- xVec-mean(xVec)
  denom <- sum(x1^2)
  n <- length(x1)
  r1 <- sum(x1[2:n]*x1[1:(n-1)])/denom
  r2 <- sum(x1[3:n]*x1[1:(n-2)])/denom
  list(r1=r1,r2=r2)
}

#(b) Generalise the function so that it takes two arguments:
#the vector xVec and an integer k which lies
#between 1 and n − 1 where n is the length of xVec.
#The function should return a vector of the values (r0 = 1, r1, . . . , rk).
tmpAcf <- function(x, k)
{
  x2 <- x - mean(x)
  denom <- sum(x2^2)
  n <- length(x)
  tmp <- function(a){ sum( x2[(a+1):n] * x2[1:(n-a)] )/denom }
  c(1, sapply(1:k, tmp))
}
