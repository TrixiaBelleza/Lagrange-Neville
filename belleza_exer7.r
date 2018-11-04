Lagrange <- function(x,y) { 
  #sort the data with respect to x coordinate
  o = order(x)
  x = x[o]
  y = y[o]    #dapat yung y ay ka-match pa rin yung x niya
  
  datapoints = length(x)
  polynomial = ""
  for(i in 1:(datapoints)) {
      for(j in 1:(datapoints)) {
          if(i != j) {
              polynomial = paste(polynomial, "(( x - ", x[j], " ) / ( ", x[i], " - ", x[j], " ))", sep="")
              if(j != (datapoints)) {
                  polynomial = paste(polynomial, " * ", sep = "")
              }
          }
          if(j == (datapoints)) {
              if(y[i] == 0) {
                  polynomial = paste(polynomial, " * 0 ", sep="")
              }
              else{ 
                  if(i == (datapoints)) {
                    polynomial = paste(polynomial, y[i], sep = "")
                  }
                  else {
                    polynomial = paste(polynomial, " * ", y[i], sep = "")
                  }
              }
          }
     
      }
      if(i != (datapoints)) {
          polynomial = paste(polynomial, " + ", sep="")
      }
  }
  f = "f <- function(x) "
  
  f = paste(f, polynomial, sep ="")
  print(paste("function is: ",f, sep =""))
  s = eval(parse(text = f))

  return(list(f = s))  
}

getColNames <- function(colLength) {
  colnames <- c("i", "x", "|x-xi|")
  
  for (i in 4:colLength) {
    colname = paste("P[i,",(i-4), "]", sep="")
    colnames <- c(colnames, colname)  
  }
  return(colnames)
}
getRowNames <- function(rowLength) {
  rownames <- c()
  for (i in 1:rowLength) {
    rownames <- c(rownames, i)      
  }
  return(rownames)
}


Neville <- function(x, y, x_val){
 
  neville_matrix = matrix(data=NA, nrow=length(x), ncol=length(x) + 3,  dimnames=list(getRowNames(length(x)), getColNames(length(x) + 3)))
  
  #get |x-xi|
  x_distance <- c()
  for(i in 1:length(x)) {
    x_distance <- c(x_distance,abs(x_val-x[i]))
  }
  
  #sort the data with respect to | x-xi |
  o = order(x_distance)
  x_distance = x_distance[o]
  x = x[o]
  y = y[o]
  
  #insert i values to first column of matrix
  for(i in 1:nrow(neville_matrix)) {
    neville_matrix[i,1] = i
  }
  #insert x values to first column of matrix
  neville_matrix[,2] = x
  #insert y values to second column of matrix
  neville_matrix[,4] = y
  #insert |x-xi| to third column of matrix
  neville_matrix[,3] = x_distance
  
  count = nrow(neville_matrix)
  
  for(k in 5:ncol(neville_matrix)) {
    for(i in 1:(count-1)) {
      neville_matrix[i,k] = ( ((x_val-neville_matrix[i,2]) * neville_matrix[i+1,k-1]) + ( (neville_matrix[i+(k-4), 2] - x_val) * neville_matrix[i,k-1] ) ) / (neville_matrix[i+(k-4), 2] - neville_matrix[i,2])
    }
    count = count-1
  }

  result = neville_matrix[1,ncol(neville_matrix)] #result can be seen at last column of first row
  return(list(mat = neville_matrix, value = result)) 
}

#x <- c(1,3,4,5)
#y <- c(0,1.0986,1.3863,1.6094)
#x <- c(1,3.5,4.25,5)
#y <- c(0,1.2528,1.4469,1.6094)
x <- c(1995, 2000, 2004, 2005, 2010, 2015)
y <- c(68349452, 75505061, 80824322, 82079348, 87940171, 93440274)
x_val = 2004

print(" --------------------------- Lagrange ------------------------- ")
lagrange = Lagrange(x,y)
print(lagrange$f(2004))

print(" --------------------------- Neville ------------------------- ")
neville_result = Neville(x,y,x_val)
print(neville_result$mat)
print(neville_result$value)

plot(1990:2020, lagrange$f(1990:2020), pch=20, col = "red", main = "Year VS Population Count", xlab = "Year", ylab="Population Count")

lagrangeModel = lm(y~poly(x,5,raw=TRUE))
lines(x, predict(lagrangeModel), col="blue")
