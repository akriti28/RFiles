
my_array <- array(1:10, dim=c(5,5,5)) 
for (i in 1:dim(my_array)[1]) { 
  for (j in 1:dim(my_array)[2]) { 
    for (k in 1:dim(my_array)[3]) { 
      my_array[i,j,k] = i*j*k 
    } 
  } 
} 
# Show a 10x10x15 chunk of your array 
my_array[1:5, 1:5, 1:5]

g<- function() { n<-readline(prompt="type")
return(as.integer(n))}
print(g())

readinteger <- function()
{ 
  n <- readline(prompt="Enter an integer: ")
  return(as.integer(n))
}

print(readinteger())