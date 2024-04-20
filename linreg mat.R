#linreg mátrix

sum_arr = function(arr) {
  summed = 0
  
  for (i in 1:length(arr)) {
    summed = summed + arr[i]
  }
  
  return (summed)
}

avr = function(arr) {
  return (sum_arr(arr) / length(arr))
}

sum_2arr = function(arr1, arr2) {
  summed = 0
  
  for (i in 1:length(arr1)) {
    summed = summed + (arr1[i] * arr2[i])
  }
  
  return (summed)
}

calc_det = function(mat) {
  return ( mat[1,1] * mat[2,2] - mat[1,2] * mat[2, 1] )
}

# prog

x = c(1, 2, 3, 4, 5)
y = c(0.9, 1.1, 2.8, 4.1, 5.3)

n = length(x)
xsum = sum_arr(x)
ysum = sum_arr(y)
xsqrSum = sum_2arr(x, x)
xySum = sum_2arr(x, y)

aup = matrix( c(xsqrSum, xySum, xsum, ysum), nrow = 2, byrow = TRUE)
bup = matrix( c(xySum, xsum, ysum, n), nrow = 2, byrow = TRUE)

osztó = matrix( c(xsqrSum, xsum, xsum, n), nrow = 2, byrow = TRUE)


#a = det(aup) / det(osztó)
#b = det(bup) / det(osztó)
a = calc_det(aup) / calc_det(osztó)
b = calc_det(bup) / calc_det(osztó)

print(a)
print(b)

linreg = c()
for (i in 1:length(x)) {
  linreg[i] = a + b * x[i]
}

print(linreg)

plot(
  x = x,
  y = y,
  type = "b", 
  xlab = 'x',
  ylab = 'y',
  main = 'lin regresszió',
  pch = 16,
  col = "black"
)

lines(
  x = x,
  y = linreg,
  col = 'red'
)

print(a)
print(b)















