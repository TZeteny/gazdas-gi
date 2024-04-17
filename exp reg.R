# exp reg = a0 + a1 * x
# a * e^bx

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

#--------------------

x = c(1, 2, 3, 4, 5)
y = c(1.3, 1.9, 2, 2.9, 4)
n = length(x)
z = c()

for (i in 1:length(y)) {
  z[i] = log(y[i])
}

bup = length(z) * sum_2arr(z, x) - sum_arr(z) * sum_arr(x)
blow = length(z) * sum_2arr(x, x) - sum_arr(x)^2

b = bup / blow

a = avr(z) - b * avr(x)
a = exp(a)

print(a)
print(b)

expreg = c()
for (i in 1:length(x)) {
  expreg[i] = a * exp(b * x[i])
}

print(expreg)

plot(
  x = x,
  y = y,
  type = "b", 
  xlab = 'x',
  ylab = 'y',
  main = 'exp regresszió',
  pch = 16,
  col = "black"
)

lines(
  x = x,
  y = expreg,
  col = 'red'
)













