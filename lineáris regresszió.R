sum_arr = function(arr) {
  summed = 0
  
  for (i in 1:length(arr)) {
    summed = summed + arr[i]
  }
  
  return (summed)
}

avr = function(arr) {
  return (sum_arr(arr) / length(arr) )
}

#--------------------

x = c(1, 2, 3, 4, 5)
y = c(0.9, 1.1, 2.8, 4.1, 5.3)

x_avr = avr(x)
y_avr = avr(y)

b_up = 0
b_low = 0
for (i in 1:length(x)) {
  b_up = b_up + ( ( x[i] - x_avr ) * ( y[i] - y_avr ) )
  b_low = b_low + ( x[i] - x_avr )^2
}

b = b_up / b_low

a = y_avr - b * x_avr

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
