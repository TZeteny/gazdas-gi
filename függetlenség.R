A = matrix( c(42, 28, 3, 17, 89, 21), nrow = 2, byrow = TRUE)
print(A)

colsums = c()
rowsums = c()

for ( col in 1:ncol(A) ) {
	colsum = 0
	for ( row in 1:nrow(A) ) {
		colsum = colsum + A[row, col]
	}
	colsums[col] = colsum
}

for (row in 1:nrow(A)) {
	rowsum = 0
	for (col in 1:ncol(A)) {
		rowsum = rowsum + A[row, col]
	}
	rowsums[row] = rowsum
}

print(colsums)
print(rowsums)

N = sum(colsums)
print(N)

khisqr = 0

for (col in 1:ncol(A)) {
  for (row in 1:nrow(A)) {
    temp = (A[row, col] - ( (colsums[col] * rowsums[row]) / N) )^2
    temp = temp / ( (colsums[col] * rowsums[row]) / N )
    khisqr = khisqr + temp
  }
}
print(khisqr)













