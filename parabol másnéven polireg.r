# Parabolikus regresszió
# ua. mint a lineáris, csak egyenes helyett parabola
# a*x^2+b*x+c

# y = x^2 + x + length(x), vagyis n
# xy = x *(x^2 + x + n)
# = x^3 + x^2 + x
# x^2 *y = x^4 + x^3 + x^2

# ennek kéne kijönni
# a = 2.5
# b = 2
# c = 3

# függvények

det2 = function(mat) {
  if ((nrow(mat) != 2) || (ncol(mat) != 2)) {
    print('2x2 kell!')
    return ()
  }
  return ( mat[1,1] * mat[2,2] - mat[1,2] * mat[2, 1] )
}

det3 = function(mat) {
  if ((nrow(mat) != 3) || (ncol(mat) != 3)) {
    print('3x3 kell!')
    return ()
  }
  
  d = 0
  
  for (col in 1:ncol(mat)) {
    if (col == 1) {
      temp = matrix(c(mat[2,2], mat[3, 2], mat[2,3], mat[3,3]), nrow = 2, byrow = TRUE)
      d = d + mat[col, 1] * det2(temp)
    } else if (col == 2) {
      temp = matrix(c(mat[1,2], mat[3, 2], mat[1,3], mat[3,3]), nrow = 2, byrow = TRUE)
      d = d - mat[col, 1] * det2(temp)
    } else {
      temp = matrix(c(mat[1,2], mat[2, 2], mat[1,3], mat[2,3]), nrow = 2, byrow = TRUE)
      d = d + mat[col, 1] * det2(temp)
    }
  }
  
  return (d)
}

# prog

x = 1:10
y = c(7.5, 17.0, 31.5, 51.0, 75.5, 105.0, 139.5, 179.0, 223.5, 273.0)

a_matrix = matrix(c(sum(x^2*y), sum(x*y), sum(y), sum(x^3), sum(x^2), sum(x), sum(x^2), sum(x), length(x)),nrow=3, ncol=3)
b_matrix = matrix(c(sum(x^4), sum(x^3), sum(x^2), sum(x^2*y), sum(x*y), sum(y), sum(x^2), sum(x), length(x)),nrow=3, ncol=3)
c_matrix = matrix(c(sum(x^4), sum(x^3), sum(x^2), sum(x^3), sum(x^2), sum(x), sum(x^2*y), sum(x*y), sum(y)),nrow=3, ncol=3)
oszto_matrix = matrix(c(sum(x^4), sum(x^3), sum(x^2), sum(x^3), sum(x^2), sum(x), sum(x^2), sum(x), length(x)),nrow=3, ncol=3)

a = det(a_matrix) / det(oszto_matrix)
b = det(b_matrix) / det(oszto_matrix)
c = det(c_matrix) / det(oszto_matrix)

parreg = a * x^2 + b * x + c
print(parreg)

plot(x, y, type="b")
lines(x,y_solution,type="b",lty=2,col="red")

print(a)
print(b)
print(c)

am = matrix( c('sum(x^2*y)', 'sum(x*y)', 'sum(y)', 'sum(x^3)', 'sum(x^2)', 'sum(x)', 'sum(x^2)', 'sum(x)', 'length(x)'), nrow = 3 )

bm = matrix(c('sum(x^4)', 'sum(x^3)', 'sum(x^2)', 'sum(x^2*y)', 'sum(x*y)', 'sum(y)', 'sum(x^2)', 'sum(x)', 'length(x)'),nrow=3, ncol=3)

cm = matrix(c('sum(x^4)', 'sum(x^3)', 'sum(x^2)', 'sum(x^3)', 'sum(x^2)', 'sum(x)', 'sum(x^2*y)', 'sum(x*y)', 'sum(y)'),nrow=3, ncol=3)

oszto = matrix(c('sum(x^4)', 'sum(x^3)', 'sum(x^2)', 'sum(x^3)', 'sum(x^2)', 'sum(x)', 'sum(x^2)', 'sum(x)', 'length(x)'),nrow=3, ncol=3)

print(am)
print(bm)
print(cm)
print(oszto)
















