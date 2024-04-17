# dobókocka szabályos-e?
# mennyire illeszkedik az elvártra

sum_arr = function(x) {
  s = 0
  for (i in 1: length(x)) {
    s = s + x[i]
  }
  
  return (s)
}

# prog

k = c(83, 91, 122, 107, 74, 123)
p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
N = sum_arr(k)

elteres = 0

for (i in 1:length(k)) {
	Npi = N * p[i]
	elteres = elteres + (k[i] - Npi)^2 / Npi
}

print(elteres)

if (elteres > 9.236) {
	print("Nem szabályos!")
} else {
	print("Szabályos")
}