# r masterclass

# 0. feladat, másolás

#x = "qgnld2"
x = "y86i0i"
z = charToRaw(iconv(x, "latin1", "UTF-8"))
for (i in 1:6) v = paste ("0x", z, sep="")
e = strtoi(v)
ax = e[1]; ay=e[2]; az=e[3]; av=e[4]; ss=sum(strtoi(v))+9

cat("ax=", ax, "\n")
cat("ay=", ay, "\n")
cat("az=", az, "\n")
cat("av=", av, "\n")
cat("ss=", ss, "\n")

ar=c( "FB","AAPL","AMZN","GOOG","NFLX","TSLA")
ai=ss-6*floor(ss/6)
ev=2022-(ss-10*floor(ss/10))
cat("ev=",ev,"\n")
cat("reszveny=",ar[ai+1],"\n")

# 1. feladat, 2d realizáció

set.seed(ss)
nx = 600
v=matrix(c(ax,abs(ax-ay),abs(ax-ay),ay),2)
w=chol(v)
z1=-log(runif(nx))
z2=-log(runif(nx))
zm=matrix(c(z1,z2),ncol=2)
zn=zm%*%w
print(length(zn))

# 2. feladat statisztikai elemzés,
# ferdeség, lapultság, függetlenség

summary(zn) # általános elemzés

library(moments)
skewness(zn) # ferdeség
kurtosis(zn) # lapultság

cor(zn) # függetlenség

#esetleg eloszlás vizsgálat
library(ggpubr)
ggdensity(zn[,1], main="sűrűségdiagram 1")
library(car)
ggplot(zn)

# 3. feladat, többD-s ábrázolás szintvonalakkal

# adatok betöltése
x = zn[,1]
y = zn[,2]

# rács készítése
grid_size = 50
x_range = seq(min(x), max(x), length.out = grid_size)
y_range = seq(min(y), max(y), length.out = grid_size)

# kétváltozós sűrűség becslése
library(MASS) # kde2d függvény csomagja
fhat = kde2d(x, y, n = grid_size)

# plotok 2x2 eljelyezése
par(mfrow = c(2,2))

# perspektivikus ábrázolás
persp(x_range, y_range, fhat$z, theta = 45, phi = 20,
  xlab = "X", ylab = "Y", zlab = "Z", main="persp ábrázolás")

#szintvonalas ábrázolás
contour(x_range, y_range, fhat$z, xlab = "X", ylab = "Y",
  main="Szintvonalak")
plot(zn, main="A zn mátrirx")

# esetleg poisson folyamat

# 4. feladat, geometriai brown folyamat
# (nem keverendő a sima brown folyamattal)

library(LSMRealOptions)
set.seed(ss+27)
n = 1 # egy szimuláció kell
t = 100/365 # 100 napra előrevetíteni
mu = ax # várható érétk
sigma = (ax + ay) / (ax + ay + az) # szórás
s0 = 100 # részvén kezdő érétéke
dt = 1/365 # naponta egy vizsgálat
gbm = GBM_simulate(n, t, mu, sigma, s0, dt)
plot(gbm, type='l')
summary(gbm)
skewness(gbm)
kurtosis(gbm)

# 5. 6. feladatok, részvény letöltés

details = read.csv("C:/Users/Z/Documents/uni/gp/META.csv")
logreturn = c()
zaro = details$Close # záró értékek kinyerése
for (i in 1:length(zaro) - 1) {
  logreturn[i] = abs(log(zaro[i+1]/zaro[i])) # logert összegyűjtése
}
chisq.test(logreturn) # khi^2 teszt

# statisztikák
hist(logreturn, main="záró árak változása")
plot(logreturn)

















