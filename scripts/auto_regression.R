#learn about auto-regression in R, create some auto-correlated random variables

#Create a normally distributed variable and plot it
set.seed(42)
norm_y <- rnorm(50)
plot(norm_y, type = "l")

#One way to create an auto-correlated random variable is the following
autocor_y <- diffinv(norm_y)
plot(autocor_y, type = 'l')

#Why does this work?
#The R-documentation tells us that diffinv() is the reverse function of diff()
#what does the diff() function do?
#diff(x) (x being a vector of length n) computes the vector y of length n-1 with y_i = x_i+1 - x_i. In short the function computes successive differences
diff(c(1:10))
diff(c(10:1))

.
#If applied to a normally distributed random variable plotted, diff() results in a zig-zag shaped line. Simply said, this is because the mean of the normal distribution
#(in this case 0) is always the most likely outcome of the random variable. Thus, a positive value is most likely to have a positive difference to the preceding random value.
#The difference to the subsequent value will however be most likely negative. This is a case of auto-correlation with lag 1 and a negative correlation coefficient.
neg_autocor_y <- diff(norm_y)
plot(neg_autocor_y, type = 'l')


#what does diffinv() do?
#The idea behind diffinv() is to take a vector of successive differences and recreate the vector from which the successive differences were calculated
diffinv(diff(c(1:10)))
#This does not return the starting vector 1:10, because diffinv() only knows the successive differences but not the starting value of the original vector
#Therefore
all.equal(diffinv(diff(c(1:10))), diffinv(diff(c(16:25))))
#starting value can be passed to the function using the xi parameter 
diffinv(diff(16:25), xi = 16)


#Here diffinv treats the normally distributed random values as successive differences of a certain starting vector. The curve starts at y_1 = 0, as no starting value was
#specified. Afterwards: y_i = y_i-1 - x_i-1. Thus, each y value functions as a base to which the next random value is added. In terms of auto-correlation,
#the values are positively auto-correlated with lag = 1.

##investigate the variable using autoregression analysis
set.seed(42)
norm_y <- rnorm(1000)
autocor_y <- diffinv(norm_y)
neg_autocor_y <- diff(norm_y)

ar(autocor_y)
ar(neg_autocor_y)

acf(autocor_y)
pacf(autocor_y)

acf(neg_autocor_y)
pacf(neg_autocor_y)
