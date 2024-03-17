?pbinom

x <- 1:100
print(x)
par(mfrow=c(1,2))


# probability density
y = dbinom(x,size=100,prob=.5)
plot(x,y)

# cumulative probability
y = pbinom(x,size=100,prob=.5)
plot(x,y)


