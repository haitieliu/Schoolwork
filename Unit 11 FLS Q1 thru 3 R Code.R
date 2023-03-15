#FLS Questions 1

install.packages("fpp")
library(fpp)

# 1. SES MODEL FOR AUS AIR 
data(ausair)

#returns a ts object
air = window(ausair, start = 1990, end = 2004)

# Always plot the data first! 
plot(air,ylab = "Airline Passegners", xlab = "Year", main = "Airline Passengers")

#fit 3 different simple exponential smoothing models ... how are they different?
# what does the h paramter do? 
fit1 = ses(air, initial = "simple",alpha = .2,h = 3)
fit2 = ses(air,initial = "simple",alpha = .6, h = 3)
fit3 = ses(air, h = 3) #defaults

# the forecast package has a nice accuracy funciton with various metrics just pass it the 
# the model and the data!  (This is the "training" data)
accuracy(fit1, ausair)
accuracy(fit2, ausair)
accuracy(fit3, ausair)

#Reset the plot
plot(air,ylab = "Airline Passegners", xlab = "Year", type = "o", xlim = c(1990, 2008),ylim = c(15,50), main = "Airline Passengers")

#Plot the estimated values from the models .. the "fitted" values are the training values.
lines(fitted(fit1), col = "blue", type = "o")
lines(fitted(fit2), col = "red", type = "o")
lines(fitted(fit3), col = "green", type = "o")

# the  $mean values are the forecasts.
lines(fit1$mean, col = "blue", type = "o")
lines(fit2$mean, col = "red", type = "o")
lines(fit3$mean, col = "green", type = "o")

# These are the actual values!  Compare visually with the forecasts!
air2008 = window(ausair, start = 1990, end = 2007)
points(air2008, type = "o")

# Compare the forecasts with the actual values with various fit metrics.  (*Note: Redundant.)
accuracy(fit1, air2008)
accuracy(fit2, air2008)
accuracy(fit3, air2008)


#Question 2:

#2 Holt's Linear Trend Model for AUS AIR
fit1h = holt(air, alpha = .8, beta = .2, initial = "simple", h = 5)
fit2h = holt(air, alpha = .8, beta = .2, initial = "simple", exponential = TRUE, h = 5)

# Check out estiamted values of the "training" data from the first holt model 
fitted(fit1h)
# Check out the forecast value (h of them)
fit1h$mean

# Reset the Plot!
plot(air,ylab = "Airline Passegners", xlab = "Year", type = "o", xlim = c(1990, 2009),ylim = c(15,60))
#Plot each models estimated values of the training data (Do these one by one to see the differences)
lines(fitted(fit1h),col = "blue", type= "o")
lines(fitted(fit2h), col = "red", type= "o")
#Plot each models forecasts (Do these one by one to see the differences)
lines(fit1h$mean, col = "blue", type= "o")
lines(fit2h$mean,col = "red", type= "o")

# Fit another model ... damped!  
fit3h = holt(air, alpha = .8, beta = .2, damped = TRUE, initial = "optimal", h = 5)
# Plot the fitted value (estimated from triaining data)
lines(fitted(fit3h), col = "darkgreen", type= "o")
# Plot the forecasts
lines(fit3h$mean,col = "darkgreen", type= "o")

# Fit another model ... what is the difference?  
fit4h = holt(air, alpha = .8, beta = .2, damped = TRUE, initial = "optimal", exponential = TRUE, h = 5)
# Plot the fitted value (estimated from triaining data)
lines(fitted(fit4h), col = "cyan", type= "o")
#Plot the forecasts
lines(fit4h$mean,col = "cyan", type= "o")

# with implicit Test set... it figures out by the time which are training and which are test. 
accuracy(fit1h, ausair)
accuracy(fit2h, ausair)
accuracy(fit3h, ausair)
accuracy(fit4h, ausair)


#with explicit Test set ... (same output)
airTest = window(ausair, start = 2005)
accuracy(fit1h, airTest)
accuracy(fit2h, airTest)
accuracy(fit3h, airTest)

#Add the actual values to visually compare forecasts to actual values
air2008 = window(ausair, start = 1990, end = 2009)
points(air2008, type = "o")


#3. Seasonal Trend

#Load the data
data("austourists")
# Read about the dataset!
?austourists


# Always plot the data first!
plot(austourists)

# returns a ts object.  
aust = window(austourists,start = c(1999,1), end = c(2005,4))

#fit an additive and multiplicative model
fit1s = hw(aust,seasonal = "additive",h = 40)
fit2s = hw(aust,seasonal = "multiplicative",h = 40)

#Plot the original data
plot(aust,ylab = "Australian Tourists", xlab = "Year", type = "o", xlim = c(1999, 2016),ylim = c(15,80))
#add the fitted values from the model (of the training data)
lines(fitted(fit1s),col = "blue", type= "o")
lines(fitted(fit2s), col = "red", type= "o")

#Now add the forecasts (add these one at a time)
lines(fit1s$mean, col = "blue", type= "o")
lines(fit2s$mean,col = "red", type= "o")

#Compare the accuracy
accuracy(fit1s,austourists)
accuracy(fit2s,austourists)

#add the actual values to visually compare the forecasts to the actual values. 
points(austourists, type = "o")

#Another Idea
fit3s = fit2s
fit3s$mean = fit3s$mean *1.1 
accuracy(fit1s,austourists)
accuracy(fit2s,austourists)
accuracy(fit3s,austourists)

#Plot the original data
plot(aust,ylab = "Australian Tourists", xlab = "Year", type = "o", xlim = c(1999, 2016),ylim = c(15,80))
#add the actual values to visually compare the forecasts to the actual values. 
points(austourists, type = "o")
lines(fit3s$mean,col = "purple", type= "o")


#Ensemble
fit4s = (fit1s$mean+fit2s$mean) / 2
accuracy(fit1s,austourists)
accuracy(fit2s,austourists)
accuracy(fit3s,austourists)
accuracy(fit4s,austourists)

plot(aust,ylab = "Australian Tourists", xlab = "Year", type = "o", xlim = c(1999, 2016),ylim = c(15,80))
points(austourists, type = "o")
lines(fit4s,col = "green", type= "o")


