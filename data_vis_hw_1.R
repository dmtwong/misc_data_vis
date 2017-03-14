install.packages("ggplot2")
library(ggplot2)
library(grid)
library(gridExtra)

df <- read.table("e:/Data Vis/ExcelFormattedGISTEMPData2CSV.csv", header=TRUE, sep=",")
str(df)

df2 <- read.table("e:/Data Vis/HW2_hkex_updated2.csv", header=TRUE, sep=",")
str(df2)
pairs(df2[2:4])
colors()

# If we plot scatter plots for each dependent variable against time seperately,
# nothing much could be revealed as each serie fluctuate 
par(mfrow = c(3,1))
plot(p_HKEx~Date,data=df2, type="l")
plot(p_HSI~Date,data=df2, type="l")
plot(r_f~Date,data=df2, type="l")

# Suppose we would like to disgard the time and look into each distribution
dist_p_HKEx = qplot(p_HKEx, data = df2, geom = "histogram", binwidth = 5, xlim = c( min(df2$p_HKEx), max(df2$p_HKEx) ))
dist_p_HSI= qplot(p_HSI, data = df2, geom = "histogram", binwidth = 1000, xlim = c( min(df2$p_HSI), max(df2$p_HSI) ))
dist_r_f = qplot(r_f, data = df2, geom = "histogram", binwidth = 0.05, xlim = c( min(df2$r_f), max(df2$r_f) ))
grid.arrange(dist_p_HKEx, dist_p_HSI, dist_r_f)

# Apparently there are no correlation between risk free rate and prices
pairs(df2[2:4])
cor(df2[2:4])

# But if transform the risk free rate into categories and higlight with color
# we can see there could be possible trends across cluster
df2$rate_catg <- cut(df2$r_f, c(-0.05,0.05,0.25,0.5))
table(df2$rate_catg)
df2$rate_catg2 <- cut(df2$r_f, c(-0.05,0.05,0.25,0.5), right=FALSE, labels=c('low', 'mod', 'high'))
qplot(p_HKEx, p_HSI, data = df2, colour = rate_catg2)

# 
df2_low <- df2[df2$rate_catg2=='low',]
df2_mid <- df2[df2$rate_catg2=='mod',]
df2_high <- df2[df2$rate_catg2=='high',]
cor(df2_low[2:4])
cor(df2_mid[2:4])
cor(df2_high[2:4])




