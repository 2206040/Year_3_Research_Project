library(OpenImageR)
red <- readImage("C:/Users/abela/OneDrive/Documents/uni/Year 3/Research project/Immunostain NS/PCF11 WT NS/PCF11_WT_NS_red1.tif")
green <- readImage("C:/Users/abela/OneDrive/Documents/uni/Year 3/Research project/Immunostain Pol NS/PCF11 WT NS/PCF11_WT_NS_green.tif")) #location of red and green images
plot(
  as.vector(red), as.vector(green),
  col = rgb(0, 0, 0, 0.05), pch = 19,
  xlab = "Red channel (Nuclear Speckle)",
  ylab = "Green channel (plasmid PCF11)",
  main = "Colocalization scatter plot"
)
# Fit a linear model (green ~ red)
fit <- lm(as.vector(green) ~ as.vector(red))
abline(fit, col = "black", lwd = 2)
correlation_result <- cor.test(as.vector(red), as.vector(green))
print(correlation_result)
