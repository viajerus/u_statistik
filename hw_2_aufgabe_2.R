library(faraway)
data("wcgs")
attach(wcgs)
library(rgl)
library(scatterplot3d)






sum(is.na(wcgs$chol))

wcgs <- wcgs[!is.na(wcgs$chol),]

# Generate a linear model
mod <- glm(chd~weight + chol + weight*chol, family="binomial", data = wcgs)

predict(mod)

# Get predicted values of mpg from wt and disp


wcgs$pred_chd <- predict(mod, type = "response")


# Define a function to remove outliers
remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA
  x
}

colors <- c("orange", "darkgreen")
colors <- colors[as.numeric(wcgs$chd)]

shapes = c(17, 18) 
shapes <- shapes[as.numeric(wcgs$chd)]

# Remove outliers for weight and chol
wcgs$weight <- remove_outliers(wcgs$weight)
wcgs$chol <- remove_outliers(wcgs$chol)


scatterplot3d(wcgs$weight, wcgs$chol, wcgs$pred_chd,
              pch = 16, angle=55, type = "h", cex.symbols = 0.5,
              xlab="Weight [in lbs]",
              ylab="Fasting serum cholesterol [in mg/dl]", 
              zlab= "Coronary heart disease developed", 
              color = colors, 
              box = TRUE, 
              main = "3D Scatter Plot")

addgrids3d(grid = c("xy", "xz", "yz"))

legend("right", legend = levels(wcgs$chd),
       col =  c("orange", "darkgreen"), pch = 16)










