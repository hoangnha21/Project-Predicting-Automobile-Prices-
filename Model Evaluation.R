#Load data
data <- read_excel("/Users/hoangnha218/Desktop/IDS572 Homework 4/Assignment4Data.xlsx")

#Remove uncessary columns
lmdata<- data[, !names(data) %in% c("Fuel","Drs", "Cyl","Abag_1","ABS","PStr")]

# Converting "Price" as numeric and factorize "Colour" variable
lmdata$Price <- as.numeric(lmdata$Price)
lmdata$Colour <- as.factor(lmdata$Colour)

mynormalization <- function(x)
{
  (x - min(x))/(max(x)-min(x))
}

lmdata<- lmdata %>% mutate_if(is.numeric, mynormalization)
summary(lmdata)

# Split train test
set.seed(123)

lm.indx <- sample(2,nrow(lmdata), replace = T, prob = c(0.7,0.3))
lm.train <- lmdata[lm.indx == 1,]
lm.test <- lmdata[lm.indx ==2,]

full <- lm(lm.train$Price ~ .,data = lm.train)
null <- lm(lm.train$Price ~ 1,data = lm.train)

step(null, scope = list(lower = null, upper = full), direction = "forward", trace = 0)

lmModel <- lm(formula = lm.train$Price ~ HP + Colour+ Clock + Age  + Grs, data = lm.train)
summary(lmModel)

# LR prediction on test data
pred.lm <- predict(lmModel, newdata = lm.test)
mean(lm.test$Price - pred.lm)^2
fitted(lmModel)
coefficients(lmModel)
residuals((lmModel))

# LR Mean square error (MSE)
MSE.lm <- sum((pred.lm - lm.test$Price)^2)/nrow(lm.test)
MSE.lm
plot(lmModel)


##Question 4
#Answer: Recommendation
