data <- read.csv('http://stats.idre.ucla.edu/stat/data/binary.csv')
data
str(data)
head(data)

data$rank <- as.factor(data$rank)
str(data)

train <- data[1:200,]
train
test <- data[201:400,]
test

model <- glm(admit ~ gre + gpa + rank, data = data,
             family = 'binomial')
model
summary(model)

model2 <- glm(admit ~ gre + gpa + rank, data = data,
              family = 'binomial')
