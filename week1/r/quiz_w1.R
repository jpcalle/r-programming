#
# Quiz week 1
#

data <- read.csv(file = '../data/hw1_data.csv', header = TRUE)

# Q11
colnames(data)

# Q12
data[1:2, ]

# Q13
nrow(data)

# Q14
data[nrow(data) - 1:0, ]
tail(data, 2)

# Q15
data[47, 'Ozone']

# Q16
sum(is.na(data[, 'Ozone']))

# Q17
mean(data[, 'Ozone'], na.rm = TRUE)

# Q18
d18 <- data[data[, 'Ozone'] > 31 & data[, 'Temp'] > 90, ]
mean(d18[, 'Solar.R'], na.rm = TRUE)

# Q19
mean(data[data[, 'Month'] == 6, 'Temp'], na.rm = TRUE)

# Q20
max(data[data[, 'Month'] == 5, 'Ozone'], na.rm = TRUE)
