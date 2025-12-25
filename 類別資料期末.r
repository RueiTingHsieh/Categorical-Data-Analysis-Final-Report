data <- read.csv("clash royal info.csv")
head(data)

# cost glm
data$Success <- round(data$Win....)
data$Failure <- 100 - data$Success
model_glm <- glm(cbind(Success, Failure) ~ Elixir_cost, family = binomial, data = data)
summary(model_glm)
odds_ratios <- exp(coef(model_glm))

# cost(factor) lm
model_factor <- lm(Win.... ~ factor(Elixir_cost), data = data)
summary(model_factor)

# cost draw
plot(data$Elixir_cost, data$Win...., main = "卡片費用 vs. 勝率", xlab = "卡片費用", ylab = "勝率 (%)", col = "blue", pch = 19, cex = 1.5, cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.1)
out2 = lm(data$Win.... ~ data$Elixir_cost)
abline(out2, col = "red", lwd = 2)

# type
model_type <- lm(Win.... ~ factor(Type), data = data)
summary(model_type)

# type draw
ggplot(data, aes(x = Type, y = Win...., fill = Type)) + geom_boxplot(alpha = 0.8, outlier.color = "red") + labs(title = "不同卡牌類型的勝率分布", x = "卡牌類型", y = "勝率 (%)") + theme_minimal() + theme(legend.position = "none")

# usage
model_ols <- lm(Win.... ~ Usage...., data = data)
summary(model_ols)

#usage draw
plot(data$Usage...., data$Win...., main = "使用率 vs. 勝率", xlab = "使用率 (%)", ylab = "勝率 (%)", col = "blue", pch = 19, cex = 1.5, cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.1)out1 = lm(data$Win.... ~ data$Usage....)
abline(out1, col = "red", lwd = 2)
