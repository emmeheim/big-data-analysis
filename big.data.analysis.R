#upload data
data <- read.csv("C:/Users/eghei/OneDrive/Documents/STAT4110/fat.dat-1.csv")
data

rownames(data) <- data[, 1]
data <- data[, -1]
fix(data)

#fix data
data$Height[data$Case == 42] <- 69.5

calculate_density_brozek <- function(body_fat_percent) {
  return(457 / (body_fat_percent + 414.2))}

data$Density[data$Case == 48] <- calculate_density_brozek(6.4)
data$Density[data$Case == 76] <- calculate_density_brozek(18.3)
data$Density[data$Case == 96] <- calculate_density_brozek(17.3)

calculate_bodyfat_brozek <- function(density_brozek) {
  return((457 / density_brozek) - 414.2)}

data$Brozek[data$Case == 182] = calculate_bodyfat_brozek(1.1089)

calculate_bodyfat_siri <- function(density_siri) {
  return((495 / density_siri) - 450)}

data$Siri[data$Case == 182] = calculate_bodyfat_siri(1.1089)

data[182,]

data = data[-182,]

data
summary(data)

#data analysis

#multiple linear regression model
lm_model <- lm(Brozek ~ Weight + Adiposity +
                 Chest + Abdomen, data = data)
summary_lm <- summary(lm_model)
print(summary_lm)

#correlation
cor_matrix <- cor(data)
print(cor_matrix)

#scatterplot
explanatory_vars = data[, c("Weight","Adiposity","Chest","Hip","Abdomen","Brozek")]
pairs(explanatory_vars, main = "Explanatory Variables Matrix")

ggplot(data, aes(x = Weight, y = Brozek)) +
  geom_point(color = "pink", size = 2) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  labs(title = "Scatterplot of Weight vs. Body Fat Percentage (Brozek)",
       x = "Weight (lbs)",
       y = "Body Fat Percentage (Brozek)") +
  theme_minimal()

ggplot(data, aes(x = Adiposity, y = Brozek)) +
  geom_point(color = "pink", size = 2) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  labs(title = "Scatterplot of Adiposity Index (kg/m^2) vs. Body Fat Percentage (Brozek)",
       x = "Adiposity Index (kg/m^2)",
       y = "Body Fat Percentage (Brozek)") +
  theme_minimal()

ggplot(data, aes(x = Chest, y = Brozek)) +
  geom_point(color = "pink", size = 2) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  labs(title = "Scatterplot of Chest Circumference (cm) vs. Body Fat Percentage (Brozek)",
       x = "Chest Circumference (cm)",
       y = "Body Fat Percentage (Brozek)") +
  theme_minimal()

ggplot(data, aes(x = Abdomen, y = Brozek)) +
  geom_point(color = "pink", size = 2) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  labs(title = "Scatterplot of Abdomen Circumference (cm) vs. Body Fat Percentage (Brozek)",
       x = "Abdomen Circumference (cm)",
       y = "Body Fat Percentage (Brozek)") +
  theme_minimal()

# diagnostic plots
par(mfrow = c(2, 2))
plot(lm_model)

# interactions
interaction1 <- lm(Brozek ~ Chest * Abdomen, data = data)
summary(interaction1)
interaction1 <- lm(Brozek ~ Weight * Chest, data = data)
summary(interaction1)
interaction1 <- lm(Brozek ~ Adiposity * Weight, data = data)
summary(interaction1)