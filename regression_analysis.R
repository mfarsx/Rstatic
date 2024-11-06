options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)



data <- read_excel("/Users/mfarsx/Documents/dev/regression/Data.xlsx", sheet = 1) # nolint

data <- data %>% distinct()

# Create Linear Regression Model
model <- lm(Price ~ `Land Size (sqm)` + Bedrooms + Bathrooms, data = data)

model_summary <- summary(model)


# 5.a - Multiple Regression - Scatter Plot with Regression Line\

plot(data$`Land Size (sqm)`, data$Price,
    xlab = "Land Size (sqm)", ylab = "Price",
    main = "Price vs Land Size"
)

abline(model, col = "blue")

# 5.c - R-squared
rsquared <- summary(model)$r.squared

plot(1,
    type = "n", axes = FALSE, xlab = "", ylab = "",
    main = paste("R-squared:", round(rsquared, 4))
)


# 5.d Testing the Overall Model Significance at a 5% Significance Level


f_statistic <- model_summary$fstatistic[1]
p_value <- pf(f_statistic, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE) # nolint

# Displaying F-statistic and p-value in a plot
plot(1,
    type = "n", axes = FALSE, xlab = "", ylab = "",
    main = paste(
        "F-statistic:", round(f_statistic, 2),
        "\nP-value:", round(p_value, 4)
    )
)

# 5.e Assessing the Significance of Independent Variables at a 5% Significance Level

coefficients <- model_summary$coefficients

plot(1,
    type = "n", axes = FALSE, xlab = "", ylab = "",
    main = paste(
        "P-values for Coefficients\n",
        "Land Size: ", round(coefficients[2, 4], 4), "\n",
        "Bedrooms: ", round(coefficients[3, 4], 4), "\n",
        "Bathrooms: ", round(coefficients[4, 4], 4)
    )
)


# 5.f Correlation Analysis and Assessment of Multicollinearity

correlation_matrix <- cor(data[, c("Land Size (sqm)", "Bedrooms", "Bathrooms")])

plot(1,
    type = "n", axes = FALSE, xlab = "", ylab = "",
    main = paste(
        "Correlation Coefficients\n",
        "Land Size & Bedrooms: ", round(correlation_matrix[1, 2], 4), "\n",
        "Land Size & Bathrooms: ", round(correlation_matrix[1, 3], 4), "\n",
        "Bedrooms & Bathrooms: ", round(correlation_matrix[2, 3], 4)
    )
)
