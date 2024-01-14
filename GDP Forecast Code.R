library(readxl)
library(lmtest)
library(car)


excel_file <- "E:\\Econ\\GDP Forecast Econometrics.xlsx"
data <- read_excel(excel_file)

View(data)


model <- lm(`GDP (billions)` ~ `Population (thousands)` + 
              `Military Spending (billions)` + `Noncyclical Unemployment Rate`
            + `Government Spending (billions)` + `Fed Funds Rate` 
            + `Inflation (CPI-Level)` + `Oil Price (WTI Crude Spot Price)`
            + `Net Exports (billions)` + `Government Debt` + 
              `Labor Force Participation Rate (Percentage)` +
              `Net Importer of Oil` + `Presidential Party IS Republican` +
              `Home Ownership Rate` + `At War` + 
              `SP500 Index` + `Foreign Investment Inflow (millions)` +
              `Consumer Sentiment` + `Federal Government Tax Revenue (billions)` +
              `Industrial Index` + `Unified Government` + 
              `Rental Vacancy Rates (Percentage)` + `Total Capital Expenditures (millions)`
            + `US Wealth (millions)`, data = data)

summary(model)

#Now Remove Insignificant Variables
model2 <- lm(`GDP (billions)` ~ `Population (thousands)` + 
              `Military Spending (billions)` + `Noncyclical Unemployment Rate`
            + `Government Spending (billions)`  
            + `Inflation (CPI-Level)` 
            + `Net Exports (billions)` + `Government Debt` + 
              `Labor Force Participation Rate (Percentage)` +
              `Net Importer of Oil` + `Presidential Party IS Republican` +
              `Home Ownership Rate` +
              `Consumer Sentiment`  + `Total Capital Expenditures (millions)`
            + `US Wealth (millions)`, data = data)

summary(model2)

#Remove 90% significance variables (only 99% confidence variables remain)

model3 <- lm(`GDP (billions)` ~ `Population (thousands)` + 
               `Military Spending (billions)` + `Noncyclical Unemployment Rate`
             + `Government Spending (billions)`  
             + `Net Exports (billions)` + `Government Debt` + 
               `Labor Force Participation Rate (Percentage)` +
               `Net Importer of Oil` +
               `Home Ownership Rate` + `Consumer Sentiment`  + 
               `Total Capital Expenditures (millions)`
             + `US Wealth (millions)`, data = data)

summary(model3)

# Test for Independence of Residuals
dw_test <- dwtest(model3)
print(dw_test)

# Homoscedasticity test
bp_test <- bptest(model3)
print(bp_test)

#Normality Test
shapiro.test(resid(model3))

#Linearity Test
ncvTest(model3)

#Standardized residuals vs. fitted values
plot(model3$fitted.values, std_residuals,
     main = "Standardized Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h = 0, col = "red")

#Normal Probability Plot
qqnorm(resid(model3))
qqline(resid(model3))


