# Load full data
full_data <- read.csv("C:\\Users\\Louis Lapp\\Documents\\Research\\MultivariableArcticFeature_Analysis\\ArticYearlyMeanDataset.csv")

# Generate table
features <- c("year", "wind_10m", "specific_humidity", "LW_down", "SW_down", "rainfall", "snowfall", "sosaline", "sst", "t2m", "surface_pressure", "sea_ice_extent")
columns <- c("X", "Y", "m", "b", "+/-", "r2", "p")
correlation_table <- data.frame(matrix(nrow = 1, ncol = length(columns)))
colnames(correlation_table) <- columns
for (feature_num in 1:length(features)){
  remaining_features <- features
  remaining_features <- remaining_features[-feature_num]
  for (sec_feature_num in 1:length(remaining_features)) {
    model = lm(eval(parse(text = features[feature_num]))~eval(parse(text = remaining_features[sec_feature_num])), data = full_data)
    coef_model = coef(model)
    correlation_table <- rbind(correlation_table, c(
      features[feature_num],
      remaining_features[sec_feature_num],
      coef_model[2],
      coef_model[1],
      ifelse(coef_model[2]>0, 1, ifelse(coef_model[2]==0, 0, -1)),
      summary(model)$r.squared,
      summary(model)$coefficients[, 4]
    ))
  }
}

write.csv(correlation_table[-c(1), ], "TwoVariableCorrelation_Table.csv")

# Generate scatterplot matrix
png(filename = "ScatterplotMatrix.png", width = 1000, height = 1000)
plot(full_data)
dev.off()

# Generate correlation matrix
png(filename = "CorrelationMatrix_Color.png", width = 1000, height = 1000)
library("corrplot")
corrplot(cor(full_data), method = "color") # circle, pie, color, number
dev.off()