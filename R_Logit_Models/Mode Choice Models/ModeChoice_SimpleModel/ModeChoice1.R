library(mlogit)

# Read ModeChoice_input.csv
df <- read.csv("C:/Users/gonza/Desktop/ModeChoice_Input.csv")
df$Mode = as.factor(df$Mode)
#df$Mode_String = as.factor(df$Mode_String)

str(df)

# Transform dataframe from wide to long
mldf = mlogit.data(df, varying = 2:4, choice = "Mode", shape = "wide")
head(mldf)

# Run MNL model
mlogit.model1 = mlogit(Mode ~ 0|Time, data = mldf, reflevel = 1)
summary(mlogit.model1)
