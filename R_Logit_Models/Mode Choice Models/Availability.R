require(devtools) 
install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")


library(mlogit)
library(data.table)

df <- read.csv("C:/Users/Ethan/Desktop/MChInput_2015_withColumns.csv")
df$Mode = relevel(as.factor(df$Mode), "Auto")
df$Income = as.factor(df$Income)
df$Status = as.factor(df$Status)
df$Gender = as.factor(df$Gender)
df$Licence = as.factor(df$Licence)
df$Work = as.factor(df$Work)
df$Family = as.factor(df$Family)
df$Level = as.factor(df$Level)

# Remove outliers
df_mode <- df[!(df$Time.Active > 600),]
df_mode <- df_mode[!(df_mode$Time.Transit > 200),]
df_mode <- df_mode[!(df_mode$Time.Auto > 150),]

df_mode <- df_mode[!((df_mode$Mode == "Active") & df_mode$Time.Active >= 300),]
df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Cars <= 0),]
df_mode <- df_mode[!(df_mode$Time.Active >= 300 & df_mode$Cars <= 0),]

# Transform dataframe from wide to long
# mldf  = mlogit.data(df_mode[sample(nrow(df), 3), ], varying = 12:14, choice = "Mode", shape = "wide")
# mldf  = mlogit.data(df_mode[c(1:15), ], varying = 12:14, choice = "Mode", shape = "wide")

mldf = mlogit.data(df_mode, varying = 12:14, choice = "Mode", shape = "wide")
# for (row in seq(2123, 3000, 3))
# {
#   print(row)
#   mldf$available = 1
#   if (!mldf[row, "Mode"])
#   {
#     mldf[row, "available"] = 0
#   }
#   model = mlogit(Mode ~ 0| 1 | Time, data = mldf, reflevel = "Transit", subset = mldf$available == 1)
#   #tryCatch(model = mlogit(Mode ~ 0| 1 | Time, data = mldf, reflevel = "Transit", subset = mldf$available == 1),
#    #        )
#   
# }


# head(mldf, 12)

# Column for availability
# mldf$available = ifelse((mldf$alt == "Auto" & mldf$Cars > 0), 1, ifelse((mldf$alt == "Transit"), 1, ifelse((mldf$alt == "Active" & mldf$Time < 300), 1, 0)))
mldf$available = (mldf$alt == "Auto" & mldf$Cars > 0) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 300)
# mldf$available = 1
mldf$available = as.logical(mldf$available)
table(mldf$available)
table(mldf$Mode & mldf$available)

# mldf[7, "available"] = 0
# mldf[89, "available"] = 0
# mldf[14, "available"] = 0
# mldf[2138, "available"] = 0
# mldf[1544, "available"] = 0
# mldf[602, "available"] = 0
# mldf[16, "available"] = 0

#$available
# Run MNL model
model = mlogit(Mode ~ 0| 1 | Time, data = mldf, reflevel = "Transit", subset = mldf$available == 1)
print(summary(model))

# for (row in seq(1, nrow(mldf), 3))
# {
#   # row - Active, row + 1 - Auto, row + 2 - Transit
#   if (!mldf[row, "available"] && !mldf[row+1, "available"])
#   {
#     print(row)
#   }
# 
#   if ((mldf[row, "Mode"] + mldf[row + 1, "Mode"] + mldf[row + 2, "Mode"]) != 1)
#   {
#     print(row)
#   }
# 
#   for (i in 0:2)
#   {
#     if (mldf[row + i, "Mode"])
#     {
#       if (!mldf[row + i, "available"])
#       {
#         print(row+i)
#       }
#     }
#   }
# }

# mldf = mlogit.data(df_mode, varying = 12:14, choice = "Mode", shape = "wide")
# 
# new_df = mldf[c(16:18, 22:24, 61:63, 70:72, 85:87, 115:117, 2107:2109),]
# write.csv(new_df, "C:/Users/Ethan/Desktop/FailureCasesActive.csv")
# 
# new_df2 = mldf[c(7:9, 31:42, 46:51, 2110:2112, 2122:2124),]
# write.csv(new_df2, "C:/Users/Ethan/Desktop/FailureCasesAuto.csv")
