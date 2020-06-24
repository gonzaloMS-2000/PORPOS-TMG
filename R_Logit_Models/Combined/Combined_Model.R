# ---- Install packages ----
require(devtools)
install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)

# ---- Mode choice model ----
# Load data
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Combined")
mode_df <- read.csv("../Mode Choice Models/MChInput_2015_withColumns_2.4.csv")
mode_df = mode_df[c('Mode', 'Licence', 'Family', 'Cars', 'Time.Auto', 'Time.Transit', 'Time.Active')]

# Reformat data
mode_df$Mode = as.factor(mode_df$Mode)
mode_df$Licence = as.factor(mode_df$Licence)
mode_df$Family = (mode_df$Family == 'Family') * 1
mode_df$Family = as.factor(mode_df$Family)

# Remove outliers and unavailable choices
mode_df <- mode_df[!(mode_df$Time.Auto > 1000),]
mode_df <- mode_df[!((mode_df$Mode == "Active") & mode_df$Time.Active >= 46.6),]
mode_df <- mode_df[!((mode_df$Mode == "Auto") & mode_df$Cars <= 1),]

# Transform dataframe from wide to long
mldf = mlogit.data(mode_df, varying = 5:7, choice = "Mode", shape = "wide")
mldf$available = (mldf$alt == "Auto" & mldf$Cars >= 2) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 46.6)

# Run model
model = mlogit(Mode ~ 0 | Family + Licence | Time, data = mldf, reflevel = "Transit", subset = mldf$available)


# ---- Accessibility ----
# campus_df <- read.csv("Combined_Input.csv")
campus_df <- read.csv("Combined_Input_Access.csv")

# Reformat data
campus_df$Campus = as.factor(campus_df$Campus)
campus_df$Segment = as.factor(campus_df$Segment)
campus_df$Licence = as.factor(campus_df$Licence)
campus_df$Family = as.factor(campus_df$Family)

# for (i in 0:6)
# {
#   print(i)
#   temp_df = campus_df[, c(2:4, (i*4)+6, (i*4)+7, (i*4)+9)]
#   names(temp_df)[names(temp_df)==paste0("Time.Auto.", i)] <- "Time.Auto"
#   names(temp_df)[names(temp_df)==paste0("Time.Transit.", i)] <- "Time.Transit"
#   names(temp_df)[names(temp_df)==paste0("Time.Active.", i)] <- "Time.Active"
#   temp_df$Mode = "Transit"
#   temp_mldf = mlogit.data(temp_df, varying = 4:6, choice = "Mode", shape = "wide")
#   mldf_av = mldf[mldf$available]
#   access = as.data.frame(logsum(model, data=temp_mldf))
#   names(access) <- paste0("Access.", i)
#   campus_df = cbind(campus_df, access)
# }

# ---- Location choice ----

# Convert data to long format
campus_df = campus_df[c(1, 5, seq(8, 32, by=4), 34:41)]
lc_mldf = mlogit.data(campus_df, varying = c(3:9, 11:17), choice = "Campus", shape = "wide")

# Prepare output matrix
num_segments = 7L
num_ascs = 6L
coefs_and_stats = array(numeric(), c(num_ascs + 4, num_segments*6))

# Iterate over segments
for (i in 0:6)
{
  print(i)

  lc_model_1 = mlogit(Campus ~ Dist, data=lc_mldf, reflevel="4", subset = lc_mldf$Segment == i)
  lc_model_2 = mlogit(Campus ~ Access, data=lc_mldf, reflevel="4", subset = lc_mldf$Segment == i)
  lc_model_3 = mlogit(Campus ~ Dist + Access, data=lc_mldf, reflevel="4", subset = lc_mldf$Segment == i)
  lc_model_4 = mlogit(Campus ~ Dist, data=lc_mldf, reflevel="4", subset = lc_mldf$Segment == i, weights = lc_mldf$Exp)
  lc_model_5 = mlogit(Campus ~ Access, data=lc_mldf, reflevel="4", subset = lc_mldf$Segment == i, weights = lc_mldf$Exp)
  lc_model_6 = mlogit(Campus ~ Dist + Access, data=lc_mldf, reflevel="4", subset = lc_mldf$Segment == i, weights = lc_mldf$Exp)
  for (k in 1:6)
  {
    col = (6*i)+k
    for (j in 1:num_ascs)
    {
      coefs_and_stats[j, col] = get(paste0("lc_model_", k))[[1]][[j]]
    }
    add = 1
    if (k %in% c(1, 3, 4, 6))  {
      coefs_and_stats[j+1, col] = get(paste0("lc_model_", k))[[1]][[j+add]]
      add = 2   }
    if (k %in% c(2, 3, 5, 6)) coefs_and_stats[j+2, col] = get(paste0("lc_model_", k))[[1]][[j+add]]

    coefs_and_stats[j+3, col] = get(paste0("lc_model_", k))[[2]][[1]]
    coefs_and_stats[j+4, col] = summary(get(paste0("lc_model_", k)))[[20]][[1]]
  }
}
coefs_and_stats
write.csv(coefs_and_stats, file="Combined_Output.csv", na = "0",
          row.names = c("ASC_SG", "ASC_SC", "ASC_MI", "ASC_YK", "ASC_RY", "ASC_OC",
                        "B_DIST", "B_ACCESS", "LLHOOD", "MCF R2"))
