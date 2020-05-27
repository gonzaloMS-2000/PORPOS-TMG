import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
import numpy as np

df_path = pd.read_csv('shortest_path.csv')
df_filt = pd.read_csv('TTS_2016_Filtered.csv')

# Transforming important columns into lists
orig = list(df_path['Origin'])
dest = list(df_path['Destination'])
home = list(df_filt['HomeZone'])
school = list(df_filt['SchoolZone'])
distance = list(df_path['Data'])
# New list that will store each distance
distance_list = []

for j in range(0,len(home),1):

    temp_home = home[j]
    temp_school = school[j]
    orig_index = 0
    temp_orig = orig[orig_index]
    while temp_orig != temp_home:
        orig_index += 2392
        temp_orig = orig[orig_index]

    # We know that HomeZone value is located from indices [orig_index, orig_index + 2392]
    temp_dest = dest[orig_index]
    for j in range(0,2391,1):
        if temp_dest == temp_school:
            temp_distance = distance[orig_index] 
        else:
            orig_index += 1
            temp_dest = dest[orig_index]

    distance_list.append(temp_distance)

# Transform distance list into dataframe column:

# Define a dictionary containing Students data 
data = {'Distance': distance_list} 
# Convert the dictionary into DataFrame 
df = pd.DataFrame(data) 
df_filt.insert(2, "Distance", distance_list, True) 
df_filt.head()

df.to_csv(r'C:\Users\gonza\Desktop\Filtered_distance.csv', index = False)