# This script is for HW2 of GLG 893
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.formula.api as sm

## Loading in data from HW1 using HDFStore object
# Need to create object that references the file first
dataStorage = pd.HDFStore('S:/Users/roushjac/Misc/Class/GLG 893/Homework 2/Data/data.h5')

# Then use the object to retreive individual dataframes from it
TempPrecip = dataStorage['TempPrecip']
disData = dataStorage['disTotal']

# That's a lot of columns! Let's only keep what we need:
TempPrecip = TempPrecip[['NAME','DATE','PRCP','TMAX','TOBS']]

#Converting dates to datetime
TempPrecip['DATE']=pd.to_datetime(TempPrecip['DATE'])
disData['Time']=pd.to_datetime(disData['Time'])

# Sorting each dataframe by date
disData=disData.sort_values(by='Time')
TempPrecip=TempPrecip.sort_values(by='DATE')

# Creating day, month, year yields for each dataframe
# Using list comprehension like a sophisticated gentleman

TempPrecip['year'] = [thisdate.year for thisdate in TempPrecip['DATE']]
TempPrecip['month'] = [thisdate.month for thisdate in TempPrecip['DATE']]
TempPrecip['day'] = [thisdate.dayofyear for thisdate in TempPrecip['DATE']]
# Using a disgusting method to find quarter according to month, since I can't figure out how to use floor() to do it
for row in range(len(TempPrecip)):
    if TempPrecip.loc[row,'month'] in [1,2,3]:
        TempPrecip.loc[row,'quarter'] = 1
    elif TempPrecip.loc[row,'month'] in [4,5,6]:
        TempPrecip.loc[row,'quarter'] = 2
    elif TempPrecip.loc[row,'month'] in [7,8,9]:
        TempPrecip.loc[row,'quarter'] = 3
    elif TempPrecip.loc[row,'month'] in [10,11,12]:
        TempPrecip.loc[row,'quarter'] = 4

disData['year'] = [a.year for a in disData['Time']]
disData['month'] = [a.month for a in disData['Time']]
disData['day'] = [a.dayofyear for a in disData['Time']]
## Doing the same quarter method for discharge
# First resetting index to be number of rows - it currently starts at 2
disData.index = range(len(disData))
for row in range(len(disData)):
    if disData.loc[row,'month'] in [1,2,3]:
        disData.loc[row,'quarter'] = 1
    elif disData.loc[row,'month'] in [4,5,6]:
        disData.loc[row,'quarter'] = 2
    elif disData.loc[row,'month'] in [7,8,9]:
        disData.loc[row,'quarter'] = 3
    elif disData.loc[row,'month'] in [10,11,12]:
        disData.loc[row,'quarter'] = 4
        
#%% Grouping datasets by station and day of the year, then station and month
# First changing data type of discharge to be numeric - it was an object (???)
disData['Discharge'] = pd.to_numeric(disData['Discharge'])
disGroup = disData.groupby(['SiteName','day'])

# First doing daily trends for discharge
# Finding median of the groupby object
disMedian = disGroup.median()
# Changing indices to columns for plotting with seaborn
disMedian = disMedian.reset_index()


### Now doing the same thing for month
disGroupMonth = disData.groupby(['SiteName','month'])
disMedianMonth = disGroupMonth.median()
disMedianMonth = disMedianMonth.reset_index()

### Finding mean maximum temperature for all days
tempMax = TempPrecip.groupby(['NAME','day'])
tempMaxMean = tempMax.mean()
tempMaxMean = tempMaxMean.reset_index()

## Finding mean maximum temp for months
tempMaxMonth = TempPrecip.groupby(['NAME','month'])
tempMaxMeanMonth = tempMaxMonth.mean()
tempMaxMeanMonth = tempMaxMeanMonth.reset_index()

## Grouping by season for discharge data
disGroupSeason = disData.groupby(['SiteName','year','quarter'])
disMedianSeason = disGroupSeason.median()
disMedianSeason = disMedianSeason.reset_index()

# Finding yearly median of the groupby object
disGroupYear = disData.groupby(['SiteName','year'])
disMedianYear = disGroupYear.median()
# Changing indices to columns for plotting with seaborn
disMedianYear = disMedianYear.reset_index()

# Now for temp and precip
tempMaxSeason = TempPrecip.groupby(['NAME','year','quarter'])
tempMaxMeanSeason = tempMaxSeason.mean()
tempMaxMeanSeason = tempMaxMeanSeason.reset_index()

## Making dataframe for avg max temp and avg precip
tempMaxYear = TempPrecip.groupby(['NAME','year'])
tempMaxMeanYear = tempMaxYear.mean()
tempMaxMeanYear = tempMaxMeanYear.reset_index()

## Now subsetting each dataframe to include only 1 site for each variable - temp and precip data will still be in the same dataframe
disOneSite = disMedianSeason[disMedianSeason['SiteName'] == 'NARROWS NO 1 PH A ENGLEBRIGHT DAM CA']
disOneSite = disOneSite[disOneSite['quarter'].isin([1,3])]

# Now for temp & precip
tpOneSite = tempMaxMeanSeason[tempMaxMeanSeason['NAME'] == 'ALAMEDA NAS, CA US']
tpOneSite = tpOneSite[tpOneSite['quarter'].isin([1,3])]


#%% Fitting a trend to the data
# First creating empty dataframes - will be populated with stats outputs
disStatsYear = pd.DataFrame({'SiteName': list(disData.SiteName.unique()),
                         'R2': None,
                         'Pval': None})

tempStatsYear = pd.DataFrame({'SiteName': list(TempPrecip.NAME.unique()),
                         'R2': None,
                         'Pval': None})

precipStatsYear = pd.DataFrame({'SiteName': list(TempPrecip.NAME.unique()),
                         'R2': None,
                         'Pval': None})

disStatsSeason = pd.DataFrame({'quarter': list(disOneSite.quarter.unique()),
                         'R2': None,
                         'Pval': None})
disStatsSeason['SiteName'] = disOneSite.SiteName.unique()[0]

tempStatsSeason = pd.DataFrame({'quarter': list(tpOneSite.quarter.unique()),
                         'R2': None,
                         'Pval': None})
tempStatsSeason['SiteName'] = tpOneSite.NAME.unique()[0]

precipStatsSeason = pd.DataFrame({'quarter': list(tpOneSite.quarter.unique()),
                         'R2': None,
                         'Pval': None})
precipStatsSeason['SiteName'] = tpOneSite.NAME.unique()[0]

# Creating 3 grouby objects - 1 for each yearly dataframe
disMedianYearGroup = disMedianYear.groupby(['SiteName'])

tempMeanYearGroup = tempMaxMeanYear.groupby(['NAME'])

precipMeanYearGroup = tempMaxMeanYear.groupby(['NAME'])

# Doing regression on each group and populating my stats dataframes
for i in disMedianYearGroup:
    sitename = i[0]
    model = sm.ols('Discharge ~ year', i[1])
    result = model.fit()
    resultR2 = result.rsquared
    resultP = result.pvalues[1]
    disStatsYear.loc[disStatsYear['SiteName'] == sitename,'R2'] = resultR2
    disStatsYear.loc[disStatsYear['SiteName'] == sitename,'Pval'] = resultP

for i in tempMeanYearGroup:
    sitename = i[0]
    if i[1]['TMAX'].isnull().all():
        pass
    else:
        model = sm.ols('TMAX ~ year', i[1])
        result = model.fit()
        resultR2 = result.rsquared
        resultP = result.pvalues[1]
        tempStatsYear.loc[tempStatsYear['SiteName'] == sitename,'R2'] = resultR2
        tempStatsYear.loc[tempStatsYear['SiteName'] == sitename,'Pval'] = resultP

for i in precipMeanYearGroup:
    sitename = i[0]
    if i[1]['PRCP'].isnull().all():
        pass
    else:
        model = sm.ols('PRCP ~ year', i[1])
        result = model.fit()
        resultR2 = result.rsquared
        resultP = result.pvalues[1]
        precipStatsYear.loc[precipStatsYear['SiteName'] == sitename,'R2'] = resultR2
        precipStatsYear.loc[precipStatsYear['SiteName'] == sitename,'Pval'] = resultP
    
# Now doing regression for each variable for 1 site only

disModelQ1 = sm.ols('Discharge ~ year', disOneSite[disOneSite['quarter'] == 1]).fit()
disModelQ3 = sm.ols('Discharge ~ year', disOneSite[disOneSite['quarter'] == 3]).fit()

tempModelQ1 = sm.ols('TMAX ~ year', tpOneSite[tpOneSite['quarter'] == 1]).fit()
tempModelQ3 = sm.ols('TMAX ~ year', tpOneSite[tpOneSite['quarter'] == 3]).fit()

precipModelQ1 = sm.ols('PRCP ~ year', tpOneSite[tpOneSite['quarter'] == 1]).fit()
precipModelQ3 = sm.ols('PRCP ~ year', tpOneSite[tpOneSite['quarter'] == 3]).fit()

# Populating seasonal stats dataframes
disStatsSeason.loc[disStatsSeason['quarter'] == 1, 'R2'] = disModelQ1.rsquared
disStatsSeason.loc[disStatsSeason['quarter'] == 1, 'Pval'] = disModelQ1.pvalues[1]
disStatsSeason.loc[disStatsSeason['quarter'] == 3, 'R2'] = disModelQ3.rsquared
disStatsSeason.loc[disStatsSeason['quarter'] == 3, 'Pval'] = disModelQ3.pvalues[1]

tempStatsSeason.loc[tempStatsSeason['quarter'] == 1, 'R2'] = tempModelQ1.rsquared
tempStatsSeason.loc[tempStatsSeason['quarter'] == 1, 'Pval'] = tempModelQ1.pvalues[1]
tempStatsSeason.loc[tempStatsSeason['quarter'] == 3, 'R2'] = tempModelQ3.rsquared
tempStatsSeason.loc[tempStatsSeason['quarter'] == 3, 'Pval'] = tempModelQ3.pvalues[1]

precipStatsSeason.loc[precipStatsSeason['quarter'] == 1, 'R2'] = precipModelQ1.rsquared
precipStatsSeason.loc[precipStatsSeason['quarter'] == 1, 'Pval'] = precipModelQ1.pvalues[1]
precipStatsSeason.loc[precipStatsSeason['quarter'] == 3, 'R2'] = precipModelQ3.rsquared
precipStatsSeason.loc[precipStatsSeason['quarter'] == 3, 'Pval'] = precipModelQ3.pvalues[1]





#%% Trying to make it work with Seaborn - wishing myself luck
# This makes a scatterplot for all 5 sites of day vs discharge
fig,ax = plt.subplots()
plt.rcParams["axes.labelsize"] = 10
sns.tsplot(data = disMedian, time = 'day', value = 'Discharge', unit = 'SiteName', condition = 'SiteName')
ax.set(xlabel = "Day", ylabel = "Discharge (Cubic ft/sec)")
ax.set_title("Daily Median Discharge")
# Put the legend out of the figure
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)

# Now for month
fig,ax = plt.subplots()
plt.rcParams["axes.labelsize"] = 10
sns.tsplot(data = disMedianMonth, time = 'month', value = 'Discharge', unit = 'SiteName', condition = 'SiteName')
ax.set(xlabel = "Month", ylabel = "Discharge (Cubic ft/sec)")
ax.set_title("Monthly Median Discharge")
# Put the legend out of the figure
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)

## Time series plot for daily maximum temperature
fig,ax = plt.subplots()
plt.rcParams["axes.labelsize"] = 10
sns.tsplot(data = tempMaxMean, time = 'day', value = 'TMAX', unit = 'NAME', condition = 'NAME')
ax.set(xlabel = "Day", ylabel = "Temperature Maximum (F)")
ax.set_title("Daily Maximum Temperature")
# Put the legend out of the figure
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)

# Now for monthly average max temp
fig,ax = plt.subplots()
plt.rcParams["axes.labelsize"] = 10
sns.tsplot(data = tempMaxMeanMonth, time = 'month', value = 'TMAX', unit = 'NAME', condition = 'NAME')
ax.set(xlabel = "Month", ylabel = "Temperature Maximum (F)")
ax.set_title("Average Monthly Maximum Temperature")
# Put the legend out of the figure
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
