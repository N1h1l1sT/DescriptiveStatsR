# DescriptiveStats

## Installation

```r
if (!("devtools" %in% rownames(installed.packages()))) install.packages("devtools", repos = "https://cloud.r-project.org")
library(devtools)
if (!("DescriptiveStats" %in% rownames(installed.packages()))) install_github("N1h1l1sT/DescriptiveStatsR", upgrade = FALSE)
library(DescriptiveStats)
```

___
#### Note:
###### _There's a working demo playing out a use-case below_
___

## Known issues
* None

___

# Tutorial: How to use it on your own dataset (For reproducible examples scroll to the end)
## Initialisation

```r
library(DescriptiveStats)

# Given any Dataset, for instance 'ExampleDS' which is based on real data from the industry:
```

![](https://github.com/N1h1l1sT/DescriptiveStatsR/blob/master/Examples/Windmills/Dataset.png?raw=true "Dataset to be analysed")

```r
#We can get Descriptive Statistics for it by calling DescriptiveStats()

ExampleStats <-
  ExampleDS %>%
  DescriptiveStats(
    CalculateGraphs = TRUE, #CalculateGraphs is the only one we NEED to set, you can skip the rest. If False then only matrices are calculated; no plots.
    DependentVar = "KWh",   #In the dataset, the name of what we consider a Dependent Variable is how much power was produced, i.e. "KWh"
    IsTimeSeries = TRUE,    #This dataset is timeseries as each row is measured 10 minutes after the previous one, so TRUE
    GroupBy = "Windmill",   #We may want to see how each windmill does compared to each other, so group by per windmill
    TimeFlowVar = "Date",   #If IsTimeSeries=TRUE and left blank, then a new TimeFlowVar is introduced as number from 1 to NROW(Dataset)
    BoxplotPointsColourVar = "WindSpeed" #It's interesting to juxtapose a second second/third variable in boxplots. For instance, see the KWh per Month and juxtapose Windspeed as a 3rd variable. Does the KWh go higher as Windspeed goes higher?
    )
```

![](https://github.com/N1h1l1sT/DescriptiveStatsR/blob/master/Examples/Windmills/DescriptiveStats%20Output.png?raw=true "DescriptiveStatS()'s output")

```r
#The above is the Console Output of the function, but the real wealth of results are yet to be seen.
#They are inside the variable 'ExampleStats' and can be saved to a folder of your choice:

ExampleStats %>%
  SaveDescrStats(
      path_stats = file.path(getwd(), "ExampleDS"), #I'm creating a new folder named 'ExampleDS'
  )
#So a folder is created, containing the following:
```

![](https://github.com/N1h1l1sT/DescriptiveStatsR/blob/master/Examples/Windmills/Descriptive%20Statistics%20Output.png?raw=true "SaveDescrStats()'s output")

#### It might seem daunting at first - too many things, but we don't need everything every time - there's a method to this madness.

* The "Categorical Descriptives.csv" and "Numerical Descriptives.csv" contain the general descriptives we see above, like "Min", "Q1", "Max", etc
* The "Pearson Cor.csv" and "Spearman Cor.csv" contain the Correlations like we see above with other csv files for the p-value and another set of csv files for the same correlations but ordered in a meaningful way (as opposed to being ordered the way the dataset variables are)

###### _This covers all the .csv files, moving on to the pictures now and then to the folders_

* The "Pearson Cor.png" and "Spearman Cor.png" show Pearson and Spearman correlations respectively with statistical significance indicators, whilst "Pearson Cor Ordered.png" and "Spearman Cor Ordered.png" do the same thing but variables are ordered to form clusters. Take Spearman correlations ordered, for instance:

![Spearman Cor Ordered.png](https://github.com/N1h1l1sT/DescriptiveStatsR/blob/master/Examples/Windmills/Spearman%20Cor%20Ordered.png?raw=true "Spearman correlations ordered")

We can see that KWh, ReactivePower and RotorSpeed are statistically significantly strongly correlated with each other, forming one cluster, whilst Orientation is not correlated with any other variable, and with ReactivePower in particular, the -0.02 correlation is not statistically significant, as portrayed by the "X" symbol.

###### _So already, 14 out of 20 pictures are addressed; it was only descriptives and correlations_

* The "Categorical Distributions.png" will disaplay barplots for each Categorical variable

![Categorical Distributions.png](https://github.com/N1h1l1sT/DescriptiveStatsR/blob/master/Examples/Windmills/Categorical%20Distributions.png?raw=true "Categorical Distributions")

Here, we can see, for instance, that we have less data for February, which might be partially explained by the fact that February has less days than other months. However, we see that not all 30-days-months or not all 31-days-months contain exactly the same amount of data (rows) either.
There is also a disparity between windmills as well.

* The "Categorical VS Dependent.png" shows a juxtaposition of boxplots, 1 for each category of the categorical variable in question.

###### _Note that here the Dependent variable is Continuous. For Categorical variable VS Categorical Dependent we get juxtaposed stacked barplots_

![Categorical VS Dependent.png](https://github.com/N1h1l1sT/DescriptiveStatsR/blob/master/Examples/Windmills/Categorical%20VS%20Dependent.png?raw=true "Categorical VS Continuous Dependent")

So, we can see that the best months for Power Production on the windmills were January, November, May and February, whilst the worst were July and October.
We also see that Windmill number 4 outperformed all other windmills by a significant margin.

Notice how the dots on low Power Production are mainly red colour, meaning WindSpeed of 10 or less, but for high values of KWh, the dots are mainly purple, so WindSpeed values of about 20, which is what we expected, but it's nice to visually confirm it.

* The "Numerical Boxplots.png" shows a boxplot for each Numerical Variable, allowing us to gauge at their distribution

![Numerical Boxplots.png](https://github.com/N1h1l1sT/DescriptiveStatsR/blob/master/Examples/Windmills/Numerical%20Boxplots.png?raw=true "Boxplots for Numerical Variables")

Watching the 1st boxplot, we see that 25% of the time the windmill basically produces zero, or near-zero power, 50% of the production is between 80 to 280 KWh.
We can also see that the orientation, while it seemingly can take any value, mostly falls between 3 zones.

* The "Numerical Distrubutions.png" is another way to see the distribution of the Numerical Variables; that is, with Histograms overlayed with the corresponding Density plot.

![Numerical Distrubutions.png](https://github.com/N1h1l1sT/DescriptiveStatsR/blob/master/Examples/Windmills/Numerical%20Distrubutions.png?raw=true "Distrubutions of Numerical Variables")

Notice, for example, the long right tail on WindSpeed's distribution. Windspeed is mainly less than 20, but it seems to be taking every value between 20 and 32, albeit very rarely.

Also notice how much easier it is to see that much of the time the Windmill produces near-zero values or very high values. Combining this with what we saw on boxplots, pretty much 25% of the time it produces nothing, 25% it produces lots of power and 50% of the time it produces normal amounts of power.

* The "Numerical VS Dependent.png" shows how the Dependent variable behaves as each numeric variable gets higher values

###### _Note that here the Dependent variable is Continuous. For Categorical variable VS Categorical Dependent we get juxtaposed boxplots_

![Numerical VS Dependent.png](https://github.com/N1h1l1sT/DescriptiveStatsR/blob/master/Examples/Windmills/Numerical%20VS%20Dependent.png?raw=true "Numerical VS Dependent Variable")

This KWh-Windspeed plot releaves something unexpected; whilst in general, the stronger the wind, the higher the energy production, there is a zero KWh production for nearly any value of wind, however high it is. Another interesting thing unearthed is that even though the trend holds true for windspeeds up to about 25, we see that for really high WindSpeed values the production is always less than that of just high WindSpeed.

* Since this dataset is Time Series, we get "Time Progression for Numerical Variables.png" which shows how each variable behaves as a function of time

![Time Progression for Numerical Variables.png](https://github.com/N1h1l1sT/DescriptiveStatsR/blob/master/Examples/Windmills/Time%20Progression%20for%20Numerical%20Variables.png?raw=true "Time Progression for Numerical Variables")

One thing that immediately becomes clear viewing this plot is that Reactive Power's behaviour changed after February

Notice how the transparent grey lines make it easy to see when there are great differences between each point in time. If the rise or fall of values is smooth, no grey line appears, like in: 1, 5, 10, 20, 15, 7, 4. If there is missing time as well, we'll see this coloured points before, coloured points after, and in the empty middle there'll be a grey line highlighting the missing pieces.

___

# Reproducible Examples:
On all the example below, all statistics will be saved in a folder under your Working Directory, which usually is your project's folder.
## 1: 'storms' dataset from dplyr
```r
#Load the Dataset
data("storms")
StormsDS <-
  storms %>% #The "storms" DataFrame is part of dplyr, so it always exists even if we don't see it
  as_tibble() %>%
  mutate(
    status = as.factor(status), #There are only 3 unique statuses in over 10,000 rows, so this is a factor variable, not a text one
    Date = ymd_h(paste0(year, "-", month, "-", day, " ", hour))
  ) %>%
  select(-c(year, month, day, hour)) %>% #Since these 4 variables are part of Date, we should remove them
  {.[, c("Date", setdiff(names(.), "Date"))]} #This line JUST makes "Date" be the 1st variable, no reason other than I like having Date variables 1st.
  
print(StormsDS)

#Run the Descriptive Statistics
DescrStats <-
  StormsDS %>%
  DescriptiveStats(
    CalculateGraphs = TRUE,
    IsTimeSeries = TRUE,
    CorrVarOrder = "PCA",
    GroupBy = "category",
    DependentVar = "wind",
    TimeFlowVar = "Date",
    BoxplotPointsColourVar = "pressure",
    BoxPlotPointAlpha = 0.6,
    BoxPlotPointSize = 0.9
  )

#Save the Descriptive Statistics
DescrStats %>%
  SaveDescrStats(
    file.path(getwd(), "StormsDS/"),
    NumWidth = 1024,
    NumHeight = 768,
    CatWidth = 800,
    CatHeight = 600,
    TimeSeriesWidth = 1280,
    TimeSeriesHeight = 800
  )
```

## 2: 'mpg' dataset from ggplot2
```r
#Load the Dataset
data("mpg")
MpgDS <-
  mpg %>% #The "mpg" DataFrame is part of ggplot2, we can also call it as ggplot2::mpg
  as_tibble() %>%
  mutate(
    Date = ymd(paste0(year, "-01-01")), #Only 2 years
    manufacturer = as.factor(manufacturer),
    trans = as.factor(trans),
    drv = factor(drv, levels = c("f", "r", "4"), labels = c("Front wheel", "Read wheel", "Four wheel")),
    fl = factor(fl, levels = c("e", "d", "r", "p", "c"), labels = c("Ethanol", "Diesel", "Regular", "Premium", "Natural gas")),
    class = as.factor(class)
  ) %>%
  select(-c(year)) %>%
  rename(
    TransmissionType = trans,
    DriveType = drv,
    CityMilesPerGallon = cty,
    HighwayMilesPerGallon = hwy,
    FuelType = fl,
    CylindersNum = cyl,
    EngineDisplacement = displ
    ) %>%
  {.[, c("Date", setdiff(names(.), "Date"))]} #This line JUST make "Date" be the 1st variable, no reason other than I like having Date variables 1st.

print(MpgDS)

#Run the Descriptive Statistics
DescrStats <-
  MpgDS %>%
  DescriptiveStats(
    CalculateGraphs = TRUE,
    CorrVarOrder = "PCA",
    GroupBy = "DriveType",
    DependentVar = "class",
    BoxplotPointsColourVar = "CityMilesPerGallon",
    BoxPlotPointAlpha = 0.7,
    BoxPlotPointSize = 1
  )


#Save the Descriptive Statistics
DescrStats %>%
  SaveDescrStats(
    file.path(getwd(), "MpgDS/"),
    NumWidth = 900,
    NumHeight = 600,
    CatWidth = 1680,
    CatHeight = 850
  )
```

## 3: 'diamonds' dataset from ggplot2
```r
#Load the Dataset
data("diamonds")
DiamondsDS <-
  diamonds %>% #The "diamonds" DataFrame is part of ggplot2, we can also call it as ggplot2::diamonds
  as_tibble()

print(DiamondsDS)

#Run the Descriptive Statistics
DescrStats <-
  DiamondsDS %>%
  DescriptiveStats(
    CalculateGraphs = TRUE,
    CorrVarOrder = "PCA",
    GroupBy = "cut",
    DependentVar = "price",
    BoxplotPointsColourVar = "carat",
    BoxPlotPointAlpha = 0.3,
    BoxPlotPointSize = 0.5
  )


#Save the Descriptive Statistics
DescrStats %>%
  SaveDescrStats(
    file.path(getwd(), "DiamondsDS/"),
    NumWidth = 1000,
    NumHeight = 700,
    CatWidth = 850,
    CatHeight = 550
  )
```

## 4: 'Titanic' dataset from Datasets
```r
#Load the Dataset
data("Titanic")
TitanicDS <-
  Titanic %>% #The "Titanic" DataFrame is part of the datasets library, we can also call it as datasets::Titanic
  as_tibble() %>%
  mutate(
    Class = as.factor(Class),
    Sex = as.factor(Sex),
    Age = as.factor(Age),
    Survived = as.factor(Survived)
  ) %>%
  uncount(n)

print(TitanicDS)

#Run the Descriptive Statistics
DescrStats <-
  TitanicDS %>%
  DescriptiveStats(
    CalculateGraphs = TRUE,
    CorrVarOrder = "PCA",
    GroupBy = "Sex",
    DependentVar = "Survived"
  )

#Save the Descriptive Statistics
DescrStats %>%
  SaveDescrStats(
    file.path(getwd(), "TitanicDS/"),
    NumWidth = 1000,
    NumHeight = 700,
    CatWidth = 850,
    CatHeight = 550
  )
```

## 5: 'CO2' dataset from Datasets
```r
#Load the Dataset
data("CO2")
Co2DS <-
  CO2 %>% #The "CO2" DataFrame is part of the datasets library, we can also call it as datasets::CO2
  as_tibble()

print(Co2DS)

#Run the Descriptive Statistics
DescrStats <-
  Co2DS %>%
  DescriptiveStats(
    CalculateGraphs = TRUE,
    GroupBy = "Type",
    DependentVar = "uptake",
    BoxplotPointsColourVar = "conc",
    BoxPlotPointAlpha = 1,
    BoxPlotPointSize = 1
  )


#Save the Descriptive Statistics
DescrStats %>%
  SaveDescrStats(
    file.path(getwd(), "CO2DS/"),
    NumWidth = 1000,
    NumHeight = 700,
    CatWidth = 850,
    CatHeight = 550
  )
```
