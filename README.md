# Can Data Science predict a supreme court justice's judgement?

## UNDER CONSTRUCTION ##

Can Data Science can predict human responses, like whether the Supreme Court will vote to over-turn a Circuit Court's ruling? Prof. Andrew Martin from Washington University at St. Louis certainly thinks so! However, the answer is not that clear-cut.

The legal system of the United States operates at the state level and at the federal or country-wide level. The federal court is divided into three levels - district courts, circuit courts, and the Supreme Court. Cases start at the district courts, where an initial decision is made about the case. The circuit courts hear appeals from the district courts, and can change the decision that was made. The Supreme Court is the highest level in the American legal system and makes the final decision on cases. 

Andrew picked .. (about the time period and why it mattered.) Using this data, he (feature selection.) Then he (his methods.) How accurate were these models? (His claims.)

Not everyone is convinced, however. The technique Andrew used, CART, reduces a lifetime's expertise to a decision tree that fits on an A4-sized paper. (Ask the average Joe, according to 584.) Andrew's work remains controversial. How about applying to a test case?

In March 2015, the International Agency for Research on Cancer, or IARC, issued a report labeling a popular herbicide as a “probable carcinogen". Modern agriculture depends on a variety of chemical compounds for plant health and nutrition. The controversial report triggered lawsuits. Do you think this is a case for the Supreme Court of the United States? 

I built an app to explore. Use the map viewer to compare and contrast usage of different chemistries in the US states based on data published by the United States Geological Survey (2012). Then, click on the tab to explore three different machine learning models based on Andrew's work. It is especially interesting to explore sensitivity of the outcome to an influential variable and compare that with one's intuition.

## References:
- Andrew Martin's _[wiki page](https://en.wikipedia.org/wiki/Andrew_D._Martin)_
- "An Introduction to Empirical Legal Research" by Andrew Martin
- A take on Andrew Martin's work by _[FiveThirtyEight](https://fivethirtyeight.com/features/why-the-best-supreme-court-predictor-in-the-world-is-some-random-guy-in-queens/)_
- Data on US herbicide usage by the _[USGS](https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/#)_
- Based on a Shiny app live at http://shiny.rstudio.com/gallery/superzip-example.html

You can run this demo with:
```
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("rstudio/leaflet")
shiny::runGitHub("rstudio/shiny-examples", subdir="063-superzip-example")
```

Data compiled for _Coming Apart: The State of White America, 1960–2010_ by Charles Murray (Crown Forum, 2012). This app was inspired by the Washington Post's interactive feature _[Washington: A world apart](http://www.washingtonpost.com/sf/local/2013/11/09/washington-a-world-apart/)_.
