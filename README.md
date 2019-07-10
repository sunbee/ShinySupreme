## Can Data Science predict a Supreme Court Justice's judgement?

Can Data Science can predict human responses, like whether the Supreme Court will vote to over-turn a Circuit Court's ruling? Prof. Andrew Martin from Washington University at St. Louis certainly thinks so! However, the answer is not that clear-cut.

The legal system of the United States operates at the state level and at the federal or country-wide level. The federal court is divided into three levels - district courts, circuit courts, and the Supreme Court. Cases start at the district courts, where an initial decision is made about the case. The circuit courts hear appeals from the district courts, and can change the decision that was made. The Supreme Court is the highest level in the American legal system and makes the final decision on cases. 

In March 2015, the International Agency for Research on Cancer, or IARC, issued a report labeling a popular herbicide as a “probable carcinogen". Modern agriculture depends on a variety of chemical compounds for plant health and nutrition. The controversial report triggered lawsuits. Do you think this is a case for the Supreme Court of the United States?

You can use the map viewer to examine the patterns of usage of different chemistries in the US states based on data published by the United States Geological Survey. Click on the tab to compare and contrast geospatial patterns in the usage of chemistries. Then, use the tab to apply three common machine learning algorithms based on Andrew Martin's work. 

Andrew Martin's wiki page
An Introduction to Empirical Legal Research by Andrew Martin
https://fivethirtyeight.com/features/why-the-best-supreme-court-predictor-in-the-world-is-some-random-guy-in-queens/
https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/# SuperZIP demo

See a version of it live at http://shiny.rstudio.com/gallery/superzip-example.html

You can run this demo with:
```
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("rstudio/leaflet")
shiny::runGitHub("rstudio/shiny-examples", subdir="063-superzip-example")
```

Data compiled for _Coming Apart: The State of White America, 1960–2010_ by Charles Murray (Crown Forum, 2012). This app was inspired by the Washington Post's interactive feature _[Washington: A world apart](http://www.washingtonpost.com/sf/local/2013/11/09/washington-a-world-apart/)_.
