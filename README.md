# info-498D-project

## Introduction:

There has been a raging debate about how the dominance of sugar and sugary beverages are contributing to the growth of these epidemics. Sugary drinks such as soft drinks, energy drinks, sweetened teas, and sport drinks offer little or no nutritional value, but massive quantities of added sugar. A single 12- ounce can of soda typically contains 40 g of sugar on an average (Source: https://www.caffeineinformer.com/sugar-in-drinks) and a study by the American Heart Association tells consumers to have no more than 6 teaspoons or 24 grams (women) or 9 teaspoons or 36 grams (men) of refined sugars each day (Source : http://newsroom.heart.org/news/sugar-sweetened-drinks-linked-to-increased-visceral-fat ). As per the findings from Berkeley, CA, this issue is especially acute in children in this region with the percentage of children consuming sugary drinks increasing from 79% to 91% and the percentage of total calories obtained from sugary drinks increasing by 60% in children ages 6 to 11 from from 1989 to 2008. Further, quantitatively, the tangible economic costs are also massive. In 2006, for instance, overweight and obesity-related costs in California were estimated at almost $21 billion.(Source: https://www.cityofberkeley.info/uploadedFiles/Clerk/Elections/Sugar%20Sweeetened%20Beverage%20Tax%20%20-%20Full%20Text.pdf). Looking at these alarming statistics, in November 2014, Berkeley became the first in the United States to pass a tax on the distribution of sugar-sweetened beverages which would be effective from 1st January, 2015 and would continue till 31st December 2026. The ordinance attempts to levy an Excise Tax of one cent ($0.01) per fluid ounce on the distribution of sugar-sweetened beverage products in the City. 

The purpose of our research project is to identify the effects of the introduction of the tax on Sugar Sweetened Beverages as a health intervention. More specifically, how this tax has affected various demographics and whether the revenue generated might actually be invested efficiently to fund awareness programs. 

## Existing Research:

There already are numerous studies that have been conducted which have analysed the effect of this tax, pre and post intervention such as :
Impact of the Berkeley Excise Tax on Sugar-Sweetened Beverage Consumption [Jennifer Falbe, ScD, MPH, Hannah R. Thompson, PhD, MPH, Christina M. Becker, BA, Nadia Rojas, MPH, Charles E. McCulloch, PhD, and Kristine A. Madsen, MD, MPH]

Changes in prices, sales, consumer spending, and beverage consumption one year after a tax on sugar-sweetened beverages in Berkeley, California, US: A before-and-after study
[Lynn D. Silver , Shu Wen Ng , Suzanne Ryan-Ibarra, Lindsey Smith Taillie, Marta Induni, Donna R. Miles, Jennifer M. Poti, Barry M. Popkin]

There is other research on the efficacy of such taxation on public health :
Cost Effectiveness of a Sugar-Sweetened Beverage Excise Tax in the U.S.
[Michael W. Long, ScD, Steven L. Gortmaker, PhD, Zachary J. Ward, MPH, Stephen C. Resch, PhD, Marj L. Moodie, DrPH, Gary Sacks, PhD, Boyd A. Swinburn, MD, Rob C. Carter, PhD, Y. Claire Wang, MD, ScD]

Sugar-Sweetened Beverages, Obesity, Type 2 Diabetes Mellitus, and Cardiovascular Disease Risk
[Vasanti S. Malik, MSc; Barry M. Popkin, PhD; George A. Bray, MD; Jean-Pierre Despre´s, PhD; Frank B. Hu, MD, PhD]

The Impact of a Tax on Sugar-Sweetened Beverages on Health and Health Care Costs: A Modelling Study
[J. Lennert Veerman, Gary Sacks, Nicole Antonopoulos, Jane Martin]

Evidence that a tax on sugar sweetened beverages reduces the obesity rate: a meta-analysis
[Maria A Cabrera Escobar, J Lennert Veerman, Stephen M Tollman, Melanie Y Bertram, Karen J Hofman]

## Data:
We'll be using two datasets for this study. 
Our primary dataset is the data collected jointly by the Public Health Institute, Oakland CA and University of North Carolina as a part of Global Food Research Program. The data for Dietary and Shopping Behavior was collected by the means of telephonic interviews, conducted before and after the Sugar Tax was implemented. The data can be obtained from here. http://globalfoodresearchprogram.web.unc.edu/research-in-the-united-states/u-s-policy-evaluations/berkeley-ssb-tax/

The second is the BRFSS survey conducted in California, with data available at County Level. The data is available with California State University, Sacramento and we have sent in a request for the data at http://www.csus.edu/research/phsrp/brfss.html 

## Audience:
A number of cities across the United States are mooting implementing Sugar Tax, whereas Philadelphia, San Francisco, Portland, Oakland have already implemented the Sugar Tax following the steps of Berkeley. Recently, Seattle became the latest city in the United States to levy the Sugar Tax. 
In an attempt to push efforts to reduce the risk of Diabetes, Obesity, and other diseases associated with high sugar consumption such as heart disease and tooth decay, governments can look at the Sugar Tax as attempt to reduce the consumption of SSBs. The NIH pegs the national burden of diabetes in the United States at $245 Billion per year (1), while Obesity is pegged at $140-210 billion per year (2). Governments at a city, state, and even possibly national level would be very interested with the findings of this study, with an eye at implementing it in the future. 

(https://www.niddk.nih.gov/health-information/communication-programs/ndep/health-professionals/practice-transformation-physicians-health-care-teams/why-transform/current-burden-diabetes-us)
https://www.ncbi.nlm.nih.gov/pubmed/22094013


## Questions:
This research attempts to quantify the effect of this tax on a city wide basis pre and post intervention. However, as the ordinance documentation for the city of Berkeley for the SSB tax points out in their findings - "An Asian resident of Berkeley is almost 3 times more likely than a white resident to have been diagnosed with diabetes, and an African American resident of Berkeley is 14 times more likely than a white resident to be hospitalized for diabetes". They also make a claim that " If the current obesity trends are not reversed, it is predicted that one in three children and nearly one-half of Latino and African American children born in the year 2000 will develop type 2 diabetes in their lifetimes". 
This brought us to examine if there was an effect of this ordinance on specific economic and demographic sections in Berkeley. Through our analysis we aim to roughly touch upon the following questions :   

* Which economic sections of the society are at a risk of Diabetes/suffer from Diabetes? 

* What were the Sugar Sweetened Beverage Consumption patterns across economic strata in Berkeley before the tax was implemented? 

* Post tax, how did the SSB consumption change in Berkeley across different economic strata? 
 
## Technical Description:

The final product will be a slideshow compiled with KnitR. One of the datasets we will be using will be obtained from California State University, Sacramento and as part of the agreement with CSUS, we are bound to maintain utmost secrecy with the data and then destroy the data as soon as the project has been completed. 
Since neither of us have any experience in working with RShiny, one of our goals will also be to reproduce the same report in RShiny. Similarly, coming in with no experience of Interactive Visualization in R, we would also want to explore plotly and crosstalk in R to create interactive visualizations. 
As we are using two different datasets, we will have to be very careful before before drawing conclusions, as the data has been collected from different sources. 


