################################################
##### Replication: Thompson for Ross (Oil) #####
#####        22 NOV - 06 DEC 2016          #####
#####  888C Comparative Political Economy  #####
################################################

# data downloaded 22 NOV 2016 from 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/17976
if(T){
# Under the Hood
      if(T){
# Preamble
if(T){
rm(list=ls())
cat("\014")
# Install Packages
if(F){
  install.packages("pacman", repos='http://cran.us.r-project.org')
  pacman::p_load("foreign", "stargazer", "dplyr", "ggplot2", "devtools",
                 "obsval","mvtnorm","arm","vioplot","lmtest","knitr","readstata13",
                 "moments","MASS","XLConnect","xlsx","plm","apsrtable","repmis","gridExtra")
}
# Load Libraries
if(T){
library(foreign)      # Load Stata, .csv, .tbl
library(stargazer)    # Nice LaTeX Table Outputs
library(dplyr)        # data management
library(ggplot2)      # nice graphs and plots
library(devtools)     #
library(obsval)       #
library(mvtnorm)      #
library(arm)          #
library(vioplot)      # violin plots
library(lmtest)       #
library(knitr)        # knit files
library(readstata13)  # read Stata 2013
library(moments)      #
library(MASS)         #
library(XLConnect)    #
library(xlsx)         # write .xlsx file
library(plm)          # create time series fixed effects models
library(apsrtable)    # Nice LaTeX Table Outputs
library(repmis)       # this is needed to read the .RData file from GitHub
library(gridExtra)    # this is the package Joel uses for putting two plots together
library(lmtest)       # this is the package Joel uses for the function he wrote
library(sandwich)     # this is the package Joel uses for the function he wrote
  }
# Load Data
if(T){
setwd("~/Documents/GitHubRepo/2016F_888C_Final-Replication/ross")
#rossdata01 <- read.table(file = "Replication data for The Oil Curse - Ross 2012.tab", header = TRUE, sep = "\t")
#save(rossdata01, file = "rossdata01.RData")
source_data("https://github.com/piton3853/2016F_888C_Final-Replication/blob/master/rossdata01.RData?raw=true")
#write.xlsx(rossdata01, "~/Documents/GitHubRepo/2016F_888C_Final-Replication/ross/Replication data for The Oil Curse - Ross 2012.xlsx")
#View(rossdata01)
}
}
            }


################################################
################################################
################################################

# Completed Scripts :: Set to 'F' for expedience; set back to 'T' if you want the outputs
      if(F){
# Table 4.1 Female Fraction of the Labor Force, 2002
if(T){
df_table_4.1.1 <- rossdata01 %>%
  filter(gdpcap2000_sup < 5000) %>% # By Income:  Low Income
  filter(year == 2002) 

model_4.1.1 <- lm(data=df_table_4.1.1, laborfemale ~ oil_gas_value100)

df_table_4.1.2 <- rossdata01 %>%
  filter(gdpcap2000_sup > 5000) %>% # By Income:  High Income
  filter(year == 2002)

model_4.1.2 <- lm(data=df_table_4.1.2, laborfemale ~ oil_gas_value100)

df_table_4.1.3 <- rossdata01 %>%
  filter(me_nafr > .5) %>% # Region:  Middle East and N. Africa
  filter(year == 2002)

model_4.1.3 <- lm(data=df_table_4.1.3, laborfemale ~ oil_gas_value100)

df_table_4.1.4 <- rossdata01 %>%
  filter(me_nafr < .5) %>% # Region:  All Other Countries
  filter(oecd < .5) %>% # Region:  All Other countries
  filter(year == 2002)

model_4.1.4 <- lm(data=df_table_4.1.4, laborfemale ~ oil_gas_value100)

df_table_4.1.5 <- rossdata01 %>%
  filter(me_nafr < .5) %>%
  filter(oecd < .5) %>%
  filter(year == 2002)

model_4.1.5 <- lm(data=df_table_4.1.5, laborfemale ~ oil_gas_value100)

df_table_4.1.6 <- rossdata01 %>%
  filter(year == 2002)

model_4.1.6 <- lm(data=df_table_4.1.6, laborfemale ~ oil_gas_value100)

stargazer(model_4.1.1,model_4.1.2,model_4.1.3, model_4.1.4, model_4.1.5, model_4.1.6, type = 'text',
          title = "Female Labor Force Participation, 2002",
          column.labels = c("Low Income","High Income","Mid East/N. Af.",
                               "All others", "Other developing countries","All States"),
          covariate.labels = c("Oil Producers","Non-oil Producers"),
          dep.var.labels = "Likelihood of Female Participation",
          dep.var.caption = "Female Labor Force Participation, 2002")
}

# Table 4.2 Textil and Clothing exports, 2002
if(T){
  df_table_4.2.1 <- rossdata01 %>%
    filter(gdpcap2000_sup < 5000) %>% # By Income:  Low Income
    filter(year == 2002) 
  
  model_4.2.1 <- lm(data=df_table_4.2.1, cloth_text2000_cap ~ oil_gas_value100)
  
  df_table_4.2.2 <- rossdata01 %>%
    filter(gdpcap2000_sup > 5000) %>% # By Income:  High Income
    filter(year == 2002) 
  
  model_4.2.2 <- lm(data=df_table_4.2.2, cloth_text2000_cap ~ oil_gas_value100)
  
  df_table_4.2.3 <- rossdata01 %>%
    filter(year == 2002)
  
  model_4.2.3 <- lm(data=df_table_4.2.3, cloth_text2000_cap ~ oil_gas_value100)
  
  stargazer(model_4.2.1,model_4.2.2,model_4.2.3, type = 'text',
            title = "Textile and clothing exports, 2002",
            column.labels = c("Low Income","High Income","All States"),
            covariate.labels = c("Oil Producers","Non-oil Producers"),
            dep.var.caption = "Textile and clothing exports",
            dep.var.labels = "Value of Textile and Clothing Exports per capita"
            )
}

# Table 4.3 Parliamentary Seats, 2002
if(T){
  df_table_4.3.1 <- rossdata01 %>%
    filter(gdpcap2000_sup < 5000) %>% # By Income:  Low Income
    filter(year == 2002) 
  
  model_4.3.1 <- lm(data=df_table_4.3.1, female_seats ~ oil_gas_value100)
  
  df_table_4.3.2 <- rossdata01 %>%
    filter(gdpcap2000_sup > 5000) %>% # By Income:  High Income
    filter(year == 2002)
  
  model_4.3.2 <- lm(data=df_table_4.3.2, female_seats ~ oil_gas_value100)
  
  df_table_4.3.3 <- rossdata01 %>%
    filter(me_nafr > .5) %>% # Region:  Middle East and N. Africa
    filter(year == 2002)
  
  model_4.3.3 <- lm(data=df_table_4.3.3, female_seats ~ oil_gas_value100)
  
  df_table_4.3.4 <- rossdata01 %>%
    filter(me_nafr < .5) %>% # Region:  All Other Countries
    filter(oecd < .5) %>% # Region:  All Other countries
    filter(year == 2002)
  
  model_4.3.4 <- lm(data=df_table_4.3.4, female_seats ~ oil_gas_value100)
  
  df_table_4.3.5 <- rossdata01 %>%
    filter(me_nafr < .5) %>%
    filter(oecd < .5) %>%
    filter(year == 2002)
  
  model_4.3.5 <- lm(data=df_table_4.3.5, female_seats ~ oil_gas_value100)
  
  df_table_4.3.6 <- rossdata01 %>%
    filter(year == 2002)
  
  model_4.3.6 <- lm(data=df_table_4.3.6, female_seats ~ oil_gas_value100)
  
  stargazer(model_4.3.1,model_4.3.2,model_4.3.3, model_4.3.4, model_4.3.5, model_4.3.6, type = 'text',
            title = "Parliamentary seats held by women, 2002",
            column.labels = c("Low Income","High Income","Mid East/N. Af.",
                              "All others", "Other developing countries","All States"),
            covariate.labels = c("Oil Producers","Non-oil Producers"),
            dep.var.caption = "Percentage of female held lower house parlimentary seats, 2002",
            dep.var.labels = "Likelihood of Female Parlimentary Seat"
            )
}
                                                    }
  
# Table 4.5
if(T){
  df_table_4.5.1 <- rossdata01 %>%
    dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1) %>%
    filter(!is.na(cty)) %>%
    filter(!is.na(year)) %>%
    filter(!is.na(FDlaborfemale)) %>%
    filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
    filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
    filter(!is.na(FDage_1)) %>%
    filter(!is.na(FDoil_gas_valuePOPred_1))
  
  model_4.5.1a <- plm(diff(FDlaborfemale) ~ diff(FDlogGDP_cap2000_sup_1) + diff(FDlogGDP_cap2000_supSQ_1) + diff(FDage_1), data = df_table_4.5.1,index=c("cty","year"),model = "within"); summary(model_4.5.1a)
  model_4.5.1b <- plm(FDlaborfemale ~ FDlogGDP_cap2000_sup_1 + FDlogGDP_cap2000_supSQ_1 + FDage_1, data = df_table_4.5.1,index=c("cty","year"),model = "random"); summary(model_4.5.1b)
  model_4.5.1c <- lm(FDlaborfemale ~ FDlogGDP_cap2000_sup_1 + FDlogGDP_cap2000_supSQ_1 + FDage_1 + factor(cty) -1, data = df_table_4.5.1); summary(model_4.5.1c)
  model_4.5.1d <- plm(FDlaborfemale ~ FDlogGDP_cap2000_sup_1 + 
                        FDlogGDP_cap2000_supSQ_1 + FDage_1 + 
                        FDoil_gas_valuePOPred_1, 
                      data = df_table_4.5.1,index=c("cty","year"),model = "within", effect = "individual"); summary(model_4.5.1d)
  
  
  # take the SA idea and generalize it.  I ask, is oil more important than culture generally.  Is oil more important that
  # beliefs about what women should be doing in general.  SA makes a lot of sense.  Subset the oil countries of SA.  
  # The SA countries should look different, if Ross idea is true.  This should just tell us if oil has effects.  Motivated
  # by the broader question.  There will be countries during this data that they had oil booms.  Brazil had an oil boom.
  # Brazil found a bunch of off shore oil in 2003-2004.  If Ross is right, we can look at only Brazil -- natural experiement.
  # Is there something unique about Brazil?  Find something similar in Mexico and Venezuela.  Do the broad regional study
  # of only SA, see if the results hold.  Do one more small analytic case study.  Do women's labor force participation 
  # move during the booms and busts.
  # don't go too far down the path of figuring out culture.  Don't spend time doing that.  Look at booms and busts and see
  # if women are entering and exiting the workforce.  
  # How to Replicate that result.  Get in the ballpark.  
  # type of government:
  # GDP growth vs. GDPPC
  stargazer(model_4.5.1a,model_4.5.1b,model_4.5.1c,type = 'text',
            title = "Female labor force participation, 1960-2002",
            covariate.labels = c("Income (log)","Income (log) squared","Working age"),
            dep.var.caption = "Annual change in female labor force participation",
            dep.var.labels = "controls")
  phtest(model_4.5.1a,model_4.5.1b) # 
}

# Ross's Stata Code:
# *Column 1: just the controls
# xtregar FDlaborfemale FDlogGDP_cap2000_sup_1 FDlogGDP_cap2000_supSQ_1 FDage_1, fe

# *Column 6: drop all Middle East & North Africa
# xtregar FDlaborfemale FDlogGDP_cap2000_sup_1 FDlogGDP_cap2000_supSQ_1 FDage_1 FDoil_gas_valuePOPred_1 if me_nafr==0, fe

require(lmtest)
require(sandwich)

PCSEs <- function (x) {
  summary(x, vcovBK(x, type = "HC1", cluter = "time"))
}

# This is the DF that I wrote during office house.  This DF provides me with the Latin America Data exclusively.
# This is the way forward.
df_table_4.5.9 <- rossdata01 %>%
  dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
  filter(latin==1) %>%
  filter(!is.na(cty)) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(FDlaborfemale)) %>%
  filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
  filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
  filter(!is.na(FDage_1)) %>%
  filter(!is.na(FDoil_gas_valuePOPred_1))

model_4.5.1a <- plm(diff(FDlaborfemale) ~ diff(FDlogGDP_cap2000_sup_1) + diff(FDlogGDP_cap2000_supSQ_1) + diff(FDage_1), data = df_table_4.5.1,index=c("cty","year"),model = "within"); summary(model_4.5.1a)

# One thing to do is draw a bunch of graphs to see if they are moving the way they should.  Are the trends moving together
# if oil goes up then labor force participation should diverge.

plot1 <- ggplot2::ggplot(data = subset(rossdata01, cty == "Brazil"), aes(x = year, y = FDlaborfemale)) + 
  geom_line() + 
  geom_smooth(se = FALSE) + 
  geom_point()

plot2 <- ggplot2::ggplot(data = subset(rossdata01, cty == "Brazil"), aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
  geom_line() + 
  geom_smooth(se = FALSE) + 
  geom_point()

gridExtra::grid.arrange(plot1, plot2)

# End Bracket to run entire script at one time
}
