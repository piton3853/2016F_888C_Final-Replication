################################################
##### Replication: Thompson for Ross (Oil) #####
#####        22 NOV - 06 DEC 2016          #####
#####  888C Comparative Political Economy  #####
################################################

# data downloaded 22 NOV 2016 from 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/17976
if(T){ # Entire Script - DO NOT COLLAPSE Normally
## 0
# Under the Hood (start fresh)
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
library(apsrtable)    # Nice LaTeX Table Outputs
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
library(repmis)       # this is needed to read the .RData file from GitHub
library(gridExtra)    # this is the package Joel uses for putting two plots together
library(lmtest)       # this is the package Joel uses for the function he wrote
library(sandwich)     # this is the package Joel uses for the function he wrote
library(mgcv)
  }
# Load Data
if(T){
setwd("~/Documents/GitHubRepo/2016F_888C_Final-Replication/ross")
#rossdata01 <- read.table(file = "Replication data for The Oil Curse - Ross 2012.tab", header = TRUE, sep = "\t")
#save(rossdata01, file = "rossdata01.RData")
source_data("https://github.com/piton3853/2016F_888C_Final-Replication/raw/master/ross/rossdata01.RData")
#write.xlsx(rossdata01, "~/Documents/GitHubRepo/2016F_888C_Final-Replication/ross/Replication data for The Oil Curse - Ross 2012.xlsx")
#View(rossdata01)
}
      }  # This originates at '# Preamble
# Functions
if(T){
  PCSEs <- function (x) {
          summary(x, vcovBK(x, type = "HC1", cluter = "time"))   # Simmons' function, used to make the standard errors more accurate
        }
  ggplotRegression <- function (fit) {
    
    require(ggplot2)
    
    ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
      geom_point() +
      stat_smooth(method = "lm", col = "blue") +
      labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                         "Intercept =",signif(fit$coef[[1]],5 ),
                         " Slope =",signif(fit$coef[[2]], 5),
                         " P =",signif(summary(fit)$coef[2,4], 5)))
  }
} # This originates at '# Functions'
      } # This originates at '# Under the Hood'

##########################################
###            Table 4.1 & 4.5         ###
##########################################
if(F){
## 4.1a
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
  filter(me_nafr < .5) %>% # Region:  All Others
  filter(year == 2002)

model_4.1.4 <- lm(data=df_table_4.1.4, laborfemale ~ oil_gas_value100); summary(model_4.1.4)

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
                               "All Others", "Other developing countries","All States"),
          covariate.labels = c("Oil Producers","Non-oil Producers"),
          dep.var.labels = "Likelihood of Female Participation",
          dep.var.caption = "Female Labor Force Participation, 2002")
low_income <- model_4.1.1$coefficients[1]+model_4.1.1$coefficients[2]; low_income
hi_income <- model_4.1.2$coefficients[1]+model_4.1.2$coefficients[2];hi_income
menaf <- model_4.1.3$coefficients[1]+model_4.1.3$coefficients[2];menaf
all_others <- model_4.1.4$coefficients[1]+model_4.1.4$coefficients[2];all_others
others <- model_4.1.5$coefficients[1]+model_4.1.5$coefficients[2];others
all_states <- model_4.1.6$coefficients[1]+model_4.1.6$coefficients[2];all_states
list(low_income,hi_income,menaf,all_others,others,all_states)
}
  
## 4.1b
# Table 4.1a (Nick Edit) - Cut "All Others" and replaced with "Latin America" Female Fraction of the Labor Force, 2002
if(T){
    df_table_4.1.1b <- rossdata01 %>%
      filter(gdpcap2000_sup < 5000) %>% # By Income:  Low Income
      filter(year == 2002) 
    
    model_4.1.1b <- lm(data=df_table_4.1.1b, laborfemale ~ oil_gas_value100); model_4.1.1b
    
    df_table_4.1.2b <- rossdata01 %>%
      filter(gdpcap2000_sup > 5000) %>% # By Income:  High Income
      filter(year == 2002)
    
    model_4.1.2b <- lm(data=df_table_4.1.2b, laborfemale ~ oil_gas_value100); model_4.1.2b
    
    df_table_4.1.3b <- rossdata01 %>%
      filter(me_nafr > .5) %>% # Region:  Middle East and N. Africa
      filter(year == 2002)
    
    model_4.1.3b <- lm(data=df_table_4.1.3b, laborfemale ~ oil_gas_value100); model_4.1.3b
    
    df_table_4.1.4b <- rossdata01 %>%
      filter(latin > .5) %>% # Region:  Latin America
      filter(year == 2002)
    
    model_4.1.4b <- lm(data=df_table_4.1.4b, laborfemale ~ oil_gas_value100); summary(model_4.1.4b)

    df_table_4.1.5b <- rossdata01 %>%
      filter(me_nafr < .5) %>%
      filter(oecd < .5) %>%
      filter(year == 2002)
    
    model_4.1.5b <- lm(data=df_table_4.1.5, laborfemale ~ oil_gas_value100)
    
    df_table_4.1.6b <- rossdata01 %>%
      filter(year == 2002)
    
    model_4.1.6b <- lm(data=df_table_4.1.6, laborfemale ~ oil_gas_value100)
    
    stargazer(model_4.1.3b, model_4.1.4b, type = 'text',
              title = "Female Labor Force Participation, 2002",
              column.labels = c("Mid East/N. Af.",
                                "Latin America"),
              covariate.labels = c("Oil Producers","Non-oil Producers"),
              dep.var.labels = "Likelihood of Female Participation",
              dep.var.caption = "Female Labor Force Participation, 2002")
    
    
    menaf2 <- model_4.1.3b$coefficients[1]+model_4.1.3$coefficients[2];menaf2
    la <- model_4.1.4b$coefficients[2]+model_4.1.4b$coefficients[1];la
    
  }
  

## 4.2
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

## 4.3
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

## 4.4
# Table 4.5.1, 4.5.4
if(T){
  df_table_4.5.1 <- rossdata01 %>%
    dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1) %>%
    filter(!is.na(cty)) %>%
    filter(!is.na(year)) %>%
    filter(!is.na(FDlaborfemale)) %>%
    filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
    filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
    filter(!is.na(FDage_1)) %>%
    filter(!is.na(FDoil_gas_valuePOPred_1)) %>%

  
  model_4.5.1a <- plm(diff(FDlaborfemale) ~ diff(FDlogGDP_cap2000_sup_1) + diff(FDlogGDP_cap2000_supSQ_1) + diff(FDage_1), data = df_table_4.5.1,index=c("cty","year"),model = "within"); #summary(model_4.5.1a)
  m_4.5.1a <- PCSEs(model_4.5.1a); m_4.5.1a
  #model_4.5.1b <- plm(FDlaborfemale ~ FDlogGDP_cap2000_sup_1 + FDlogGDP_cap2000_supSQ_1 + FDage_1, data = df_table_4.5.1,index=c("cty","year"),model = "random"); #summary(model_4.5.1b)
  #m_4.5.1b <- PCSEs(model_4.5.1b)
  #model_4.5.1c <- lm(FDlaborfemale ~ FDlogGDP_cap2000_sup_1 + FDlogGDP_cap2000_supSQ_1 + FDage_1 + factor(cty) -1, data = df_table_4.5.1); #summary(model_4.5.1c)
  #m_4.5.1c <- PCSEs(model_4.5.1c)
  model_4.5.4a <- plm(diff(FDlaborfemale) ~ diff(FDlogGDP_cap2000_sup_1) + 
                        diff(FDlogGDP_cap2000_supSQ_1) + diff(FDage_1) + 
                        diff(FDoil_gas_valuePOPred_1), 
                      data = df_table_4.5.1,index=c("cty","year"),model = "within", effect = "individual"); #summary(model_4.5.1d)
  m_4.5.4a <- PCSEs(model_4.5.4a); m_4.5.4a

  stargazer(model_4.5.1a,model_4.5.4a,type = 'text',
            title = "Female labor force participation, 1960-2002",
            covariate.labels = c("Income (log)","Income (log) squared","Working age","Oil Income"),
            dep.var.caption = "Annual change in female labor force participation",
            dep.var.labels = "controls")
}  # This originates at: ## 4
} # Collapses Tables 4.1 - 4.5


#########################################
###           The Way Forward         ###
#########################################
# Graph Latin America; This is the way forward (##6 - ##10).
## 6. Top 6 Latin American oil producers, according to Investopedia 12/03/2016:  
  # http://www.investopedia.com/articles/investing/101315/biggest-oil-producers-latin-america.asp
  # specific data taken from UTDallas.edu on 12/03/2016; https://www.utdallas.edu/~pujana/latin/PDFS/Lecture%2012-%20LAoil.pdf

# GENERAL LATIN AMERICA
if(T){
  df_table_4.5.LA <- rossdata01 %>%
    dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                  FDlogGDP_cap2000_supSQ_1,FDage_1,
                  FDoil_gas_valuePOPred_1,latin) %>%
    filter(latin==1) %>%
    filter(!is.na(cty)) %>%
    filter(!is.na(year)) %>%
    filter(!is.na(FDlaborfemale)) %>%
    filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
    filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
    filter(!is.na(FDage_1)) %>%
    filter(!is.na(FDoil_gas_valuePOPred_1))
  }

#############################
###       Mexico (LT)     ###
#############################
if(T){ # Mexico ON / OFF Switch
# 1. Mexico Dataframe (General) (Pemex, monopoloy)
  if(T){
      df_table_4.5.Mexico_general <- rossdata01 %>%
      dplyr::select(cty,year,latin,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                  FDlogGDP_cap2000_supSQ_1,FDage_1,
                  FDoil_gas_valuePOPred_1) %>%
      filter(latin==1) %>%
      filter(cty=="Mexico") %>%
      filter(!is.na(year)) %>%
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
  }
# 2. Mexico Dataframe (Discrete)
  if(T){
  # Mexico (discrete) Dataframe from 1985 - 1995
  #### Look at BOOM:: `72 - `75; `85 - `92; `90 - `00
  #### Look at BUST:: `75 - `85; `96 - `03
  # BOOM `72 - `75 (discrete01)
  if(T){
    df_table_4.5.Mexico_discrete01 <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                    FDlogGDP_cap2000_supSQ_1,FDage_1,
                    FDoil_gas_valuePOPred_1,latin) %>%
      filter(cty=="Mexico") %>%
      filter(year>=1972 & year<=1975) %>% 
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
  }
  # BOOM `85 - `92 (discrete02)
  if(T){
    df_table_4.5.Mexico_discrete02 <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                    FDlogGDP_cap2000_supSQ_1,FDage_1,
                    FDoil_gas_valuePOPred_1,latin) %>%
      filter(cty=="Mexico") %>%
      filter(year>=1985 & year<=1992) %>% 
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
  }
  # BOOM `92 - `96 (discrete03)
  if(T){
    df_table_4.5.Mexico_discrete03 <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                    FDlogGDP_cap2000_supSQ_1,FDage_1,
                    FDoil_gas_valuePOPred_1,latin) %>%
      filter(latin==1) %>%
      filter(cty=="Mexico") %>%
      filter(year>=1992 & year<=1996) %>% 
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
  }
  # BOOM `75 - `80 (discrete06)
  if(T){
    df_table_4.5.Mexico_discrete06 <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                    FDlogGDP_cap2000_supSQ_1,FDage_1,
                    FDoil_gas_valuePOPred_1,latin) %>%
      filter(cty=="Mexico") %>%
      filter(year>=1975 & year<=1980) %>% 
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
  }
  # BUST `80 - `85 (discrete04)
  if(T){
    df_table_4.5.Mexico_discrete04 <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                    FDlogGDP_cap2000_supSQ_1,FDage_1,
                    FDoil_gas_valuePOPred_1,latin) %>%
      filter(latin==1) %>%
      filter(cty=="Mexico") %>%
      filter(year>1980 & year<1986) %>% 
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
  }
  # BUST `01 - `03 (discrete05)
  if(T){
    df_table_4.5.Mexico_discrete05 <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                    FDlogGDP_cap2000_supSQ_1,FDage_1,
                    FDoil_gas_valuePOPred_1,latin) %>%
      filter(latin==1) %>%
      filter(cty=="Mexico") %>%
      filter(year>=2001 & year<=2004) %>% 
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
  }
}
# 3. Mexico Visualization
require(mgcv)
  # Mexico (General) No coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
      Mexico_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Mexico_general, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Mexico General Labor"); Mexico_general_plot1
      
      Mexico_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Mexico_general, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(se = F) + 
        geom_point() +
        ggtitle("Mexico General Oil"); Mexico_general_plot2
      
      gridExtra::grid.arrange(Mexico_general_plot1, Mexico_general_plot2)
    }  # This origiantes at Mexico (General) No coefficients Switch'
  # Mexico (General) Plot
  if(T){
    Mexico_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Mexico_general, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Mexico General Labor"); Mexico_general_plot1
    
    Mexico_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Mexico_general, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(se = F,method = "lm",formula = y ~ poly(x,10),size = 1) + 
      geom_point() +
      ggtitle("Mexico General Oil"); Mexico_general_plot2
    
    gridExtra::grid.arrange(Mexico_general_plot1, Mexico_general_plot2)
    
  } # This originates at # Mexico (General) Plot
  # Mexico (Discrete) Plot No Coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
        Mexico_discrete_plot1 <- ggplot2::ggplot(data = df_table_4.5.Mexico_discrete01, aes(x = year, y = FDlaborfemale)) + 
          geom_line() + 
          geom_smooth(se = FALSE) +
          geom_point() +
          ggtitle("Mexico Discrete Labor"); Mexico_discrete_plot1
        
        Mexico_discrete_plot2 <- ggplot2::ggplot(data = df_table_4.5.Mexico_discrete01, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
          geom_line() + 
          stat_smooth(method = "lm",se = FALSE) + 
          geom_point() +
          ggtitle("Mexico Discrete Oil"); Mexico_discrete_plot2
        
        gridExtra::grid.arrange(Mexico_discrete_plot1, Mexico_discrete_plot2)
      }  # This origiantes at # Mexico (Discrete) Plot No Coefficients Switch
} # Originates at Mexico ON / OFF Switch

##############################
###       Venezuela (LT)   ###
##############################
  if(T){ # Venezuela ON / OFF Switch
# 1. Venezuela Dataframe (General) (PdVSA nationalized, 1976)
  if(T){
      df_table_4.5.Venezuela_general <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                  FDlogGDP_cap2000_supSQ_1,FDage_1,
                  FDoil_gas_valuePOPred_1,latin) %>%
      filter(cty=="Venezuela, RB") %>%
      filter(!is.na(year)) %>%
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
}
# 2. Venezuela (discrete) Dataframe
  if(T){
  # Venezuela (discrete) dataframe from 1970 - 1983
  ### Look at 
      # 1970 - 1975 (boom) |
  # BOOM `70 - `75
  if(T){
    df_table_4.5.Venezuela_discrete <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
      filter(latin==1) %>%
      filter(cty=="Venezuela, RB") %>%
      filter(year>=1970 & year<=1975) %>% 
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
    head(df_table_4.5.Venezuela_discrete)
    tail(df_table_4.5.Venezuela_discrete)
  }
      # 1976 - 1980 (boom) |
  # BOOM `76 - `80
  if(T){
    df_table_4.5.Venezuela_discrete <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
      filter(latin==1) %>%
      filter(cty=="Venezuela, RB") %>%
      filter(year>=1976 & year=1980) %>% 
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
    head(df_table_4.5.Venezuela_discrete)
    tail(df_table_4.5.Venezuela_discrete)
  }
      # 1975 - 1979 (bust) |
  # BUST `75 - `79
  if(T){
    df_table_4.5.Venezuela_discrete <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
      filter(latin==1) %>%
      filter(cty=="Venezuela, RB") %>%
      filter(year>=1975 & year<=1979) %>% 
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
    head(df_table_4.5.Venezuela_discrete)
    tail(df_table_4.5.Venezuela_discrete)
  }
      # 1980 - 1983 (bust)
}
# 3. Venezuela Visualization
require(mgcv)
  # Venezuela (General) no coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
    Venezuela_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Venezuela, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Venezuela General Labor"); Venezuela_general_plot1
    
    Venezuela_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Venezuela, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(se = FALSE) + 
      geom_point() +
      ggtitle("Venezuela General Oil"); Venezuela_general_plot2
    
    gridExtra::grid.arrange(Venezuela_general_plot1, Venezuela_general_plot2)}
  # Venezuela (General) Plot
  if(T){
    Venezuela_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Venezuela_general, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Venezuela General Labor"); Venezuela_general_plot1
    
    Venezuela_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Venezuela_general, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(se = F,method = "lm",formula = y ~ poly(x,10),size = 1) + 
      geom_point() +
      ggtitle("Venezuela General Oil"); Venezuela_general_plot2
    
    gridExtra::grid.arrange(Venezuela_general_plot1, Venezuela_general_plot2)
    
  } # This originates at # 3. Venezuela (General) Plot
  # Venezuela (Discrete) Plot No Coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
      Venezuela_discrete_plot1 <- ggplot2::ggplot(data = df_table_4.5.Venezuela_discrete, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Venezuela Discrete Labor"); Venezuela_discrete_plot1
      
      Venezuela_discrete_plot2 <- ggplot2::ggplot(data = df_table_4.5.Venezuela_discrete, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(method = "lm",se = FALSE) + 
        geom_point() +
        ggtitle("Venezuela Discrete Oil"); Venezuela_discrete_plot2
      
      gridExtra::grid.arrange(Venezuela_discrete_plot1, Venezuela_discrete_plot2)
    } # This originates at # Venezuela (Discrete) Plot No Coefficients switch
} # Originates at Venezuela ON / OFF Switch

###########################
###       Brazil        ###
###########################
if(T){ # Brazil ON / OFF Switch
# 1. Brazil Dataframe (General) (Petrobras joint)
  if(T){
      df_table_4.5.Brazil_general <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                    FDlogGDP_cap2000_supSQ_1,FDage_1,
                    FDoil_gas_valuePOPred_1,latin) %>%
        filter(cty=="Brazil") %>%
        filter(!is.na(year)) %>%
        filter(!is.na(FDlaborfemale)) %>%
        filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
        filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
        filter(!is.na(FDage_1)) %>%
        filter(!is.na(FDoil_gas_valuePOPred_1))
      }
# 2. Brazil (discrete) Dataframe
  if(T){
  # Brazil (discrete) Dataframe from 1985 - 1995
  # Look at 
  # BUST `85 - `90 |
  if(T){
    df_table_4.5.Brazil_discrete <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
      filter(latin==1) %>%
      filter(cty=="Brazil") %>%
      filter(year>=1985 & year<=1990) %>%
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
    head(df_table_4.5.Brazil_discrete)
    tail(df_table_4.5.Brazil_discrete)
  }
  # BOOM `96 - `01
  if(T){
    df_table_4.5.Brazil_discrete <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
      filter(latin==1) %>%
      filter(cty=="Brazil") %>%
      filter(year>=1996 & year<=2001) %>%
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
    head(df_table_4.5.Brazil_discrete)
    tail(df_table_4.5.Brazil_discrete)
  }
}
# 3. Brazil Visualization
require(mgcv)
  # Brazil (General) no coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
    Brazil_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Brazil, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Brazil General Labor"); Brazil_general_plot1
    
    Brazil_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Brazil, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(se = FALSE) + 
      geom_point() +
      ggtitle("Brazil General Oil"); Brazil_general_plot2
    
    gridExtra::grid.arrange(Brazil_general_plot1, Brazil_general_plot2)}
  # Brazil (General) Plot
  if(T){
    Brazil_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Brazil_general, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Brazil General Labor"); Brazil_general_plot1
    
    Brazil_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Brazil_general, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(se = F,method = "lm",formula = y ~ poly(x,10),size = 1) + 
      geom_point() +
      ggtitle("Brazil General Oil"); Brazil_general_plot2
    
    gridExtra::grid.arrange(Brazil_general_plot1, Brazil_general_plot2)
    
  } # This originates at # 3. Brazil (General) Plot
  # Brazil (Discrete) Plot No Coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
    Brazil_discrete_plot1 <- ggplot2::ggplot(data = df_table_4.5.Brazil_discrete, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Brazil Discrete Labor"); Brazil_discrete_plot1
    
    Brazil_discrete_plot2 <- ggplot2::ggplot(data = df_table_4.5.Brazil_discrete, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(method = "lm",se = FALSE) + 
      geom_point() +
      ggtitle("Brazil Discrete Oil"); Brazil_discrete_plot2
    
    gridExtra::grid.arrange(Brazil_discrete_plot1, Brazil_discrete_plot2)
  } # This originates at # Brazil (Discrete) Plot No Coefficients switch
} # Originates at Brazil ON / OFF Switch
  
##############################
###     Argentina (LT)     ###
##############################
if(T){ # Argentina ON / OFF Switch
# 1. Argentina Dataframe (General) (Petrobras joint)
  if(T){
    df_table_4.5.Argentina_general <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                    FDlogGDP_cap2000_supSQ_1,FDage_1,
                    FDoil_gas_valuePOPred_1,latin) %>%
      filter(cty=="Argentina") %>%
      filter(!is.na(year)) %>%
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
  }
# 2. Argentina (discrete) Dataframe
  if(T){
    # Argentina (discrete) Dataframe from 1985 - 1995
    # Look at 
    # BUST `85 - `90 |
    if(T){
      df_table_4.5.Argentina_discrete <- rossdata01 %>%
        dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
        filter(latin==1) %>%
        filter(cty=="Argentina") %>%
        filter(year>=1985 & year<=1990) %>%
        filter(!is.na(FDlaborfemale)) %>%
        filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
        filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
        filter(!is.na(FDage_1)) %>%
        filter(!is.na(FDoil_gas_valuePOPred_1))
      head(df_table_4.5.Argentina_discrete)
      tail(df_table_4.5.Argentina_discrete)
    }
    # BOOM `96 - `01
    if(T){
      df_table_4.5.Argentina_discrete <- rossdata01 %>%
        dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
        filter(latin==1) %>%
        filter(cty=="Argentina") %>%
        filter(year>=1996 & year<=2001) %>%
        filter(!is.na(FDlaborfemale)) %>%
        filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
        filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
        filter(!is.na(FDage_1)) %>%
        filter(!is.na(FDoil_gas_valuePOPred_1))
      head(df_table_4.5.Argentina_discrete)
      tail(df_table_4.5.Argentina_discrete)
    }
  }
# 3. Argentina Visualization
require(mgcv)
  # Argentina (General) no coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
    Argentina_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Argentina, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Argentina General Labor"); Argentina_general_plot1
    
    Argentina_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Argentina, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(se = FALSE) + 
      geom_point() +
      ggtitle("Argentina General Oil"); Argentina_general_plot2
    
    gridExtra::grid.arrange(Argentina_general_plot1, Argentina_general_plot2)}
  # Argentina (General) Plot
  if(T){
    Argentina_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Argentina_general, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Argentina General Labor"); Argentina_general_plot1
    
    Argentina_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Argentina_general, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(se = F,method = "lm",formula = y ~ poly(x,10),size = 1) + 
      geom_point() +
      ggtitle("Argentina General Oil"); Argentina_general_plot2
    
    gridExtra::grid.arrange(Argentina_general_plot1, Argentina_general_plot2)
    
  } # This originates at # 3. Argentina (General) Plot
  # Argentina (Discrete) Plot No Coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
    Argentina_discrete_plot1 <- ggplot2::ggplot(data = df_table_4.5.Argentina_discrete, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Argentina Discrete Labor"); Argentina_discrete_plot1
    
    Argentina_discrete_plot2 <- ggplot2::ggplot(data = df_table_4.5.Argentina_discrete, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(method = "lm",se = FALSE) + 
      geom_point() +
      ggtitle("Argentina Discrete Oil"); Argentina_discrete_plot2
    
    gridExtra::grid.arrange(Argentina_discrete_plot1, Argentina_discrete_plot2)
  } # This originates at # Argentina (Discrete) Plot No Coefficients switch  
} # Originates at Argentina ON / OFF Switch
  
#############################
###       Colombia        ###
#############################
if(T){ # Colombia ON / OFF Switch
# 1. Colombia Dataframe (General) (Petrobras joint)
  if(T){
    df_table_4.5.Colombia_general <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                    FDlogGDP_cap2000_supSQ_1,FDage_1,
                    FDoil_gas_valuePOPred_1,latin) %>%
      filter(cty=="Colombia") %>%
      filter(!is.na(year)) %>%
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
  }
# 2. Colombia (discrete) Dataframe
  if(T){
    # Colombia (discrete) Dataframe from 1985 - 1995
    # Look at 
    # BUST `85 - `90 |
    if(T){
      df_table_4.5.Colombia_discrete <- rossdata01 %>%
        dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
        filter(latin==1) %>%
        filter(cty=="Colombia") %>%
        filter(year>=1985 & year<=1990) %>%
        filter(!is.na(FDlaborfemale)) %>%
        filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
        filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
        filter(!is.na(FDage_1)) %>%
        filter(!is.na(FDoil_gas_valuePOPred_1))
      head(df_table_4.5.Colombia_discrete)
      tail(df_table_4.5.Colombia_discrete)
    }
    # BOOM `96 - `01
    if(T){
      df_table_4.5.Colombia_discrete <- rossdata01 %>%
        dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
        filter(latin==1) %>%
        filter(cty=="Colombia") %>%
        filter(year>=1996 & year<=2001) %>%
        filter(!is.na(FDlaborfemale)) %>%
        filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
        filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
        filter(!is.na(FDage_1)) %>%
        filter(!is.na(FDoil_gas_valuePOPred_1))
      head(df_table_4.5.Colombia_discrete)
      tail(df_table_4.5.Colombia_discrete)
    }
  }
# 3. Colombia Visualization
require(mgcv)
  # Colombia (General) no coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
    Colombia_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Colombia, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Colombia General Labor"); Colombia_general_plot1
    
    Colombia_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Colombia, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(se = FALSE) + 
      geom_point() +
      ggtitle("Colombia General Oil"); Colombia_general_plot2
    
    gridExtra::grid.arrange(Colombia_general_plot1, Colombia_general_plot2)}
  # Colombia (General) Plot
  if(T){
    Colombia_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Colombia_general, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Colombia General Labor"); Colombia_general_plot1
    
    Colombia_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Colombia_general, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(se = F,method = "lm",formula = y ~ poly(x,10),size = 1) + 
      geom_point() +
      ggtitle("Colombia General Oil"); Colombia_general_plot2
    
    gridExtra::grid.arrange(Colombia_general_plot1, Colombia_general_plot2)
    
  } # This originates at # 3. Colombia (General) Plot
  # Colombia (Discrete) Plot No Coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
    Colombia_discrete_plot1 <- ggplot2::ggplot(data = df_table_4.5.Colombia_discrete, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Colombia Discrete Labor"); Colombia_discrete_plot1
    
    Colombia_discrete_plot2 <- ggplot2::ggplot(data = df_table_4.5.Colombia_discrete, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(method = "lm",se = FALSE) + 
      geom_point() +
      ggtitle("Colombia Discrete Oil"); Colombia_discrete_plot2
    
    gridExtra::grid.arrange(Colombia_discrete_plot1, Colombia_discrete_plot2)
  } # This originates at # Colombia (Discrete) Plot No Coefficients switch
} # Originates at Colombia ON / OFF Switch
  
############################
###     Ecuador (LT)     ###
############################
if(T){ # Ecuador ON / OFF Switch
# 1. Ecuador Dataframe (General) (Petrobras joint)
  if(T){
    df_table_4.5.Ecuador_general <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                    FDlogGDP_cap2000_supSQ_1,FDage_1,
                    FDoil_gas_valuePOPred_1,latin) %>%
      filter(cty=="Ecuador") %>%
      filter(!is.na(year)) %>%
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
  }
# 2. Ecuador (discrete) Dataframe
  if(T){
    # Ecuador (discrete) Dataframe from 1985 - 1995
    # Look at 
    # BUST `85 - `90 |
    if(T){
      df_table_4.5.Ecuador_discrete <- rossdata01 %>%
        dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
        filter(latin==1) %>%
        filter(cty=="Ecuador") %>%
        filter(year>=1985 & year<=1990) %>%
        filter(!is.na(FDlaborfemale)) %>%
        filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
        filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
        filter(!is.na(FDage_1)) %>%
        filter(!is.na(FDoil_gas_valuePOPred_1))
      head(df_table_4.5.Ecuador_discrete)
      tail(df_table_4.5.Ecuador_discrete)
    }
    # BOOM `96 - `01
    if(T){
      df_table_4.5.Ecuador_discrete <- rossdata01 %>%
        dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
        filter(latin==1) %>%
        filter(cty=="Ecuador") %>%
        filter(year>=1996 & year<=2001) %>%
        filter(!is.na(FDlaborfemale)) %>%
        filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
        filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
        filter(!is.na(FDage_1)) %>%
        filter(!is.na(FDoil_gas_valuePOPred_1))
      head(df_table_4.5.Ecuador_discrete)
      tail(df_table_4.5.Ecuador_discrete)
    }
  }
# 3. Ecuador Visualization
require(mgcv)
  # Ecuador (General) no coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
    Ecuador_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Ecuador, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Ecuador General Labor"); Ecuador_general_plot1
    
    Ecuador_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Ecuador, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(se = FALSE) + 
      geom_point() +
      ggtitle("Ecuador General Oil"); Ecuador_general_plot2
    
    gridExtra::grid.arrange(Ecuador_general_plot1, Ecuador_general_plot2)}
  # Ecuador (General) Plot
  if(T){
    Ecuador_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Ecuador_general, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Ecuador General Labor"); Ecuador_general_plot1
    
    Ecuador_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Ecuador_general, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(se = F,method = "lm",formula = y ~ poly(x,10),size = 1) + 
      geom_point() +
      ggtitle("Ecuador General Oil"); Ecuador_general_plot2
    
    gridExtra::grid.arrange(Ecuador_general_plot1, Ecuador_general_plot2)
    
  } # This originates at # 3. Ecuador (General) Plot
  # Ecuador (Discrete) Plot No Coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
    Ecuador_discrete_plot1 <- ggplot2::ggplot(data = df_table_4.5.Ecuador_discrete, aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) +
      geom_point() +
      ggtitle("Ecuador Discrete Labor"); Ecuador_discrete_plot1
    
    Ecuador_discrete_plot2 <- ggplot2::ggplot(data = df_table_4.5.Ecuador_discrete, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      stat_smooth(method = "lm",se = FALSE) + 
      geom_point() +
      ggtitle("Ecuador Discrete Oil"); Ecuador_discrete_plot2
    
    gridExtra::grid.arrange(Ecuador_discrete_plot1, Ecuador_discrete_plot2)
  } # This originates at # Ecuador (Discrete) Plot No Coefficients switch
} # Originates at Ecuador ON / OFF Switch
  
############################
###       Suriname      ###
############################
if(T){ # Suriname ON / OFF Switch
# 1. Suriname Dataframe (General) (Petrobras joint)
  if(T){
      df_table_4.5.Suriname_general <- rossdata01 %>%
        dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                      FDlogGDP_cap2000_supSQ_1,FDage_1,
                      FDoil_gas_valuePOPred_1,latin) %>%
        filter(cty=="Suriname") %>%
        filter(!is.na(year)) %>%
        filter(!is.na(FDlaborfemale)) %>%
        filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
        filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
        filter(!is.na(FDage_1)) %>%
        filter(!is.na(FDoil_gas_valuePOPred_1))
    }
# 2. Suriname (discrete) Dataframe
  if(T){
      # Suriname (discrete) Dataframe from 1985 - 1995
      # Look at 
      # BUST `85 - `90 |
      if(T){
        df_table_4.5.Suriname_discrete <- rossdata01 %>%
          dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
          filter(latin==1) %>%
          filter(cty=="Suriname") %>%
          filter(year>=1985 & year<=1990) %>%
          filter(!is.na(FDlaborfemale)) %>%
          filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
          filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
          filter(!is.na(FDage_1)) %>%
          filter(!is.na(FDoil_gas_valuePOPred_1))
        head(df_table_4.5.Suriname_discrete)
        tail(df_table_4.5.Suriname_discrete)
      }
      # BOOM `96 - `01
      if(T){
        df_table_4.5.Suriname_discrete <- rossdata01 %>%
          dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
          filter(latin==1) %>%
          filter(cty=="Suriname") %>%
          filter(year>=1996 & year<=2001) %>%
          filter(!is.na(FDlaborfemale)) %>%
          filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
          filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
          filter(!is.na(FDage_1)) %>%
          filter(!is.na(FDoil_gas_valuePOPred_1))
        head(df_table_4.5.Suriname_discrete)
        tail(df_table_4.5.Suriname_discrete)
      }
    }
# 3. Suriname Visualization
require(mgcv)
  # Suriname (General) no coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
      Suriname_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Suriname, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Suriname General Labor"); Suriname_general_plot1
      
      Suriname_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Suriname, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(se = FALSE) + 
        geom_point() +
        ggtitle("Suriname General Oil"); Suriname_general_plot2
      
      gridExtra::grid.arrange(Suriname_general_plot1, Suriname_general_plot2)}
  # Suriname (General) Plot
  if(T){
      Suriname_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Suriname_general, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Suriname General Labor"); Suriname_general_plot1
      
      Suriname_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Suriname_general, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(se = F,method = "lm",formula = y ~ poly(x,10),size = 1) + 
        geom_point() +
        ggtitle("Suriname General Oil"); Suriname_general_plot2
      
      gridExtra::grid.arrange(Suriname_general_plot1, Suriname_general_plot2)
      
    } # This originates at # 3. Suriname (General) Plot
  # Suriname (Discrete) Plot No Coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
      Suriname_discrete_plot1 <- ggplot2::ggplot(data = df_table_4.5.Suriname_discrete, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Suriname Discrete Labor"); Suriname_discrete_plot1
      
      Suriname_discrete_plot2 <- ggplot2::ggplot(data = df_table_4.5.Suriname_discrete, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(method = "lm",se = FALSE) + 
        geom_point() +
        ggtitle("Suriname Discrete Oil"); Suriname_discrete_plot2
      
      gridExtra::grid.arrange(Suriname_discrete_plot1, Suriname_discrete_plot2)
    } # This originates at # Suriname (Discrete) Plot No Coefficients switch
  } # Originates at Suriname ON / OFF Switch

##########################
###       Bolivia      ###
##########################
if(T){ # Bolivia ON / OFF Switch
# 1. Bolivia Dataframe (General) (Petrobras joint)
  if(T){
      df_table_4.5.Bolivia_general <- rossdata01 %>%
        dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                      FDlogGDP_cap2000_supSQ_1,FDage_1,
                      FDoil_gas_valuePOPred_1,latin) %>%
        filter(cty=="Bolivia") %>%
        filter(!is.na(year)) %>%
        filter(!is.na(FDlaborfemale)) %>%
        filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
        filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
        filter(!is.na(FDage_1)) %>%
        filter(!is.na(FDoil_gas_valuePOPred_1))
    }
# 2. Bolivia (discrete) Dataframe
  if(T){
      # Bolivia (discrete) Dataframe from 1985 - 1995
      # Look at 
      # BUST `85 - `90 |
      if(T){
        df_table_4.5.Bolivia_discrete <- rossdata01 %>%
          dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
          filter(latin==1) %>%
          filter(cty=="Bolivia") %>%
          filter(year>=1985 & year<=1990) %>%
          filter(!is.na(FDlaborfemale)) %>%
          filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
          filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
          filter(!is.na(FDage_1)) %>%
          filter(!is.na(FDoil_gas_valuePOPred_1))
        head(df_table_4.5.Bolivia_discrete)
        tail(df_table_4.5.Bolivia_discrete)
      }
      # BOOM `96 - `01
      if(T){
        df_table_4.5.Bolivia_discrete <- rossdata01 %>%
          dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
          filter(latin==1) %>%
          filter(cty=="Bolivia") %>%
          filter(year>=1996 & year<=2001) %>%
          filter(!is.na(FDlaborfemale)) %>%
          filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
          filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
          filter(!is.na(FDage_1)) %>%
          filter(!is.na(FDoil_gas_valuePOPred_1))
        head(df_table_4.5.Bolivia_discrete)
        tail(df_table_4.5.Bolivia_discrete)
      }
    }
# 3. Bolivia Visualization
require(mgcv)
    # Bolivia (General) no coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
    if(T){
      Bolivia_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Bolivia, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Bolivia General Labor"); Bolivia_general_plot1
      
      Bolivia_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Bolivia, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(se = FALSE) + 
        geom_point() +
        ggtitle("Bolivia General Oil"); Bolivia_general_plot2
      
      gridExtra::grid.arrange(Bolivia_general_plot1, Bolivia_general_plot2)}
    # Bolivia (General) Plot
    if(T){
      Bolivia_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Bolivia_general, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Bolivia General Labor"); Bolivia_general_plot1
      
      Bolivia_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Bolivia_general, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(se = F,method = "lm",formula = y ~ poly(x,10),size = 1) + 
        geom_point() +
        ggtitle("Bolivia General Oil"); Bolivia_general_plot2
      
      gridExtra::grid.arrange(Bolivia_general_plot1, Bolivia_general_plot2)
      
    } # This originates at # 3. Bolivia (General) Plot
    # Bolivia (Discrete) Plot No Coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
    if(T){
      Bolivia_discrete_plot1 <- ggplot2::ggplot(data = df_table_4.5.Bolivia_discrete, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Bolivia Discrete Labor"); Bolivia_discrete_plot1
      
      Bolivia_discrete_plot2 <- ggplot2::ggplot(data = df_table_4.5.Bolivia_discrete, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(method = "lm",se = FALSE) + 
        geom_point() +
        ggtitle("Bolivia Discrete Oil"); Bolivia_discrete_plot2
      
      gridExtra::grid.arrange(Bolivia_discrete_plot1, Bolivia_discrete_plot2)
    } # This originates at # Bolivia (Discrete) Plot No Coefficients switch
  } # Originates at Bolivia ON / OFF Switch
  
#######################
###       Cuba      ###
#######################
if(T){ # Cuba ON / OFF Switch
# 1. Cuba Dataframe (General) (Petrobras joint)
  if(T){
      df_table_4.5.Cuba_general <- rossdata01 %>%
        dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                      FDlogGDP_cap2000_supSQ_1,FDage_1,
                      FDoil_gas_valuePOPred_1,latin) %>%
        filter(cty=="Cuba") %>%
        filter(!is.na(year)) %>%
        filter(!is.na(FDlaborfemale)) %>%
        filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
        filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
        filter(!is.na(FDage_1)) %>%
        filter(!is.na(FDoil_gas_valuePOPred_1))
    }
# 2. Cuba (discrete) Dataframe
  if(T){
  # Cuba (discrete) Dataframe from 1985 - 1995
  # Look at 
  # BUST `85 - `90 |
  if(T){
        df_table_4.5.Cuba_discrete <- rossdata01 %>%
          dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
          filter(latin==1) %>%
          filter(cty=="Cuba") %>%
          filter(year>=1985 & year<=1990) %>%
          filter(!is.na(FDlaborfemale)) %>%
          filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
          filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
          filter(!is.na(FDage_1)) %>%
          filter(!is.na(FDoil_gas_valuePOPred_1))
        head(df_table_4.5.Cuba_discrete)
        tail(df_table_4.5.Cuba_discrete)
      }
  # BOOM `96 - `01
  if(T){
        df_table_4.5.Cuba_discrete <- rossdata01 %>%
          dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
          filter(latin==1) %>%
          filter(cty=="Cuba") %>%
          filter(year>=1996 & year<=2001) %>%
          filter(!is.na(FDlaborfemale)) %>%
          filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
          filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
          filter(!is.na(FDage_1)) %>%
          filter(!is.na(FDoil_gas_valuePOPred_1))
        head(df_table_4.5.Cuba_discrete)
        tail(df_table_4.5.Cuba_discrete)
      }
    }
# 3. Cuba Visualization
require(mgcv)
# Cuba (General) no coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
      Cuba_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Cuba, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Cuba General Labor"); Cuba_general_plot1
      
      Cuba_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Cuba, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(se = FALSE) + 
        geom_point() +
        ggtitle("Cuba General Oil"); Cuba_general_plot2
      
      gridExtra::grid.arrange(Cuba_general_plot1, Cuba_general_plot2)}
  # Cuba (General) Plot
  if(T){
      Cuba_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Cuba_general, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Cuba General Labor"); Cuba_general_plot1
      
      Cuba_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Cuba_general, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(se = F,method = "lm",formula = y ~ poly(x,10),size = 1) + 
        geom_point() +
        ggtitle("Cuba General Oil"); Cuba_general_plot2
      
      gridExtra::grid.arrange(Cuba_general_plot1, Cuba_general_plot2)
      
    } # This originates at # 3. Cuba (General) Plot
  # Cuba (Discrete) Plot No Coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
      Cuba_discrete_plot1 <- ggplot2::ggplot(data = df_table_4.5.Cuba_discrete, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Cuba Discrete Labor"); Cuba_discrete_plot1
      
      Cuba_discrete_plot2 <- ggplot2::ggplot(data = df_table_4.5.Cuba_discrete, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(method = "lm",se = FALSE) + 
        geom_point() +
        ggtitle("Cuba Discrete Oil"); Cuba_discrete_plot2
      
      gridExtra::grid.arrange(Cuba_discrete_plot1, Cuba_discrete_plot2)
    } # This originates at # Cuba (Discrete) Plot No Coefficients switch
  } # Originates at Cuba ON / OFF Switch
  
###########################
###   Trinidad (LT)     ###
###########################
if(T){ # Trinidad ON / OFF Switch
# 1. Trinidad Dataframe (General) (Petrobras joint)
  if(T){
      df_table_4.5.Trinidad_general <- rossdata01 %>%
        dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,
                      FDlogGDP_cap2000_supSQ_1,FDage_1,
                      FDoil_gas_valuePOPred_1,latin) %>%
        filter(cty=="Trinidad") %>%
        filter(!is.na(year)) %>%
        filter(!is.na(FDlaborfemale)) %>%
        filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
        filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
        filter(!is.na(FDage_1)) %>%
        filter(!is.na(FDoil_gas_valuePOPred_1))
    }
# 2. Trinidad (discrete) Dataframe
  if(T){
      # Trinidad (discrete) Dataframe from 1985 - 1995
      # Look at 
      # BUST `85 - `90 |
      if(T){
        df_table_4.5.Trinidad_discrete <- rossdata01 %>%
          dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
          filter(latin==1) %>%
          filter(cty=="Trinidad") %>%
          filter(year>=1985 & year<=1990) %>%
          filter(!is.na(FDlaborfemale)) %>%
          filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
          filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
          filter(!is.na(FDage_1)) %>%
          filter(!is.na(FDoil_gas_valuePOPred_1))
        head(df_table_4.5.Trinidad_discrete)
        tail(df_table_4.5.Trinidad_discrete)
      }
      # BOOM `96 - `01
      if(T){
        df_table_4.5.Trinidad_discrete <- rossdata01 %>%
          dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
          filter(latin==1) %>%
          filter(cty=="Trinidad") %>%
          filter(year>=1996 & year<=2001) %>%
          filter(!is.na(FDlaborfemale)) %>%
          filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
          filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
          filter(!is.na(FDage_1)) %>%
          filter(!is.na(FDoil_gas_valuePOPred_1))
        head(df_table_4.5.Trinidad_discrete)
        tail(df_table_4.5.Trinidad_discrete)
      }
    }
# 3. Trinidad Visualization
require(mgcv)
# Trinidad (General) no coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
      Trinidad_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Trinidad, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Trinidad General Labor"); Trinidad_general_plot1
      
      Trinidad_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Trinidad, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(se = FALSE) + 
        geom_point() +
        ggtitle("Trinidad General Oil"); Trinidad_general_plot2
      
      gridExtra::grid.arrange(Trinidad_general_plot1, Trinidad_general_plot2)}
  # Trinidad (General) Plot
  if(T){
      Trinidad_general_plot1 <- ggplot2::ggplot(data = df_table_4.5.Trinidad_general, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Trinidad General Labor"); Trinidad_general_plot1
      
      Trinidad_general_plot2 <- ggplot2::ggplot(data = df_table_4.5.Trinidad_general, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(se = F,method = "lm",formula = y ~ poly(x,10),size = 1) + 
        geom_point() +
        ggtitle("Trinidad General Oil"); Trinidad_general_plot2
      
      gridExtra::grid.arrange(Trinidad_general_plot1, Trinidad_general_plot2)
      
    } # This originates at # 3. Trinidad (General) Plot
  # Trinidad (Discrete) Plot No Coefficients Switch - USE THIS TO VIEW BOOMS AND BUSTS OVERALL
  if(T){
      Trinidad_discrete_plot1 <- ggplot2::ggplot(data = df_table_4.5.Trinidad_discrete, aes(x = year, y = FDlaborfemale)) + 
        geom_line() + 
        geom_smooth(se = FALSE) +
        geom_point() +
        ggtitle("Trinidad Discrete Labor"); Trinidad_discrete_plot1
      
      Trinidad_discrete_plot2 <- ggplot2::ggplot(data = df_table_4.5.Trinidad_discrete, aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
        geom_line() + 
        stat_smooth(method = "lm",se = FALSE) + 
        geom_point() +
        ggtitle("Trinidad Discrete Oil"); Trinidad_discrete_plot2
      
      gridExtra::grid.arrange(Trinidad_discrete_plot1, Trinidad_discrete_plot2)
    } # This originates at # Trinidad (Discrete) Plot No Coefficients switch
  } # Originates at Trinidad ON / OFF Switch
  

################################################
######            White Board             ######
################################################ 
## 7 Original Plot - put it on the white board - unneeded !!!!
# Simmons' original ggplot of the Brazil Data
  if(F){
    brazil_general_plot1 <- ggplot2::ggplot(data = subset(rossdata01, cty == "Brazil"), aes(x = year, y = FDlaborfemale)) + 
      geom_line() + 
      geom_smooth(se = FALSE) + 
      geom_point()
    
    brazil_general_plot2 <- ggplot2::ggplot(data = subset(rossdata01, cty == "Brazil"), aes(x = year, y = FDoil_gas_valuePOPred_1)) + 
      geom_line() + 
      geom_smooth(se = FALSE) + 
      geom_point()
    
    gridExtra::grid.arrange(brazil_general_plot1, brazil_general_plot2)
  }  # This originates at '## 7 Original Plot'

## 8 Build Discrete Dataframe for Latin America - unneeded !!!!
# Latin America (discrete) Dataframe from 1985 - 1995
  if(F){
    df_table_4.5.LA_discrete <- rossdata01 %>%
      dplyr::select(cty,year,FDlaborfemale,FDlogGDP_cap2000_sup_1,FDlogGDP_cap2000_supSQ_1,FDage_1,FDoil_gas_valuePOPred_1,latin) %>%
      filter(latin==1) %>%
      filter(!is.na(cty)) %>%
      filter(year>1984 & year<1996) %>%
      filter(!is.na(FDlaborfemale)) %>%
      filter(!is.na(FDlogGDP_cap2000_sup_1)) %>%
      filter(!is.na(FDlogGDP_cap2000_supSQ_1)) %>%
      filter(!is.na(FDage_1)) %>%
      filter(!is.na(FDoil_gas_valuePOPred_1))
    head(df_table_4.5.LA_discrete)
    tail(df_table_4.5.LA_discrete)
  }  # This originates at '## 8 Build Discrete Dataframe'

  

# Ross's Stata Code:
# *Column 1: just the controls
# xtregar FDlaborfemale FDlogGDP_cap2000_sup_1 FDlogGDP_cap2000_supSQ_1 FDage_1, fe

# *Column 6: drop all Middle East & North Africa
# xtregar FDlaborfemale FDlogGDP_cap2000_sup_1 FDlogGDP_cap2000_supSQ_1 FDage_1 FDoil_gas_valuePOPred_1 if me_nafr==0, fe

# One thing to do is draw a bunch of graphs to see if they are moving the way they should.  Are the trends moving together
# if oil goes up then labor force participation should diverge.


}  # End Bracket to run entire script at one time

