#Data available at https://github.com/AdrianSotoM
#Please, address all correspondence about this code to adrian.sotom@incmnsz.mx
#Running it takes a few minutes, I've set up a sound to let you know 
#when it's done in case you want to do something else while it runs.

#Working directory setup
setwd("C:/Users/adria/Dropbox/UIEM/LEAD/Proyectos/FreeBHB")

# Ensuring reproducibility of this script.
# Since "easystats" is currently not available in CRAN, if you don't have it,
# you'll need to install manually.
# install.packages("easystats", repos = "https://easystats.r-universe.dev")

# Now, confirm you have "pacman" installed. If you don't have "pacman" but want
# to install it, remove the # in the line below and press "Enter".
# install.packages("pacman") 

#Packages setup
pacman::p_load(dplyr,tidyr,ggstatsplot,readxl,tableone,easystats,
               patchwork,MASS,see,qqplotr,bootStepAIC,performance,
               rpart,rpart.plot,gtools,broom,lmtest,visdat,report,
               parameters,ggcharts,conflicted,car,rattle,cvms,
               mlogit,MLmetrics,beepr)

#Solving duplicate functions conflicts
conflict_prefer("select","dplyr")
conflict_prefer("filter", "dplyr")

#Data upload
data <- read.csv("data.csv")
men <- data %>% filter(sex=="m")
women <- data %>% filter(sex=="f")
women <- CreateContTable(data=women,vars=c("age","height","weight","mgperkg","bmi"))
women
men <- CreateContTable(data=men,vars=c("age","height","weight","mgperkg","bmi"))
men

#Data structure reshape
ph <- gather(data, visit, ph, aph1:aph3, factor_key=TRUE) %>% select(visit,ph)
co2 <- gather(data, visit, co2, aco21:aco23, factor_key=TRUE) %>% select(visit,co2)
hco <- gather(data, visit, hco, ahco1:ahco3, factor_key=TRUE) %>% select(visit,hco)
na <- gather(data, visit, na, mNa1:mNa3, factor_key=TRUE) %>% select(visit,na)
k <- gather(data, visit, k, mK1:mK3, factor_key=TRUE) %>% select(visit,k)
cl <- gather(data, visit, cl, mCl1:mCl3, factor_key=TRUE) %>% select(visit,cl)
ca <- gather(data, visit, ca, mCa1:mCa3, factor_key=TRUE) %>% select(visit,ca)

#Blood Gas Analyses
phtab <- CreateContTable(data=ph,strata="visit",vars="ph")
phtab
co2tab <- CreateContTable(data=co2,strata="visit",vars="co2")
co2tab
hcotab <- CreateContTable(data=hco,strata="visit",vars="hco")
hcotab
natab <- CreateContTable(data=na,strata="visit",vars="na")
natab
ktab <- CreateContTable(data=k,strata="visit",vars="k")
ktab
cltab <- CreateContTable(data=cl,strata="visit",vars="cl")
cltab
catab <- CreateContTable(data=ca,strata="visit",vars="ca")
catab

#Correlation of symptom frequency with weight-adjusted dose
f3 <- ggscatterstats(data,mgperkg,reportedss,
                     xlab  = "Weight adjusted dose (mg of D-BHB/kg)",
                     ylab  = "Reported secondary symptoms",
                     subtitle = "R = -0.11, p = 0.60",
                     results.subtitle = F, marginal = F,
                     smooth.line.args = list(size = 1.5, color = "black", method = "lm", formula = y ~ x,                           na.rm = TRUE) 
) + ggplot2::theme(plot.title = element_text(size=26,face = "bold"),
                 plot.title.position = "plot",
                 plot.margin = margin(1,1,1,1,unit="cm"),
                 axis.line = element_line(color = "black",size = 1, linetype = "solid"),
                 axis.title.y = element_text(size=24,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")),
                 axis.title.x = element_text(size=24,colour = "black",face="bold",margin = margin(t=1,unit="cm")),
                 axis.text = element_text(size=24,colour = "black",face="bold", margin = margin(t=1,r=1,unit="cm")),
                 plot.subtitle = element_text(size=22,colour = "black",hjust = 0.5,margin = margin(b=1,t=1,unit="cm")),
                 plot.caption = element_text(size=22,colour="gray40",face="bold.italic",hjust = 1,margin = margin(t=0.5,unit="cm"))
  ) 
f3
