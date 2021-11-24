## ----setup, include=FALSE------------------------------------------------------------------------------------------------
rm(list=ls())
dir = "~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/Dispersal/Winter_2020/stats/"
setwd(dir) 

# tables
library(knitr)
library(kableExtra)

# plotting
library(rethinking) 
library(ggformula)
library(plot.matrix)
library(plotly)
library(tidyr)

# modeling and data manipulation
library(lme4)
library(nnet) # multinom package
library(dplyr)

knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------------------------------
knitr::purl("delta_yes_no.Rmd", output = "delta_yes_no.R") # convert Rmd to R script


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
# key = response in T2 - response in T1
Event = c("flew in both trials", "flew in T2 only", " flew in neither trials", "flew in T1 only")
Encoding = c(2, 1,0,-1)
key = cbind(Event, Encoding)

kable(key) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))  %>%
  kable_classic(html_font = "Cambria") %>%
  add_header_above(c("Delta Flight Response Key" = 2 )) 


## ------------------------------------------------------------------------------------------------------------------------
source_path = "~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/Rsrc/"

script_names = c("center_flight_data.R", # Re-centers data 
                 "clean_flight_data.R", # Loads and cleans data
                 "unique_flight_data.R",
                 "get_warnings.R", 
                 "compare_models.R",
                 "regression_output.R", # Cleans regression outputs; prints in color or B&W
                 "AICprobabilities.R",
                 "multinom_functions.R")

for (script in script_names) { 
  path = paste0(source_path, script)
  source(path) 
}

output_col = FALSE # Change to TRUE if working in Base R or RStudio; FALSE if generating an HTML

data <- read_flight_data("data/all_flight_data-Winter2020.csv")
data_all <- data[[1]]
data_tested <- data[[2]]
d <- create_delta_data(data_tested)


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
# for machine learning | creates CSV files to be read into the python script
#test = d[,c(1:30,59,63:73)] # for machine learning
#short_test = d[,c(1:2,5,66:68,71:73)]
#write.csv(test, file="unique_data-Winter2020.csv")
#write.csv(short_test, file="unique_data-Winter2020.csv")


## ------------------------------------------------------------------------------------------------------------------------
colnames(d)[c(1:2,5,66:68,71:73)]
# for wing-to-body ratio color creating
d$w2b_col <- 0


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
# fig.height=2.4, fig.width=2.7
bar = function() {
  counts <- table(data_tested$flew_b, data_tested$trial_type)
  counts <- counts[1:2,2:3]
  barplot(counts, #main="Flight Distribution by Trial",
  xlab="Trial", col=c("lightblue","darkgoldenrod2"),
  legend = c("no flew", "yes flew"), ylim=c(0,250), beside=TRUE)
}
bar()


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
# key = change in mass
Event = c("gained mass from T1 to T2", "no mass change between trails", "lost mass from T1 to T2")
Sign = c("pos","0","neg")
key = cbind(Event, Sign)

kable(key) %>%
  kable_styling(bootstrap_options = "striped", "hover", full_width = F)  %>%
  add_header_above(c("Delta Mass Key" = 2 )) 


## ------------------------------------------------------------------------------------------------------------------------
df <- d %>%
  filter(!is.na(mass_diff), !is.na(flight_case)) 
  
df <- df[with(df, order(mass_per)),]
n_trials = nrow(df)

df$flight_case <- relevel(as.factor(df$flight_case), ref = "0")


## ------------------------------------------------------------------------------------------------------------------------
null <- multinom(flight_case ~ 1, data = df) 


## ------------------------------------------------------------------------------------------------------------------------
model <- multinom(flight_case ~ mass_per, data = df) 
model_table = calculate_P(model)


## ------------------------------------------------------------------------------------------------------------------------
run_multinom_model = function(d) {
  m <- multinom(flight_case ~ mass_per, trace=FALSE, data = d) 
  model_table = calculate_P(m, print_table=FALSE)
  return(model_table)
}
ML_eqs = get_significant_models(7)


## ------------------------------------------------------------------------------------------------------------------------
gsub("Mass Change",  "Mass Percent Change", prediction_equations(model_table))


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
eq1 = "Flew in T1 only rather than T2 only = 1.22 + 0.04 Mass %"
eq2 = "Flew in both rather than T1 only = 1.25 - 0.02 Mass %"
eq3 = "Flew in both rather than T2 only = 2.48 + 0.02 Mass %"

significance = c("sig", "sig", "not sig")
t = cbind(n_trials, c(eq1, eq2, eq3), significance)
colnames(t) = c("N","Model", "P")

kable(t) %>%
  kable_styling(bootstrap_options = "striped", "hover", full_width = F) 


## ------------------------------------------------------------------------------------------------------------------------
exp(0.04*20)


## ------------------------------------------------------------------------------------------------------------------------
exp(40*-0.02) # 2.5 times less likely to fly twice if gain mass
exp(-40*-0.02) # 2.5 times more likely to fly twice if loose mass


## ------------------------------------------------------------------------------------------------------------------------
exp(coef(model)*20) # this compares to no fly, the baseline # gain large % mass
exp(coef(model)*5) # gain small % mass
exp(coef(model)*-5) # loose small % mass
exp(coef(model)*-20) # loose large % mass
# 1/0 not significant


## ------------------------------------------------------------------------------------------------------------------------
head(pp <- fitted(model))


## ----echo=FALSE, fig.width=3.2*1.7*2, fig.height=2.8*2-------------------------------------------------------------------
plot2 = function(df, pp) {
    plot(df$mass_per, pp[,1], xlim=c(-40, 103), ylim=c(0,1), col="red", type="l", 
         ylab="Flight Case Probability", xlab="Percent Change in Mass From T1 to T2 (%)", 
         pch=2, cex=0.45) # no flight_diff
  points(df$mass_per, pp[,4], col="darkred", type="l", pch=3, cex=0.45)
  points(df$mass_per, pp[,2], col="blue", type="l", cex=0.45) # flew in T1 but not T2
  points(df$mass_per, pp[,3], col="darkgreen", type="l", cex=0.45) # flew in T2 but not T1
  text(-36,0.65, labels="Did Not \nFly", col="red")
  text(20,0.6, labels="Flew Twice", col="darkred")
  text(85,0.85, labels="Flew in T1 only", col="blue")
  text(20,0.1, labels="Flew in T2 only", col="darkgreen")
  abline(v=53, lty=2, col="black")
  abline(v=-16.5, lty=2, col="black")
  abline(v=-28, lty=2, col="black")
}

pp2 = fitted(model)
df2 <- df # for summary purposes
plot2(df2,pp2)


## ------------------------------------------------------------------------------------------------------------------------
df <- df[with(df, order(mass_per)),]


## ------------------------------------------------------------------------------------------------------------------------
data <- data.frame(R = df$flight_case, 
         A = df$mass_per,
         B = df$sex_c)
model_script = paste0(source_path,"generic multinomial models- multinom 1RF + 2 FF.R")
model_comparisonsAIC(model_script)

## ------------------------------------------------------------------------------------------------------------------------
anova(m3, m4, test="Chisq") # Adding A*B does not improve fit


## ------------------------------------------------------------------------------------------------------------------------
delta_mass_model <- multinom(flight_case ~ mass_per + sex_c, data = df)
model_table = calculate_P2(delta_mass_model, "mass_per", "sex_c")


## ------------------------------------------------------------------------------------------------------------------------
prediction_equations2(model_table, " Mass Percent Change", " Sex ")


## ------------------------------------------------------------------------------------------------------------------------
run_multinom_model = function(d) {
  m <- multinom(flight_case ~ mass_per + sex_c, trace=FALSE, data = d) 
  model_table = calculate_P2(m, "mass_per", "sex_c", print_table=FALSE)
  return(model_table)
}
par(mfrow=c(1,2))
MASS_ML = get_significant_models(15) # mass per
SEX_ML = get_significant_models(16) # sex


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
# 2 vs. -1
Odds = c("$\\underline{P(Yi=flew\\,twice)}$", "$P(Yi=flew\\, in\\, T1\\, only)$", " ")
effect = c("Intercept", "$\\delta$ Mass %", "Sex")
pars = c("$\\alpha$", "$\\beta_1$", "$\\beta_2\\, (F=1 | M=-1)$")
key1 = getting_oddsf(Odds, effect, pars, MASS_ML[[1]], 3) 

# 2 vs. 1
Odds = c("$\\underline{P(Yi=flew\\,twice)}$", "$P(Yi=flew\\, in\\, T2\\, only)$", " ")
key2 = getting_oddsf(Odds, effect, pars, MASS_ML[[3]], 3) 

# 2 vs. 0
Odds = c("$\\underline{P(Yi=flew\\,twice)}$", "$P(Yi=did\\, not\\, fly)$", " ")
key3 = getting_oddsf(Odds, effect, pars, MASS_ML[[2]], 3) 

# -1 vs. 0
Odds = c("$\\underline{P(Yi=flew\\, in\\, T1\\, only)}$", "$P(Yi=did\\, not\\, fly)$", " ")
key4 = getting_oddsf(Odds, effect, pars, MASS_ML[[2]], 1) 

# 1 vs. 0
Odds = c("$\\underline{P(Yi=flew\\, in\\, T2\\, only)}$", "$P(Yi=did\\, not\\, fly)$", " ")
key5 = getting_oddsf(Odds, effect, pars, MASS_ML[[2]], 2) 

# -1 vs. 1
Odds = c("$\\underline{P(Yi=flew\\, in\\, T1\\, only)}$", "$P(Yi=flew\\, in\\, T2\\, only)$", " ")
key6 = getting_oddsf(Odds, effect, pars, MASS_ML[[3]], 2)

key = rbind(key1, key2, key3, key4, key5, key6)
rownames(key)<-NULL
colnames(key) = c("Odds","Effect", "Parameter", "Estimate", "SE", "exp($\\beta$)*", "Wald", "p")


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
kable(key, caption="Table 1. Estimated Parameters, Standard Errors, and Wald Test Statistics for All Main Effects Model") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))  %>%
  column_spec(1, width="6cm", background="white") %>%
  kable_classic(html_font = "Cambria") %>%
  row_spec(c(3,6,9,12,15), extra_css = "border-bottom: 1px solid") %>%
  #column_spec(1, color = "black", background = "white") %>%
  footnote(general = " * Instead of a 1% mass increase, which is relatively too small, the mass percent change estimates were multiplied by 20 before calculating the log odds. These changes better represent experimental observation and offers a more realistic odds. ",
           general_title = " ", number_title = "* "
           )


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
eq1 = "Flew in T1 only rather than T2 only = 5.81 + 0.05 Mass % + 4.93 Sex"
eq2 = "Flew in both rather than T1 only = 1.14 - 0.02 Mass % - 0.21 Sex"
eq3 = "Flew in both rather than T2 only = 6.94 + 0.03 Mass % + 4.72 Sex"

significance = c("mass** | sex**", "mass** | not sex", "not mass | sex**")
t = cbind(n_trials, c(eq1, eq2, eq3), significance)
colnames(t) = c("N","Model", "P")

kable(t) %>%
  kable_styling(bootstrap_options = "striped", "hover", full_width = F) 


## ------------------------------------------------------------------------------------------------------------------------
exp(0.05*20)


## ------------------------------------------------------------------------------------------------------------------------
exp(20*-0.02) # 20 times less likely to fly twice if gain mass
exp(-20*-0.02) # 20 times more likely to fly twice if loose mass


## ------------------------------------------------------------------------------------------------------------------------
exp(0.05*20 + 4.93)


## ------------------------------------------------------------------------------------------------------------------------
exp(-0.02*20 - 0.21)
exp(-0.02*-40 - 0.21)


## ------------------------------------------------------------------------------------------------------------------------
exp(coef(delta_mass_model)*20) # this compares to no fly, the baseline # gain large % mass
exp(coef(delta_mass_model)*5) # gain small % mass
exp(coef(delta_mass_model)*-5) # loose small % mass
exp(coef(delta_mass_model)*-20) # loose large % mass
# 1/0 mass not sig but sex**


## ------------------------------------------------------------------------------------------------------------------------
head(pp <- fitted(delta_mass_model))


## ------------------------------------------------------------------------------------------------------------------------
df$index = 1:nrow(df)
females = df %>%
  filter(sex=="F")
males = df %>%
  filter(sex=="M")
Frows = females$index
Mrows = males$index


## ----echo=FALSE, fig.width=3.2*1.7*2, fig.height=2.8*2-------------------------------------------------------------------
plot3 = function(df, pp) {
  plot(df$mass_per[Frows], pp[Frows,1], ylim=c(0,1), xlim=c(-40,104), col="red",
     type="l",
     #type="b", cex=0.45, pch=16, 
     ylab="Flight Case Probability", 
     xlab="Percent Mass Change From T1 to T2 (%)", lty=1, cex.axis=1.2, cex.lab=1.3)  
  points(df$mass_per[Mrows], pp[Mrows,1], col="red", type="l", cex=0.45, lty=2) 
  # pch=c(2,3)[as.factor(df$no_response_b[Frows])]
  points(df$mass_per[Frows], pp[Frows,2], col="blue", type="l", #type="b", cex=0.45, pch=16
         ) 
  points(df$mass_per[Mrows], pp[Mrows,2], col="blue", type="l", cex=0.45, lty=2)
  points(df$mass_per[Frows], pp[Frows,3], col="darkgreen", cex=0.45, type="l", pch=16) 
  points(df$mass_per[Mrows], pp[Mrows,3], col="darkgreen", type="l", cex=0.45, lty=2) 
  points(df$mass_per[Frows], pp[Frows,4], col="darkred", 
         type="l", #type="b", cex=0.45, pch=16
         ) 
  points(df$mass_per[Mrows], pp[Mrows,4], col="darkred", type="l", cex=0.45, lty=2) 
  mtext("A", side=3, adj=0.01, line=0.5, cex=1.5, font=2)
  text(68,0.7, labels="Flew in T1 only", col="blue")
  text(-27,0.9, labels="Did Not Fly", col="red")
  text(13,0.37, labels="Flew Twice", col="darkred")
  #text(75,0.05, labels="Flew in T2 only", col="darkgreen")
  # female line is always higher
  # text(0.0,0.96, labels="F", col="red")
  # text(0.0,0.7, labels="M", col="red")
  # text(0.02,0.2, labels="F", col="blue")
  # text(0.0,0.25, labels="M", col="blue")
  # text(0.025,0.05, labels="F", col="darkgreen")
  # text(0.01,0.10, labels="M", col="darkgreen")
  # abline(v=40.5, lty=1, col="black")
  # abline(v=7.5, lty=3, col="black")
  # abline(v=-17, lty=3, col="black")
  # rect(56,-4,-24,2,col = rgb(0.5,0.5,0.5,1/4))
  # abline(v=-0.008, lty=2, col="slategrey")
  legend(83, 1.0,
         legend = c("female","male"),
         lty=1:2,
         col="black",
         cex=1.1)  
}
pp3 <- fitted(delta_mass_model)
df3 <- df # for summary purposes
plot3(df3,pp3)


## ------------------------------------------------------------------------------------------------------------------------
df <- df[with(df, order(mass_per)),]


## ------------------------------------------------------------------------------------------------------------------------
data <- data.frame(R = df$flight_case, 
         A = df$mass_per,
         B = df$sex_c,
         C = df$host_c)
model_script = paste0(source_path,"generic multinomial models- multinom 1RF + 3 FF.R")
model_comparisonsAIC(model_script)


## ------------------------------------------------------------------------------------------------------------------------
anova(m4, m7, test="Chisq") # Adding C (host plant) does not improve fit


## ------------------------------------------------------------------------------------------------------------------------
df <- df[with(df, order(mass_per)),]
df$wing2body_c = df$wing2body - mean(df$wing2body)
df$wing2body_scaled = df$wing2body_c/sd(df$wing2body)*100 # normalized and then multiplied by 100


## ------------------------------------------------------------------------------------------------------------------------
data <- data.frame(R = df$flight_case, 
         A = df$mass_per,
         B = df$sex_c,
         C = df$wing2body_c)
model_script = paste0(source_path,"generic multinomial models- multinom 1RF + 3 FF.R")
model_comparisonsAIC(model_script)


## ------------------------------------------------------------------------------------------------------------------------
anova(m7, m12, test="Chisq") # adding A*C does not improve fit
anova(m7, m13, test="Chisq") # Adding B*C does not improve fit


## ------------------------------------------------------------------------------------------------------------------------
model <- multinom(flight_case ~ mass_per + sex_c + wing2body_c, data = df)
model_table = calculate_P3(model)


## ------------------------------------------------------------------------------------------------------------------------
run_multinom_model = function(d) {
  m <- multinom(flight_case ~ mass_per + sex_c + wing2body_c, trace=FALSE, data = d)
  model_table = calculate_P3(m, print_table=FALSE)
  return(model_table)
}

# ML's below are all the same
par(mfrow=c(2,2))
MASS_PER_ML = get_significant_models(19) # mass%
SEX_ML = get_significant_models(20) # sex
WING2BODY_ML = get_significant_models(21) # wing2body


## ------------------------------------------------------------------------------------------------------------------------
# 2 vs. -1
#Odds = c("$\\frac{P(Yi=flew\\,twice)}{P(Yi=flew\\, in\\, T1\\, only)}$", " ", " ", " ")
Odds = c("$\\underline{P(Yi=flew\\,twice)}$", "$P(Yi=flew\\, in\\, T1\\, only)$", " ", " ")
effect = c("Intercept", "$\\delta$ Mass %", "Sex", "Wing-to-Body")
pars = c("$\\alpha$", "$\\beta_1$", "$\\beta_2\\, (F=1 | M=-1)$", "$\\beta_3\\,(\\mu=0)$ ")
key1 = getting_odds(Odds, effect, pars, MASS_PER_ML[[1]], 3) 

# 2 vs. 1
Odds = c("$\\underline{P(Yi=flew\\,twice)}$", "$P(Yi=flew\\, in\\, T2\\, only)$", " ", " ")
key2 = getting_odds(Odds, effect, pars, MASS_PER_ML[[3]], 3) 

# 2 vs. 0
Odds = c("$\\underline{P(Yi=flew\\,twice)}$", "$P(Yi=did\\, not\\, fly)$", " ", " ")
key3 = getting_odds(Odds, effect, pars, MASS_PER_ML[[2]], 3) 

# -1 vs. 0
Odds = c("$\\underline{P(Yi=flew\\, in\\, T1\\, only)}$", "$P(Yi=did\\, not\\, fly)$", " ", " ")
key4 = getting_odds(Odds, effect, pars, MASS_PER_ML[[2]], 1) 

# 1 vs. 0
Odds = c("$\\underline{P(Yi=flew\\, in\\, T2\\, only)}$", "$P(Yi=did\\, not\\, fly)$", " ", " ")
key5 = getting_odds(Odds, effect, pars, MASS_PER_ML[[2]], 2) 

# -1 vs. 1
Odds = c("$\\underline{P(Yi=flew\\, in\\, T1\\, only)}$", "$P(Yi=flew\\, in\\, T2\\, only)$", " ", " ")
key6 = getting_odds(Odds, effect, pars, MASS_PER_ML[[3]], 2)

key = rbind(key1, key2, key3, key4, key5, key6)
rownames(key)<-NULL
colnames(key) = c("Odds","Effect", "Parameter", "Estimate", "SE", "exp($\\beta$)*", "Wald", "p")
#  "exp($\\sum_{n=1}^{3}{\\beta_n}$)*"


## ------------------------------------------------------------------------------------------------------------------------
kable(key, caption="Table 1. Estimated Parameters, Standard Errors, and Wald Test Statistics for All Main Effects Model") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))  %>%
  column_spec(1, width="6cm", background="white") %>%
  kable_classic(html_font = "Cambria") %>%
  row_spec(c(4,8,12,16,20), extra_css = "border-bottom: 1px solid") %>%
  #column_spec(1, color = "black", background = "white") %>%
  footnote(general = " * Instead of a 1% mass increase, which is relatively too small, or a 1 unit increase in the ratio, which is 2 magnitudes higher than approximately one standard deviation from the mean, mass and wing-to-body ratio estimates were multiplied by 20 and divided by 100, respectively, before calculating the log odds. These changes better represent experimental observation and offers a more realistic impact on the odds. ",
           general_title = " ", number_title = "* "
           )


## ------------------------------------------------------------------------------------------------------------------------
# t1 vs. did not fly
#exp(-0.01 - 0.76 + 28.09/100) # 20% increase in mass, female, and 1/20 increase in the wing2body ratio length
1/40
exp(23.74/40-.57)
exp(23.74/40+.57)
exp(0.02*0 + 0.76 + 28.09/100)

min(d$wing2body) - max(d$wing2body)


## ------------------------------------------------------------------------------------------------------------------------
exp(0.02*20 - 0.76 + 28.09/100) # 20% increase in mass, female, and 1/20 increase in the wing2body ratio length

exp(0.02*0 + 0.76 + 28.09/100)

# confidence interval for wing2body ratio
exp((28.09 + 1.96*9.72)/100) 
exp((28.09 - 1.96*9.72)/100)


## ------------------------------------------------------------------------------------------------------------------------
head(pp <- fitted(model))


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
df$index = 1:nrow(df)
females = df %>%
  filter(sex=="F")
males = df %>%
  filter(sex=="M")
Frows = females$index
Mrows = males$index


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
plot4 = function(df, pp, PP, gradient=TRUE, circles=TRUE, stochasticity=TRUE, points=TRUE) {
  c = 0.65
  c2 = 1.2
  if (stochasticity) {
    plot(df$mass_per[Frows], pp[Frows,1], ylim=c(0,1), xlim=c(-40,104), col="red",type="l", 
       ylab="Flight Case Probability", xlab="Percent Change in Mass From T1 to T2 (%)", 
       lty=1, cex.axis=1.2, cex.lab=1.3) 
    points(df$mass_per[Frows], pp[Frows,2], col="blue", type="l")
    points(df$mass_per[Frows], pp[Frows,4], col="darkorange1", type="l") #darkred
  }
  if (points){
    plot(df$mass_per[Frows], PP[Frows,1], ylim=c(0,1), xlim=c(-40,104), col="red",type="l", 
       ylab="Flight Case Probability", xlab="Percent Change in Mass From T1 to T2 (%)", 
       lty=1, cex.axis=1.2, cex.lab=1.3) 
    points(df$mass_per[Frows], PP[Frows,2], col="blue", type="l")
    points(df$mass_per[Frows], PP[Frows,4], col="darkorange1", type="l") #darkred   
  }
  mtext(expression(italic("Females")), side=3, adj=0.05, line=-2, cex=1.5, font=2)
  mtext("B", side=3, adj=0.01, line=0.5, cex=1.5, font=2)
  text(64,0.7, labels="Flew in T1 only", col="blue", cex=c2)
  text(22,0.85, labels="Did Not Fly", col="red", cex=c2) 
  text(82,0.37, labels="Flew Twice", col="darkorange3", cex=c2) #maroon
  #text(53, 0.99, labels = expression(italic("flight_case ~ mass% + sex + wing2body")), cex=1.2)
  # legend(49, 1.1, 
  #        lty=1, 
  #        col=c("darkred", "blue", "darkgreen", "red"), 
  #        legend=c("Flew Twice", "Flew T1 Only", "Flew T2 Only", "Did Not Fly"), 
  #        text.col=c("darkred", "blue", "darkgreen", "red"),
  #        cex=1.1)
  
  if (gradient) {
    rbPal <- colorRampPalette(c('black','red'))
    df$w2b_col <- rbPal(10)[as.numeric(cut(df$wing2body,breaks = 10))]
    points(df$mass_per[Frows], pp[Frows, 1], pch=20, col=df$w2b_col[Frows])
    
    rbPal <- colorRampPalette(c('black','royalblue1'))
    df$w2b_col <- rbPal(10)[as.numeric(cut(df$wing2body,breaks = 10))]
    points(df$mass_per[Frows], pp[Frows, 2], pch=20, col=df$w2b_col[Frows])
    
    rbPal <- colorRampPalette(c('black','orange')) # violetred1
    df$w2b_col <- rbPal(10)[as.numeric(cut(df$wing2body,breaks = 10))]
    points(df$mass_per[Frows], pp[Frows, 4], pch=20, col=df$w2b_col[Frows])
  }

 if (circles) {
  # Mark points in the graph with high wing2body ratio vs. points with low wing2body ratio.
  test <- df[with(df, order(wing2body)),] # ascending order

  small = test %>%
    filter(sex =="F", wing2body < 0.7184934)
  large = test %>%
    filter(sex == "F", wing2body > 0.7184934)
  srows = small$index
  lrows = large$index

  points(df$mass_per[lrows], pp[lrows,1], col="red", type="p", cex=c, pch=16)
  points(df$mass_per[srows], pp[srows,1], col="red", type="p", cex=c)
  # those with smaller wing2body ratio were more likely to NOT fly
  points(df$mass_per[lrows], pp[lrows,2], col="blue", type="p", cex=c, pch=16)
  points(df$mass_per[srows], pp[srows,2], col="blue", type="p", cex=c)
  points(df$mass_per[lrows], pp[lrows,4], col="darkred", type="p", cex=c, pch=16)
  points(df$mass_per[srows], pp[srows,4], col="darkred", type="p", cex=c)
 }
  
}
pp6 = pp
df6 = df


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
plot5 = function(df, pp, PP, gradient=TRUE, circles=TRUE, stochasticity=TRUE, points=TRUE) {
  c = 0.65
  c2 = 1.2
  if (stochasticity) {
    plot(df$mass_per[Mrows], pp[Mrows,1], ylim=c(-0.00,0.8+0.05), xlim=c(-25,58), col="red",type="l", 
       ylab=" ", xlab="Percent Mass Change from T1 to T2 (%)", 
       lty=1, cex.axis=1.2, cex.lab=1.3)
    points(df$mass_per[Mrows], pp[Mrows,2], col="blue", type="l", cex=0.45, lty=1)
    points(df$mass_per[Mrows], pp[Mrows,3], col="darkgreen", type="l", cex=0.45, lty=1) 
    points(df$mass_per[Mrows], pp[Mrows,4], col="darkorange1", type="l", cex=0.45, lty=1) # darkred
  }
  if (points) {
    plot(df$mass_per[Mrows], PP[Mrows,1], ylim=c(-0.00,0.8), xlim=c(-25,58), col="red",type="l", 
       ylab=" ", xlab="Percent Mass Change from T1 to T2 (%)", 
       lty=1, cex.axis=1.2, cex.lab=1.3)
    points(df$mass_per[Mrows], PP[Mrows,2], col="blue", type="l", cex=0.45, lty=1)
    points(df$mass_per[Mrows], PP[Mrows,3], col="darkgreen", type="l", cex=0.45, lty=1) 
    points(df$mass_per[Mrows], PP[Mrows,4], col="darkorange1", type="l", cex=0.45, lty=1) # darkred
  }
  mtext(expression(italic("Males")), side=3, adj=0.05, line=-2, cex=1.5, font=2)
  mtext("C", side=3, adj=0.01, line=0.5, cex=1.5, font=2)
  text(22,0.35, labels="Flew in T1 only", col="blue", cex=c2)
  text(52,0.17, labels="Did Not Fly", col="red", cex=c2)
  text(-16, 0, labels="Flew in T2 only", col="darkgreen", cex=c2)
  text(-17,0.69, labels="Flew Twice", col="darkorange3", cex=c2) # 13,0.77, maroon, 13,0.77

  if (circles) {
    legend(39, .81+0.05, 
           pch=c(16,1), 
           title="Wing-to-body", 
           legend=c("> mean", "< mean"), 
           cex=1.1)
  }
  if (gradient) {
    text(50,0.84, labels="Wing-to-body", cex=c2)
    #label = c("0.77", "0.70", "0.63")
    # legend(39, .81+0.05, 
    #        pch=c(16), 
    #        title="Wing-to-body", 
    #        col=c("grey", "grey50", "black"),
    #        legend=label, #c("high", "low")
    #        cex=1.1)
  }
  
  if (gradient) {
    rbPal <- colorRampPalette(c('black','red'))
    df$w2b_col <- rbPal(10)[as.numeric(cut(df$wing2body,breaks = 10))]
    points(df$mass_per[Mrows], pp[Mrows, 1], pch=20, col=df$w2b_col[Mrows])
    
    rbPal <- colorRampPalette(c('black','royalblue1'))
    df$w2b_col <- rbPal(10)[as.numeric(cut(df$wing2body,breaks = 10))]
    points(df$mass_per[Mrows], pp[Mrows, 2], pch=20, col=df$w2b_col[Mrows])
    
    rbPal <- colorRampPalette(c('black','palegreen2'))
    df$w2b_col <- rbPal(10)[as.numeric(cut(df$wing2body,breaks = 10))]
    points(df$mass_per[Mrows], pp[Mrows, 3], pch=20, col=df$w2b_col[Mrows])
    
    rbPal <- colorRampPalette(c('black','orange')) #violetred1
    df$w2b_col <- rbPal(10)[as.numeric(cut(df$wing2body,breaks = 10))]
    points(df$mass_per[Mrows], pp[Mrows, 4], pch=20, col=df$w2b_col[Mrows])
  }

 if (circles) {
  # Mark points in the graph with high wing2body ratio vs. points with low wing2body ratio. 
  test <- df[with(df, order(wing2body)),] # ascending order
  #test$index
  
  small = test %>%
    filter(sex =="M", wing2body < 0.7184934)
  large = test %>%
    filter(sex == "M", wing2body > 0.7184934)
  srows = small$index
  lrows = large$index
  
  points(df$mass_per[lrows], pp[lrows,1], col="red", type="p", cex=c, pch=16)
  points(df$mass_per[srows], pp[srows,1], col="red", type="p", cex=c)
  # those with smaller wing2body ratio were more likely to NOT fly
  points(df$mass_per[lrows], pp[lrows,2], col="blue", type="p", cex=c, pch=16)
  points(df$mass_per[srows], pp[srows,2], col="blue", type="p", cex=c)
  points(df$mass_per[lrows], pp[lrows,3], col="darkgreen", type="p", cex=c, pch=16)
  points(df$mass_per[srows], pp[srows,3], col="darkgreen", type="p", cex=c)
  points(df$mass_per[lrows], pp[lrows,4], col="darkred", type="p", cex=c, pch=16)
  points(df$mass_per[srows], pp[srows,4], col="darkred", type="p", cex=c)
 }
}


## ----echo=FALSE, fig.width=0.7, fig.height=1-----------------------------------------------------------------------------
# Function to plot color bar
color.bar <- function(lut, min, max=77, nticks=3, ticks=seq(min, max, len=nticks), title='') {
    scale = (length(lut)-1)/(max-min)
    
    final_ticks=seq(min/100, max/100, len=nticks)
    #final_ticks = c("0.63", "0.66", "0.70", "0.74", "0.77")
    final_ticks = c("0.63", "0.70", "0.77")
    
    #dev.new(width=1.75, height=5)
    plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
    axis(2, ticks, las=1, labels=final_ticks)
    for (i in 1:(length(lut)-1)) {
     y = (i-1)/scale + min
     rect(0,y,10,y+1/scale, col=lut[i], border=NA)
    }
}


## ------------------------------------------------------------------------------------------------------------------------
# getting those small subset plots and scales in the top right hand corner.
plot_histograms = function() {
  x = 0.48
  v <- c(x-0.13,x, 0.85, 0.90)
  par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  hist(df$wing2body[Frows], col="white", main="", cex.axis=0.9) 
  text(0.64,45, labels="w2b", cex=0.9)

  x = 0.88
  v <- c(x-0.13, x, 0.85, 0.90)
  par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  hist(df$wing2body[Mrows], col="white", main="", cex.axis=0.9, xlim=c(0.64,0.78)) 
  text(0.66,40, labels="w2b", cex=0.9)
}
plot_color_scale = function() {
  x = 0.98
  v <- c(x-0.025, x, 0.75, 0.86)
  par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  color.bar(colorRampPalette(c("black", "grey"))(1000), 63)
}


## ----echo=FALSE, fig.width=3.2*1.7*2, fig.height=2.8*2-------------------------------------------------------------------
par(mfrow=c(1,2), tcl=-0.5) # length of tick marks set at default

par(mai=c(1,0.85,0.4,0)) # bottom, right, top, left
plot4(df6,pp6, pp3, gradient=TRUE, circles=FALSE, stochasticity=TRUE, points=FALSE)

par(mai=c(1,0.6,0.4,0.05))
plot5(df6,pp6, pp3, gradient=TRUE, circles=FALSE, stochasticity=TRUE, points=FALSE)

plot_histograms()
plot_color_scale()

## ----echo=FALSE----------------------------------------------------------------------------------------------------------
plot3 = function(df, pp) {
  c2 = 1.2
  plot(df$mass_per[Frows], pp[Frows,1], ylim=c(0,1), xlim=c(-40,104), col="red",
     type="l",
     ylab="Flight Case Probability", 
     xlab="Percent Mass Change From T1 to T2 (%)", lty=1, cex.axis=1.2, cex.lab=1.3)  
  points(df$mass_per[Mrows], pp[Mrows,1], col="red", type="l", cex=0.45, lty=2) 
  points(df$mass_per[Frows], pp[Frows,2], col="blue", type="l", #type="b", cex=0.45, pch=16
         ) 
  points(df$mass_per[Mrows], pp[Mrows,2], col="blue", type="l", cex=0.45, lty=2)
  points(df$mass_per[Frows], pp[Frows,3], col="darkgreen", cex=0.45, type="l", pch=16) 
  points(df$mass_per[Mrows], pp[Mrows,3], col="darkgreen", type="l", cex=0.45, lty=2) 
  points(df$mass_per[Frows], pp[Frows,4], col="darkorange3", type="l") 
  points(df$mass_per[Mrows], pp[Mrows,4], col="darkorange2", type="l", cex=0.45, lty=2, lwd=1.3) 
  mtext("A", side=3, adj=0.01, line=0.5, cex=1.5, font=2)
  text(68,0.7, labels="Flew in T1 only", col="blue", cex=c2)
  text(-27,0.9, labels="Did Not Fly", col="red", cex=c2)
  text(13,0.37, labels="Flew Twice", col="darkorange3", cex=c2)
  text(70,0.05, labels="Flew in T2 only", col="darkgreen", cex=c2)
  legend(83, 1.0,
         legend = c("female","male"),
         lty=1:2,
         col="black",
         cex=1.1)  
}


## ----echo=FALSE, fig.width=3.2*1.7*2, fig.height=2.8*2-------------------------------------------------------------------
par(mfrow=c(1,1), tcl=-0.5)
#plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')
#par(mai=c(1,0,0.4,0.05)) 
plot3(df3,pp3)


## ------------------------------------------------------------------------------------------------------------------------
df <- d %>%
  filter(!is.na(egg_diff), !is.na(mass_diff), !is.na(flew_diff), sex_c == 1)

df <- df[with(df, order(mass_diff)),]

n_tfemales = nrow(df)

df$flight_case <- relevel(as.factor(df$flight_case), ref = "0")


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
data_fem = data_tested %>%
    filter(sex=="F")
bar2 = function() {
  # fig.height=2.4, fig.width=2.7
  counts <- table(data_fem$eggs_b, data_fem$trial_type) # data_tested$flew_b
  counts <- counts[1:2,2:3]
  barplot(counts, #main="Flight Distribution by Trial",
    xlab="Trial", col=c("lightblue","darkgoldenrod2"),
    legend = c("no eggs", "yes eggs"), ylim=c(0,110), beside=TRUE)
} 
bar2()


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
# key = response in T2 - response in T1
Event = c("laid eggs in both trials", "laid eggs in T2 only", "laid eggs in neither trials", "laid eggs in T1 only")
Encoding = c(2, 1,0,-1)
key = cbind(Event, Encoding)

kable(key) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))  %>%
  kable_classic(html_font = "Cambria") %>%
  add_header_above(c("Delta Flight Response Key" = 2 )) 


## ------------------------------------------------------------------------------------------------------------------------
df <- d %>%
  filter(!is.na(egg_case), !is.na(mass_diff), !is.na(flew_diff), sex_c == 1)

df <- df[with(df, order(mass_diff)),]

n_tfemales = nrow(df)

df$flight_case <- relevel(as.factor(df$flight_case), ref = "0")


## ------------------------------------------------------------------------------------------------------------------------
df$flight_case = droplevels(df$flight_case) # no female bug only flew in T2
null <- multinom(flight_case ~ 1, data = df) 


## ------------------------------------------------------------------------------------------------------------------------
model <- multinom(flight_case ~ egg_case, data = df) 
model_table = calculate_P(model)

## ------------------------------------------------------------------------------------------------------------------------
anova(null, model, test="Chisq") # adding egg_case improves fit


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
run_multinom_model = function(d) {
  m <- multinom(flight_case ~ egg_case, trace=FALSE, data = d) 
  model_table = calculate_P(m, print_table=FALSE)
  return(model_table)
}
ML_eqs = get_significant_modelsf(7) #-1/0 egg case not sig | 0/-1 or 2/-1 egg case not sig | -1/2 egg case not sig | but 0/2 and 2/0 egg case**


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
# key 
Host = c("Golden Rain Tree (GRT)", "Balloon Vine (BV)")
Encoding = c(1,-1)
key = cbind(Host, Encoding)

kable(key) %>%
  kable_styling(bootstrap_options = "striped", "hover", full_width = F)  %>%
  add_header_above(c("Host Plant Key" = 2 )) 


## ------------------------------------------------------------------------------------------------------------------------
data <- data.frame(R = df$flight_case, 
         A = df$egg_case,
         B = df$mass_diff,
         C = df$host_c)
model_script = paste0(source_path,"generic multinomial models- multinom 1RF + 3 FF.R")
model_comparisonsAIC(model_script)

## ------------------------------------------------------------------------------------------------------------------------
anova(m4, m7, test="Chisq") # Adding C does not improve fit
anova(m7, m13, test="Chisq") # Adding  mass_diff*host does not improve fit

## ------------------------------------------------------------------------------------------------------------------------
#model <- multinom(flight_case ~ mass_diff + egg_case, data = df) 
df <- df[with(df, order(mass_per)),]
model <- multinom(flight_case ~ mass_per + egg_case, data = df) 
model_table = calculate_P2(model, "mass_per", "egg_case")


## ------------------------------------------------------------------------------------------------------------------------
exp(0.02*20 + 1.10) # egg case = -1
exp(0.02*20 - 1.10) # egg case = 1
exp(0.02*20) # egg case = 0
exp(0.02*20 - 1.10*2) # egg case = 2


## ------------------------------------------------------------------------------------------------------------------------
run_multinom_model = function(d) {
  m <- multinom(flight_case ~ mass_per + egg_case, trace=FALSE, data = d) 
  model_table = calculate_P2(m, "mass_per", "egg_case", print_table=FALSE)
  return(model_table)
}
par(mfrow=c(1,2)) 
ML_eqs = get_significant_modelsf(15) # mass_per 
ML_eqs = get_significant_modelsf(16) # egg_case


## ------------------------------------------------------------------------------------------------------------------------
# 2 vs. -1
Odds = c("$\\underline{P(Yi=flew\\,twice)}$", "$P(Yi=flew\\, in\\, T1\\, only)$", " ")
effect = c("Intercept", "$\\delta$ Mass %", "Egg Response")
pars = c("$\\alpha$", "$\\beta_1$", "$\\beta_2$")
key1 = getting_oddsf(Odds, effect, pars, ML_eqs[[1]], 2) 

# 2 vs. 0
Odds = c("$\\underline{P(Yi=flew\\,twice)}$", "$P(Yi=did\\, not\\, fly)$", " ")
key2 = getting_oddsf(Odds, effect, pars, ML_eqs[[2]], 2) 

# -1 vs. 0
Odds = c("$\\underline{P(Yi=flew\\, in\\, T1\\, only)}$", "$P(Yi=did\\, not\\, fly)$", " ")
key3 = getting_oddsf(Odds, effect, pars, ML_eqs[[2]], 1) 

key = rbind(key1, key2, key3)
rownames(key)<-NULL
colnames(key) = c("Odds","Effect", "Parameter", "Estimate", "SE", "exp($\\beta$)*", "Wald", "p")


## ------------------------------------------------------------------------------------------------------------------------
kable(key, caption="Table 2. Estimated Parameters, Standard Errors, and Wald Test Statistics for All Main Effects Model") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))  %>%
  column_spec(1, width="6cm", background="white") %>%
  kable_classic(html_font = "Cambria") %>%
  row_spec(c(3,6,9), extra_css = "border-bottom: 1px solid")  %>%
  footnote(general = " * Instead of a 1% mass increase, which is relatively too small, mass percent estimates were multiplied by 20 before calculating the log odds. These changes better represent experimental observations and offers a more realistic odds.",
           general_title = " ", number_title = "* "
           )


## ------------------------------------------------------------------------------------------------------------------------
tb = model_table
# -1 / 2 | Flew in T1, not T2
I = (tb[1,1] - tb[2,1])
M = (tb[1,2] - tb[2,2])
E = (tb[1,3] - tb[2,3])
EQ1 = paste0("log(pi_-1 / pi_2) = ", round(I, 2), " + ", round(M,2), " Mass %", " + ", round(E, 2), " Egg Case", "     Flew in T1, rather than both" )
EQ1  # mass_dff** | sex not


## ------------------------------------------------------------------------------------------------------------------------
head(pp <- fitted(model))

## ----echo=FALSE----------------------------------------------------------------------------------------------------------
df$index = 1:nrow(df)

eggT1 = df %>%
  filter(egg_case==-1)
egg0 = df %>%
  filter(egg_case==0)
egg2 = df %>%
  filter(egg_case==2)
eggT2 = df %>%
  filter(egg_case==1)

eggT1_rows = eggT1$index
egg_0rows = egg0$index
egg_2rows = egg2$index
eggT2_rows = eggT2$index


## ----echo=FALSE, fig.width=3.2*1.7*2, fig.height=2.8*2-------------------------------------------------------------------
plot6 = function(df, pp) {
  # only laid eggs in T1
  plot(df$mass_per[eggT1_rows], pp[eggT1_rows,1], ylim=c(0,1.05), xlim=c(-36,108), col="red", type="l", lty=1,main="Females Only", ylab="Flight Case Probability", xlab="Percent Change in Mass From T1 to T2 (g)")
  # xlim=c(-0.045,0.07)
  points(df$mass_per[eggT1_rows], pp[eggT1_rows,2], col="blue", type="l", lty=1, cex=0.45) 
  points(df$mass_per[eggT1_rows], pp[eggT1_rows,3], col="black", type="l", lty=1, cex=0.45) 
  # no egg change
  points(df$mass_per[egg_0rows], pp[egg_0rows,1], col="red", type="l", lty=2) # did not fly in either
  points(df$mass_per[egg_0rows], pp[egg_0rows,2], col="blue", type="l", lty=2) # flew in T1 only
  points(df$mass_per[egg_0rows], pp[egg_0rows,3], col="black", type="l", lty=2) # flew in both
  # eggs twice
  points(df$mass_per[egg_2rows], pp[egg_2rows,1], col="red", type="l", lty=4) # did not fly in either
  points(df$mass_per[egg_2rows], pp[egg_2rows,2], col="blue", type="l", lty=4) # flew in T1 only
  points(df$mass_per[egg_2rows], pp[egg_2rows,3], col="black", type="l", lty=4) # flew in both
  # only laid eggs in T2
  points(df$mass_per[eggT2_rows], pp[eggT2_rows,1], col="red", type="l", lty=3) # flew in neither
  points(df$mass_per[eggT2_rows], pp[eggT2_rows,2], col="blue", type="l", lty=3) # flew in T1 only
  points(df$mass_per[eggT2_rows], pp[eggT2_rows,3], col="black", type="l", lty=3) # flew in both
  
  # rect(-0.038,-1,0.064,2, NA, col = rgb(0.5,0,0.5,1/15), border="pink") # most likely to not fly unless gain more than about 0.025 g mass and then fly only in T1 (eggs were laid twice).
  # rect(-0.045,-1,0.038,2, NA, col = rgb(0.5,0.2,0.5,1/15), border="pink") # most likely to not fly (2nd widest mass change, and laid eggs in T2)
  # rect(-0.039,-1,0.008,2, NA, col = rgb(0.1,0.5,0.5,1/10), border="lightblue") # most likely to fly twice (lost mass and laid eggs only in T1)
  # rect(-0.005,-1,0.029,2, NA, col = rgb(0.4,0.5,0.5,1/4), border="lightblue") # most likely to fly twice always (smallest mass change and did not lay eggs)
  # 
  text(70,0.68, labels="Flew in T1 only", col="blue") # 0.059,0.54
  text(-20,0.95, labels="Did Not Fly", col="red") # -0.036,0.95
  text(-20,0.55, labels="Flew Twice", col="black") # -0.02,0.35
  legend(76, 1.07,
         legend = c("laid eggs in T1","no eggs laid", "eggs laid twice", "laid eggs in T2"),
         lty=1:4,
         col="black",
         cex=0.8)
}
pp5 = fitted(model)
df5 = df
plot6(df5,pp5) 


## ------------------------------------------------------------------------------------------------------------------------
df <- df[with(df, order(mass_diff)),]
matrix = rbind(cbind(df$mass_diff[eggT1_rows], 
            pp[eggT1_rows,1],pp[eggT1_rows,2], pp[eggT1_rows,3]),
      cbind(df$mass_diff[egg_0rows], pp[egg_0rows,1], pp[egg_0rows,2],
            pp[egg_0rows,3]), 
      cbind(df$mass_diff[egg_2rows], pp[egg_2rows,1], pp[egg_2rows,2],
            pp[egg_2rows,3]),
      cbind(df$mass_diff[eggT2_rows], pp[eggT2_rows,1], pp[eggT2_rows,2], 
            pp[eggT2_rows,3]))
colnames(matrix) = c("mass_diff", "No", "T1", "Twice")
#write.csv(matrix, "data/animate_eggs.csv") # for interactive graphs

df <- df[with(df, order(mass_per)),]
matrix = rbind(cbind(df$mass_per[eggT1_rows], 
            pp[eggT1_rows,1],pp[eggT1_rows,2], pp[eggT1_rows,3]),
      cbind(df$mass_per[egg_0rows], pp[egg_0rows,1], pp[egg_0rows,2],
            pp[egg_0rows,3]), 
      cbind(df$mass_per[egg_2rows], pp[egg_2rows,1], pp[egg_2rows,2],
            pp[egg_2rows,3]),
      cbind(df$mass_per[eggT2_rows], pp[eggT2_rows,1], pp[eggT2_rows,2], 
            pp[eggT2_rows,3]))
colnames(matrix) = c("mass_per", "No", "T1", "Twice")
#write.csv(matrix, "data/animate_eggs-perc.csv") # for interactive graphs


## ------------------------------------------------------------------------------------------------------------------------
df <- d %>%
  filter(!is.na(egg_case), !is.na(mass_diff), !is.na(flew_diff), !is.na(wing2body), sex_c == 1)

df <- df[with(df, order(mass_diff)),]

n_tfemales = nrow(df)

df$flight_case <- relevel(as.factor(df$flight_case), ref = "0")


## ----warning=FALSE-------------------------------------------------------------------------------------------------------
data <- data.frame(R = df$flight_case, 
         A = df$egg_case,
         B = df$mass_diff,
         C = df$wing2body)
model_script = paste0(source_path,"generic multinomial models- multinom 1RF + 3 FF.R")
model_comparisonsAIC(model_script)

## ------------------------------------------------------------------------------------------------------------------------
anova(m4, m7, test="Chisq") # adding wing2body does not include fit
anova(m7, m12, test="Chisq") # Adding A*C does not improve fit
anova(m7, m11, test="Chisq") 
anova(m7, m13, test="Chisq") 
anova(m4, m8, test="Chisq") # Adding A*B does not improve fit


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
# key = response in T2 - response in T1
Event = c("flew in both trials", "flew in T2 only", " flew in neither trials", "flew in T1 only")
Encoding = c(2, 1,0,-1)
key = cbind(Event, Encoding)

kable(key) %>%
  kable_styling(bootstrap_options = "striped", "hover", full_width = F)  %>%
  add_header_above(c("Delta Flight Response Key" = 2 )) 


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
# key = change in mass
Event = c("gained mass from T1 to T2", "no mass change between trails", "lost mass from T1 to T2")
Sign = c("pos","0","neg")
key = cbind(Event, Sign)

kable(key) %>%
  kable_styling(bootstrap_options = "striped", "hover", full_width = F)  %>%
  add_header_above(c("Delta Mass Key" = 2 )) 


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
# key 
Host = c("Golden Rain Tree (GRT)", "Balloon Vine (BV)")
Encoding = c(1,-1)
key = cbind(Host, Encoding)

kable(key) %>%
  kable_styling(bootstrap_options = "striped", "hover", full_width = F)  %>%
  add_header_above(c("Host Plant Key" = 2 )) 


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
# key = response in T2 - response in T1
Event = c("flew in both trials", "flew in neither trial", "flew in T1 only")
Encoding = c(2,0,-1)
key = cbind(Event, Encoding)

kable(key) %>%
  kable_styling(bootstrap_options = "striped", "hover", full_width = F)  %>%
  add_header_above(c("Delta Flight Response Key" = 2 )) 


## ----echo=FALSE----------------------------------------------------------------------------------------------------------
# key = response in T2 - response in T1
Event = c("laid eggs in both trials", "laid eggs in T2 only", "laid eggs in neither trials", "laid eggs in T1 only")
Encoding = c(2, 1,0,-1)
key = cbind(Event, Encoding)

kable(key) %>%
  kable_styling(bootstrap_options = "striped", "hover", full_width = F)  %>%
  add_header_above(c("Delta Egg Response Key" = 2 )) 


## ----echo=FALSE, fig.width=3.2*1.7*2, fig.height=2.8*2-------------------------------------------------------------------
plot2(df2,pp2)


## ----echo=FALSE, fig.width=3.2*1.7*2, fig.height=2.8*2-------------------------------------------------------------------
plot3(df3,pp3)


## ----echo=FALSE, fig.width=3.2*1.7*2, fig.height=2.8*2-------------------------------------------------------------------
par(mfrow=c(1,2), tcl=-0.5) # length of tick marks set at default

par(mai=c(1,0.85,0.4,0)) # bottom, right, top, left
plot4(df6,pp6, pp3, gradient=TRUE, circles=FALSE, stochasticity=TRUE, points=FALSE)

par(mai=c(1,0.6,0.4,0.05))
plot5(df6,pp6, pp3, gradient=TRUE, circles=FALSE, stochasticity=TRUE, points=FALSE)

plot_histograms()
plot_color_scale()


## ----echo=FALSE, fig.width=3.2*1.7*2, fig.height=2.8*2-------------------------------------------------------------------
plot6(df5,pp5)

