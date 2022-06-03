
df = read.csv("data/Exp1_clean.csv")

#Libraries
library(plyr);library(dplyr);
library(tidyverse);library(infer)
library(rstatix)
library(lme4)

#Functions
se <- function(x) {sqrt(var(x)/length(x))}
midround <- function(x,base){
  base*round(x/base)
}

# For graphing
them <-   theme(axis.title.x = element_text(face="bold", size=12,
                                            margin=margin(b=20, t=15)),
                axis.text.x  = element_text(size=12, color='black', hjust=0.4,
                                            margin=margin(t=3)),
                axis.title.y = element_text(face="bold", angle=90, size=12,
                                            margin=margin(l=20, r=15)),
                axis.text.y  = element_text(size=12, color='black', vjust=0.45,
                                            margin=margin(r=3)),
                axis.line = element_line(color='black', size=0),
                axis.ticks.length=unit(5, 'pt'),
                strip.text.x = element_text(face="bold", size=12, 
                                            margin = margin(t=10, b=10, unit='pt')),
                strip.text.y = element_text(face="bold", size=12, 
                                            margin = margin(t=10, b=10, unit='pt')),
                strip.background = element_blank(),
                legend.position = 'right',
                legend.text  = element_text(size=10, face='bold', 
                                            margin=margin(t=5, b=5, r=18, l=0, unit='pt')),
                legend.spacing.y = unit(6, 'pt'),
                legend.key.width = unit(15, 'pt'),
                legend.key.height = unit(6, 'pt'),
                legend.background = element_rect(colour = NA), 
                legend.title= element_text(size=11, face='bold'),
                panel.background =  element_blank(), 
                panel.border = element_rect(fill = NA, color = "black", size=1),
                panel.spacing = unit(1, 'lines'),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank()
)

df$nValDiff = df$X.L.R.
df$task2 <- factor(df$task, levels = c("experiment_VB","experiment_FR"), 
                   labels = c("Value-Based Reward","Accuracy-Based Reward"))

# Look at the entire experiment
glm <- glmer(formula = accuracy ~ I(totalValue/10)*task2+absRelativeValue+
               (1|subject)+(0+I(totalValue/10)|subject)+(0+absRelativeValue|subject)+(0+task2|subject),
             data=hddm_data3[hddm_data3$absRelativeValue>0,],
             family = binomial(link="logit"),
             glmerControl(optimizer="bobyqa",
                          optCtrl=list(maxfun=2e5)))
summary(glm)

# Run it for each condition separately
glmVB <- glmer(formula = accuracy ~ I(totalValue/10)+absRelativeValue+nValDiff+
                 (1|subject)+(0+I(totalValue/10)|subject)+(0+absRelativeValue|subject)+(0+nValDiff|subject),
               data=hddm_data3[hddm_data3$task=="experiment_VB" & hddm_data3$absRelativeValue>0,],
               family = binomial(link="logit"),
               glmerControl(optimizer="bobyqa",
                            optCtrl=list(maxfun=2e5)))
glmFR <- glmer(formula = accuracy ~ I(totalValue/10)+absRelativeValue+nValDiff+
                 (1|subject)+(0+I(totalValue/10)|subject)+(0+absRelativeValue|subject)+(0+nValDiff|subject),
               data=hddm_data3[hddm_data3$task=="experiment_FR" & hddm_data3$absRelativeValue>0,],
               family = binomial(link="logit"),
               glmerControl(optimizer="bobyqa",
                            optCtrl=list(maxfun=2e5)))
summary(glmVB)
summary(glmFR)

# Plot the results
png(file="results/exp1_accuracy_valType.png",
    width = 800, height = 600
    )
df %>%
  mutate(value_type = ifelse(totalValue > quantile(totalValue, probs = .75), "High-Value",
                             ifelse(totalValue < quantile(totalValue, probs = .25), "Low-Value", 
                                    "Middle-Value")),
         value_type = factor(value_type, levels = c("Low-Value","Middle-Value","High-Value")),
         relative_value_bin = midround(abs(relativeValue), 2)) %>%
  group_by(subject, task2,value_type, relative_value_bin) %>%
  summarise(acc = mean(accuracy)) %>%
  ggplot(aes(x = value_type, y = acc, color = value_type)) +
    them + 
    stat_summary() +
  guides(color = "none") +
  labs(y = "P(Choose Best)",
       x = "Value Type") +
  facet_grid(~task2)
dev.off()
