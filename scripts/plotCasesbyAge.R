# Plot increase in cases across different age groups
# author: Tom Wenseleers
# Date last modified: 2020-10-14

source("scripts/downloadData.R")

slice = function(from=as.Date("2020-07-1"), 
                 to=Sys.Date()-3) { 
  from = as.Date(from)
  to = as.Date(to)
  tmp = full_join(filter(rawcases,
                          SEX == "All",
                          (AGEGROUP != "All") & (AGEGROUP != "unknown")),
                   rawhospit,
                   by = c("PROVINCE","REGION","DATE")) %>%
    filter(PROVINCE == "All")
  df = tmp %>%
        mutate(DATE = as.Date(DATE)) %>%
        filter(REGION == "Belgium",
            between(DATE, from, to)) %>%
            na.omit()
  df$AGEGROUP = factor(df$AGEGROUP, levels=rev(unique(df$AGEGROUP)))
  return(df) }



data_casesage = read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv", encoding="UTF-8")

# increase of confirmed nr of cases (loess smoothed) in different age categories over time
library(ggthemes)
data = slice(from=as.Date("2020-03-1"), to=Sys.Date()-3)
qplot(data=data, x=DATE, y=CASES, colour=AGEGROUP, geom="smooth", 
      method="loess", # or method="gam", formula = y ~ s(x, bs = "cs"),
      se=FALSE, lwd=I(1.2)) + scale_y_log10() +
  scale_color_discrete(h=c(270, 0), c=150) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_hc() 
ggsave("cases_by_age.png", width = 7, height = 5)


# normalized stacked column plot of age distribution over time using raw data
data = slice(from=as.Date("2020-03-1"), to=Sys.Date()-3)
ggplot(data=data, aes(x=DATE, y=CASES, colour=AGEGROUP, fill=AGEGROUP)) + geom_col(position = "fill") + 
  scale_color_discrete(h=c(270, 0), c=150) +
  scale_fill_discrete(h=c(270, 0), c=150)  +
  guides(color = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE)) +
  ylab("PROPORTION OF NEW CONFIRMED CASES") +
  theme_hc() 
ggsave("cases_by_age_proportions.png", width = 7, height = 5)

# normalized stacked column plot of age distribution over time using log10(raw data+1)
data = slice(from=as.Date("2020-03-1"), to=Sys.Date()-3)
ggplot(data=data, aes(x=DATE, y=log10(CASES+1), colour=AGEGROUP, fill=AGEGROUP)) + geom_col(position = "fill") + 
  scale_color_discrete(h=c(270, 0), c=150) +
  scale_fill_discrete(h=c(270, 0), c=150)  +
  guides(color = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE)) +
  ylab("PROPORTION OF NEW CONFIRMED CASES") +
  theme_hc() 
ggsave("cases_by_age_proportions.png", width = 7, height = 5)


# stacked column plot of age distribution over time using raw data on a linear scale
ggplot(data=data, aes(x=DATE, y=CASES, colour=AGEGROUP, fill=AGEGROUP)) + geom_col() + 
  scale_color_discrete(h=c(270, 0), c=150) +
  scale_fill_discrete(h=c(270, 0), c=150)  +
  guides(color = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE)) +
  ylab("NUMBER OF NEW CONFIRMED CASES") +
  theme_hc() 
ggsave("cases_by_age_stacked.png", width = 7, height = 5)

library(mgcv)
data$DATE_NUM = as.numeric(data$DATE)
data$WEEKDAY = weekdays(as.Date(data$DATE))
fit = gamm(CASES ~ s(DATE_NUM, by=AGEGROUP)+AGEGROUP+WEEKDAY, 
           correlation=corAR1(), # or corAR1(form = ~ 1 | AGEGROUP)
           family=poisson, data=data)
library(splines)
fit = glm(CASES ~ ns(DATE_NUM,df=20)*AGEGROUP+WEEKDAY, 
           family=poisson, data=data)
fit = gam(CASES ~ s(DATE_NUM,by=AGEGROUP,bs="ps")+AGEGROUP+WEEKDAY, 
          family=poisson, data=data)
library(emmeans)
x = unique(data$DATE_NUM)
df = list(DATE_NUM=x) # AGEGROUP=levels(data$AGEGROUP)
preds = as.data.frame(emmeans(fit, ~DATE_NUM+AGEGROUP, 
                               at=df, 
                               type="response", data=data))
data2 = merge(data,preds,by=c("DATE_NUM","AGEGROUP"))
head(data2)
# emmeans(fit, ~DATE_NUM, at=list(DATE_NUM=seq(min(data$DATE_NUM),max(data$DATE_NUM), by=1)), type="response", data=data)
# data$CASES_SMOOTH = y_pred[match(data$DATE_NUM,x)]
ggplot(data=data2, aes(x=DATE, y=rate, colour=AGEGROUP, fill=AGEGROUP)) + 
  geom_area(position = 'fill') +
  geom_col(aes(x=DATE, y=CASES), colour=NA, alpha=I(0.3), position = 'fill') + 
  scale_color_discrete(h=c(270, 0), c=150) +
  scale_fill_discrete(h=c(270, 0), c=150)  +
  guides(color = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE)) +
  ylab("PROPORTION OF NEW CONFIRMED CASES") +
  theme_hc() 
ggplot(data=data2, aes(x=DATE, y=log10(rate+1), colour=AGEGROUP, fill=AGEGROUP)) + 
  geom_area(position = 'stack') +
  geom_col(aes(x=DATE, y=log10(CASES+1)), colour=NA, alpha=I(0.3), position = 'stack') + 
  # facet_wrap(~AGEGROUP, ncol=1,scales="free")+
  scale_color_discrete(h=c(270, 0), c=150) +
  scale_fill_discrete(h=c(270, 0), c=150)  +
  guides(color = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE)) +
  ylab("NUMBER OF NEW CONFIRMED CASES") +
  theme_hc() 


# stacked column plot of age distribution over time using raw data with log10(y+1) counts
data$LOG10CASES = log10(data$CASES+1)
ggplot(data=data, aes(x=DATE, y=LOG10CASES, colour=AGEGROUP, fill=AGEGROUP)) + geom_col() + 
  scale_color_discrete(h=c(270, 0), c=150) +
  scale_fill_discrete(h=c(270, 0), c=150)  +
  guides(color = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE)) +
  ylab("LOG10(NUMBER OF NEW CONFIRMED CASES+1)") +
  theme_hc() 
ggsave("cases_by_age_stacked_log10.png", width = 7, height = 5)
