# Plot increase in cases across different age groups
# author: Tom Wenseleers
# Date last modified: 2020-10-14

source("scripts/downloadData.R")

slice = function(from=as.Date("2020-07-1"), to=Sys.Date()-3) { 
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



# increase of confirmed nr of cases (loess smoothed) in different age categories over time
library(ggthemes)
data = slice(from=as.Date("2020-07-1"), to=Sys.Date()-3)
qplot(data=data, x=DATE, y=CASES, colour=AGEGROUP, geom="smooth", 
      method="loess", # or method="gam", formula = y ~ s(x, bs = "cs"),
      se=FALSE, lwd=I(1.2)) + scale_y_log10() +
  scale_color_discrete(h=c(270, 0), c=150) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_hc() 
ggsave("cases_by_age.png", width = 7, height = 5)


# stacked column plot of age distribution over time using raw data
data = slice(from=as.Date("2020-03-1"), to=Sys.Date()-3)
ggplot(data=data, aes(x=DATE, y=CASES, colour=AGEGROUP, fill=AGEGROUP)) + geom_col(position = "fill") + 
  scale_color_discrete(h=c(270, 0), c=150) +
  scale_fill_discrete(h=c(270, 0), c=150)  +
  guides(color = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE)) +
  ylab("PROPORTION OF NEW CONFIRMED CASES") +
  theme_hc() 
ggsave("cases_by_age_stacked.png", width = 7, height = 5)
