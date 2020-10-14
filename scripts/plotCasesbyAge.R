# Plot increase in cases across different age groups
# author: Tom Wenseleers
# Date last modified: 2020-10-14

source("scripts/downloadData.R")

tmp <- full_join(filter(rawcases,
                        SEX == "All",
                        (AGEGROUP != "All") & (AGEGROUP != "unknown")),
                 rawhospit,
                 by = c("PROVINCE","REGION","DATE")) %>%
  filter(PROVINCE == "All")

slice <- tmp %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(REGION == "Belgium",
         between(DATE, as.Date("2020-07-1"),
                 Sys.Date()-3)) %>%
  na.omit()

slice$AGEGROUP = factor(slice$AGEGROUP, levels=rev(unique(slice$AGEGROUP)))

library(ggthemes)
qplot(data=slice, x=DATE, y=CASES, colour=AGEGROUP, geom="smooth", 
      method="loess", # or method="gam", formula = y ~ s(x, bs = "cs"),
      se=FALSE, lwd=I(1.2)) + scale_y_log10() +
  scale_color_discrete(h=c(270, 0), c=150) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_hc() 
ggsave("cases_by_age.png", width = 7, height = 5)
