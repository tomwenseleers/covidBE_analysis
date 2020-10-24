# PLOTS OF HOSPITALISATIONS & EXTRAPOLATIONS ####
# author: Tom Wenseleers
# Date last modified: 2020-10-24

# stacked column plot of hospitalisations over time in different provinces on a linear scale

# hospit_byprovince = function(from=as.Date("2020-07-1"), 
#                              to=Sys.Date()) { 
#   from = as.Date(from)
#   to = as.Date(to)
#   df = rawhospit %>%
#     mutate(DATE = as.Date(DATE)) %>%
#     filter(#REGION != "Belgium", # & PROVINCE != "All",
#       between(DATE, from, to)) %>%
#     na.omit()
#   return(df) }
# source("scripts/downloadData.R") # Liège data were not imported for some reason, so just getting it directly from Sciensano here
# data_hosp = hospit_byprovince(from=as.Date("2020-03-1"), to=Sys.Date())
data_hosp = read.csv("https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv", encoding="UTF-8")
s = aggregate(data_hosp$TOTAL_IN, by=list(PROVINCE=data_hosp$PROVINCE), FUN=sum)
data_hosp$PROVINCE = as.factor(data_hosp$PROVINCE) 
data_hosp$PROVINCE = factor(data_hosp$PROVINCE, levels=s$PROVINCE[order(s$x)]) # order by total nr of hospitalisations
data_hosp_prov = data_hosp[data_hosp$PROVINCE!="All",]
# data_hosp_total = data_hosp[data_hosp$PROVINCE=="All"&data_hosp$REGION=="Belgium",]

library(mgcv)
library(splines)
data_hosp_prov$DATE = as.Date(data_hosp_prov$DATE)
data_hosp_prov$DATE_NUM = as.numeric(data_hosp_prov$DATE)
data_hosp_prov$WEEKDAY = as.factor(weekdays(as.Date(data_hosp_prov$DATE)))
fit_totalin = gam(TOTAL_IN ~ s(DATE_NUM, by=PROVINCE, bs="ad", m=c(3,2,1,0)) + PROVINCE + WEEKDAY, # or m=NA for default
                  family=poisson, data=data_hosp_prov) 
# we could also contemplate gamm model with correlation=corAR1() or corAR1(form = ~ 1 | PROVINCE) or
# a Poisson GLM with TOTAL_IN ~ ns(DATE_NUM,df=20)*PROVINCE+WEEKDAY
# see https://fromthebottomoftheheap.net/2020/06/03/extrapolating-with-gams/ for 
# some nice tests on extrapolating with GAMs
fit_totalinicu = gam(TOTAL_IN_ICU ~ s(DATE_NUM,by=PROVINCE,bs="ad", m=c(3,2,1,0))+PROVINCE+WEEKDAY, # or m=NA for default
                  family=poisson, data=data_hosp_prov) 
# calculate expected marginal means over average weekday + make extrapolations
library(emmeans)
daystoextrapol = 14 # with 14 day extrapolation

preds_totalin = as.data.frame(emmeans(fit_totalin, ~DATE_NUM+PROVINCE, 
                                      at=list(DATE_NUM=seq(min(data_hosp_prov$DATE_NUM),
                                                           max(data_hosp_prov$DATE_NUM)+daystoextrapol)), # with exptrapolation 
                                      type="response", data=data_hosp_prov))[,-c(4,5)]
colnames(preds_totalin)[3:5]=c("TOTAL_IN_SMOOTH","TOTAL_IN_SMOOTH_LCL","TOTAL_IN_SMOOTH_UCL")
preds_totalin = data.frame(DATE=as.Date(preds_totalin$DATE_NUM, origin="1970-01-01"), preds_totalin)
data_hosp_prov2 = merge(data_hosp_prov, preds_totalin, by=c("DATE","PROVINCE"), all=TRUE)

preds_totalinicu = as.data.frame(emmeans(fit_totalinicu, ~DATE_NUM+PROVINCE, 
                                      at=list(DATE_NUM=seq(min(data_hosp_prov$DATE_NUM),
                                                           max(data_hosp_prov$DATE_NUM)+daystoextrapol)), # with exptrapolation 
                                      type="response", data=data_hosp_prov))[,-c(4,5)]
colnames(preds_totalinicu)[3:5]=c("TOTAL_IN_ICU_SMOOTH","TOTAL_IN_ICU_SMOOTH_LCL","TOTAL_IN_ICU_SMOOTH_UCL")
preds_totalinicu = data.frame(DATE=as.Date(preds_totalinicu$DATE_NUM, origin="1970-01-01"), preds_totalinicu)
data_hosp_prov2 = merge(data_hosp_prov2, preds_totalinicu, by=c("DATE","PROVINCE"), all=TRUE)

data_hosp_prov_total = aggregate(data_hosp_prov2$TOTAL_IN_SMOOTH,by=list(data_hosp_prov2$DATE),FUN=sum)
data_hosp_prov_total[which(data_hosp_prov_total$x>14000)[1],] 
# by 3d of Nov PHASE 2B of 14 000 hospitalised Covid patients could be exceeded
data_hosp_prov_total_icu = aggregate(data_hosp_prov2$TOTAL_IN_ICU_SMOOTH,by=list(data_hosp_prov2$DATE),FUN=sum)
data_hosp_prov_total_icu[which(data_hosp_prov_total_icu$x>2000)[1],] 
# by 4th of Nov FASE 2B of 2000 hospitalised Covid patients would be exceeded

data_hosp_prov2$DATE_NUM = as.numeric(data_hosp_prov2$DATE)
data_hosp_prov2$EXTRAPOLATED = relevel(factor(ifelse(data_hosp_prov2$DATE_NUM>=max(data_hosp_prov$DATE_NUM),"yes","no")),ref="no")
data_hosp_prov2$TOTAL_IN[is.na(data_hosp_prov2$TOTAL_IN)] = 0 
data_hosp_prov2$TOTAL_IN_ICU[is.na(data_hosp_prov2$TOTAL_IN_ICU)] = 0 

# plot of total nr of hospitalised patients
plot_hosp = ggplot(data=data_hosp_prov2, aes(x=DATE, y=TOTAL_IN+1)) + 
  geom_area(aes(y=TOTAL_IN_SMOOTH, colour=NULL, fill=PROVINCE, alpha=EXTRAPOLATED)) +
  scale_alpha_manual(guide=FALSE, values=c(0,0.5)) +
  geom_area(aes(lwd=I(1.2), colour=NULL, fill=PROVINCE), position="stack") + 
  # facet_wrap(~PROVINCE, nrow=2) +
  # scale_y_log10() +
  scale_color_discrete("", h=c(270, 0), c=150) +
  scale_fill_discrete("", h=c(270, 0), c=150)  +
  scale_x_continuous(breaks=as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01")),
                     labels=months(as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01")))) +
  guides(color = guide_legend(reverse=T, nrow=2, byrow=T), fill = guide_legend(reverse=T, nrow=2, byrow=T)) +
  theme_hc() + theme(legend.position="bottom", # c(0.8,0.7)) 
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank()) + 
  geom_hline(yintercept = 300*5, lwd=I(0.5), lty=3) + # Hospital Contigency Plan Action Phases
  geom_hline(yintercept = 500*5, lwd=I(0.5), lty=2) +
  geom_hline(yintercept = 1000*5, lwd=I(0.8), lty=2) +
  geom_hline(yintercept = 1500*7, lwd=I(0.8), lty=1) +
  geom_hline(yintercept = 2000*7, lwd=I(1.2), lty=1) +
  labs(title = "HOSPITALISED COVID PATIENTS (IC+NON-IC) IN BELGIUM",
       subtitle = "with Hospital Contigency Plan Action Phases (HTSC)") +
  geom_text(data=data.frame(x=as.Date(c("2020-07-1")),
                            y=c(300*5,500*5,1000*5,1500*7,2000*7),
                            label=c("phase 0","phase 1A","phase 1B","phase 2A","phase 2B")), 
            aes(x=x, y=y, label=label, vjust=-0.4))
plot_hosp
ggsave("hospitalisations_by_province_stacked.png", width = 7, height = 5)

# plot of total nr of patients at ICU
plot_icu = ggplot(data=data_hosp_prov2, aes(x=DATE, y=TOTAL_IN_ICU)) + 
  geom_area(aes(y=TOTAL_IN_ICU_SMOOTH, colour=NULL, fill=PROVINCE, alpha=EXTRAPOLATED)) +
  scale_alpha_manual(guide=FALSE, values=c(0,0.5)) +
  geom_area(aes(lwd=I(1.2), colour=NULL, fill=PROVINCE), position="stack") + 
  # facet_wrap(~PROVINCE, nrow=2) +
  scale_color_discrete("", h=c(270, 0), c=150) +
  scale_fill_discrete("", h=c(270, 0), c=150)  +
  scale_x_continuous(breaks=as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01")),
                     labels=months(as.Date(c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01")))) +
  guides(color = guide_legend(reverse=T, nrow=2, byrow=T), fill = guide_legend(reverse=T, nrow=2, byrow=T)) +
  theme_hc() + theme(legend.position="bottom", # c(0.8,0.7)) 
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank()) +
  geom_hline(yintercept = 300, lwd=I(0.5), lty=3) + # Hospital Contigency Plan Action Phases
  geom_hline(yintercept = 500, lwd=I(0.5), lty=2) +
  geom_hline(yintercept = 1000, lwd=I(0.8), lty=2) +
  geom_hline(yintercept = 1500, lwd=I(0.8), lty=1) +
  geom_hline(yintercept = 2000, lwd=I(1.2), lty=1) +
  labs(title = "COVID PATIENTS AT INTENSIVE CARE IN BELGIUM",
       subtitle = "with Hospital Contigency Plan Action Phases (HTSC)") +
  geom_text(data=data.frame(x=as.Date(c("2020-07-1")),
                            y=c(300,500,1000,1500,2000),
                            label=c("phase 0","phase 1A","phase 1B","phase 2A","phase 2B")), 
            aes(x=x, y=y, label=label, vjust=-0.4))
plot_icu
ggsave("ICU_by_province_stacked.png", width = 7, height = 5)

library(ggpubr)
ggarrange(plot_hosp, plot_icu, ncol=1, common.legend=TRUE, legend="bottom")
ggsave("hospitalisations_ICU_by_province_stacked.png", width=7, height=10)
