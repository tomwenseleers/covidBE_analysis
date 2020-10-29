# FIT PDF OF HOSPITALISATION, IC ADMISSION oR DEATH USING NONNEGATIVE POISSON GLMs ####
# author: Tom Wenseleers
# Date last modified: 2020-10-24

data_hosp = read.csv("https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv", encoding="UTF-8")
head(data_hosp)

data_mort = read.csv("https://epistat.sciensano.be/Data/COVID19BE_MORT.csv", encoding="UTF-8")
head(data_mort)

data_cases_age = read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv", encoding="UTF-8")
head(data_cases_age)

data_cases_muni = read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_MUNI.csv", encoding="UTF-8")
head(data_cases_muni)

data_cases_muni_cum = read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_MUNI_CUM.csv", encoding="UTF-8")
head(data_cases_muni_cum)

data_tests = read.csv("https://epistat.sciensano.be/Data/COVID19BE_tests.csv", encoding="UTF-8")
head(data_tests)

