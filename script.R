# Reproduction code for "Gender differences on the age-related distal-to-proximal 
# shift in joint kinetics during running"
# 
# author: Simon Nolte
# date: Nov 2026
#
# part of the I4R Sports Science Project

# load packages
library(readxl) # data import
library(car)    # ANOVA analysis
library(purrr)  # functional programming

#### PART 0: PREPARE DATA -------------------------------------------

# data is taken from the link in the article's data availability statement
# https://docs.google.com/spreadsheets/d/1WitvPg_7cfSngXeJnzNpqyy6UwF5cDj_/edit?gid=984014039#gid=984014039
# I cannot include the data in this repository, because of unclear licensing
# download the data as data.xslx in the project folder
pi <- read_excel("data.xlsx", sheet = "Participant Info", n_max = 47)
names(pi)[1:12] <- c("Subject", "AgeY", "Group", "AgeG", "Sex", "Mass", "Height", "BMI", "Years", "Wk", "Pace", "Speed")
d <- read_excel("data.xlsx", sheet = "All Results", n_max = 47)
names(d)[3] <- "AgeG"
# calculate relative speed
pi$Speed_rel <- 100 * 2.7 / pi$Speed
# convert distance variables from miles to km
pi$Wk_km <- pi$Wk * 1.60934
pi$Pace_km <- pi$Pace / 1.60934

# function to calculate 2x2 between-subject anova (Type III SPSS default)
options(contrasts = c("contr.sum", "contr.poly")) # sets contrasts as in SPSS
p3 <- function(x, dd) { a <- car::Anova(lm(x ~ factor(dd$AgeG) * factor(dd$Sex)), type = 3); unname(a$`Pr(>F)`[2:4]) }

# function to make a table for descriptive and ANOVA data
make_table <- function(var, x, round = TRUE) {
  p <- p3(x[[var]], x)
  t <- data.frame(
    item = var,
    mean_young_w = mean(x[[var]][x$Group == 1]),
    sd_young_w = sd(x[[var]][x$Group == 1]),
    mean_young_m = mean(x[[var]][x$Group == 3]),
    sd_young_m = sd(x[[var]][x$Group == 3]),
    mean_old_w = mean(x[[var]][x$Group == 2]),
    sd_old_w = sd(x[[var]][x$Group == 2]),
    mean_old_m = mean(x[[var]][x$Group == 4]),
    sd_old_m = sd(x[[var]][x$Group == 4]),
    p_age = p[1],
    p_gender = p[2],
    p_interaction = p[3]
  )
  if (round) {
    t[,2:9] <- round(t[,2:9], 2)
    t[,10:12] <- round(t[,10:12], 3)
  }
  t
}

# PART 1: REPRODUCTION -------------------------------------------

# Table 1 Reproduction
t1_vars <- c("AgeY", "Mass", "Height", "BMI", "Wk_km", "Years", "Pace_km", "Speed_rel")
t1 <- lapply(t1_vars, make_table, x = pi) |> purrr::list_rbind()

# all reproduce, besides p-values for preferred pace and p-values for relative
# preferred pace (interaction is definitely incorrect here)

# preferred pace p-values are actually prefered speed:
make_table("Speed", pi) # confirmed

# Table 2 Reproduction
t2_vars <- c("Hip_EXT_Moment", "Knee_EXT_Moment", "Ankle_PF_Moment", "HipEccPower", "HipConcPower", "KneeEccPower", "KneeConcPower", "AnkleEccPower", "AnkleConcPower", "HpNegWk", "HpPosWk", "KnNegWk", "KnPosWk", "AnNegWk", "AnPosWk", "TotalNeg", "TotalPos", "Biomech Plasticity")
t2 <- lapply(t2_vars, make_table, x = d) |> purrr::list_rbind()
t2

# all reproduce, but note that p-value for age effect for ankle positive is above 0.05
# * although it is labelled as significant
# in addition, incorrect labels (superscripts) for ratio

# Table 3 Reproduction
t3_vars <- c("Stance Time", "STRIKE INDEX", "Fy1", "Fy2", "Fz")
t3 <- lapply(t3_vars, make_table, x = d) |> purrr::list_rbind()
t3
# all reproduce

# PART 2: FURTHER FINDINGS -------------------------------------------

## 2.1 Inclusion criteria -----------------------------------------------------
sum(pi$AgeY[pi$AgeG == 1] > 30) # 10 participants over 30 years in young group
sum(pi$AgeY[pi$AgeG == 2] > 70) # 1 participant over 70 years in middle-old group
sum(pi$Wk_km < 16)              # 3 participants with running distance under 16km/wk

# exclude all participants outside the criteria:
d_excl_all <- d[((pi$AgeY <= 30 | pi$AgeY >= 50) & pi$AgeY <= 70) & pi$Wk_km > 16,]
pi_excl_all <- pi[((pi$AgeY <= 30 | pi$AgeY >= 50) & pi$AgeY <= 70) & pi$Wk_km > 16,]
nrow(d_excl_all) # 33 (14 participants excluded)

# reproduce finding in restricted data set
t1_excl <- lapply(t1_vars, make_table, x = pi_excl_all) |> purrr::list_rbind()
p3(pi_excl_all$Speed, pi_excl_all)
# gender effect on BMI: from 0.019 to 0.220
# * gender effect on preferred speed: from 0.017 to 0.290
# gender effect on relative pace: from 0.003 to 0.189

t2_excl <- lapply(t2_vars, make_table, x = d_excl_all) |> purrr::list_rbind()
# age effect on hip negative joint power: from 0.013 to 0.084
# * age effect on hip positive joint power: from 0.018 to 0.462
# X gender effect on knee positive joint power: from 0.28 to 0.024
# * age effect on hip positive joint work: from 0.003 to 0.061
# age effect on knee negative joint work: from 0.005 to 0.056
# age effect on ankle negative joint work: from 0.023 to 0.070
# *X(SEE ABOVE) age effect on ankle positive joint work: from 0.052 to 0.021
# gender effect on ankle positive joint work: from 0.018 to 0.168

t3_excl <- lapply(t3_vars, make_table, x = d_excl_all) |> purrr::list_rbind()
# gender effect on stance time: from 0.042 to 0.154
# age effect on stride index: from 0.021 to 0.131

# X from non-significant to significant
# * abstract finding

## 2.2 Identical data rows -------------------------------------------------------
dup <- d[c(36, 39),]
# same data for 16 variables

# if one participant is removed
d_dedup <- d[-39,]
pi_dedup <- pi[-39,]

lapply(t1_vars, make_table, x = pi_dedup) |> purrr::list_rbind()
# no changes

lapply(t2_vars, make_table, x = d_dedup) |> purrr::list_rbind()
# age effect for ankle plantarflexor torque from 0.038 to 0.052
# *(SEE ABOVE) age effect for ankle positive work: from 0.052 to 0.072

lapply(t3_vars, make_table, x = d_dedup) |> purrr::list_rbind()
# gender effect for stance time from 0.042 to 0.070

## 2.3 Normality assessment -----------------------------------------------------
test_normality <- function(var, x) {
  # test normality for raw data
  p_raw <- shapiro.test(x[[var]])$p.value
  # test normality for ANOVA residuals
  r <- resid(lm(x[[var]] ~ factor(x[["AgeG"]]) * factor(x[["Sex"]])))
  p_res <- shapiro.test(r)$p.value
  data.frame(
    item = var,
    p_raw = p_raw,
    p_raw_sig = ifelse(p_raw < 0.05, TRUE, FALSE),
    p_res = p_res,
    p_res_sig = ifelse(p_res < 0.05, TRUE, FALSE)
  )
}

n1 <- lapply(t1_vars, test_normality, x = pi) |> purrr::list_rbind()
n2 <- lapply(t2_vars, test_normality, x = d) |> purrr::list_rbind()
n3 <- lapply(t3_vars, test_normality, x = d) |> purrr::list_rbind()
# minus first entry because here no ANOVA was calculated
n_all <- rbind(n1[-1,], n2, n3)

# number of variables with violation of normality
sum(n_all$p_raw_sig) # 16
sum(n_all$p_res_sig) # 10

# rank-ANOVA instead:
# ankle negative work
p3(rank(d$AnNegWk), d) # main effect of age from 0.023 to 0.078
# hip:ankle positive ratio
p3(rank(d$`Biomech Plasticity`), d) # main effect of gender from 0.094 to 0.022

## 2.5 Effect size mismatch ---------------------------------------------------
# function to calculate cohen's d across groups
cohen <- function(var, x, by) {
  if (by == "age") {
    a <- x[[var]][x$AgeG == 1]
    b <- x[[var]][x$AgeG == 2]
  } else if (by == "gender") {
    a <- x[[var]][x$Sex == 1]
    b <- x[[var]][x$Sex == 2]
  }
  (mean(a) - mean(b)) / metapp::sd_pooled(sd(a), sd(b), length(a), length(b))
}

# effect sizes in section 3.1
sapply(c("Mass", "Height", "BMI", "Pace_km", "Speed_rel"), cohen, x = pi, by = "gender")
# slightly of for height, rel Speed reported at 1.0 recalculated at 0.71
# match for other
sapply(c("Years", "Pace_km"), cohen, x = pi, by = "age")

# effect sizes in section 3.2
sapply(c("AnkleConcPower", "AnPosWk", "Hip_EXT_Moment", "HipConcPower", "Biomech Plasticity", "Fy2", "Stance Time", "STRIKE INDEX"), cohen, x = d, by = "gender")
# ankle positive work: reported as 1.0, calculated as 0.72
# ratio: reported as 0.3, calculated as 0.48
# peak propulsive GRF: reported as 0.6, calculated as 0.77
# stance time: reported as 0.4, calculated as 0.62
# rest matching more or less when considering rounding

sapply(c("Ankle_PF_Moment", "AnkleConcPower", "AnPosWk", "AnNegWk", "HipEccPower", "HipConcPower", "HipEccPower", "HpPosWk", "KnNegWk", "Biomech Plasticity", "Fy2", "Fz", "STRIKE INDEX"), cohen, x = d, by = "age")
# hip concentric power; reported as 0.9, calculated as 0.71
# knee joint neg work: reported as 0.3, calculated as 0.88
# rest matches

## 2.6 Family-wise error ------------------------------------------------------

# get exact p-values
alld <- rbind(
  lapply(t1_vars[-1], make_table, x = pi[-1,], round = FALSE) |> purrr::list_rbind(),
  lapply(t2_vars, make_table, x = d, round = FALSE) |> purrr::list_rbind(),
  lapply(t3_vars, make_table, x = d, round = FALSE) |> purrr::list_rbind()
)
# main claims (in order of comment table)
main <- c(6, 36, 22, 16, 46, 18, 12, 42)

p_vals <- c(alld$p_age, alld$p_gender)
length(p_vals) # 60
# among them significant
sum(p_vals < 0.05) # 26
# after BH-correction
p_bh <- p.adjust(p_vals, "BH")
sum(p_bh < 0.05) # 17
# after Holm correction
p_holm <- p.adjust(p_vals, "holm")
sum(p_holm < 0.05) # 4

# abstract findings
p_vals[main]
p_bh[main]
# (age effect on ankle positive work from 0.052 to 0.116)
# age effect on hip positive power from 0.018 to 0.053
# gender effect hip positive power from 0.029 to 0.074
p_holm[main]
# all become insignificant

## 2.7 create Table 1 ---------------------------------------------------------

# restrict data to participants inside inclusion criteria
alld_excl_all <- rbind(
  lapply(t1_vars[-1], make_table, x = pi_excl_all[-1,], round = FALSE) |> purrr::list_rbind(),
  lapply(t2_vars, make_table, x = d_excl_all, round = FALSE) |> purrr::list_rbind(),
  lapply(t3_vars, make_table, x = d_excl_all, round = FALSE) |> purrr::list_rbind()
)
p_vals_excl_all <- c(alld_excl_all$p_age, alld_excl_all$p_gender)
p_vals_excl_all[main]

# remove duplicated row
alld_dedup <- rbind(
  lapply(t1_vars[-1], make_table, x = pi_dedup[-1,], round = FALSE) |> purrr::list_rbind(),
  lapply(t2_vars, make_table, x = d_dedup, round = FALSE) |> purrr::list_rbind(),
  lapply(t3_vars, make_table, x = d_dedup, round = FALSE) |> purrr::list_rbind()
)
p_vals_dedup <- c(alld_dedup$p_age, alld_dedup$p_gender)
p_vals_dedup[main]
