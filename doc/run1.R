
library(dplyr)
library(R2jags)
library(rjags)
library(rstanarm)

df <- readRDS("inst/default_data/df1.rds")
df <- readRDS("inst/default_data/df2.rds")
df <- readRDS("inst/default_data/df3.rds")


n = nrow(df)
iso.c <- recode$country.code
iso.i <- df$country.code
C <- length(iso.c)
name.c <- recode$name
region.c <- recode$sub_region
major.c <- recode$Major.area
regnames <- unique(region.c)
R <- length(regnames)

# geth.i <- Getc.i(house.i, house.h)
# geth.c <- Getc.i(iso.h, iso.c)
#getc.h <- Getc.i(iso.i = , iso.c = name.c)
getr.c <- Getc.i(iso.i = region.c, iso.c = regnames)
getc.i <- Getc.i(iso.i = paste(iso.i), iso.c = paste(iso.c))
region.i <- region.c[getc.i]
# add a check here .. idk why but we get warnings
getr.i <- getr.c[getc.i]
df$region <- getr.i %>% as.factor()
getr.c <- Getc.i(iso.i = region.c, iso.c = regnames)
#need to dummy encode for jags

#just variables in common model for refference
# as.factor(cooking_fuel_cat1) + bmi + as.factor(wealth_index_3_v190) + age_v012 + as.factor(sub_region) + as.factor(insurance_v481) + as.factor(education_cat_v106
jagsdata <- list(y.i = df$birth_weight_f,
                 fuel.i = df$fuel1,
                 bmi.i = df$bmi,
                 water.i = df$water,
                 insur.i = df$insurance,
                 edu.i = df$education,
                 age1.i = as.numeric(df$age<20),
                 age2.i = as.numeric(df$age>=35),
                 wealth1.i = as.numeric(df$wealth==1),
                 wealth2.i = as.numeric(df$wealth==2),
                 getc.i = getc.i,
                 #getitrain.j,
                 C  = C, R = R, n = n,
                 getr.c = getr.c,
                 regnames = regnames, name.c = name.c, iso.c = iso.c)
WriteModelbinom()
mod <- jags.parallel(
  data = jagsdata,
  parameters.to.save = c("alpha.c","beta1","beta2","beta3", "beta4", "beta5", "beta6", "beta7"),
  model.file = "model.txt",
  n.chains = 3, n.iter = 1500, n.burnin = 500)
save(mod, file = "data2/model1.rda")

mcmc.array <- mod$BUGSoutput$sims.array

max(mod$BUGSoutput$summary[, "Rhat"]) # not yet ok for final inference but ok for first checking
which.max(mod$BUGSoutput$summary[, "Rhat"])

get_percentiles <- function(par) {
  return(mod$BUGSoutput$summary[par, c("2.5%","50%","97.5%")] %>% exp %>% round(2))
}
get_percentiles("beta1")
pnames <- c("beta1","beta2","beta3", "beta4", "beta5", "beta6", "beta7")
lapply(pnames, get_percentiles)

#a.c[getc.i[i]] + beta1*fuel.i[i] + beta2*insur.i[i] + beta3*edu.i[i] + beta4*age1.i[i] + beta5*age2.i[i] + beta6*wealth1.i[i] + beta7*wealth2.i[i]
