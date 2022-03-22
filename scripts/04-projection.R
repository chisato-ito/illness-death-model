####################### Run after 03-remission #################################
# Specify projection time frame-------------------------------------------------
yearsToModel  <- 2019:2030
nProjYears    <- length(yearsToModel)

# Select relevant years from Destatis population projection
N             <- t(as.matrix(pop[pop$Variante == 2 & # Variant2:
                                  pop$mw == sexString2 &
                                  pop$Simulationsjahr %in% yearsToModel, 5+ages]))
dt            <- 1/52 #temporal resolution in units per year: weekly
timeSteps     <- seq(from = yearsToModel[1], to = yearsToModel[nProjYears], by = dt)
nTimeSteps    <- length(timeSteps)
ageSteps      <- seq(from = ages[1], to = ages[nAges], by = dt)
nAgeSteps     <- length(ageSteps)

S.hd <- matrix(data = 0, nrow = nAgeSteps, ncol = nTimeSteps) # susceptible
C.hd <- matrix(data = 0, nrow = nAgeSteps, ncol = nTimeSteps) # cases

# Upsample population with an implicit assumption that births are uniformly distributed
# over year
# People entering population
N0.hd <- dt*approx(yearsToModel, N[1,], xout = timeSteps)$y 
# Age-distribution at the start year
N.hd <- dt*approx(ages, N[,1], xout = ageSteps)$y

# Initial point in time
p_        <- fct_p(timeSteps[1], ageSteps)
S.hd[,1]  <- N.hd*(1 - p_)
C.hd[,1]  <- N.hd*p_

for (tIdx in 2:nTimeSteps) {
  ii_ <- fct_inc(timeSteps[tIdx], ageSteps[-1])
  rr_ <- fct_rem(timeSteps[tIdx], ageSteps[-1])
  m0  <- fct_m0 (timeSteps[tIdx], ageSteps[-1])
  m1  <- fct_m1 (timeSteps[tIdx], ageSteps[-1])
  
  # S, C from previous step
  Sp_ <- S.hd[-nAgeSteps, tIdx-1]
  Cp_ <- C.hd[-nAgeSteps, tIdx-1]
  
  # S, C in actual step
  S_  <- Sp_ * (1 - (ii_ + m0)*dt) + Cp_ * rr_*dt
  C_  <- Cp_ * (1 - (rr_ + m1)*dt) + Sp_ * ii_*dt
  
  S.hd[,tIdx] <- c(N0.hd[tIdx], S_)
  C.hd[,tIdx] <- c(          0, C_)
}

# Total cases for each time step
timeStepsum <- colSums(C.hd)
df.C <- data.frame("year" = timeSteps, "cases" = timeStepsum)
# Export to CSV
write_csv(df.C, file = paste0("out_data/",sexString,".csv"))

# Table summary: IDM vs prevalence extrapolation -------------------------------
# Data frame for table summary
df.C.yr <- df.C %>%
  filter(year %in% yearsToModel) %>% 
  mutate(changevsbase = cases - cases[year == 2019]) %>% 
  mutate(perchangevsbase = changevsbase/cases[year == 2019]*100)

# Simple "prevalence extrapolation" for each projection year
p.extrap <- yearsToModel
for (i in 1:ncol(N)) {
  p.extrap[i] <- sum(N[,i]*fct_p(2018+i, ages))
}
df.C.yr <- df.C.yr %>% 
  add_column(p.extrap) %>% 
  mutate(changevsbase_ = p.extrap - p.extrap[year == 2019]) %>% 
  mutate(perchangevsbase_ = changevsbase_/p.extrap[year == 2019]*100)

# Table summary
tab1 <- df.C.yr %>% 
  filter(year %in% c("2019","2020","2025","2030")) %>% 
  gt() %>% 
  tab_header(title = "Estimated number of people with anxiety disorders",
             subtitle = sexString) %>% 
  tab_spanner(label = "Illness-death model", 
              columns = c(cases, changevsbase, perchangevsbase)) %>% 
  tab_spanner(label = "Extrapolation of prevalence", 
              columns = c(p.extrap, changevsbase_, perchangevsbase_)) %>% 
  cols_label(year = "Year", cases = "cases (thousands)",
             changevsbase = "change from 2019 (thousands)", 
             perchangevsbase = "% change from 2019",
             p.extrap = "cases (thousands)", 
             changevsbase_ = "change from 2019 (thousands)",
             perchangevsbase_ = "% change from 2019") %>% 
  fmt_number(columns = 2:7, decimals = 2) %>% 
  tab_options(heading.title.font.size = "medium", heading.subtitle.font.size = "medium", 
              column_labels.font.size = 14) 
gtsave(tab1, filename = paste0("Table1_",sexString,".rtf"), path = "figures")
