require(dplyr)
require(tidyr)
load("prepared.data.Rdata")
source("scripts/shinydash/04-Functions.R")
## summarise actual results
###############################################################################
results.orig %>%
  summarise(registered.count=sum(Electorate),
            remain.count=sum(Remain),
            leave.count=sum(Leave)) %>%
  mutate(turnout.count = remain.count + leave.count,
         turnout.prop = turnout.count/registered.count,
         remain.prop = remain.count/turnout.count,
         leave.prop = leave.count/turnout.count) -> results.summary

## because the completeness data is for 7 age groups, not 6 as the
## rest of the turnout and resutls estimates, we need the UK
## population data to weight it properly:
###############################################################################
# first tidy up the UK. pop data
UK.population.orig %>% 
  gather(age, count, 4:94) %>%
  mutate(age=as.numeric(substring(age, 2)),
         age.group = cut(age, c(17, 19, 24, 34, 44, 54, 64, 91))) %>%
  select(age, age.group,count) ->
  UK.population.tidy 

# then get totals for 7 age groups
UK.population.tidy %>%
  select(age.group, count) %>%
  group_by(age.group) %>%
  summarise(count = sum(count)) %>%
  filter(!is.na(age.group)) %>%
  mutate(age.group = as.character(completeness.orig$age.group) )->
  UK.population.7age.groups

# now merge with complteness and reduce to 6 groups
full_join(UK.population.7age.groups,
          completeness.orig) %>%
  mutate(registered.count = count*registration.prop,
         age.group = c("18-24", "18-24",
                       UK.population.7age.groups$age.group[3:7])) %>%
  group_by(age.group) %>%
  summarise(count = sum(count),
            registered.count= sum(registered.count),
            registered.prop = registered.count/count) %>%
  full_join(turnout.orig) %>%
  full_join((results.LA.orig)) -> all.6age.groups

rm(UK.population.7age.groups, completeness.orig, UK.population.orig)

# ## compare with results 
# estimates.summary <- FunCalculateResult(all.6age.groups)
# left_join(gather(estimates.summary, var, value), 
#           gather(results.summary, var, value),
#           by=c("var"="var"))

## Now we add the new registrations
# group new registrtions into 6 groups
registrations.grouped.orig %>%
  mutate(gr = c(0,0,1,2,3,4,5,6,6,7)) %>% 
  filter(!gr %in% c(0,7)) %>%
  group_by(gr) %>%
  summarise(count=sum(sum)) %>%
  ungroup() -> registrations.6age.groups


# add the new registrations, recalculate reg.prop and tweak turnout.prop:
all.6age.groups %>%
  mutate(registered.count= registered.count + registrations.6age.groups$count,
         registered.prop = registered.count / count,
         turnout.prop = c(40, 62, 72.5, 77, 81, 83)/100) %>%
  select(-registered.count) ->
  all.6age.groups 

# estimates.summary <- FunCalculateResult(all.6age.groups)
# ## compare with results 
# left_join(gather(estimates.summary, var, value), 
#           gather(results.summary, var, value),
#           by=c("var"="var"))
# check plot:


# OK, now let's add the life expectancy
UK.life.exp.orig$ex[91] <- mean(UK.life.exp.orig[UK.life.exp.orig$X.x>=90,6])
inner_join(UK.population.tidy, UK.life.exp.orig, by=c("age"="X.x"))%>%
  select(age, count, ex) %>%
  filter(age>=18)  %>%
  mutate(years.left = count*ex,
         age.group = cut(age, c(17,  24, 34, 44, 54, 64, 91))) %>%
  group_by(age.group) %>%
  summarise(years.left=sum(years.left)) ->  life.expectancy
all.6age.groups$years.left <- life.expectancy$years.left

rm(life.expectancy, 
   registrations.6age.groups,
   registrations.grouped.orig,
   results.orig,
   results.LA.orig,
   turnout.orig,
   UK.life.exp.orig,
   FunBestPlot,
   FunCalculateResult)
# FunBestPlot(all.6age.groups, base = "lexp")
# estimates.summary <- FunCalculateResult(all.6age.groups, base = "lexp")

save(results.summary,
     all.6age.groups,
     UK.population.tidy ,
     file="AllReady.Rdata")

save(results.summary,
     all.6age.groups,
     UK.population.tidy,
     file="scripts/shiny/AllReady.Rdata")
save(results.summary,
     all.6age.groups,
     UK.population.tidy,
     file="scripts/shinydash/AllReady.Rdata")

getwd()
