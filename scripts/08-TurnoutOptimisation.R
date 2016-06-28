### degrouping not worikin..
load("AllReady.Rdata")

all.6age.groups %>%
  mutate(registered.count= registered.count + registrations.6age.groups$count,
         registered.prop = registered.count / count) %>%
  select(-registered.count) ->
  all.6age.groups.old

source("scripts/shinydash/04-Functions.R")
abs((FunCalculateResult(all.6age.groups.old)[c(5,8:9)]-results.summary[5:7]))

x <- all.6age.groups.old

require(alabama)

fn <- function(par) sum((par[1:5]-par[2:6]^2),
                        (par[7:11]-par[8:12]^2),
                        (par[13:17]-par[14:18]^2))

heq <- function(par) {
  all.6age.groups.old$registered.prop <- par[1:6]
  all.6age.groups.old$turnout.prop <- par[7:12]
  all.6age.groups.old$remain.prop <- par[13:18]
  h <- rep(NA, 1)
  h[1] <- as.numeric(FunCalculateResult(all.6age.groups.old)[5]) - 0.7215
  h[2] <- as.numeric(FunCalculateResult(all.6age.groups.old)[8]) - 0.4811
  h[3] <- as.numeric(FunCalculateResult(all.6age.groups.old)[9]) - 0.5189
  h
}


hin <- function(par) {
  all.6age.groups.old$registered.prop <- par[1:6]
  all.6age.groups.old$turnout.prop <- par[7:12]
  all.6age.groups.old$remain.prop <- par[13:18]
  h <- rep(NA, 1)
  h[1:18] <- par
  h[19:36] <- -(par-1)
  h[37:41]  <- -(par[1:5]-par[2:6])
  h[42] <- par[1] - 0.8
  h[43] <- par[6] - 0.95
  h
}
par <- p0
p0 <- c(all.6age.groups.old$registered.prop,
        all.6age.groups.old$turnout.prop,
        all.6age.groups.old$remain.prop )

ans <- constrOptim.nl(par=p0, fn=fn,  heq=heq, hin=hin)


plot(ans$par)
