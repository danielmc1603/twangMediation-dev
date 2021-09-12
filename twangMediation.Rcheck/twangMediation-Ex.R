pkgname <- "twangMediation"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "twangMediation-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('twangMediation')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("NSDUH_female")
### * NSDUH_female

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NSDUH_female
### Title: A dataset containing the substance use condition and sexual
###   orientation of 40293 women respondents to the 2017 & 2018 National
###   Survey of Drug Use and Health.
### Aliases: NSDUH_female
### Keywords: datasets

### ** Examples

## Not run: 
##D data(NSDUH_female)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NSDUH_female", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bal.table.mediation")
### * bal.table.mediation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bal.table.mediation
### Title: Compute the balance table for mediation object.
### Aliases: bal.table.mediation

### ** Examples

data("tMdat")

## tMdat is small simulated data set included in twangMediation for 
## demonstrating the functions. See ?tMdat for details

fit.es.max <- wgtmed(M ~ w1 + w2 + w3,
                      data = tMdat,
                      a_treatment = "A",
                      y_outcome = "Y",
                      total_effect_wts = tMdat$te.wgt,
                      method = "logistic"
                      )

bal.table.mediation(fit.es.max)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bal.table.mediation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("desc.effects")
### * desc.effects

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: desc.effects
### Title: Describe the effects
### Aliases: desc.effects

### ** Examples

data("tMdat")

## tMdat is small simulated data set included in twangMediation for 
## demonstrating the functions. See ?tMdat for details

fit.es.max <- wgtmed(M ~ w1 + w2 + w3,
                      data = tMdat,
                      a_treatment = "A",
                      y_outcome = "Y",
                      total_effect_wts = tMdat$te.wgt,
                      method = "ps",
                      ps_n.trees=1500,
                      ps_shrinkage=0.01,
                      ps_stop.method=c("es.max")
                      )

desc.effects(fit.es.max)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("desc.effects", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("desc.effects.mediation")
### * desc.effects.mediation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: desc.effects.mediation
### Title: Describe the effects from a mediation object
### Aliases: desc.effects.mediation

### ** Examples

data("tMdat")

## tMdat is small simulated data set included in twangMediation for 
## demonstrating the functions. See ?tMdat for details

fit.es.max <- wgtmed(M ~ w1 + w2 + w3,
                      data = tMdat,
                      a_treatment = "A",
                      y_outcome = "Y",
                      total_effect_wts = tMdat$te.wgt,
                      method = "ps",
                      ps_n.trees=1500,
                      ps_shrinkage=0.01,
                      ps_stop.method=c("es.max")
                      )

desc.effects(fit.es.max)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("desc.effects.mediation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dx.wts.mediation")
### * dx.wts.mediation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dx.wts.mediation
### Title: Compute diagnostics assessing covariates balance.
### Aliases: dx.wts.mediation

### ** Examples

data("tMdat")

## tMdat is small simulated data set included in twangMediation for 
## demonstrating the functions. See ?tMdat for details

fit.es.max <- wgtmed(M ~ w1 + w2 + w3,
                      data = tMdat,
                      a_treatment = "A",
                      y_outcome = "Y",
                      total_effect_wts = tMdat$te.wgt,
                      method = "ps",
                      ps_n.trees=1500,
                      ps_shrinkage=0.01,
                      ps_stop.method=c("es.max")
                      )

## dx.wts.mediation is used internally by bal.table.mediation,
##   print.mediation, and summary.mediation

summary(fit.es.max)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dx.wts.mediation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.mediation")
### * plot.mediation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.mediation
### Title: Plot the 'mediation' object.
### Aliases: plot.mediation

### ** Examples

data("tMdat")

## tMdat is small simulated data set included in twangMediation for 
## demonstrating the functions. See ?tMdat for details

fit.es.max <- wgtmed(M ~ w1 + w2 + w3,
                      data = tMdat,
                      a_treatment = "A",
                      y_outcome = "Y",
                      total_effect_wts = tMdat$te.wgt,
                      method = "ps",
                      ps_n.trees=1500,
                      ps_shrinkage=0.01,
                      ps_stop.method=c("es.max")
                      )

plot(fit.es.max, plots="optimize")
plot(fit.es.max, plots="boxplot")
plot(fit.es.max, plots="asmd")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.mediation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print.bal.table.mediation")
### * print.bal.table.mediation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.bal.table.mediation
### Title: Default print statement for 'mediation' class
### Aliases: print.bal.table.mediation

### ** Examples

data("tMdat")

## tMdat is small simulated data set included in twangMediation for 
## demonstrating the functions. See ?tMdat for details

fit.es.max <- wgtmed(M ~ w1 + w2 + w3,
                      data = tMdat,
                      a_treatment = "A",
                      y_outcome = "Y",
                      total_effect_wts = tMdat$te.wgt,
                      method = "ps",
                      ps_n.trees=1500,
                      ps_shrinkage=0.01,
                      ps_stop.method=c("es.max")
                      )

bal.table.mediation(fit.es.max)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print.bal.table.mediation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print.mediation")
### * print.mediation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print.mediation
### Title: Default print statement for 'mediation' class
### Aliases: print.mediation

### ** Examples

data("tMdat")

## tMdat is small simulated data set included in twangMediation for 
## demonstrating the functions. See ?tMdat for details

fit.es.max <- wgtmed(M ~ w1 + w2 + w3,
                      data = tMdat,
                      a_treatment = "A",
                      y_outcome = "Y",
                      total_effect_wts = tMdat$te.wgt,
                      method = "ps",
                      ps_n.trees=1500,
                      ps_shrinkage=0.01,
                      ps_stop.method=c("es.max")
                      )

print(fit.es.max)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print.mediation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary.mediation")
### * summary.mediation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary.mediation
### Title: Displays a useful description of a 'mediation' object.
### Aliases: summary.mediation

### ** Examples

data("tMdat")

## tMdat is small simulated data set included in twangMediation for 
## demonstrating the functions. See ?tMdat for details

## The tMdat data contains the following variables
## See ?tMdat for details

fit.es.max <- wgtmed(M ~ w1 + w2 + w3,
                      data = tMdat,
                      a_treatment = "A",
                      y_outcome = "Y",
                      total_effect_wts = tMdat$te.wgt,
                      method = "ps",
                      ps_n.trees=1500,
                      ps_shrinkage=0.01,
                      ps_stop.method=c("es.max")
                      )

summary(fit.es.max)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary.mediation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tMdat")
### * tMdat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tMdat
### Title: Simulated data for twangMediation
### Aliases: tMdat
### Keywords: datasets

### ** Examples

## Not run: 
##D data(tMdat)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tMdat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wgtmed")
### * wgtmed

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wgtmed
### Title: Weighted mediation analysis.
### Aliases: wgtmed
### Keywords: models multivariate

### ** Examples

data("tMdat")

## tMdat is small simulated data set included in twangMediation for 
## demonstrating the functions. See ?tMdat for details

head(tMdat)

## The tMdat data contains the following variables:
##   w1, w2, w3 -- Simulatad covariates
##   A   -- Simulated dichotomous exposure indicator
##   M   -- Simulated discrete mediator (11 values)
##   Y   -- Simulated continuous outcome
##   te.wgt -- Estimated inverse probability weight, estimated using 
##             GBM via the twang ps function

fit.es.max <- wgtmed(M ~ w1 + w2 + w3,
                      data = tMdat,
                      a_treatment = "A",
                      y_outcome = "Y",
                      total_effect_wts = tMdat$te.wgt,
                      method = "ps",
                      ps_n.trees=1500,
                      ps_shrinkage=0.01,
                      ps_stop.method=c("es.max")
                      )

fit.es.max



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wgtmed", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
