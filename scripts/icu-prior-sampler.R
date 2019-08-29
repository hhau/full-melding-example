library(coda)
library(rjags)
library(futile.logger)

model_file <- "scripts/common/icu-prior/model.txt"
data_file <- "scripts/common/icu-prior/data.txt"
inits1_file <- "scripts/common/icu-prior/inits1.txt"
inits2_file <- "scripts/common/icu-prior/inits2.txt"
inits3_file <- "scripts/common/icu-prior/inits2.txt"

load_from_file <- function(file){
  x <- new.env()
  sys.source(file, envir = x)
  as.list(x)
}

dat <- load_from_file(data_file)
inits1 <- load_from_file(inits1_file)

flog.info("Warming up ICU model (Prior)")

jags <- jags.model(
  model_file,
  data = dat,
  inits = list(inits1),
  n.chains = 1,
  n.adapt = 10000
)

monitor <- c(
  "lambda", "mu", "exp.exits", "ICU", "dev.ICU", "avgLoS", "coeffs", "intcp", 
  "coeffChild", "precLambda", "dev.pos", "pos", "lowpos", "tot.lambda",
  "tot.conf"
)

flog.info("Sampling ICU model (Prior)")

icu_prior_samples <- coda.samples(
  model = jags,
  variable.names = monitor,
  n.iter = 50000,
  thin = 1
)

flog.info("Saving ICU samples to disk (Prior)")

saveRDS(
  object = icu_prior_samples,
  file = "rds/icu-prior-samples.rds"
)
