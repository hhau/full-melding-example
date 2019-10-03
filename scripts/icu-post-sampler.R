library(coda)
library(rjags)
library(futile.logger)

model_file <- "scripts/common/icu-post/model.txt"
data_file <- "scripts/common/icu-post/data.txt"
inits1_file <- "scripts/common/icu-post/inits1.txt"
inits2_file <- "scripts/common/icu-post/inits2.txt"
inits3_file <- "scripts/common/icu-post/inits2.txt"

load_from_file <- function(file){
  x <- new.env()
  sys.source(file, envir = x)
  as.list(x)
}

dat <- load_from_file(data_file)
inits1 <- load_from_file(inits1_file)
inits2 <- load_from_file(inits2_file)
inits3 <- load_from_file(inits3_file)

flog.info("Warming up ICU model (Posterior)")

jags <- jags.model(
  model_file,
  data = dat,
  inits = list(inits1, inits2),
  n.chains = 2,
  n.adapt = 1000
)

monitor <- c(
  "lambda", "mu", "exp.exits", "ICU", "dev.ICU", "avgLoS", "coeffs", "intcp", 
  "coeffChild", "precLambda", "dev.pos", "pos", "lowpos", "tot.lambda",
  "tot.conf"
)

flog.info("Sampling ICU model (Posterior)")

icu_post_samples <- coda.samples(
  model = jags,
  variable.names = monitor,
  n.iter = 50000,
  thin = 50
)

flog.info("Saving ICU samples to disk (Posterior)")

saveRDS(
  object = icu_post_samples,
  file = "rds/icu-post-samples.rds"
)
