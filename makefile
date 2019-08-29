RSCRIPT = Rscript
PLOT_SETTINGS = scripts/common/plot-settings.R
TEX_FILES = $(wildcard tex-input/*.tex) \
	$(wildcard tex-input/*/*.tex) \
	$(wildcard tex-input/*/*/*.tex)

# if you wildcard the all-target, then nothing will happen if the target doesn't
# exist (no target). hard code the target.
# CHANGE THIS:
WRITEUP = full-melding-model-ex.pdf

RDS = rds
SCRIPTS = scripts
COMMON = scripts/common
PLOTS = plots
STAN_FILES = $(SCRIPTS)/stan-files

# add to as needed
ALL_PLOTS = $(PLOTS)/prior-comparison.pdf \
	$(PLOTS)/icu-posterior.pdf

all : $(WRITEUP)

# knitr is becoming more picky about encoding, specify UTF-8 input
$(WRITEUP) : $(wildcard *.rmd) $(TEX_FILES) $(ALL_PLOTS)
	$(RSCRIPT) -e "rmarkdown::render(input = Sys.glob('*.rmd'), encoding = 'UTF-8')"

# ICU prior sample + KDE (ICU is model 1)
ICU_PRIOR_SAMPLES = $(RDS)/icu-prior-samples.rds
ICU_POST_SAMPLES = $(RDS)/icu-post-samples.rds

$(ICU_PRIOR_SAMPLES) : $(SCRIPTS)/icu-prior-sampler.R $(wildcard $(COMMON)/icu-prior/*)
	$(RSCRIPT) $<

$(ICU_POST_SAMPLES) : $(SCRIPTS)/icu-post-sampler.R $(wildcard $(COMMON)/icu-post/*)
	$(RSCRIPT) $<

$(PLOTS)/icu-posterior.pdf : $(SCRIPTS)/icu-post-plotter.R $(ICU_POST_SAMPLES) $(PLOT_SETTINGS)
	$(RSCRIPT) $<

$(RDS)/model1-kde.rds : $(SCRIPTS)/icu-prior-kde.R $(ICU_PRIOR_SAMPLES) 
	$(RSCRIPT) $<

$(RDS)/model1-dprior-im.rds : $(RDS)/model1-kde.rds

$(RDS)/model1-im.rds : $(RDS)/model1-kde.rds

# Severity prior
SEV_PRIOR_SAMPLES = $(RDS)/sev-prior-samples.rds

$(SEV_PRIOR_SAMPLES) : $(SCRIPTS)/sev-prior-sampler.R $(STAN_FILES)/sev-prior.stan
	$(RSCRIPT) $<

# prior plotter
$(PLOTS)/prior-comparison.pdf : $(SCRIPTS)/prior-plotter.R $(ICU_PRIOR_SAMPLES) $(SEV_PRIOR_SAMPLES) $(PLOT_SETTINGS)
	$(RSCRIPT) $<

# wsre estimate
$(RDS)/sev-prior-wsre-estimate.rds : $(SCRIPTS)/sev-prior-wsre-sampler.R $(STAN_FILES)/sev-prior-wsre.stan
	$(RSCRIPT) $<