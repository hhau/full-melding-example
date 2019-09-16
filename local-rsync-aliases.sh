alias push-latest-code=\
"
rsync \
  -avr \
  -e ssh \
  --update \
  --exclude rds \
  --exclude plots \
  --exclude logs \
  . \
  aam71@login.hpc.cam.ac.uk:2019-08-20_full-melding-example
" 

alias pull-latest-results=\
"
rsync \
  -avr \
  -e ssh \
  --update \
  aam71@login.hpc.cam.ac.uk:2019-08-20_full-melding-example/ \
  . 
" 