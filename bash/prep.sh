# needs to be sourced, not executed
#
# . ./prep.sh   # ok
# ./prep.sh     # not ok

if [ "$1" != "--local" ]; then
    module load R-bundle-CRAN/2023.12-foss-2023a
else 
    # default on Habrok, mkae it consistent on local machine
    Rscript -e ".libPaths( c('~/.R/libs', .libPaths()))"
fi
Rscript -e "install.packages(c('optparse', 'devtools'), repos='https://cloud.r-project.org')"
Rscript -e "devtools::install_github('rsetienne/DAISIE', repos = 'https://github.com', ref = 'develop')"
Rscript -e "devtools::install()"
