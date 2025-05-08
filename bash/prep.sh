# needs to be sourced, not executed
#
# . ./prep.sh   # ok
# ./prep.sh     # not ok

module load R-bundle-CRAN/2023.12-foss-2023a
Rscript -e "devtools::install_github('rsetienne/DAISIE', ref = 'develop')"
Rscript -e "install.packages('optparse')"
Rscript -e "devtools::install()"
