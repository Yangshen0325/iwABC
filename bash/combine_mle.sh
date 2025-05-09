#!/bin/bash

any=`ls -AU | head -1`
head -1 "$any" > mle.Rdata
find -type f -name "*.txt" -exec awk 'FNR==2 {print}' {} + >> mle.Rdata
