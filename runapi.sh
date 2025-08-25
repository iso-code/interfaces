#!/bin/bash

# Change to the directory where your api.R lives
#cd "$(dirname "$0")"

# Run the R Plumber API
Rscript -e ".libPaths('/home/lanuv.nrw.de/gaj/custom-r-libs');
		library(plumber); 
		pr <- plumber::plumb('api.R'); 
		pr\$run(host = '0.0.0.0', port = 8010)"

