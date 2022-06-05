# Set up a new python environment for necessary python analysis.
# 2022.06
# Jun

# packages required
library(reticulate)

# install miniconda if not did before
install_miniconda()

# create the new conda env
conda_create("dexibit_interview_task")

# check if created correctly
conda_list()

# point to the right python env
use_condaenv("dexibit_interview_task")

#' install necessary packages (alternatively these packages could be installed
#' via terminal by pip install)

py_install("pandas", envname = "dexibit_interview_task")
py_install("scikit-learn", envname = "dexibit_interview_task")
