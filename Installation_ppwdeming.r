# Installing package from .tar.gz, via R terminal

#################### ADJUST and RUN these lines ####################
# edit to point to location of .tar.gz file, containing compiled function
setwd("path/to/folder/location")
# edit to adjust for name of current package-installation file
install.packages("PPWDeming_0.0.0.9008.tar.gz", repos=NULL, type="source")
####################################################################

library('ppwdeming')  # this loads the contents of the current PPWDeming package
