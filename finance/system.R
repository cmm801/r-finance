

##########################################################################################################################
#  Routine: install.packages.R
#
# This line seems to work on the Ubuntu server, where letting install.packages use its default arguments does not
##########################################################################################################################

install.packages.R <- function( package.name ) {
  install.packages( package.name, lib = '/usr/local/lib/R', repos = "http://cran.us.r-project.org", method = 'wget' )
  # install.packages( '/tmp/RtmpDHbgXx/downloaded_packages/hash_2.2.6.tar.gz', repos = NULL, type = 'source' )
};

