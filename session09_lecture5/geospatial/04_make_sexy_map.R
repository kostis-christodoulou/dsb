remotes::install_github("lina2497/Giftmap")
library(Giftmap)

# all of the actual R code can be found at
# https://github.com/lina2497/Giftmap/blob/master/R/make_sexy_map.r

# make_sexy_map(location, short_name, monochrome = FALSE)
#
# Arguments
# location: A character string describing the target, gets passed to osmdata::getbb().
# short_name: A character string to be used as a caption.
# monochrome: Logical, if TRUE output is black and white.

make_sexy_map("Bath UK",
              "Bath",
              monochrome = TRUE)
              #monochrome = FALSE)


make_sexy_map("Venice, Italy",
              "Venice",
              monochrome = TRUE)
              #monochrome = FALSE)

              
