library(magick)
library(dplyr)
#setwd("C:/Users/cre25e/Dropbox/000 Functional-Structural Coral Model MR_AC/000 FSC Anna and Michael/1_outputs/plots for animations")


#setwd("C:/Users/cre25e/Dropbox/000 Functional-Structural Coral Model MR_AC/000 FSC Anna and Michael/1_Plots/plot series/graphical")
# this last bit isn't working on big computer?
#setwd("H:/cre25e/Dropbox/000 Functional-Structural Coral Model MR_AC/000 FSC Anna and Michael/1_Plots/plot series/Volume series")

# set your workng directory to where your files are located
list.files(pattern = "0 RUGOSITY*.png", full.names = T) %>% # find the saved temporal images
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("animation_allts.gif") # write to current dir
# file.remove(list.files(pattern=".png")) # remove the fi


# ----------------------------------------------------------------------
setwd("C:/Users/cre25e/Dropbox/000 Functional-Structural Coral Model MR_AC/000 FSC Anna and Michael/1_Animations/plots for animations/NL-IH")

setwd("C:/Users/acresswe/OneDrive - Australian Institute of Marine Science/SCIENCE ARCHIVE/PHD/000 Functional-Structural Coral Model MR_AC/000 FSC Anna and Michael/1_Animations/plots for animations/NL-NH")

# this last bit isn't working on big computer?
# set your workng directory to where your files are located
list.files(pattern = "*.png", full.names = T) %>% # find the saved temporal images
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2, loop = 0) %>% # animates, can opt for number of loops
  image_write("NL-IH.gif") # write to current dir
# file.remove(list.files(pattern=".png")) # remove the fi


# NOTE: This seems to create multiple large temporary files and not delete them, e.g.:
C:\Users\acresswe\AppData\Local\Temp\RtmpWyEYhU