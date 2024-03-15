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

# this last bit isn't working on big computer?
# set your workng directory to where your files are located
list.files(pattern = "*final.png", full.names = T) %>% # find the saved temporal images
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=10) %>% # animates, can opt for number of loops
  image_write("NL-IH animation.gif") # write to current dir
# file.remove(list.files(pattern=".png")) # remove the fi


setwd("C:/Users/acresswe/Australian Institute of Marine Science/RRAP M&DS - 2_DataWorkflows/IPMF/model_runs/Counterfactuals_Jan23/plots/MapAnimations/30141")
# this last bit isn't working on big computer?
# set your workng directory to where your files are located
list.files(pattern = "*map.png", full.names = T) %>% # find the saved temporal images
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=10) %>% # animates, can opt for number of loops
  image_write("C-scape.gif") # write to current dir
# file.remove(list.files(pattern=".png")) # remove the fi






# NOTE: This seems to create multiple large temporary files and not delete them, e.g. check:
C:\Users\acresswe\AppData\Local\Temp\RtmpWyEYhU