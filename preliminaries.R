library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(viridis)
library(tidyverse)
library(lubridate)
library(tidyr)
library(readxl)
library(imputeTS) 
library(reshape2)
library(patchwork)  #combine separate ggplots
library(scales)
library(zoo)
library(RColorBrewer)
library(zeallot) #for list output
require(broom)   #for summary lm function 
library(AICcmodavg)
library(zetadiv)
library(caret)  # for calculate R2
#library(pscl)  # for calculate McFadden's R-squared for model

## Color palette
cpalete = c( "#999999", "#E69F00", '#336699',"#99FF33", "#CC6699", "#cc0000", "#414487FF" ,"#35B779FF") 


