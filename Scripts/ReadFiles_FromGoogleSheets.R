suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(googlesheets))

#get the EPL_Goals datasets from Google sheets
goals1718 <- gs_title("EPL_Goals1718")
goals1617 <- gs_title("EPL_Goals1617")
goals1516 <- gs_title("EPL_Goals1516")
goals1415 <- gs_title("EPL_Goals1415")
goals1314 <- gs_title("EPL_Goals1314")
goals1213 <- gs_title("EPL_Goals1213")
goals1112 <- gs_title("EPL_Goals1112")
goals1011 <- gs_title("EPL_Goals1011")
goals0910 <- gs_title("EPL_Goals0910")
goals0809 <- gs_title("EPL_Goals0809")

#read the sheets into R
goals1718 <- gs_read(ss=goals1718)
goals1617 <- gs_read(ss=goals1617)
goals1516 <- gs_read(ss=goals1516)
goals1415 <- gs_read(ss=goals1415)
goals1314 <- gs_read(ss=goals1314)
goals1213 <- gs_read(ss=goals1213)
goals1112 <- gs_read(ss=goals1112)
goals1011 <- gs_read(ss=goals1011)
goals0910 <- gs_read(ss=goals0910)
goals0809 <- gs_read(ss=goals0809)

#rbind together all the seasons
combined <- rbind(goals1718,goals1617,goals1516,goals1415,goals1314,goals1213,goals1112,goals1011,goals0910,goals0809)

#add season index column to combined
seasons <- c("1718","1617","1516","1415","1314","1213","1112","1011","0910","0809")

seasons_rep <- rep(seasons, each = 20)

combined <- cbind(combined,seasons_rep)
