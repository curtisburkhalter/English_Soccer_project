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
combined_goals <- rbind(goals1718,goals1617,goals1516,goals1415,goals1314,goals1213,goals1112,goals1011,goals0910,goals0809)

#add season index column to combined
seasons <- c("1718","1617","1516","1415","1314","1213","1112","1011","0910","0809")

seasons_rep <- rep(seasons, each = 20)

combined_goals <- cbind(combined_goals,seasons_rep)

#write the file to the 'Data' directory
write_csv(combined_goals,here("English_Soccer_project","Data","combined_goals.csv"), col_names = TRUE)

#get the EPL_Fouls datasets from Google sheets
fouls1718 <- gs_title("EPL_Fouls1718")
fouls1617 <- gs_title("EPL_Fouls1617")
fouls1516 <- gs_title("EPL_Fouls1516")
fouls1415 <- gs_title("EPL_Fouls1415")
fouls1314 <- gs_title("EPL_Fouls1314")
fouls1213 <- gs_title("EPL_Fouls1213")
fouls1112 <- gs_title("EPL_Fouls1112")
fouls1011 <- gs_title("EPL_Fouls1011")
fouls0910 <- gs_title("EPL_Fouls0910")
fouls0809 <- gs_title("EPL_Fouls0809")

#read the sheets into R
fouls1718 <- gs_read(ss=fouls1718)
fouls1617 <- gs_read(ss=fouls1617)
fouls1516 <- gs_read(ss=fouls1516)
fouls1415 <- gs_read(ss=fouls1415)
fouls1314 <- gs_read(ss=fouls1314)
fouls1213 <- gs_read(ss=fouls1213)
fouls1112 <- gs_read(ss=fouls1112)
fouls1011 <- gs_read(ss=fouls1011)
fouls0910 <- gs_read(ss=fouls0910)
fouls0809 <- gs_read(ss=fouls0809)

#rbind together all the seasons
combined_fouls <- rbind(fouls1718,fouls1617,fouls1516,fouls1415,fouls1314,fouls1213,fouls1112,fouls1011,fouls0910,fouls0809)

#add season index column to combined
seasons <- c("1718","1617","1516","1415","1314","1213","1112","1011","0910","0809")

seasons_rep <- rep(seasons, each = 20)

combined_fouls <- cbind(combined_fouls,seasons_rep)

#write the file to the 'Data' directory
write_csv(combined_fouls,here("English_Soccer_project","Data","combined_fouls.csv"), col_names = TRUE)

#get the EPL_Corners datasets from Google sheets
corners1718 <- gs_title("EPL_Corners1718")
corners1617 <- gs_title("EPL_Corners1617")
corners1516 <- gs_title("EPL_Corners1516")
corners1415 <- gs_title("EPL_Corners1415")
corners1314 <- gs_title("EPL_Corners1314")
corners1213 <- gs_title("EPL_Corners1213")
corners1112 <- gs_title("EPL_Corners1112")
corners1011 <- gs_title("EPL_Corners1011")
corners0910 <- gs_title("EPL_Corners0910")
corners0809 <- gs_title("EPL_Corners0809")

#read the sheets into R
corners1718 <- gs_read(ss=corners1718)
corners1617 <- gs_read(ss=corners1617)
corners1516 <- gs_read(ss=corners1516)
corners1415 <- gs_read(ss=corners1415)
corners1314 <- gs_read(ss=corners1314)
corners1213 <- gs_read(ss=corners1213)
corners1112 <- gs_read(ss=corners1112)
corners1011 <- gs_read(ss=corners1011)
corners0910 <- gs_read(ss=corners0910)
corners0809 <- gs_read(ss=corners0809)

#rbind together all the seasons
combined_corners <- rbind(corners1718,corners1617,corners1516,corners1415,corners1314,corners1213,corners1112,corners1011,corners0910,corners0809)

#add season index column to combined
seasons <- c("1718","1617","1516","1415","1314","1213","1112","1011","0910","0809")

seasons_rep <- rep(seasons, each = 20)

combined_corners <- cbind(combined_corners,seasons_rep)

#write the file to the 'Data' directory
write_csv(combined_corners,here("English_Soccer_project","Data","combined_corners.csv"), col_names = TRUE)

#get the EPL_Shots datasets from Google sheets
shots1718 <- gs_title("EPL_Shots1718")
shots1617 <- gs_title("EPL_Shots1617")
shots1516 <- gs_title("EPL_Shots1516")
shots1415 <- gs_title("EPL_Shots1415")
shots1314 <- gs_title("EPL_Shots1314")
shots1213 <- gs_title("EPL_Shots1213")
shots1112 <- gs_title("EPL_Shots1112")
shots1011 <- gs_title("EPL_Shots1011")
shots0910 <- gs_title("EPL_Shots0910")
shots0809 <- gs_title("EPL_Shots0809")

#read the sheets into R
shots1718 <- gs_read(ss=shots1718)
shots1617 <- gs_read(ss=shots1617)
shots1516 <- gs_read(ss=shots1516)
shots1415 <- gs_read(ss=shots1415)
shots1314 <- gs_read(ss=shots1314)
shots1213 <- gs_read(ss=shots1213)
shots1112 <- gs_read(ss=shots1112)
shots1011 <- gs_read(ss=shots1011)
shots0910 <- gs_read(ss=shots0910)
shots0809 <- gs_read(ss=shots0809)

#rbind together all the seasons
combined_shots <- rbind(shots1718,shots1617,shots1516,shots1415,shots1314,shots1213,shots1112,shots1011,shots0910,shots0809)

#add season index column to combined
seasons <- c("1718","1617","1516","1415","1314","1213","1112","1011","0910","0809")

seasons_rep <- rep(seasons, each = 20)

combined_shots <- cbind(combined_shots,seasons_rep)

#write the file to the 'Data' directory
write_csv(combined_shots,here("English_Soccer_project","Data","combined_shots.csv"), col_names = TRUE)
