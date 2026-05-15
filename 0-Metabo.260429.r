library(readr)
########################################
#creation of METABO from deiso
#############################################


input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/03.deiso.260429.60&70'
#input_dir <- 'D:/Administration 2025/mansour/fichiers/metabo_publi/06.application.291/after 24.04.2026/NEW.APPLI.publi.2.NICA.3'
#input_dir <- 'D:/Administration 2025/mansour/fichiers/metabo_publi/06.application.291/NEW.APPLI.publi.2.NICA.2'

#input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/03.deiso/OLs' 
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/03.deiso/OLs/LAST'
setwd(input_dir)
input_files <- list.files(input_dir, pattern = "_deisopeaklist.CSV", recursive = TRUE)

#input_files <- list.files(input_dir, pattern = "_METABO.csv", recursive = TRUE)
# Select files
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  
  basenames <- c("mz", "into")
  Xtract <- matrix(ncol=length(basenames), nrow = nrow(csv_active))
  colnames(Xtract) <- c(basenames)
  
  Xtract[,"mz"]=cbind(csv_active$mz)
  Xtract[,"into"]=cbind(csv_active$into)
  
  Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), "_METABO.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }

########################################
# the files need to be normalized METABO.N 
#############################################
 
#the original version was 291 but the trouble are with the difference between 60 and 70 eV. => 199 or 227
#input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/04.Metabo'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/04.Metabo/OLs'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/00.story/OLs+application/NICA291.oriprotocol'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/00.story/OLs+application/NICA227.oriprotocol'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/00.story/OLs+application/NICA199.oriprotocol'
setwd(input_dir)
input_files <- list.files(input_dir, pattern = "_METABO.csv", recursive = TRUE)
input_files <- list.files(input_dir, pattern = "_MA_NICA.csv", recursive = TRUE)
macible = 291.006575

# Normalization of metabo with m/z =199, 
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/04.Metabo/OLs'
setwd(input_dir)
input_files <- list.files(input_dir, pattern = "_METABO.csv", recursive = TRUE)
macible = 199.040068

# Normalization of metabo with m/z =227, 
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/04.Metabo/OLs'
setwd(input_dir)
input_files <- list.files(input_dir, pattern = "_METABO.csv", recursive = TRUE)
macible = 227.034983

ppm = 3
delta = macible*ppm*1e-6
macible.int = 0

## looking for the maximal value of the target
for(i in 1:length(input_files)){
      csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
      Xtract <- csv_active
      Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
      if (as.numeric(Xtract.max$into)>=macible.int){
        macible.int<-   as.numeric(Xtract.max$into)
      }
}
##the real work    
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
  Xtract$into = as.character(macible.int*as.numeric(csv_active$into)/as.numeric(Xtract.max$into))
  #Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-25), "_DP_291_MA_N.csv",sep ="")
  #write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  #Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-25), "_DP_199_MA_N.csv",sep ="")
  #write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  #Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-25), "_DP_227_MA_N.csv",sep ="")
  #write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), ".csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)  
  }
 
  
#####################################  
## exactmass correction if needed
############################################# 
# Lockmass of metabo with m/z =199 

input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/06.metabo.NI/OLs/NI199'


setwd(input_dir)
input_files <- list.files(input_dir, pattern = "_MA_N.csv", recursive = TRUE)
macible = 199.040068

# Lockmass of metabo with m/z =227
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/06.metabo.NI/OLs/NI227'
setwd(input_dir)
input_files <- list.files(input_dir, pattern = "_MA_N.csv", recursive = TRUE)
macible = 227.034983

# Lockmass of metabo with m/z =291
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/06.metabo.NI/OLs/NI291'
setwd(input_dir)
input_files <- list.files(input_dir, pattern = "_MA_N.csv", recursive = TRUE)
macible = 291.006575


ppm = 3
delta = macible*ppm*1e-6
macible.int = 0
Xtractdelta1 <- list()

for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
  delta1 <- as.numeric(Xtract.max$mz)/macible
  Xtractdelta1[[i]] <- delta1
  Xtract$mz = as.character(as.numeric(csv_active$mz)/Xtractdelta1[[i]])
  Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-9), "_MA_NI.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
 } 
  
  
  
  
  
  
  
  
##################################################
#          Collision was mastered by Mansour :-)
#           we let the 07 as remembrance and luck number !
##################################################

input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/07.metabo.NIC/OLs'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/07.metabo.NIC/OLs+application'
setwd(input_dir)
input_files <- list.files(input_dir, pattern = "_MA_NI.csv", recursive = TRUE)
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)  
 Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 4),"C.csv",sep ="")
  write.csv(csv_active, file = Xtractfile, row.names = FALSE)
 }
  
#######################################################
#             Collision and artefacts !

#         Removal of large electronic noise : Special macible = 217.53  delta = 0.47 
#         files are rewrite as NICA
#######################################################

input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/08.metabo.NICA/'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/08.metabo.NICA/'
setwd(input_dir)
input_files <- list.files(input_dir, pattern = ".csv", recursive = TRUE)
macible = 217.53
delta = 0.47
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  matches <- csv_active[abs(as.numeric(csv_active$mz) - macible) <= delta, ]
  if (nrow(matches) > 0) {
  Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0)   
 Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 4),"A.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }
 }
#######################################################
#             Collision and artefacts !

#         Removal of punctual artefacts : 
#           macible = 65.0033 delta = 0.001
#           macible = 89.0033 delta = 0.001
#           macible = 113.0033 delta = 0.001
#           macible = 69.00   delta = 0.02
#           macible = 74.97   delta = 0.02
#           macible = 74.99   delta = 0.02
#           macible = 92.63   delta = 0.02
#           macible = 93.00   delta = 0.02
#           macible = 63.0240 delta = 0.001
#           macible = 93.0337 delta = 0.001
#           macible = 93.0345 delta = 0.001
#           macible = 199.0357 delta = 0.0001
#           macible = 150.9955 delta = 0.001
#           macible =  162.9955 delta = 0.001
#           macible = 82.9694 delta = 0.02
#           macible = 98.9643 delta = 0.02
#
####################################################### 
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/08.metabo.NICA/'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/08.metabo.NICA/'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Preparation/' 
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA227/60eV/L.alboflavida'

setwd(input_dir)
input_files <- list.files(input_dir, pattern = "NICA.csv", recursive = TRUE)
input_files <- list.files(input_dir, pattern = ".csv", recursive = TRUE)
mescibles <- c(92.63,  74.99, 74.97, 69, 93,  82.9694,   98.9643)

delta = 0.02


mescibles <- c(65.0033, 63.0240, 150.9955, 199.0357, 162.9955, 199.0357, 113.0033, 89.0033,  93.0337, 93.0345, 135.0088, 171.0451, 261.9992, 171.0406, 149.0243, 155.0138, 131.0138, 190.9905, 247.9836, 263.0070, 128.9749, 169.0294, 231.0254, 218.0067, 127.0189, 161.0607, 167.0349, 115.0553, 170.0328, 170.0372, 143.0501, 105.0345, 115.0189, 197.0244, 171.0087, 121.0295, 173.0243, 99.0240, 143.0138, 164.4106)





    mescibles <- c(63.02267, 63.024, 65.00164, 65.0033, 89.0033,
    93.0337, 93.0345, 99.024, 105.0345, 107.01384, 113.0033,
    113.04907, 113.11897, 115.0189, 115.0553, 121.0295,
    121.24429, 127.0189, 128.9749, 131.0138, 135.0088,
    143.0138, 143.0501, 149.0243, 150.9955, 155.0138,
    160.61866, 161.0607, 162.9955, 164.4106, 167.0349,
    169.0294, 170.0328, 170.0372, 171.0087, 171.0406,
    171.0451, 173.0243, 174.58752, 176.24559, 178.99048,
    182.77562, 190.9905, 197.0244, 198.03218, 199.0357,
    199.03805, 203.03501, 206.98536, 209.20204, 218.0022,
    218.0049, 218.0067, 218.0125, 218.05426, 221.48285,
    231.0254, 231.0245, 237.15244, 247.01191, 247.01355, 247.9836, 256.03777,
    261.9992, 263.007, 275.602, 291.00237, 291.00255, 308.67018,
    129.9782, 291.28426, 146.63795, 191.39311, 273.51995, 112.8072,
    199.47949, 243.02666, 218.00961, 218.01397, 194.9418, 202.1647,
    247.98794, 143.04568, 136.50805, 172.0529, 218.00018, 194.9418,
    95.00034, 185.88095, 202.16478, 302.51698, 169.08877, 305.16377,
    291.2841, 159.0088, 245.6072, 90.58165, 124.85431, 200.73787,
    299.91853, 254.00534, 126.9956, 227.03235)
    #alboflavida background
    mescibles <- c(141.2441, 83.3665, 227.0306, 246.93838, 257.7039, 119.65123, 166.72765, 305.2583,
    224.14895, 309.55359, 118.82409, 80.84969, 133.01366, 305.0848, 62.33747, 175.88104, 114.9588,219.93252, 234.98027, 151.5292, 
140.71583, 178.02265, 262.97493, 218.9854, 184.01642, 206.00942, 227.07062, 235.01249, 166.99036, 198.02772,
122.03283, 184.01218, 150.02767, 290.99321, 175.03552, 120.54122, 141.24402, 83.36646, 257.70393, 119.65123, 166.72765, 182.98529,
305.2583, 224.14895, 115.01674, 309.55359, 118.82409, 80.84969, 133.01366, 62.33747, 305.08481, 175.88104, 255.02982, 114.95881,
300.70401, 178.88158, 145.00456, 133.50318, 111.36812, 226.93386, 146.83396, 107.85145, 305.86795, 126.77534, 289.62223, 225.91479,
115.29465, 139.00989, 240.69123, 130.7795, 152.15297, 161.67857, 120.84629, 223.66563, 195.06178, 279.1085, 214.28248)

 
  mescibles <- c(183.00863, 234.00441, 92.3875, 119.6325, 152.06265, 199.06806, 260.99566, 246.98038, 264.29755, 100.76102,
105.58643, 266.21441, 290.99225, 87.02397, 91.01888, 211.19412, 149.90797, 269.05155, 276.72921, 219.98893, 183.04295, 122.64931,
262.42774, 210.49911, 261.04474, 87.98857, 201.66712, 240.00627, 245.45009, 99.89741, 155.91537, 261.99516, 177.0165, 150.2907,
291.80059, 244.62514, 293.70246, 213.78262, 211.40552, 234.26365, 133.29012, 275.17349, 193.27986, 106.3399, 215.58996, 102.61883,
159.73251, 105.70919, 128.12937, 61.4061, 115.0532, 93.17011, 138.46312, 243.05627, 247.3478, 210.95953, 239.53712, 149.36911,
193.50194, 156.57669, 226.34523, 179.99522, 303.11942, 162.78525, 131.00882)

 
 
 
 
 
 
    
    delta = 0.0010




for(i in 1:length(input_files)){
    csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
    Xtract <- csv_active
    for (macible in mescibles){
        matches <- csv_active[abs(as.numeric(csv_active$mz) - macible) <= delta, ]
        if (nrow(matches) > 0) {
            if (Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into>0){
            Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0)
            }else{
            Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0)
            }
            
        }
    }
    Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 4),".csv",sep ="")
    write.csv(Xtract, file = Xtractfile, row.names = FALSE)
}
  
#############################################################################################  
# 01/05/2026 rebutal
# we are going to produce the substract files, but with selected OL5 or OL7
# @ 60eV
# the 247 will be taken as an exact amount of OL5 or OL7
# beware consensusSpectrum is an accumulation processus with discard 1/3 out... or else
# combinespectrum should act properly with intensity function according to the comments
library(MSnbase)
#https://rdrr.io/bioc/MSnbase/man/meanMzInts.html
#https://rdrr.io/bioc/MSnbase/man/consensusSpectrum.html
#https://lgatto.github.io/MSnbase/reference/combineSpectra.html
library(readr)
# The idea is good so let's try and create the mean spectrum
#logically we don't mind about which file is in use NICA291, NICA199 or NICA227
# we need after the substraction to Normalized again !
# OL5 the chosen one is NICA291
input_dir <- 'D:/Administration 2025/mansour/fichiers/metabo_publi/06.application.291/after 24.04.2026/NEW.APPLI.publi.2.NICA.3.Substract/OL5' 
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/OL5/60eV'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/OL5/70eV'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA291/70eV/OL5'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA291/60eV/OL5'

input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA227/70eV/OL5'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA227/60eV/OL5'


input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA199/70eV/OL5'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA199/60eV/OL5'

setwd(input_dir) 
input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)
csv_active <- read.csv2(file = input_files[[1]], sep = ",", header = TRUE)
sp1 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
csv_active <- read.csv2(file = input_files[[2]], sep = ",", header = TRUE)
sp2 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
csv_active <- read.csv2(file = input_files[[3]], sep = ",", header = TRUE)
sp3 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
spall <- MSpectra(sp1, sp2, sp3)
# the cons in fact coms :-) 
# the mzd should be in aggreement with MteaboAnalyst => 0.0010 close to 2 electrons
# ppm should be 3 
# 
# idea : this could be a nice way to get a multiplicate : 0.0001, 0.0005, 0.0010 and so on 
# min presence : 
#
cons <- combineSpectra(spall, mzd = 0.0010, ppm = 3, intensityFun = max)
#cons <- combineSpectra(spall,  mzd = 0.0010, intensityFun = max)
# there was a conflict between mnsbase and r-script first use a matrix and then convert to data.frame
# this solved the issue
basenames <- c("mz", "into")
spectreconS <- matrix(ncol=length(basenames), nrow = cons[[1]]@peaksCount)
colnames(spectreconS) <- c(basenames)
for (i in 1:cons[[1]]@peaksCount){
spectreconS[i,"mz"] <- cons[[1]]@mz[i]
spectreconS[i,"into"] <- cons[[1]]@intensity[i]
}
spectreOL5.60ev <- data.frame(spectreconS)
#spectreOL5.70ev <- data.frame(spectreconS)

# preparation for automatization when really finished
Xtract <- spectreOL5.60ev
#Xtract <- spectreOL5.70ev
file <- input_files[[3]]
# job's done
Xtractfile <- paste(substr(file,1,nchar(file)-18), "DP_199_OL5_60eV_NICA_mean.csv",sep ="")
#Xtractfile <- paste(substr(file,1,nchar(file)-18), "DP_199_OL5_70eV_NICA_mean.csv",sep ="")
write.csv(Xtract, file = Xtractfile, row.names = FALSE)

# OL7
input_dir <- 'D:/Administration 2025/mansour/fichiers/metabo_publi/06.application.291/after 24.04.2026/NEW.APPLI.publi.2.NICA.3.Substract/OL7' 
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/OL7/60eV'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/OL7/70eV'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA291/70eV/OL7'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA291/60eV/OL7'

input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA227/70eV/OL7'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA227/60eV/OL7'

input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA199/70eV/OL7'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA199/60eV/OL7'

setwd(input_dir) 
input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)
csv_active <- read.csv2(file = input_files[[1]], sep = ",", header = TRUE)
sp1 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
csv_active <- read.csv2(file = input_files[[2]], sep = ",", header = TRUE)
sp2 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
csv_active <- read.csv2(file = input_files[[3]], sep = ",", header = TRUE)
sp3 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
spall <- MSpectra(sp1, sp2, sp3)
# the cons in fact coms :-) 
# the mzd should be in aggreement with MteaboAnalyst => 0.0010 close to 2 electrons
# ppm should be 3 
# 
# idea : this could be a nice way to get a multiplicate : 0.0001, 0.0005, 0.0010 and so on 
# min presence : 
cons <- combineSpectra(spall,  mzd = 0.0010, intensityFun = max)
# there was a conflict between mnsbase and r-script first use a matrix and then convert to data.frame
# this solved the issue
basenames <- c("mz", "into")
spectreconS <- matrix(ncol=length(basenames), nrow = cons[[1]]@peaksCount)
colnames(spectreconS) <- c(basenames)
for (i in 1:cons[[1]]@peaksCount){
spectreconS[i,"mz"] <- cons[[1]]@mz[i]
spectreconS[i,"into"] <- cons[[1]]@intensity[i]
}
spectreOL7.60ev <- data.frame(spectreconS)
#spectreOL7.70ev <- data.frame(spectreconS)

# preparation for automatization when really finished
Xtract <- spectreOL7.60ev
#Xtract <- spectreOL7.70ev
file <- input_files[[3]]
# job's done
Xtractfile <- paste(substr(file,1,nchar(file)-18), "DP_199_OL7_60eV_NICA_mean.csv",sep ="")
#Xtractfile <- paste(substr(file,1,nchar(file)-18), "DP_199_OL7_70eV_NICA_mean.csv",sep ="")
write.csv(Xtract, file = Xtractfile, row.names = FALSE)

# we have 2 mean files of a sort example  ol5@70ev and ol7@70ev for a status N291


# treatment of L. alboflavida and P. quernea - 247 is either OL5 or OL7
# first find the 247 in the files of L. alboflavida
# calculate the normalized file with the intensities (only OL5 and OL7)
# within 3 ppm remove the normalized file and saved
# at the end this file is not NICA anymore


#291
#P. quernea 70ev
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA291/70eV/P.quernea'
#L. alboflavida 70ev
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA291/70eV/L.alboflavida'
#291
#P. quernea 60ev
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA291/60eV/P.quernea'
#L. alboflavida 60ev
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA291/60eV/L.alboflavida'

#227
#P. quernea 70ev
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA227/70eV/P.quernea'
#L. alboflavida 70ev
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA227/70eV/L.alboflavida'
#P. quernea 60ev
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA227/60eV/P.quernea'
#L. alboflavida 60ev
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA227/60eV/L.alboflavida'

#199
#P. quernea 70ev
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA199/70eV/P.quernea'
#L. alboflavida 70ev
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA199/70eV/L.alboflavida'
#P. quernea 60ev
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA199/60eV/P.quernea'
#L. alboflavida 60ev
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/NICA199/60eV/L.alboflavida'


setwd(input_dir) 
input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)
# minus OL 5
# minus OL 7 (parallel not fully automatized)
for(file in input_files){
# initialization of spectra at the right intensity for OL5
      spectreOL5.RI <- spectreOL5.60ev
# initialization of spectra at the right intensity for OL5
    #  spectreOL5.RI <- spectreOL5.70ev
# initialization of spectra at the right intensity for OL7
      spectreOL7.RI <- spectreOL7.60ev 
# initialization of spectra at the right intensity for OL7
  #    spectreOL7.RI <- spectreOL7.70ev
      csv_active <- read.csv2(file = file, sep = ",", header = TRUE)
      Xtract <- csv_active
      macible = 247.0166
      ppm = 3
      delta = macible*ppm*1e-6
      Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
      macible.int<- 0.00
      #in each file the intensity of 247.0166 is found
      if (as.numeric(Xtract.max$into) >= macible.int){
        macible.int<-  as.numeric(Xtract.max$into)
      }
      if (macible.int>0){
      #generation of the OL5 with the right intensity
      #monspectre.max = spectreconS[abs(spectreconS$mz-macible)<=delta,]
      #monspectre$into[] <- macible.int*(monspectre$into[])/monspectre.max$into    
      monspectre.max = spectreOL5.RI[abs(spectreOL5.RI$mz-macible)<=delta,]
      spectreOL5.RI$into[] <- macible.int*(spectreOL5.RI$into[])/monspectre.max$into
      #generation of the OL7 with the right intensity
      monspectre.max = spectreOL7.RI[abs(spectreOL7.RI$mz-macible)<=delta,]
      spectreOL7.RI$into[] <- macible.int*(spectreOL7.RI$into[])/monspectre.max$into
      }
      #first OL5 vs file 
      myj <- 1
      for (i in 1:nrow(spectreOL5.RI)){
        macible = spectreOL5.RI$mz[i]
        ppm = 3
        delta = macible*ppm*1e-6
        for (j in myj:nrow(Xtract)){
          if (abs(as.numeric(Xtract$mz[j])-macible)<=delta){
            Xtract$into[j] <- as.character(as.numeric(Xtract$into[j])-spectreOL5.RI$into[i])
            myj <- j + 1
            j <- nrow(Xtract)
                      if(myj > j){
            myj <- j 
          }
          }
        }
      }  
      ############### 60ev _DP_291_MA_NICA.csv
      Xtractfile <- paste(substr(file,1,nchar(file)-12), "_60ev_MA_NICAS_OL5.csv",sep ="")
      ############### 70eV_DP_291_MA_NICA.csv
     # Xtractfile <- paste(substr(file,1,nchar(file)-12), "_70ev_MA_NICAS_OL5.csv",sep ="")
      write.csv(Xtract, file = Xtractfile, row.names = FALSE)
      #reinitialization of Xtract
      Xtract <- csv_active      
      #first OL7 vs file .
      myj <- 1
      for (i in 1:nrow(spectreOL7.RI)){
        macible = spectreOL7.RI$mz[i]
        ppm = 3
        delta = macible*ppm*1e-6
        for (j in myj:nrow(Xtract)){
          if (abs(as.numeric(Xtract$mz[j])-macible)<=delta){ 
            Xtract$into[j] <- as.character(as.numeric(Xtract$into[j])-spectreOL7.RI$into[i])
            myj <- j + 1
            j <- nrow(Xtract)
          if(myj > j){
            myj <- j 
          }
          }
        }
      }    
      ############### 60ev
       Xtractfile <- paste(substr(file,1,nchar(file)-12), "_60ev_MA_NICAS_OL7.csv",sep ="")
      ############### 70eV
    #  Xtractfile <- paste(substr(file,1,nchar(file)-12), "_70ev_MA_NICAS_OL7.csv",sep ="")
      write.csv(Xtract, file = Xtractfile, row.names = FALSE)
}
# the substraction is done

#list realized
# 291 70 ev PQ and LA
# 291 60 ev PQ and LA
# 227 70 ev PQ and LA
# 227 60 ev PQ and LA
# 199 70 ev PQ and LA
# 199 60 ev PQ and LA


#Normalization of metabo for folder 291 with m/z =291 
#Normailzation of metabo for folder 291
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/Preparation/NICAS199'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/N.OL2OL4LA/NICA291'

input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.substract.NICA/260503/N.LA/NICA199'
setwd(input_dir) 
#input_files <- list.files(input_dir, pattern = "NICA", recursive = TRUE)

input_files <- list.files(input_dir, pattern = ".csv", recursive = TRUE)
macible = 199.040068
#macible = 227.034983
#macible = 291.006575
#227.034983, 291.006575, 199.040068
ppm = 3
delta = macible*ppm*1e-6
macible.int = 0
## looking for the maximal value of the target
for(i in 1:length(input_files)){
      csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
      Xtract <- csv_active
      Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
      if (as.numeric(Xtract.max$into)>=macible.int){
        macible.int<-   as.numeric(Xtract.max$into)
      }
}
##the real work   on all files
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
  Xtract$into = as.character(macible.int*as.numeric(csv_active$into)/as.numeric(Xtract.max$into))
  Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), ".csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }
    
    
  
  
  ################################################################################
  #  2026/05/06
  #  the subtract project
  #  an automatization of real mean spectra is needed : impossible to stay whithout
  #
  ################################################################################
  #
  #  The function combineSpectra 
  #  beware of intensityFun = base::mean !
  #  beware of mzFun = base::mean !
  #  beware of method ! one can try consensusSpectrum, meanMzInts
  #  the mzd is at the moment 0.0010 2e-
  #  really strange @ 0.0008 less ion left as @ 0.0010 less ion left as @ 0.0012
  #
  ################################################################################
  # preparation are files with a high reduction of artefact by observation in Metaboanalyst
  # 6 files were used
  # 3 files created @ mzd (0.0008, 0.0010 and 0.0012)
  #
  #input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Preparation'  
  
  setwd(input_dir) 
  input_files <- list.files(input_dir, pattern = "MA_NICA.csv", recursive = TRUE)
  for(j in seq(1,length(input_files)-5, by = 6)){
    csv_active <- read.csv2(file = input_files[[j]], sep = ",", header = TRUE)
    sp1 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #sp1 <- createMassSpectrum(mass= as.numeric(csv_active$mz), intensity= as.numeric(csv_active$into))
    csv_active <- read.csv2(file = input_files[[j+1]], sep = ",", header = TRUE)
    sp2 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #sp2 <- createMassSpectrum(mass= as.numeric(csv_active$mz), intensity= as.numeric(csv_active$into))
    csv_active <- read.csv2(file = input_files[[j+2]], sep = ",", header = TRUE)
    sp3 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #sp3 <- createMassSpectrum(mass= as.numeric(csv_active$mz), intensity= as.numeric(csv_active$into))
    csv_active <- read.csv2(file = input_files[[j+3]], sep = ",", header = TRUE)
    sp4 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #sp1 <- createMassSpectrum(mass= as.numeric(csv_active$mz), intensity= as.numeric(csv_active$into))
    csv_active <- read.csv2(file = input_files[[j+4]], sep = ",", header = TRUE)
    sp5 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #sp2 <- createMassSpectrum(mass= as.numeric(csv_active$mz), intensity= as.numeric(csv_active$into))
    csv_active <- read.csv2(file = input_files[[j+5]], sep = ",", header = TRUE)
    sp6 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #sp3 <- createMassSpectrum(mass= as.numeric(csv_active$mz), intensity= as.numeric(csv_active$into))
    spall <- MSpectra(sp1, sp2, sp3, sp4, sp5, sp6)
    cons <- combineSpectra(spall, mzd = 0.0010, intensityFun = base::mean, mzFun = base::mean, method = consensusSpectrum)
    basenames <- c("mz", "into")
    spectreconS <- matrix(ncol=length(basenames), nrow = cons[[1]]@peaksCount)
    colnames(spectreconS) <- c(basenames)
    for (i in 1:cons[[1]]@peaksCount){
      spectreconS[i,"mz"] <- cons[[1]]@mz[i]
      spectreconS[i,"into"] <- cons[[1]]@intensity[i]
    }
    SpectrumDF <- data.frame(spectreconS)   
    Xtractfile <- paste(substr(input_files[[j]],1,nchar(input_files[[j]])-8), "cons10_NICA.csv",sep ="")
    write.csv(SpectrumDF, file = Xtractfile, row.names = FALSE)
        cons <- combineSpectra(spall, mzd = 0.0008, intensityFun = base::mean, mzFun = base::mean, method = consensusSpectrum)
    basenames <- c("mz", "into")
    spectreconS <- matrix(ncol=length(basenames), nrow = cons[[1]]@peaksCount)
    colnames(spectreconS) <- c(basenames)
    for (i in 1:cons[[1]]@peaksCount){
      spectreconS[i,"mz"] <- cons[[1]]@mz[i]
      spectreconS[i,"into"] <- cons[[1]]@intensity[i]
    }
    SpectrumDF <- data.frame(spectreconS)   
    Xtractfile <- paste(substr(input_files[[j]],1,nchar(input_files[[j]])-8), "cons08_NICA.csv",sep ="")
    write.csv(SpectrumDF, file = Xtractfile, row.names = FALSE)
    cons <- combineSpectra(spall, mzd = 0.0012, intensityFun = base::mean, mzFun = base::mean, method = consensusSpectrum)
    basenames <- c("mz", "into")
    spectreconS <- matrix(ncol=length(basenames), nrow = cons[[1]]@peaksCount)
    colnames(spectreconS) <- c(basenames)
    for (i in 1:cons[[1]]@peaksCount){
      spectreconS[i,"mz"] <- cons[[1]]@mz[i]
      spectreconS[i,"into"] <- cons[[1]]@intensity[i]
    }
    SpectrumDF <- data.frame(spectreconS)   
    Xtractfile <- paste(substr(input_files[[j]],1,nchar(input_files[[j]])-8), "cons12_NICA.csv",sep ="")
    write.csv(SpectrumDF, file = Xtractfile, row.names = FALSE)
  }
  #################################################################################
  #TEST
      # 2nd turn with 2.5 mDa
    #cons <- combineSpectra(spall, mzd = 0.0025, intensityFun = base::mean, mzFun = base::mean, method = consensusSpectrum)   
    #spall <-  meanMzInts(list(sp1, sp2, sp3), mzd = 0.0010, ppm = 3, weighted = TRUE)
    #myaveragespetrum <- averageMassSpectra(spall, method="sum")
    #sp4 <- new("Spectrum1", mz = as.numeric(myaveragespetrum@mass), intensity = as.numeric(myaveragespetrum@intensity))
    #cons <- MSpectra(sp4)
    #cons <- consensusSpectrum(spall, mzd = 0.0010, minProp = 0, weighted = TRUE)
    #cons <- MSpectra(cons)
    #cons <- combineSpectra(spall, mzd = 0.0010, ppm = 3, intensityFun = mean, mzFun = base::mean) : better but not for all
    #cons <- combineSpectra(spall, mzd = 0.0010, ppm = 3, intensityFun = max) : this was also bullshit
    #cons <- combineSpectra(spall,  mzd = 0.0010, intensityFun = max) : this was a real bullshit
    # there was a conflict between mnsbase and r-script first use a matrix and then convert to data.frame
    #  this solved the issue
      #spall <- list(sp1, sp2, sp3)    
    # the cons in fact coms :-) 
    # the mzd should be in aggreement with MteaboAnalyst => 0.0010 close to 2 electrons
    # ppm should be 3 
    # 
    # idea : this could be a nice way to get a multiplicate : 0.0001, 0.0005, 0.0010 and so on 
    # min presence : 
    #  
    #mzFun = base::mean
     #mescibles <- c(256.03777, 65.00164, 63.02267, 
#    182.77562, 176.24559, 113.11897, 218.05426,
 #   308.67018, 237.15244, 275.6020, 113.04907,
  #  121.24429, 218.0125, 160.61866, 218.0022,
   # 203.03501, 198.03218, 291.00237, 221.48285, 
#    218.0049, 206.98536, 178.99048, 209.20204, 
 #   247.01355, 247.01191, 291.00255, 199.03805, 174.58752, 107.01384)
  #  delta = 0.0010
   # Xtract <- SpectrumDF
    #for (macible in mescibles){
     # matches <- SpectrumDF[abs(as.numeric(SpectrumDF$mz) - macible) <= delta, ]
      #if (nrow(matches) > 0) {
       # Xtract[abs(as.numeric(SpectrumDF$mz) - macible) <= delta,]$into <- as.character(0.0)  
      #}
    #}
  #################################################################################
   
    
  ################################################################################
  # 
  #  How to go further ?
  #  fact : spectrum minus (247) = nearly exactly a file with only OL2 and OL4 
  #  => hypothesis : removal + normalization should bring the point more likely to OL2 for PQ or OL4 for LA ?
  #  => hypothesis : awaited OL5 for PQ ?
  ##  important point process 247 can't be used at the same time with OL2 or OL4 removal process 
  ##  another method :  is the computation of a complete fit aOL2 + bOL4 + cOL5 + dOL7, this time with 
  ##  an overall present ion like 199, 227 or 291
  #  247 is an obvious proof of the presence of OL5 or OL7 in LA and PQ
  #  a focus is done to try and get a trend  
  #  files of mixture are prepared : 4;3:1;2:2;1:3;4
  #  each Normalization set is treated separately (3)
  #  each collision energy set is treated separately (2)
  #  only OL5 and OL7 as cons10 are involved
  #  
  #  calculation are only applied on LA and PQ
  #
  #  at the end LA or PQ (0) 3 files, LAsubtract or PQsubtract 5 files : it's enough for MA 6.0
  #  on the 3D scores plot it will be possible to check which proportion is the best
  #
  #  let automatize for 6 conditions 
  #
  ################################################################################
  # 6 wd 
  # to simplify a table of path is created
  Processing247wd <- as.table (rbind('D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA199/60eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA199/70eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA227/60eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA227/70eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA291/60eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA291/70eV'))
  \260512.22\Preparation\NICA199\60eV\OL5
  
   Processing247wd <- as.table (rbind('D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA199/60eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA199/70eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA227/60eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA227/70eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA291/60eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA291/70eV')) 
  
  
  # properties are put in a table too
  n_prop <- as.table (rbind("prop40", "prop31", "prop22", "prop13", "prop04"))
  for (j in 1:6){  
  # the working directory
    mywd <- Processing247wd[j]
    setwd(mywd)
    #########################################################
    #  preparation the needed OL5 and OL7 files are selected
    #########################################################
    input_dir <- paste(mywd,"/OL5",sep ="")
    setwd(input_dir)
    #there's only one we don't mind
    #OL5.ori <- list.files(input_dir, pattern = "cons10_NICA.csv", recursive = TRUE)
    OL5.ori <- list.files(input_dir, pattern = "mmi12_NICA.csv", recursive = TRUE)
    OL5.ori <- paste(input_dir, OL5.ori, sep ="/")
    input_dir <- paste(mywd,"/OL7",sep ="")
    setwd(input_dir)
    #there's only one we don't mind
    OL7.ori <- list.files(input_dir, pattern = "mmi12_NICA.csv", recursive = TRUE)
    OL7.ori <- paste(input_dir, OL7.ori, sep ="/")
    setwd(mywd)
    #########################################################
    #  preparation of mixture files  
    #########################################################
    # mixture will contain the path
    mixture <- as.table (rbind(c(OL5.ori, OL5.ori,OL5.ori,OL5.ori), 
    c(OL5.ori, OL5.ori, OL5.ori, OL7.ori),
    c(OL5.ori, OL5.ori, OL7.ori, OL7.ori),
    c(OL5.ori, OL7.ori, OL7.ori, OL7.ori),
    c(OL7.ori, OL7.ori, OL7.ori, OL7.ori))) 
    #n_prop the name of a proportion
    n_prop <- as.table (rbind("prop40", "prop31", "prop22", "prop13", "prop04"))
    #start with five proportion
    for (proportion in 1:5){
    #first path as first spectrum and so on
      csv_active <- read.csv2(file = mixture[proportion,1], sep = ",", header = TRUE)
      sp1 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
      csv_active <- read.csv2(file = mixture[proportion,2], sep = ",", header = TRUE)
      sp2 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
      csv_active <- read.csv2(file = mixture[proportion,3], sep = ",", header = TRUE)
      sp3 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
      csv_active <- read.csv2(file = mixture[proportion,4], sep = ",", header = TRUE)
      sp4 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #create a spectrum
      spall <- MSpectra(sp1, sp2, sp3, sp4)
    #the mean spectrum logically (in metaboanalyst 
      cons <- combineSpectra(spall, mzd = 0.0010, ppm = 3, intensityFun = base::mean, mzFun = base::mean, method = meanMzInts)
    #intern trick to create the spectrumDF    
      basenames <- c("mz", "into")
      spectreconS <- matrix(ncol=length(basenames), nrow = cons[[1]]@peaksCount)
      colnames(spectreconS) <- c(basenames)
      for (i in 1:cons[[1]]@peaksCount){
        spectreconS[i,"mz"] <- cons[[1]]@mz[i]
        spectreconS[i,"into"] <- cons[[1]]@intensity[i]
      }
      SpectrumDF <- data.frame(spectreconS)   
    #preparation of the filename placed in the folder mixture
      Xtractfile <- paste(mywd,"/mixture",sep ="")
    #still in mywd verification of existence of "mixture" and creation if needed
      if (file.exists("mixture")){}else{dir.create(file.path(mywd, "mixture"))}
      Xtractfile <- paste(Xtractfile,"/mixtureOL5OL7_",sep ="")      
      Xtractfile <- paste(Xtractfile,substr(mywd, 87, 93),sep ="")
      Xtractfile <- paste(Xtractfile,"_",sep ="")  
      Xtractfile <- paste(Xtractfile,substr(mywd, 95, 98),sep ="")
      Xtractfile <- paste(Xtractfile,"_",sep ="")
      Xtractfile <- paste(Xtractfile,n_prop[proportion],sep ="") 
      Xtractfile <- paste(Xtractfile,".csv",sep ="")      
      write.csv(SpectrumDF, file = Xtractfile, row.names = FALSE)
      
    }

 
 
    
  }

  
  ##############################################################################
  #
  #  the files are created to make when we want the rest of the process
  #
  #  then locate and save the 5 compositions in temporary files
  #  
  #  go inside LA dir and start the creation of 15 files
  # 
  #  go inside PQ dir and start the creation of 15 files
  #
  #  roll over each directory like above and first go where it is needed 
  #
  ##############################################################################
  Processing247wd <- as.table (rbind('D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA199/60eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA199/70eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA227/60eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA227/70eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA291/60eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA291/70eV'))
  
  
  Processing247wd <- as.table (rbind('D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA199/60eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA199/70eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA227/60eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA227/70eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA291/60eV',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA291/70eV'))


 # for (property in 1:6){ 
 #     for (property in 3:6){
          for (property in 5:6){
    # the working directory
    mywd <- Processing247wd[property]
    # go to the mixture
    mywdtp <- paste(mywd,"/mixture",sep ="")
    setwd(mywdtp)
    input_files <- list.files(mywdtp, pattern = ".csv", recursive = TRUE) 
    # the 5 files of proportion
    prop40 <- read.csv2(file = input_files[5], sep = ",", header = TRUE)
    prop40[,1] <- as.numeric(prop40[,1])
    prop40[,2] <- as.numeric(prop40[,2])
    prop31 <- read.csv2(file = input_files[4], sep = ",", header = TRUE)
    prop31[,1] <- as.numeric(prop31[,1])
    prop31[,2] <- as.numeric(prop31[,2])
    prop22 <- read.csv2(file = input_files[3], sep = ",", header = TRUE)
    prop22[,1] <- as.numeric(prop22[,1])
    prop22[,2] <- as.numeric(prop22[,2])
    prop13 <- read.csv2(file = input_files[2], sep = ",", header = TRUE)
    prop13[,1] <- as.numeric(prop13[,1])
    prop13[,2] <- as.numeric(prop13[,2])
    prop04 <- read.csv2(file = input_files[1], sep = ",", header = TRUE)
    prop04[,1] <- as.numeric(prop04[,1])
    prop04[,2] <- as.numeric(prop04[,2])

    for (lichen in 1:2){
      if (lichen == 1){
        #
        #  The idea was only to make something nice to get all files first in LA and second in PQ
        #  i'm tired of running n times my script :-)
        #
        mywdtp <- paste(mywd,"/L.alboflavida",sep ="")
      }else{
        mywdtp <- paste(mywd,"/P.quernea",sep ="")        
      }
      setwd(mywdtp)
      input_files <- list.files(mywdtp, pattern = ".csv", recursive = TRUE) 
      for(file in input_files){
        # initialization of prop.RI the inetnsity 
        prop40RI <- prop40
        prop31RI <- prop31
        prop22RI <- prop22
        prop13RI <- prop13
        prop04RI <- prop04     
       # prop40.RI$into[] <- as.numeric(prop40$into[]) 
       # prop31.RI$into[] <- as.numeric(prop31$into[]) 
       # prop22.RI$into[] <- as.numeric(prop22$into[]) 
       # prop13.RI$into[] <- as.numeric(prop13$into[]) 
       # prop04.RI$into[] <- as.numeric(prop04$into[])           
        # the file of work 
        csv_active <- read.csv2(file = file, sep = ",", header = TRUE)
        Xtract <- csv_active
        macible = 247.0166
        ppm = 3
        delta = macible*ppm*1e-6
        Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
        macible.int<- 0.00
        #in each file the intensity of 247.0166 is found
        if (as.numeric(Xtract.max$into) >= macible.int){
          macible.int<-  as.numeric(Xtract.max$into)
        }
        if (macible.int>0){
          # generation of all files with the right intensity
          # prop 40
          monspectre.max = prop40RI[abs(prop40RI$mz-macible)<=delta,]
          prop40RI$into[] <- macible.int*(prop40RI$into[])/monspectre.max$into
          # prop 31
          monspectre.max = prop31RI[abs(prop31RI$mz-macible)<=delta,]
          prop31RI$into[] <- macible.int*(prop31RI$into[])/monspectre.max$into
          # prop 22
          monspectre.max = prop22RI[abs(prop22RI$mz-macible)<=delta,]
          prop22RI$into[] <- macible.int*(prop22RI$into[])/monspectre.max$into
          # prop 13
          monspectre.max = prop13RI[abs(prop13RI$mz-macible)<=delta,]
          prop13RI$into[] <- macible.int*(prop13RI$into[])/monspectre.max$into
          # prop 04
          monspectre.max = prop04RI[abs(prop04RI$mz-macible)<=delta,]
          prop04RI$into[] <- macible.int*(prop04RI$into[])/monspectre.max$into
        }
        # prop 40 
        myj <- 1
        for (i in 1:nrow(prop40RI)){
          macible = prop40RI$mz[i]
          ppm = 3
          delta = macible*ppm*1e-6
          for (j in myj:nrow(Xtract)){
            if (abs(as.numeric(Xtract$mz[j])-macible)<=delta){
              Xtract$into[j] <- as.character(as.numeric(Xtract$into[j])-prop40RI$into[i])
              myj <- j + 1
              j <- nrow(Xtract)
              if(myj > j){
                myj <- j 
              }
            }
          }
        }  
        Xtractfile <- paste(substr(file,1,nchar(file)-4), "_OL5OL7prop40.csv",sep ="")
        write.csv(Xtract, file = Xtractfile, row.names = FALSE)
        #reinitialization of Xtract
        Xtract <- csv_active
        # prop 31 
        myj <- 1
        for (i in 1:nrow(prop31RI)){
          macible = prop31RI$mz[i]
          ppm = 3
          delta = macible*ppm*1e-6
          for (j in myj:nrow(Xtract)){
            if (abs(as.numeric(Xtract$mz[j])-macible)<=delta){
              Xtract$into[j] <- as.character(as.numeric(Xtract$into[j])-prop31RI$into[i])
              myj <- j + 1
              j <- nrow(Xtract)
              if(myj > j){
                myj <- j 
              }
            }
          }
        }  
        Xtractfile <- paste(substr(file,1,nchar(file)-4), "_OL5OL7prop31.csv",sep ="")
        write.csv(Xtract, file = Xtractfile, row.names = FALSE)
        #reinitialization of Xtract
        Xtract <- csv_active
        # prop 22 
        myj <- 1
        for (i in 1:nrow(prop22RI)){
          macible = prop22RI$mz[i]
          ppm = 3
          delta = macible*ppm*1e-6
          for (j in myj:nrow(Xtract)){
            if (abs(as.numeric(Xtract$mz[j])-macible)<=delta){
              Xtract$into[j] <- as.character(as.numeric(Xtract$into[j])-prop22RI$into[i])
              myj <- j + 1
              j <- nrow(Xtract)
              if(myj > j){
                myj <- j 
              }
            }
          }
        }  
        Xtractfile <- paste(substr(file,1,nchar(file)-4), "_OL5OL7prop22.csv",sep ="")
        write.csv(Xtract, file = Xtractfile, row.names = FALSE)
        #reinitialization of Xtract
        Xtract <- csv_active
        # prop 13 
        myj <- 1
        for (i in 1:nrow(prop13RI)){
          macible = prop13RI$mz[i]
          ppm = 3
          delta = macible*ppm*1e-6
          for (j in myj:nrow(Xtract)){
            if (abs(as.numeric(Xtract$mz[j])-macible)<=delta){
              Xtract$into[j] <- as.character(as.numeric(Xtract$into[j])-prop13RI$into[i])
              myj <- j + 1
              j <- nrow(Xtract)
              if(myj > j){
                myj <- j 
              }
            }
          }
        }  
        Xtractfile <- paste(substr(file,1,nchar(file)-4), "_OL5OL7prop13.csv",sep ="")
        write.csv(Xtract, file = Xtractfile, row.names = FALSE)
        #reinitialization of Xtract
        Xtract <- csv_active
        # prop 04 
        myj <- 1
        for (i in 1:nrow(prop04RI)){
          macible = prop04RI$mz[i]
          ppm = 3
          delta = macible*ppm*1e-6
          for (j in myj:nrow(Xtract)){
            if (abs(as.numeric(Xtract$mz[j])-macible)<=delta){
              Xtract$into[j] <- as.character(as.numeric(Xtract$into[j])-prop04RI$into[i])
              myj <- j + 1
              j <- nrow(Xtract)
              if(myj > j){
                myj <- j 
              }
            }
          }
        }  
        Xtractfile <- paste(substr(file,1,nchar(file)-4), "_OL5OL7prop04.csv",sep ="")
        write.csv(Xtract, file = Xtractfile, row.names = FALSE)
      } #end of for file     
    }
  }
  ###########################################################################################
  #
  #  The last step : by the subtraction the normalization is imperfect now
  #  
  #
  #
  ###########################################################################################

#D:\Administration 2025\mansour\Rebutal-Donnees\09.subtract.NICA\260510\ConsMixSubtract

#  Processing247wd <- as.table (rbind('D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA199',
#  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA227',
#  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260506/Cons/NICA291'))
#D:\Administration 2025\mansour\Rebutal-Donnees\09.subtract.NICA\260512.22\Preparation
   Processing247wd <- as.table (rbind('D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA199',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA227',
  'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation/NICA291'))
  for (property in 1:3){  
    # the working directory
    mywd <- Processing247wd[property]
    setwd(mywd)
    input_files <- list.files(mywd, pattern = ".csv", recursive = TRUE)
    if (property == 1){macible <- 199.040068}
    if (property == 2){macible <- 227.034983}
    if (property == 3){macible <- 291.006575}
    ppm = 3
    delta = macible*ppm*1e-6
    macible.int = 0
    ## looking for the maximal value of the target
    for(i in 1:length(input_files)){
      csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
      Xtract <- csv_active
      Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
      if (as.numeric(Xtract.max$into)>=macible.int){
        macible.int<-   as.numeric(Xtract.max$into)
      }
    }
    ##the real work    
    for(i in 1:length(input_files)){
      csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
      Xtract <- csv_active
      Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
      Xtract$into = as.character(macible.int*as.numeric(csv_active$into)/as.numeric(Xtract.max$into))
      Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), ".csv",sep ="")
      write.csv(Xtract, file = Xtractfile, row.names = FALSE)  
    }
  }
  
  ###########################################################################
  #old stuff trash ideas
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################################################################
  #  2026/05/12
  #  the subtract project
  #  an automatization of real mean spectra is needed : impossible to stay whithout
  #
  ################################################################################
  #
  #  The function combineSpectra 
  #  beware of intensityFun = base::mean !
  #  beware of mzFun = base::mean !
  #  beware of method ! one can try consensusSpectrum, meanMzInts
  #  the mzd is at the moment 0.0010 2e-
  # 
  #
  ################################################################################
  # preparation are files with a high reduction of artefact by observation in Metaboanalyst
  # 6 files were used
  # 3 files created @ mzd (0.0014, 0.0010 and 0.0012)
  #
  input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/09.subtract.NICA/260512.22/Preparation'    
  setwd(input_dir) 
  input_files <- list.files(input_dir, pattern = "MA_NICA.csv", recursive = TRUE)
  for(j in seq(1,length(input_files)-5, by = 6)){
    csv_active <- read.csv2(file = input_files[[j]], sep = ",", header = TRUE)
    sp1 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #sp1 <- createMassSpectrum(mass= as.numeric(csv_active$mz), intensity= as.numeric(csv_active$into))
    csv_active <- read.csv2(file = input_files[[j+1]], sep = ",", header = TRUE)
    sp2 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #sp2 <- createMassSpectrum(mass= as.numeric(csv_active$mz), intensity= as.numeric(csv_active$into))
    csv_active <- read.csv2(file = input_files[[j+2]], sep = ",", header = TRUE)
    sp3 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #sp3 <- createMassSpectrum(mass= as.numeric(csv_active$mz), intensity= as.numeric(csv_active$into))
    csv_active <- read.csv2(file = input_files[[j+3]], sep = ",", header = TRUE)
    sp4 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #sp1 <- createMassSpectrum(mass= as.numeric(csv_active$mz), intensity= as.numeric(csv_active$into))
    csv_active <- read.csv2(file = input_files[[j+4]], sep = ",", header = TRUE)
    sp5 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #sp2 <- createMassSpectrum(mass= as.numeric(csv_active$mz), intensity= as.numeric(csv_active$into))
    csv_active <- read.csv2(file = input_files[[j+5]], sep = ",", header = TRUE)
    sp6 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
    #sp3 <- createMassSpectrum(mass= as.numeric(csv_active$mz), intensity= as.numeric(csv_active$into))
    spall <- MSpectra(sp1, sp2, sp3, sp4, sp5, sp6)
    cons <- combineSpectra(spall, mzd = 0.0010, intensityFun = base::mean, mzFun = base::mean, method = consensusSpectrum)
    basenames <- c("mz", "into")
    spectreconS <- matrix(ncol=length(basenames), nrow = cons[[1]]@peaksCount)
    colnames(spectreconS) <- c(basenames)
    for (i in 1:cons[[1]]@peaksCount){
      spectreconS[i,"mz"] <- cons[[1]]@mz[i]
      spectreconS[i,"into"] <- cons[[1]]@intensity[i]
    }
    SpectrumDF <- data.frame(spectreconS)   
    Xtractfile <- paste(substr(input_files[[j]],1,nchar(input_files[[j]])-8), "cons10_NICA.csv",sep ="")
    write.csv(SpectrumDF, file = Xtractfile, row.names = FALSE)
    cons <- combineSpectra(spall, mzd = 0.0014, intensityFun = base::mean, mzFun = base::mean, method = consensusSpectrum)
    basenames <- c("mz", "into")
    spectreconS <- matrix(ncol=length(basenames), nrow = cons[[1]]@peaksCount)
    colnames(spectreconS) <- c(basenames)
    for (i in 1:cons[[1]]@peaksCount){
      spectreconS[i,"mz"] <- cons[[1]]@mz[i]
      spectreconS[i,"into"] <- cons[[1]]@intensity[i]
    }
    SpectrumDF <- data.frame(spectreconS)   
    Xtractfile <- paste(substr(input_files[[j]],1,nchar(input_files[[j]])-8), "cons14_NICA.csv",sep ="")
    write.csv(SpectrumDF, file = Xtractfile, row.names = FALSE)
    cons <- combineSpectra(spall, mzd = 0.0012, intensityFun = base::mean, mzFun = base::mean, method = consensusSpectrum)
    basenames <- c("mz", "into")
    spectreconS <- matrix(ncol=length(basenames), nrow = cons[[1]]@peaksCount)
    colnames(spectreconS) <- c(basenames)
    for (i in 1:cons[[1]]@peaksCount){
      spectreconS[i,"mz"] <- cons[[1]]@mz[i]
      spectreconS[i,"into"] <- cons[[1]]@intensity[i]
    }
    SpectrumDF <- data.frame(spectreconS)   
    Xtractfile <- paste(substr(input_files[[j]],1,nchar(input_files[[j]])-8), "cons12_NICA.csv",sep ="")
    write.csv(SpectrumDF, file = Xtractfile, row.names = FALSE)
    
    cons <- combineSpectra(spall, mzd = 0.0010, intensityFun = base::mean, mzFun = base::mean, method = meanMzInts)
    basenames <- c("mz", "into")
    spectreconS <- matrix(ncol=length(basenames), nrow = cons[[1]]@peaksCount)
    colnames(spectreconS) <- c(basenames)
    for (i in 1:cons[[1]]@peaksCount){
      spectreconS[i,"mz"] <- cons[[1]]@mz[i]
      spectreconS[i,"into"] <- cons[[1]]@intensity[i]
    }
    SpectrumDF <- data.frame(spectreconS)   
    Xtractfile <- paste(substr(input_files[[j]],1,nchar(input_files[[j]])-8), "mmi10_NICA.csv",sep ="")
    write.csv(SpectrumDF, file = Xtractfile, row.names = FALSE)
    cons <- combineSpectra(spall, mzd = 0.0014, intensityFun = base::mean, mzFun = base::mean, method = meanMzInts)
    basenames <- c("mz", "into")
    spectreconS <- matrix(ncol=length(basenames), nrow = cons[[1]]@peaksCount)
    colnames(spectreconS) <- c(basenames)
    for (i in 1:cons[[1]]@peaksCount){
      spectreconS[i,"mz"] <- cons[[1]]@mz[i]
      spectreconS[i,"into"] <- cons[[1]]@intensity[i]
    }
    SpectrumDF <- data.frame(spectreconS)   
    Xtractfile <- paste(substr(input_files[[j]],1,nchar(input_files[[j]])-8), "mmi14_NICA.csv",sep ="")
    write.csv(SpectrumDF, file = Xtractfile, row.names = FALSE)
    cons <- combineSpectra(spall, mzd = 0.0012, intensityFun = base::mean, mzFun = base::mean, method = meanMzInts)
    basenames <- c("mz", "into")
    spectreconS <- matrix(ncol=length(basenames), nrow = cons[[1]]@peaksCount)
    colnames(spectreconS) <- c(basenames)
    for (i in 1:cons[[1]]@peaksCount){
      spectreconS[i,"mz"] <- cons[[1]]@mz[i]
      spectreconS[i,"into"] <- cons[[1]]@intensity[i]
    }
    SpectrumDF <- data.frame(spectreconS)   
    Xtractfile <- paste(substr(input_files[[j]],1,nchar(input_files[[j]])-8), "mmi12_NICA.csv",sep ="")
    write.csv(SpectrumDF, file = Xtractfile, row.names = FALSE)
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

#######################################################
#             Collision and artefacts !
#         Removal of large electronic noise : Special macible = 217.53  delta = 0.47
#         Removal of ponctual signals : 
#
      
########################################################
## supprimer une masse sur un delta macible 217.53 delta 0.47
##########################################################
input_files <- list.files(input_dir, pattern = "METABO_NI2", recursive = TRUE)
macible = 217.53
delta = 0.47
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  matches <- csv_active[abs(as.numeric(csv_active$mz) - macible) <= delta, ]
  if (nrow(matches) > 0) {
  Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0)   
 Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 14),"METABO_NICA.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }
 }
 #macible = 261.9992 delta = 0.02
#macible = 150.9956 delta = 0.02
#macible = 149.0243 delta = 0.02
#macible = 171.0451 delta = 0.02
#macible = 171.0407 delta = 0.02
#macible = 143.0502 delta = 0.02
#macible = 275.9829 delta = 0.02
#macible = 169.0294 delta = 0.02
#macible = 291.0026 delta = 0.0002
#macible = 184.80495 delta = 0.02
#macible = 248.9959 delta = 0.02
#macible = 148.9244 delta = 0.02
#macible = 155.0139 delta = 0.02
#macible = 228.0429 delta = 0.02
#macible = 190.9905 delta = 0.02
#macible = 219.0218 delta = 0.02
#macible = 231.0254 delta = 0.02
#macible = 162.9956 delta = 0.06
#macible = 99.96433 delta = 0.06
#macible = 93.00 delta = 0.02
#macible = 69.00 delta = 0.02
#macible = 74.97 delta = 0.02
#macible = 74.99 delta = 0.02
#macible = 63.0240 delta = 0.02 
#macible = 92.63 delta = 0.02
#   
########################################################
## supprimer une masse sur groupe de masse
##########################################################
input_files <- list.files(input_dir, pattern = "METABO_NICA", recursive = TRUE)
mescibles <- c(159.0451, 172.0530, 122.0329, 159.0087, 271.0247, 179.0459, 235.0123, 247.0119, 74.1614, 200.2454, 191.0270, 125.5128, 88.2153, 202.5626, 238.9612, 243.0264, 112.0486, 291.0391, 207.0173, 263.9628, 151.1403, 141.9943, 137.5695, 222.9289, 238.6633, 197.7082, 180.7108, 268.62407, 196.3099, 88.2362, 103.6957, 61.4098, 113.0778, 280.1908, 107.4421, 254.98124, 256.2020, 171.0088, 190.9906, 149.0243, 128.9749, 170.0373, 247.0166, 162.9956, 143.0502, 211.0401, 115.0553, 278.6153, 183.1008, 191.4010, 273.5199, 238.0507, 228.8560, 227.0304, 77.0270, 121.2979, 192.1288, 213.7317, 138.0002, 170.4663, 119.8403, 130.5435, 207.6218) 
#mescibles <- c(261.9992, 150.9956, 92.63, 63.0240, 74.99, 74.97, 69, 93, 99.96433) 
delta = 0.02
for(i in 1:length(input_files)){
    csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
    Xtract <- csv_active
    for (macible in mescibles){
        matches <- csv_active[abs(as.numeric(csv_active$mz) - macible) <= delta, ]
        if (nrow(matches) > 0) {
            Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0)  
        }
    }
    Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 15),"METABO_NICA.csv",sep ="")
    write.csv(Xtract, file = Xtractfile, row.names = FALSE)
}
 
 
 ################################################
 ##
 ##
 ##          VIP
 ##
 ##
 #################################################
 
 library(readr)
########################################
#creation of METABO from deiso
############################################

input_dir <- 'D:/Administration 2025/mansour/fichiers/metabo_publi/06.application.291/NEW.APPLI.publi.2.NICA.2'
 
#normalization simon
target_mzs <-  c(291.00658, 63.02402, 65.00319, 93.03369, 101.03966, 103.01896, 105.03457, 107.01388, 117.03462, 141.03458, 143.05018, 149.02435, 155.05024, 161.06076, 163.04004, 167.03489, 171.04512, 181.00618, 182.03734, 185.02434, 191.02696, 198.03221, 199.03513, 199.03819, 199.04005, 199.04481, 199.06943, 211.04008, 215.03505, 219.02186, 223.01685, 227.03043, 227.03269, 228.04289, 247.0166, 248.99586, 255.02983, 256.03767, 271.02467, 275.98293)
#full intensity
target_mzs <-  c(65.00319, 89.0397,  107.01388, 117.03462, 127.05533, 129.03463, 138.99562, 141.03458,  155.05024, 157.02941,  182.03734, 182.98537, 185.02434, 199.04005, 223.01685, 227.03505, 247.0166, 248.99586, 255.02983, 256.03767)
 
 setwd(input_dir)
input_files <- list.files(input_dir, pattern = "_METABO_NICA.csv", recursive = TRUE)
#target_mzs <- c(291.00658, 117.03462, 199.04005, 247.0166, 183.04505, 185.02434, 63.02402, 65.00319, 182.98537, 155.05024, 107.01388, 101.03966, 182.03734, 159.04508, 138.99562, 127.05533, 163.04004, 141.03458, 157.02941, 131.05021, 255.02983, 248.99586, 154.04239, 143.05018, 145.02947, 256.03767, 227.03505, 147.04514, 223.01685, 89.0397, 129.03463)

#target_mzs <- c(291.00658, 117.03462, 199.04005, 247.0166, 183.04505, 185.02434,  182.98537, 155.05024,  101.03966, 182.03734, 159.04508, 138.99562, 127.05533, 163.04004, 141.03458, 157.02941, 131.05021, 255.02983, 248.99586, 154.04239, 143.05018, 145.02947, 256.03767, 227.03505, 147.04514, 223.01685, 89.0397, 129.03463)

ppm <- 5
process_target_mz <- function(file, target_mzs) {  
  summary_table <- data.frame(mz = numeric(), into = numeric(), stringsAsFactors = FALSE) 
  csv_active <- read.csv(file, sep = ",", header = TRUE)  
  for (target_mz in target_mzs) {
    delta <- target_mz * ppm * 1e-6
    matches <- csv_active[abs(as.numeric(csv_active$mz) - target_mz) <= delta, ]
    if (nrow(matches) > 0) {
      for (i in 1:nrow(matches)) {
        summary_table <- rbind(summary_table, data.frame(mz = matches$mz[i], into = matches$into[i]))
      }
    }else{
        summary_table <- rbind(summary_table, data.frame(mz = target_mz, into = 0.0))
    }
  }
  Xtractfile <- paste(substr(file,1,nchar(file)-16), "_METABO.NICA.LOI.FI",sep ="")
  output_filename <- paste(Xtractfile,".csv",sep ="")
  write.csv(summary_table, file = output_filename, row.names = FALSE)
  cat(paste("Summary table for target mz", target_mz, "created and saved as", output_filename, "\n"))
}
for (file in input_files) {
    process_target_mz(file, target_mzs)
}
 
 
 
 
 
macible = 261.9992
delta = 0.02
macible = 150.9956
delta = 0.02
macible = 149.0243
delta = 0.02
macible = 171.0451
delta = 0.02
macible = 171.0407
delta = 0.02
macible = 143.0502
delta = 0.02
macible = 275.9829
delta = 0.02
macible = 169.0294
delta = 0.02
macible = 291.0026
delta = 0.0002
macible = 184.80495
delta = 0.02
macible = 248.9959
delta = 0.02
macible = 148.9244
delta = 0.02
macible = 155.0139
delta = 0.02
macible = 228.0429
delta = 0.02
macible = 190.9905
delta = 0.02
macible = 219.0218
delta = 0.02
macible = 231.0254
delta = 0.02
macible = 162.9956
delta = 0.06
macible = 99.96433
delta = 0.06
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  matches <- csv_active[abs(as.numeric(csv_active$mz) - macible) <= delta, ]
  if (nrow(matches) > 0) {
  Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0) 
  
  #Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 4),"_A.csv",sep ="")
 Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 15),"METABO_NICA.csv",sep ="")
  #Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 16),"METABO_NICA.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }
 }
macible = 93.00
delta = 0.02

macible = 69.00
delta = 0.02


 macible = 74.97
delta = 0.02


for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0)   
  #Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 4),"_A.csv",sep ="")
  Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 15),"METABO_NICA.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
 }  
   macible = 63.0240
delta = 0.02 
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0)   
  #Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 4),"_A.csv",sep ="")
  Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 15),"METABO_NICA.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
 }  
    macible = 65.0033
delta = 0.02 
 for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0)   
  #Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 4),"_A.csv",sep ="")
  Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 15),"METABO_NICA.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
 }
     macible = 93.0338
delta = 0.0001 
 for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0)   
  #Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 4),"_A.csv",sep ="")
  Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 15),"METABO_NICA.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
 }
      macible = 93.0330
delta = 0.0003 
 for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0)   
  #Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 4),"_A.csv",sep ="")
  Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 15),"METABO_NICA.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
 }
 
  
  
  
   
## exactmass correction if needed
 
input_dir <- 'C:/Users/COMPUTER/Documents/Mansour/Final/story/291/' 
setwd(input_dir)  
input_files <- list.files(input_dir, pattern = "_METABO_A_N.csv", recursive = TRUE)
macible = 291.006575
macible.mz = 291.006575
ppm = 3
delta = macible*ppm*1e-6
macible.int = 0
Xtractdelta1 <- list()

for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
  delta1 <- as.numeric(Xtract.max$mz)/macible.mz
  Xtractdelta1[[i]] <- delta1
  Xtract$mz = as.character(as.numeric(csv_active$mz)/Xtractdelta1[[i]])
  Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-15), "_METABO_A_N_I.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
 }
 
## creation of an extraction table
 
input_dir <- 'C:/Users/COMPUTER/Documents/Mansour/Final/metabo/291/291.60/60eV/'

setwd(input_dir)
input_files <- list.files(input_dir, pattern = "_METABO_A_N_I.csv", recursive = TRUE)
target_mzs <- c(115.05529, 117.03459)
ppm <- 3
process_target_mz <- function(target_mz) {
  delta <- target_mz * ppm * 1e-6
  summary_table <- data.frame(File = character(), mz = numeric(), into = numeric(), stringsAsFactors = FALSE) 
  for (file in input_files) {
    csv_active <- read.csv(file, sep = ",", header = TRUE)
    matches <- csv_active[abs(as.numeric(csv_active$mz) - target_mz) <= delta, ]
    if (nrow(matches) > 0) {
      for (i in 1:nrow(matches)) {
        summary_table <- rbind(summary_table, data.frame(File = file, mz = matches$mz[i], into = matches$into[i]))
      }
    }
  }
  output_filename <- paste("summary_table_", target_mz, ".csv")
  write.csv(summary_table, file = output_filename, row.names = FALSE)
  cat(paste("Summary table for target mz", target_mz, "created and saved as", output_filename, "\n"))
}
for (target_mz in target_mzs) {
  process_target_mz(target_mz)
}

## creation of files with only the target values
input_dir <- 'C:/Users/CRMPO/Documents/2024/mansour/fichiers/metabo_publi/06.application.291/291.ANI.applications.241117.1836.NICA/60eV.NICA/L. alboflavida'
input_dir <- 'C:/Users/CRMPO/Documents/2024/mansour/fichiers/metabo_publi/06.application.291/291.ANI.applications.241117.1836.NICA/60eV.NICA/M. antiqua'
input_dir <- 'C:/Users/CRMPO/Documents/2024/mansour/fichiers/metabo_publi/06.application.291/291.60eV.NICA.LOI.all.corr/OL7'
input_dir <- 'C:/Users/CRMPO/Documents/2024/mansour/fichiers/metabo_publi/06.application.291/291.60eV.NICA.LOI.all.corr/OL5'



input_dir <- 'C:/Users/CRMPO/Documents/2024/mansour/fichiers/metabo_publi/06.application.291/291.60eV.NICA.LOI.all.corr/M. antiqua'
input_dir <- 'C:/Users/CRMPO/Documents/2024/mansour/fichiers/metabo_publi/06.application.291/291.60eV.NICA.LOI.all.corr/L. alboflavida'
input_dir <- 'C:/Users/CRMPO/Documents/2024/mansour/fichiers/metabo_publi/06.application.291/291.60eV.NICA.LOI.all.corr/OL2'

input_dir <- 'C:/Users/CRMPO/Documents/2024/mansour/fichiers/metabo_publi/06.application.291/291.60eV.NICA.LOI.all.corr/OL7'
setwd(input_dir)
input_files <- list.files(input_dir, pattern = "_METABO_NICA.csv", recursive = TRUE)
#target_mzs <- c(291.00658, 117.03462, 199.04005, 247.0166, 183.04505, 185.02434, 63.02402, 65.00319, 182.98537, 155.05024, 107.01388, 101.03966, 182.03734, 159.04508, 138.99562, 127.05533, 163.04004, 141.03458, 157.02941, 131.05021, 255.02983, 248.99586, 154.04239, 143.05018, 145.02947, 256.03767, 227.03505, 147.04514, 223.01685, 89.0397, 129.03463)

target_mzs <- c(291.00658, 117.03462, 199.04005, 247.0166, 183.04505, 185.02434,  182.98537, 155.05024,  101.03966, 182.03734, 159.04508, 138.99562, 127.05533, 163.04004, 141.03458, 157.02941, 131.05021, 255.02983, 248.99586, 154.04239, 143.05018, 145.02947, 256.03767, 227.03505, 147.04514, 223.01685, 89.0397, 129.03463)

ppm <- 3
process_target_mz <- function(file, target_mzs) {  
  summary_table <- data.frame(mz = numeric(), into = numeric(), stringsAsFactors = FALSE) 
  csv_active <- read.csv(file, sep = ",", header = TRUE)  
  for (target_mz in target_mzs) {
    delta <- target_mz * ppm * 1e-6
    matches <- csv_active[abs(as.numeric(csv_active$mz) - target_mz) <= delta, ]
    if (nrow(matches) > 0) {
      for (i in 1:nrow(matches)) {
        summary_table <- rbind(summary_table, data.frame(mz = matches$mz[i], into = matches$into[i]))
      }
    }
  }
  Xtractfile <- paste(substr(file,1,nchar(file)-16), "_METABO.NICA",sep ="")
  output_filename <- paste("summary_table_", Xtractfile, ".csv")
  write.csv(summary_table, file = output_filename, row.names = FALSE)
  cat(paste("Summary table for target mz", target_mz, "created and saved as", output_filename, "\n"))
}
for (file in input_files) {
    process_target_mz(file, target_mzs)
}

117.03462, 199.04005, 247.0166, 183.04505, 185.02434, 63.02402, 65.00319, 182.98537, 155.05024, 107.01388, 101.03966, 182.03734, 159.04508, 138.99562, 127.05533, 163.04004, 141.03458, 157.02941, 131.05021, 255.02983, 248.99586, 154.04239, 143.05018, 145.02947, 256.03767, 227.03505, 147.04514, 223.01685, 89.0397, 129.03463


115.05529, 117.03459, 171.0451, 182.98541, 247.01663, 248.99589, 255.02985, 256.03769, 183.04504, 185.02432, 199.04001, 227.03503



# during the realization of the poster a deception was growing inside me 
# The normalization of 291 was interesting when it was logical that all files have around 30 %¨of 291.
# but with the real sample its intensity @60ev or @70ev for the same material is really not 30 %
# in order to figure out when it could be a better visualization possible : the 4 references are staying nomalized by max(291)
# then the files of real sample are coherce to the maximal BPI in all references spectra.
# the idea could be to give more legacy for the references than the inherent differences of the fact to be a real material

#first @60ev
reference_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/09.Test.fileinprotocol/test.proto.mat@60ev/reference'
setwd(reference_dir)  
input_files <- list.files(reference_dir, pattern = "_NICA.csv", recursive = TRUE)
macible.int = 0

## looking for the maximal value of the target
for(i in 1:length(input_files)){
      csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
      #csv_active <- read.csv2(file = input_files[[1]], sep = ",", header = TRUE)
      Xtract <- csv_active
      Xtract.max = csv_active[as.numeric(csv_active$into)== max(abs(as.numeric(csv_active$into)), na.rm = FALSE),]
      if (as.numeric(Xtract.max$into)>=macible.int){
        macible.int<-   as.numeric(Xtract.max$into)
      }
}
input_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/09.Test.fileinprotocol/test.proto.mat@60ev/real.mat' 

setwd(input_dir) 
input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)
##the real work    
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
        #csv_active <- read.csv2(file = input_files[[1]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract.max = csv_active[as.numeric(csv_active$into)== max(abs(as.numeric(csv_active$into)), na.rm = FALSE),]
  Xtract$into = as.character(macible.int*as.numeric(csv_active$into)/as.numeric(Xtract.max$into))
  Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), "_NBPI.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }
  #@60 ev : not really good, the BPI is a 117, there's then a surexpression of 227 in M antiqua
  
 #then @70ev
reference_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/09.Test.fileinprotocol/test.proto.mat@70ev/reference'
setwd(reference_dir)  
input_files <- list.files(reference_dir, pattern = "_NICA.csv", recursive = TRUE)
macible.int = 0

## looking for the maximal value of the target
for(i in 1:length(input_files)){
      csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
      #csv_active <- read.csv2(file = input_files[[1]], sep = ",", header = TRUE)
      Xtract <- csv_active
      Xtract.max = csv_active[as.numeric(csv_active$into)== max(abs(as.numeric(csv_active$into)), na.rm = FALSE),]
      if (as.numeric(Xtract.max$into)>=macible.int){
        macible.int<-   as.numeric(Xtract.max$into)
      }
}

input_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/09.Test.fileinprotocol/test.proto.mat@70ev/real.mat' 
setwd(input_dir) 
input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)
##the real work    
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
        #csv_active <- read.csv2(file = input_files[[1]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract.max = csv_active[as.numeric(csv_active$into)== max(abs(as.numeric(csv_active$into)), na.rm = FALSE),]
  Xtract$into = as.character(macible.int*as.numeric(csv_active$into)/as.numeric(Xtract.max$into))
  Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), "_NBPI.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }
#@70 ev : not really good, the BPI is a 117, there's then a surexpression of 227 in M antiqua   
  
# in protocol add the real @60 ev


reference_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/09.Test.fileinprotocol/291.NICA.4.241202@60ev/reference'
setwd(reference_dir)  
input_files <- list.files(reference_dir, pattern = "_NICA.csv", recursive = TRUE)
macible.int = 0

## looking for the maximal value of the target
for(i in 1:length(input_files)){
      csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
      #csv_active <- read.csv2(file = input_files[[1]], sep = ",", header = TRUE)
      Xtract <- csv_active
      Xtract.max = csv_active[as.numeric(csv_active$into)== max(abs(as.numeric(csv_active$into)), na.rm = FALSE),]
      if (as.numeric(Xtract.max$into)>=macible.int){
        macible.int<-   as.numeric(Xtract.max$into)
      }
}

input_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/09.Test.fileinprotocol/291.NICA.4.241202@60ev/real.mat' 
setwd(input_dir) 
input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)
##the real work    
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
        #csv_active <- read.csv2(file = input_files[[1]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract.max = csv_active[as.numeric(csv_active$into)== max(abs(as.numeric(csv_active$into)), na.rm = FALSE),]
  Xtract$into = as.character(macible.int*as.numeric(csv_active$into)/as.numeric(Xtract.max$into))
  Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), "_NBPI.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }
  
  #not working
  
  # (test protocol + real.mat@60ev ) all with 291(int) = 2952004
  #Normalization of metabo for folder 291 with m/z =291 
#C:\Users\CRMPO\Documents\2025\mansour\STORY\09.Test.fileinprotocol\291.NICA.4.241202+Real.mat
input_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/09.Test.fileinprotocol/291.NICA.4.241202+Real.mat/reference' 
setwd(input_dir)  
input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)
macible = 291.006575
ppm = 3
delta = macible*ppm*1e-6
macible.int = 0

## looking for the maximal value of the target
for(i in 1:length(input_files)){
      csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
      Xtract <- csv_active
      Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
      if (as.numeric(Xtract.max$into)>=macible.int){
        macible.int<-   as.numeric(Xtract.max$into)
      }
}
##the real work   on real mat
input_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/09.Test.fileinprotocol/291.NICA.4.241202+Real.mat/real.mat' 
setwd(input_dir) 
input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
  Xtract$into = as.character(macible.int*as.numeric(csv_active$into)/as.numeric(Xtract.max$into))
  Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), "_N.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }
  
  # not bad at all the samples are coherent with what is  known
  
  
  # (test protocol + real.mat@70ev ) all with 291(int) = 2952004
  #Normalization of metabo for folder 291 with m/z =291 

input_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/09.Test.fileinprotocol/291.NICA.4.241202+Real.mat@70ev/reference' 
setwd(input_dir)  
input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)
macible = 291.006575
ppm = 3
delta = macible*ppm*1e-6
macible.int = 0

## looking for the maximal value of the target
for(i in 1:length(input_files)){
      csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
      Xtract <- csv_active
      Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
      if (as.numeric(Xtract.max$into)>=macible.int){
        macible.int<-   as.numeric(Xtract.max$into)
      }
}
##the real work   on real mat @ 70ev
input_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/09.Test.fileinprotocol/291.NICA.4.241202+Real.mat@70ev/real.mat' 
setwd(input_dir) 
input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
  Xtract$into = as.character(macible.int*as.numeric(csv_active$into)/as.numeric(Xtract.max$into))
  Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), "_N.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }
  
  # PC1 and PC2 separation of real mat 
 
 ##########################################################################################################
 
  # we are going to produce the substract files
  # @ 60eV
  # the 247 will be taken as an exact amount of a mixture of OL5+OL7.
  # the residual file will be normalized with 291 
  library(MSnbase)
  #C:\Users\CRMPO\Documents\2025\mansour\STORY\06.application.291\60eV.NICA
  input_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/06.application.291/60eV.NICA/OL5+7' 
setwd(input_dir)  
#https://rdrr.io/bioc/MSnbase/man/meanMzInts.html
#https://rdrr.io/bioc/MSnbase/man/consensusSpectrum.html

input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)

csv_active <- read.csv2(file = input_files[[1]], sep = ",", header = TRUE)
sp1 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
csv_active <- read.csv2(file = input_files[[2]], sep = ",", header = TRUE)
sp2 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
csv_active <- read.csv2(file = input_files[[3]], sep = ",", header = TRUE)
sp3 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
csv_active <- read.csv2(file = input_files[[4]], sep = ",", header = TRUE)
sp4 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
csv_active <- read.csv2(file = input_files[[5]], sep = ",", header = TRUE)
sp5 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
csv_active <- read.csv2(file = input_files[[6]], sep = ",", header = TRUE)
sp6 <- new("Spectrum1", mz = as.numeric(csv_active$mz), intensity = as.numeric(csv_active$into))
spall <- MSpectra(sp1, sp2, sp3,sp4, sp5, sp6)
cons <- consensusSpectrum(spall, mzd = 0.0001, minProp = 1/6)
input_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/06.application.291/60eV.NICA/real.mat' 
setwd(input_dir)  
input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)
macible = 247.0166
ppm = 3
delta = macible*ppm*1e-6 
basenames <- c("mz", "into")
spectreconS <- matrix(ncol=length(basenames), nrow = cons@peaksCount)
colnames(spectreconS) <- c(basenames)
for (i in 1:cons@peaksCount){
spectreconS[i,"mz"] <- cons@mz[i]
spectreconS[i,"into"] <- cons@intensity[i]
}
spectreconS <- data.frame(spectreconS)
monspectre <- spectreconS
for(file in input_files){
      monspectre <- spectreconS
      csv_active <- read.csv2(file = file, sep = ",", header = TRUE)
      Xtract <- csv_active
      macible = 247.0166
      ppm = 3
      delta = macible*ppm*1e-6
      Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
      macible.int<- 0.00
      #in each file the intensity of 247.0166 is found
      if (as.numeric(Xtract.max$into) >= macible.int){
        macible.int<-  as.numeric(Xtract.max$into)
      }
      if (macible.int>0){
      #generation of the monspectre with the right intensity
      monspectre.max = spectreconS[abs(spectreconS$mz-macible)<=delta,]
      monspectre$into[] <- macible.int*(monspectre$into[])/monspectre.max$into      
      }
      # 
      for (i in 1:nrow(monspectre)){
        macible = monspectre$mz[i]
        ppm = 3
        delta = macible*ppm*1e-6
        for (j in 1:nrow(Xtract)){
          if (abs(as.numeric(Xtract$mz[j])-macible)<=delta){
              if ((as.numeric(Xtract$into[j])-monspectre$into[i])<=0.01){
                Xtract$into[j] <- "0.00"      
              } else {
                Xtract$into[j] <- as.character(as.numeric(Xtract$into[j])-monspectre$into[i])
              }      
          }
        }
      }    
      Xtractfile <- paste(substr(file,1,nchar(file)-4), "_substractOL5&7.csv",sep ="")
      write.csv(Xtract, file = Xtractfile, row.names = FALSE)
}
# the substraction is done
# the substract file must be renormalized by 291 
# C:\Users\CRMPO\Documents\2025\mansour\STORY\06.application.291\60eV.NICA.Substract

#Normalization of metabo for folder 291 with m/z =291 

input_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/06.application.291/60eV.NICA.Substract/reference' 
setwd(input_dir)  
input_files <- list.files(input_dir, pattern = "_NICA.csv", recursive = TRUE)
macible = 291.006575
ppm = 3
delta = macible*ppm*1e-6
macible.int = 0

## looking for the maximal value of the target
for(i in 1:length(input_files)){
      csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
      Xtract <- csv_active
      Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
      if (as.numeric(Xtract.max$into)>=macible.int){
        macible.int<-   as.numeric(Xtract.max$into)
      }
}
##the real work   on real mat @ 70ev
input_dir <- 'C:/Users/CRMPO/Documents/2025/mansour/STORY/06.application.291/60eV.NICA.Substract/real.mat' 
setwd(input_dir) 
input_files <- list.files(input_dir, pattern = "_substractOL5&7.csv", recursive = TRUE)
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  Xtract.max = csv_active[abs(as.numeric(csv_active$mz)-macible)<=delta,]
  Xtract$into = as.character(macible.int*as.numeric(csv_active$into)/as.numeric(Xtract.max$into))
  Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), "_N.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }
 
 
 
 
  
  #########################
  #
  #   nettoyage <100 et >288
  #
  #########################
  input_files <- list.files(input_dir, pattern = "METABO_NI", recursive = TRUE)
macible = 100
delta = 0
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  matches <- csv_active[(as.numeric(csv_active$mz) - macible) <= delta, ]
  if (nrow(matches) > 0) {
  Xtract[(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0)   
 Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 13),"METABO_NI1.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }
 }
  input_files <- list.files(input_dir, pattern = "METABO_NI1", recursive = TRUE)

macible = 280
delta = 0
for(i in 1:length(input_files)){
  csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
  Xtract <- csv_active
  matches <- csv_active[(as.numeric(csv_active$mz) - macible) >= delta, ]
  if (nrow(matches) > 0) {
  Xtract[(as.numeric(csv_active$mz) - macible) >= delta,]$into <- as.character(0.0)   
 Xtractfile <- paste(substr(input_files[[i]], 1, nchar(input_files[[i]]) - 14),"METABO_NI2.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  }
 }
  