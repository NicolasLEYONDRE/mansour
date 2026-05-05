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
setwd(input_dir)
input_files <- list.files(input_dir, pattern = "_METABO.csv", recursive = TRUE)
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
  Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-25), "_DP_199_MA_N.csv",sep ="")
  write.csv(Xtract, file = Xtractfile, row.names = FALSE)
  #Xtractfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-25), "_DP_227_MA_N.csv",sep ="")
  #write.csv(Xtract, file = Xtractfile, row.names = FALSE)
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

setwd(input_dir)
input_files <- list.files(input_dir, pattern = "NICA.csv", recursive = TRUE)
mescibles <- c(92.63,  74.99, 74.97, 69, 93,  82.9694,   98.9643)
delta = 0.02


mescibles <- c(65.0033, 63.0240, 150.9955, 199.0357, 162.9955, 199.0357, 113.0033, 89.0033,  93.0337, 93.0345, 135.0088, 171.0451, 261.9992, 171.0406, 149.0243, 155.0138, 131.0138, 190.9905, 247.9836, 263.0070, 128.9749, 169.0294, 231.0254, 218.0067, 127.0189, 161.0607, 167.0349, 115.0553, 170.0328, 170.0372, 143.0501, 105.0345, 115.0189, 197.0244, 171.0087, 121.0295, 173.0243, 99.0240, 143.0138, 164.4106)
delta = 0.0010



for(i in 1:length(input_files)){
    csv_active <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
    Xtract <- csv_active
    for (macible in mescibles){
        matches <- csv_active[abs(as.numeric(csv_active$mz) - macible) <= delta, ]
        if (nrow(matches) > 0) {
            Xtract[abs(as.numeric(csv_active$mz) - macible) <= delta,]$into <- as.character(0.0)  
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
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########################################################################
  #old stuff trash ideas
  
  
  
  
  
  
  
  
  

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
  