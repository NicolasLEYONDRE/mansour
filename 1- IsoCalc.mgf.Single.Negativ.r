library("MSnbase")
library(readr)
# Set directory
input_dir <- 'D:/Rapport-Mansour-TCHEDRE/291/mgf'
#C:/Users/CRMPO\Documents\2023\publi fragmentation\NG_PU_marqués\files_processed\0_peaklists\MSMS\mgf\STD.NEG
#D:/Projet-NG/files_processed/0_peaklists/MSMS/mgf/STD.NEG
setwd(input_dir) 
input_files <- list.files(input_dir, pattern = "[.]mgf", full.names = TRUE, recursive = TRUE)
n <- length(input_files)
#creating summed scans

for(i in (1:n)){
	# read the mgf file as scan
	monfichier <- readMgfData(input_files[[i]])
	spec <- MSpectra(spectra(monfichier))    
    comS <- combineSpectra(spec, mzd = 0.0005, ppm = 0.5, intensityFun = max)
    #mzd = 0.0005, ppm = 0.5, intensityFun = max,
    mzMAX <- comS[[1]]@precursorMz+19
    myPeaklist <- NA
    myPeaklist$mz <- NA
    myPeaklist$into <- NA
    myPeaklist$mz <- comS[[1]]@mz
    myPeaklist$into <- comS[[1]]@intensity
  	basenames <- c("uha100", "is_monoiso", "estim_nC", "estim_nN", "estim_nO", "estim_nS", "estim_nCl", "estim_nBr")
  	isopeaklist_gen <- matrix(ncol=length(basenames))
  	colnames(isopeaklist_gen) <- c(basenames) 
    mat.list <- list(myPeaklist,isopeaklist_gen)
  	df.list <- lapply(mat.list, as.data.frame)
  	cat.df <- function(d1,d2) {d1[names(d2)] <- d2; d1}
  	as_one.df <- Reduce(cat.df, df.list)
 	isopeaklist <- as_one.df[order(as.numeric(as_one.df$mz)),]
    isopeaklist <- subset(isopeaklist, select = -NA.)  
    isopeaklist <- subset(isopeaklist, isopeaklist$mz <= mzMAX, drop = TRUE)

    myboolean <- FALSE
    gauge <- 100
    pass <- 1
    while(myboolean == FALSE) {
        if(pass != 10) {    
            bp_into <- as.numeric(max(as.numeric(isopeaklist[,"into"]), na.rm =TRUE))
            isopeaklist$uha100 <- as.numeric((isopeaklist$into - (isopeaklist$into %% gauge))/gauge)    
            frequence <- table(isopeaklist$uha100)
            freqMAX <- max(frequence)
            freqMAXm <- (freqMAX - (freqMAX %% 1000))/1000
            freqMAXtemp <- (freqMAX - freqMAXm*1000)
            freqMAXc <- (freqMAXtemp - (freqMAXtemp %% 100))/100
            freqMAXtemp <- (freqMAX - freqMAXm*1000 - freqMAXc*100)
            freqMAXd <- (freqMAXtemp - (freqMAXtemp %% 10))/10
            freqMAXu <- freqMAXtemp %% 10
            freqCUT <- max(as.numeric(names(frequence[frequence == freqMAX])))
            freqCUTm <- (freqCUT - (freqCUT %% 1000))/1000
            freqCUTtemp <- (freqCUT - freqCUTm*1000)
            freqCUTc <- (freqCUTtemp - (freqCUTtemp %% 100))/100
            freqCUTtemp <- (freqCUT - freqCUTm*1000 - freqCUTc*100)
            freqCUTd <- (freqCUTtemp - (freqCUTtemp %% 10))/10
            freqCUTu <- freqCUTtemp %% 10
            if(freqMAXm>0) { #freqMAXm <>0 freqMAX> 1000 => we need to reduce the gauge
                mycoef <- (freqMAX/700)/0.8 # increase of a bit to compensate
                gauge <- round(gauge/mycoef,0)
                pass <- pass + 1            
            } else if(freqMAXc>0) {  #freqMAXm =0 and freqmaxc<> 0 freqmax > 100 => not bad we have to focus between 5 and 8 
                if(freqMAXc<5){ # freqMAX 499 and lesscan be adjusted by expanding the gauge 
                    mycoef <- (700/freqMAX)/0.8 # increase of a bit to compensate
                    gauge <- round(gauge*mycoef,0)
                    pass <- pass + 1
                } else if(freqMAXc>8) { # freqMAX 900 and more can be adjusted by reducing the gauge
                    mycoef <- (freqMAX/700)/0.8 # increase of a bit to compensate
                    gauge <- round(gauge/mycoef,0)
                    pass <- pass + 1
                } else { # freqMAX should be perfect now but what about freqCUT
                    if(freqCUTm>0) {
                        #case B FreqCUT of thousands with freqMAX of 500 to 899
                        # simple acceptation with comments  
                        isopeaklist <- subset(isopeaklist, isopeaklist$uha100 >= freqCUT, drop = TRUE) 
                        CSVfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), pass,sep =".")
                        CSVfile <- paste(CSVfile,"B", sep =".")
                        CSVfile <- paste(CSVfile,gauge, sep =".")
                        CSVfile <- paste(CSVfile,freqCUT , sep =".")
                        CSVfile <- paste(CSVfile,freqMAX , sep =".")
                        CSVfile <- paste(CSVfile,".csv" , sep ="")
                        write.csv(isopeaklist, file = CSVfile)
                        myboolean <- TRUE
                    } else if(freqCUTc>0) {
                        #case B FreqCUT of hundreds with freqMAX of 500 to 899
                        # simple acceptation with comments  
                        isopeaklist <- subset(isopeaklist, isopeaklist$uha100 >= freqCUT, drop = TRUE) 
                        CSVfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), pass,sep =".")
                        CSVfile <- paste(CSVfile,"B", sep =".")
                        CSVfile <- paste(CSVfile,gauge, sep =".")
                        CSVfile <- paste(CSVfile,freqCUT , sep =".")
                        CSVfile <- paste(CSVfile,freqMAX , sep =".")
                        CSVfile <- paste(CSVfile,".csv" , sep ="")
                        write.csv(isopeaklist, file = CSVfile)
                        myboolean <- TRUE
                    } else if(freqCUTd>0) {if(freqCUTd==1) {if(freqCUTu<5) {
                                p_thresh <- freqCUT*gauge
                                # the freqCUT is too low there's certainly at least 2 levels of noise in the spectrum
                                # best way remove the low intensity peaks
                                # empiric observation 2.2 e-5
                                q_thresh <- as.numeric(2.2e-5*bp_into)
                                if(p_thresh <= q_thresh) {
                                    isopeaklist <- subset(isopeaklist, isopeaklist$into >= q_thresh, drop = TRUE)
                                    #start with normal gauge
                                    gauge <- 100
                                    #start over   
                                    pass <- pass + 1
                                } else {
                                    # CASE C FreqCUT of 10 to 14 with freqMAX of 500 to 899
                                    # acceptation with comments
                                    isopeaklist <- subset(isopeaklist, isopeaklist$uha100 >= freqCUT, drop = TRUE) 
                                    CSVfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), pass,sep =".")
                                    CSVfile <- paste(CSVfile,"C", sep =".")
                                    CSVfile <- paste(CSVfile,gauge, sep =".")
                                    CSVfile <- paste(CSVfile,freqCUT , sep =".")
                                    CSVfile <- paste(CSVfile,freqMAX , sep =".")
                                    CSVfile <- paste(CSVfile,".csv" , sep ="")
                                    write.csv(isopeaklist, file = CSVfile)    
                                    myboolean <- TRUE
                                }
                            } else { #FreqCUT is a bit low
                                mycoef <- (20/freqCUT) 
                                gauge <- round(gauge/mycoef,0)                      
                                pass <- pass + 1                   
                            }                    
                        } else if(freqCUTd>7) { #FreqCUT is a bit to high
                            mycoef <- (freqCUT/70) 
                            gauge <- round(gauge*mycoef,0)                  
                            pass <- pass + 1                   
                        } else {
                            #case A 
                            # there we are perfection between 20 and 70
                            # the group of peaks found is a statistical noise created by ionization and processing
                            # it's like a gaussian of intensity population but one has to be careful
                            # the process will remove all the next groups downto to 0.7*freqMAX
                            freq.target <- round(freqMAX*0.7,0) 
                            my_i <- 1
                            while(myboolean == FALSE) {                                
                                if(frequence[[as.character(freqCUT + my_i)]] <= freq.target) {
                                    #FOUND
                                    isopeaklist <- subset(isopeaklist, isopeaklist$uha100 > freqCUT + my_i, drop = TRUE) 
                                    CSVfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), pass,sep =".")
                                    CSVfile <- paste(CSVfile,"A", sep =".")
                                    CSVfile <- paste(CSVfile,gauge, sep =".")
                                    CSVfile <- paste(CSVfile,freqCUT , sep =".")
                                    CSVfile <- paste(CSVfile,freqMAX , sep =".")
                                    CSVfile <- paste(CSVfile,".csv" , sep ="")
                                    write.csv(isopeaklist, file = CSVfile)
                                    myboolean <- TRUE                                
                                } else {
                                    my_i <- my_i + 1
                                }
                            }
                        }
                    } else {
                        p_thresh <- freqCUT*gauge
                        # the freqCUT is too low there's certainly at least 2 levels of noise in the spectrum
                        # best way remove the low intensity peaks
                        # empiric observation 2.2 e-5
                        q_thresh <- as.numeric(2.2e-5*bp_into)
                        if(p_thresh <= q_thresh) {
                            isopeaklist <- subset(isopeaklist, isopeaklist$into >= q_thresh, drop = TRUE)
                            #start with normal gauge
                            gauge <- 100
                            #start over   
                            pass <- pass + 1
                        } else {
                            # CASE C FreqCUT of 0 to 9 with freqMAX of 500 to 899
                            # simple acceptation with comments  
                            isopeaklist <- subset(isopeaklist, isopeaklist$uha100 >= freqCUT, drop = TRUE) 
                            CSVfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), pass,sep =".")
                            CSVfile <- paste(CSVfile,"C", sep =".")
                            CSVfile <- paste(CSVfile,gauge, sep =".")
                            CSVfile <- paste(CSVfile,freqCUT , sep =".")
                            CSVfile <- paste(CSVfile,freqMAX , sep =".")
                            CSVfile <- paste(CSVfile,".csv" , sep ="")
                            write.csv(isopeaklist, file = CSVfile)
                            myboolean <- TRUE
                        }
                    }
                }            
            } else if(freqMAXd>0) {  #freqMAXm =0 and freqmaxc = 0 freqmax > 10 => we need to expand the gauge 
                mycoef <- (700/freqMAX)/0.8 # increase of a bit to compensate
                gauge <- round(gauge*mycoef,0)  
                pass <- pass + 1
            }
        } else {
            # CASE D NOT PASS
                            CSVfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), pass,sep =".")
                            CSVfile <- paste(CSVfile,"D.Manual.csv", sep =".")
                            write.csv(isopeaklist, file = CSVfile)
                            myboolean <- TRUE     
        }
    }

	#POS
  	#correction <- +0.000549 
	#NEG
	correction <- -0.000549 
    bp_into <- as.numeric(max(as.numeric(isopeaklist[,"into"]), na.rm =TRUE))
    q_thresh <- as.numeric(0.01/100*bp_into)
    nmax <- nrow(isopeaklist)
  	for (p in (1:nmax)){
            nmin <- p
            into_M <- as.numeric(isopeaklist[p,"into"])
            p_thresh <- as.numeric(0.01/100*bp_into)
           if ((into_M >= p_thresh)==TRUE){ 
        
    		for (q in (nmin:nmax)){
      			mz_M <- as.numeric(isopeaklist[p,"mz"])
      			mz_M1 <- as.numeric(isopeaklist[q,"mz"])
      			into_M <- as.numeric(isopeaklist[p,"into"])
      			into_M1 <- as.numeric(isopeaklist[q,"into"])   
                delta <- (mz_M1-mz_M)  
                 # Set ppm cut-off
                ppm_value <- 3
                deiso_ppm <- 1e-6 * mz_M * ppm_value  
                if(((delta>=(0.9970-deiso_ppm))==TRUE)&((delta<=(2.0043+deiso_ppm))==TRUE)){    

  			# Set maximum intensity for isotope contribution depanding on m/z (see Table)
    			## Correction of electron mass      
        		target <- mz_M+correction     
    			## Setting upper limit for the estimated number of elements depending on m/z
    			if((target <= 200)==TRUE){
     				Cmax <- 15
      				Nmax <- 8
      				Omax <- 7
     				Smax <- 6
      				Clmax <- 4
      				Brmax <- 2
    			}     
    			if(((target>200)==TRUE)&((target<=400)==TRUE)){
      				Cmax <- 30
      				Nmax <- 10
      				Omax <- 14
      				Smax <- 12
      				Clmax <- 7
      				Brmax <- 4      
    			}

    			if(((target>400)==TRUE)&((target<=600)==TRUE)){
      				Cmax <- 42
      				Nmax <- 13
      				Omax <- 21
      				Smax <- 12
      				Clmax <- 8
      				Brmax <- 6      
    			}  
    
    			if(((target>600)==TRUE)&((target<=800)==TRUE)){
      				Cmax <- 56
      				Nmax <- 16
      				Omax <- 25
      				Smax <- 20
      				Clmax <- 10
      				Brmax <- 8      
    			}
    
    			if(((target>800)==TRUE)&((target<=1000)==TRUE)){
      				Cmax <- 66
      				Nmax <- 25
      				Omax <- 37
      				Smax <- 20
      				Clmax <- 11
      				Brmax <- 8      
    			}
    
    			if(((target>1000)==TRUE)&((target<=1500)==TRUE)){
     				Cmax <- 100
      				Nmax <- 26
      				Omax <- 44
      				Smax <- 20
      				Clmax <- 11
      				Brmax <- 8      
    			}
   			if(((target>1500)==TRUE)&((target<=2000)==TRUE)){
      				Cmax <- 134
      				Nmax <- 27
      				Omax <- 51
      				Smax <- 20
      				Clmax <- 11
      				Brmax <- 8      
    			}  
    			if(((target>2000)==TRUE)&((target<=2500)==TRUE)){
      				Cmax <- 168
      				Nmax <- 28
      				Omax <- 58
      				Smax <- 20
      				Clmax <- 11
      				Brmax <- 8      
    			} 
    			if(((target>2500)==TRUE)&((target<=3000)==TRUE)){
      				Cmax <- 202
      				Nmax <- 29
      				Omax <- 65
      				Smax <- 20
      				Clmax <- 11
      				Brmax <- 8      
    			} 
                    percent_M1 <- (into_M1/into_M)*100
                    est_nC <- percent_M1/1.0815728
                    est_nO <- percent_M1/0.20549
                    est_nN <- percent_M1/0.36533
                    est_nS33 <- percent_M1/0.78956
                    est_nS34 <- percent_M1/4.47416
                    est_nCl <- percent_M1/31.99578
                    est_nBr <- percent_M1/97.27757		
    			bp_into <- as.numeric(max(isopeaklist[,"into"], na.rm =TRUE))
    			q_thresh <- 0.01/100*bp_into
     			if (((mz_M + 1.003355 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 1.003355 + deiso_ppm)==TRUE)) {
                         				
                    if((is.na(into_M1)==FALSE)&(is.na(into_M)==FALSE)) {
                        if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nC <= Cmax)==TRUE)) {
                                isopeaklist[p,"estim_nC"] = round(est_nC)
                                isopeaklist[q,"is_monoiso"] = "13C" 
                        }
                    }
                    else{
                        if((is.na(into_M)==TRUE)) {
                                isopeaklist[p,"estim_nC"] = paste("nointensity,", into_M1)
                        }
                    }                    
                }                    
    			if (((mz_M + 2.004245 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 2.004245 + deiso_ppm)==TRUE)) {
                                   
                    if((is.na(into_M1)==FALSE)&(is.na(into_M)==FALSE)) {
                        if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nO <= Omax)==TRUE)) {
                            isopeaklist[p,"estim_nO"] = round(est_nO)
                            isopeaklist[q,"is_monoiso"] = "18O"
                        }
                    }                   
    			}
    			if (((mz_M + 0.997035 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 0.997035 + deiso_ppm)==TRUE)) {
                                   
                    if((is.na(into_M1)==FALSE)&(is.na(into_M)==FALSE)) {      				
                    if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nN <= Nmax)==TRUE)) {
        				isopeaklist[p,"estim_nN"] = round(est_nN)
                        isopeaklist[q,"is_monoiso"] = "15N"
      				}
                    }
                }
			## Sulphur
    			### 34S
    			if (((mz_M + 1.995796 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 1.995796 + deiso_ppm)==TRUE)) {
             
                    if((is.na(into_M1)==FALSE)&(is.na(into_M)==FALSE)) {                 
      				if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nS34 <= Smax)==TRUE)) {                    
        				isopeaklist[p,"estim_nS"] = round(est_nS34) 
                        isopeaklist[q,"is_monoiso"] = "34S"  
      				} 
                    }                  
    			} 
    			### 33S (might be detected as well if the number of S atoms is high, but is least adapted for calculation of estim_nS)
    			if (((mz_M + 0.999388 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 0.999388 + deiso_ppm)==TRUE)) {
                    if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nS33 <= Smax)==TRUE)) {                    
        				isopeaklist[p,"estim_nS"] = round(est_nS33) 
                    isopeaklist[q,"is_monoiso"] = "33S"
                    } 
                    } 
                }
			## Halogens
    			#### Bromine (unlock code if necessary)
			#      if (((mz_M + 1.997952 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 1.997952 + deiso_ppm)==TRUE)) {
			#        if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nBr <= Brmax)==TRUE)) {
			#          isopeaklist[q,"is_monoiso"] = "81Br"
			#          isopeaklist[p,"estim_nBr"] = round(est_nBr)
			#        } 
			#      } 
    			### Chlorine
      			if (((mz_M + 1.997050 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 1.997050 + deiso_ppm)==TRUE)) {
        			if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nCl <= Clmax)==TRUE)) {
          				isopeaklist[q,"is_monoiso"] = "37Cl"
          				isopeaklist[p,"estim_nCl"] = round(est_nCl)
        			} 
      			} 
                
            }#end of 2 Da    
            else{   
                # don't need to go after the THR zone
                if((delta>(2.0043+deiso_ppm))==TRUE){ 
                    q <- nmax
                }
            }        
      		} # end of q
            #THRS
            else{isopeaklist[p,"is_monoiso"]="THRS"}  
        } 
                    
            
           
            
	} # end of p  
  	for(x in (1:nrow(isopeaklist))){
    		if(is.na(isopeaklist[x,"is_monoiso"]==TRUE)==TRUE){isopeaklist[x,"is_monoiso"]=1}
 	}# end of x
      	## Creates a CSV export file (unlock to print peaklist w/ identified isotopic clusters)          
     	 CSVfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), "_isopeaklist.CSV",sep ="")
     	 write.csv(isopeaklist, file = CSVfile)    
    	deisopeaklist <- subset(isopeaklist, isopeaklist[,"is_monoiso"] == 1)
        #lock.ref=257.247507 # ASAP + LOW
	#lock.ref=413.266231 # ESI +
	#lock.ref=391.284286 # ASAP + MID
     # 	a<-which(abs(as.numeric(deisopeaklist[,"mz"])-lock.ref)==min(abs(as.numeric(deisopeaklist[,"mz"])-lock.ref)))
     # 	lock.mes<-as.numeric(deisopeaklist[a,"mz"])
     # 	lock.into<-as.numeric(deisopeaklist[a,"into"])
     # 	c <- lock.ref/lock.mes      
     # 	for(z in 1:nrow(deisopeaklist)){
     #   	mz.mes <- as.numeric(deisopeaklist[z,"mz"])
      #  	mz.into <- as.numeric(deisopeaklist[z,"into"])
      #  	deisopeaklist[z,"mz"] = c*mz.mes
      #	}# end of z    
	## Creates a CSV export file
	#CSVfile <- paste(substr(input_files[[61]],1,nchar(input_files[[1]])-6), "_deisopeaklist.CSV",sep ="")
	CSVfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-4), "_deisopeaklist.CSV",sep ="")
	write.csv(deisopeaklist, file = CSVfile)
} 
    
    
    
    
    
    #############################################
    #
    # from the noise.nly to deisoto 
    # spectrum in negative ion mode
    #############################################
# Set directory
#input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/02.Noise.NLY'
input_dir <- 'D:/Administration 2025/mansour/Rebutal-Donnees/02.Noise.NLY/OLs/2605011540'

setwd(input_dir) 
input_files <- list.files(input_dir, pattern = "[.]csv", full.names = TRUE, recursive = TRUE)
n <- length(input_files) 
for(i in (1:n)){
    isopeaklist <- read.csv2(file = input_files[[i]], sep = ",", header = TRUE)
	#NEG
	correction <- -0.000549 
    bp_into <- as.numeric(max(as.numeric(isopeaklist[,"into"]), na.rm =TRUE))
    q_thresh <- as.numeric(0.01/100*bp_into)
    nmax <- nrow(isopeaklist)
  	for (p in (1:nmax)){
            nmin <- p
            into_M <- as.numeric(isopeaklist[p,"into"])
            p_thresh <- as.numeric(0.01/100*bp_into)
           if ((into_M >= p_thresh)==TRUE){        
    		for (q in (nmin:nmax)){
      			mz_M <- as.numeric(isopeaklist[p,"mz"])
      			mz_M1 <- as.numeric(isopeaklist[q,"mz"])
      			into_M <- as.numeric(isopeaklist[p,"into"])
      			into_M1 <- as.numeric(isopeaklist[q,"into"])   
                delta <- (mz_M1-mz_M)  
                 # Set ppm cut-off
                ppm_value <- 3
                deiso_ppm <- 1e-6 * mz_M * ppm_value  
                # protection inside the THR zone 
                if(((delta>=(0.9970-deiso_ppm))==TRUE)&((delta<=(2.0043+deiso_ppm))==TRUE)){    

  			# Set maximum intensity for isotope contribution depanding on m/z (see Table)
    			## Correction of electron mass      
        		target <- mz_M+correction     
    			## Setting upper limit for the estimated number of elements depending on m/z
    			if((target <= 200)==TRUE){
     				Cmax <- 15
      				Nmax <- 8
      				Omax <- 7
     				Smax <- 6
      				Clmax <- 4
      				Brmax <- 2
    			}     
    			if(((target>200)==TRUE)&((target<=400)==TRUE)){
      				Cmax <- 30
      				Nmax <- 10
      				Omax <- 14
      				Smax <- 12
      				Clmax <- 7
      				Brmax <- 4      
    			}

    			if(((target>400)==TRUE)&((target<=600)==TRUE)){
      				Cmax <- 42
      				Nmax <- 13
      				Omax <- 21
      				Smax <- 12
      				Clmax <- 8
      				Brmax <- 6      
    			}  
    
    			if(((target>600)==TRUE)&((target<=800)==TRUE)){
      				Cmax <- 56
      				Nmax <- 16
      				Omax <- 25
      				Smax <- 20
      				Clmax <- 10
      				Brmax <- 8      
    			}
    
    			if(((target>800)==TRUE)&((target<=1000)==TRUE)){
      				Cmax <- 66
      				Nmax <- 25
      				Omax <- 37
      				Smax <- 20
      				Clmax <- 11
      				Brmax <- 8      
    			}
    
    			if(((target>1000)==TRUE)&((target<=1500)==TRUE)){
     				Cmax <- 100
      				Nmax <- 26
      				Omax <- 44
      				Smax <- 20
      				Clmax <- 11
      				Brmax <- 8      
    			}
   			if(((target>1500)==TRUE)&((target<=2000)==TRUE)){
      				Cmax <- 134
      				Nmax <- 27
      				Omax <- 51
      				Smax <- 20
      				Clmax <- 11
      				Brmax <- 8      
    			}  
    			if(((target>2000)==TRUE)&((target<=2500)==TRUE)){
      				Cmax <- 168
      				Nmax <- 28
      				Omax <- 58
      				Smax <- 20
      				Clmax <- 11
      				Brmax <- 8      
    			} 
    			if(((target>2500)==TRUE)&((target<=3000)==TRUE)){
      				Cmax <- 202
      				Nmax <- 29
      				Omax <- 65
      				Smax <- 20
      				Clmax <- 11
      				Brmax <- 8      
    			} 
                    percent_M1 <- (into_M1/into_M)*100
                    est_nC <- percent_M1/1.0815728
                    est_nO <- percent_M1/0.20549
                    est_nN <- percent_M1/0.36533
                    est_nS33 <- percent_M1/0.78956
                    est_nS34 <- percent_M1/4.47416
                    est_nCl <- percent_M1/31.99578
                    est_nBr <- percent_M1/97.27757		
    			bp_into <- as.numeric(max(isopeaklist[,"into"], na.rm =TRUE))
    			q_thresh <- 0.01/100*bp_into
     			if (((mz_M + 1.003355 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 1.003355 + deiso_ppm)==TRUE)) {
                         				
                    if((is.na(into_M1)==FALSE)&(is.na(into_M)==FALSE)) {
                        if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nC <= Cmax)==TRUE)) {
                                isopeaklist[p,"estim_nC"] = round(est_nC)
                                isopeaklist[q,"is_monoiso"] = "13C" 
                        }
                    }
                    else{
                        if((is.na(into_M)==TRUE)) {
                                isopeaklist[p,"estim_nC"] = paste("nointensity,", into_M1)
                        }
                    }                    
                }                    
    			if (((mz_M + 2.004245 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 2.004245 + deiso_ppm)==TRUE)) {
                                   
                    if((is.na(into_M1)==FALSE)&(is.na(into_M)==FALSE)) {
                        if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nO <= Omax)==TRUE)) {
                            isopeaklist[p,"estim_nO"] = round(est_nO)
                            isopeaklist[q,"is_monoiso"] = "18O"
                        }
                    }                   
    			}
    			if (((mz_M + 0.997035 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 0.997035 + deiso_ppm)==TRUE)) {
                                   
                    if((is.na(into_M1)==FALSE)&(is.na(into_M)==FALSE)) {      				
                    if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nN <= Nmax)==TRUE)) {
        				isopeaklist[p,"estim_nN"] = round(est_nN)
                        isopeaklist[q,"is_monoiso"] = "15N"
      				}
                    }
                }
			## Sulphur
    			### 34S
    			if (((mz_M + 1.995796 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 1.995796 + deiso_ppm)==TRUE)) {
             
                    if((is.na(into_M1)==FALSE)&(is.na(into_M)==FALSE)) {                 
      				if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nS34 <= Smax)==TRUE)) {                    
        				isopeaklist[p,"estim_nS"] = round(est_nS34) 
                        isopeaklist[q,"is_monoiso"] = "34S"  
      				} 
                    }                  
    			} 
    			### 33S
    			if (((mz_M + 0.999388 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 0.999388 + deiso_ppm)==TRUE)) {
                    if((is.na(into_M1)==FALSE)&(is.na(into_M)==FALSE)) {                 
      				if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nS33 <= Smax)==TRUE)) {                    
        				isopeaklist[p,"estim_nS"] = round(est_nS33) 
                        isopeaklist[q,"is_monoiso"] = "33S"  
      				} 
                    }  
                }
			## Halogens
    			#### Bromine (unlock code if necessary)
			#      if (((mz_M + 1.997952 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 1.997952 + deiso_ppm)==TRUE)) {
			#        if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nBr <= Brmax)==TRUE)) {
			#          isopeaklist[q,"is_monoiso"] = "81Br"
			#          isopeaklist[p,"estim_nBr"] = round(est_nBr)
			#        } 
			#      } 
    			### Chlorine
      			if (((mz_M + 1.997050 - deiso_ppm <= mz_M1)==TRUE)&((mz_M1 <= mz_M + 1.997050 + deiso_ppm)==TRUE)) {
        			if(((into_M1 >= q_thresh)==TRUE)&((into_M >= q_thresh)==TRUE)&((est_nCl <= Clmax)==TRUE)) {
          				isopeaklist[q,"is_monoiso"] = "37Cl"
          				isopeaklist[p,"estim_nCl"] = round(est_nCl)
        			} 
      			} 
                
            }#end of 2 Da  
            else{   
                # don't need to go after the THR zone
                if((delta>(2.0043+deiso_ppm))==TRUE){ 
                    q <- nmax
                }
            }    
      		} # end of q
            
            }#THRS
           else{isopeaklist[p,"is_monoiso"]="THRS"}            
            
           
            
	} # end of p  
  	for(x in (1:nrow(isopeaklist))){
    		if(is.na(isopeaklist[x,"is_monoiso"]==TRUE)==TRUE){isopeaklist[x,"is_monoiso"]=1}
 	}# end of x
      	## Creates a CSV export file (unlock to print peaklist w/ identified isotopic clusters)    
        ## myfile              
        CSVfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-18), "_isopeaklist.CSV",sep ="")
        write.csv(isopeaklist, file = CSVfile)    
    	deisopeaklist <- subset(isopeaklist, isopeaklist[,"is_monoiso"] == 1)
        #lock.ref=257.247507 # ASAP + LOW
	#lock.ref=413.266231 # ESI +
	#lock.ref=391.284286 # ASAP + MID
     # 	a<-which(abs(as.numeric(deisopeaklist[,"mz"])-lock.ref)==min(abs(as.numeric(deisopeaklist[,"mz"])-lock.ref)))
     # 	lock.mes<-as.numeric(deisopeaklist[a,"mz"])
     # 	lock.into<-as.numeric(deisopeaklist[a,"into"])
     # 	c <- lock.ref/lock.mes      
     # 	for(z in 1:nrow(deisopeaklist)){
     #   	mz.mes <- as.numeric(deisopeaklist[z,"mz"])
      #  	mz.into <- as.numeric(deisopeaklist[z,"into"])
      #  	deisopeaklist[z,"mz"] = c*mz.mes
      #	}# end of z    
	## Creates a CSV export file	
	CSVfile <- paste(substr(input_files[[i]],1,nchar(input_files[[i]])-16), "_deisopeaklist.CSV",sep ="")
    
	write.csv(deisopeaklist, file = CSVfile) 
}    
    

    
    
    








