plot4 <- function(iData = "household_power_consumption.txt", iDate = c("1/2/2007","2/2/2007")){
        
        ####### Getting Names of the variables #######
        
        vNamesVariables <- names(read.table(iData, header = TRUE, sep = ";", nrows = 1))
        
        ####### Reading the data with the respective input dates #######
        
        dHouPwrCon <- as.data.frame(matrix(0,nrow=0,ncol=length(vNamesVariables)))#Empty dataframe for merge in for-loop
        names(dHouPwrCon)<-vNamesVariables #Assigning the names to the columns
        
        ltempdata<-list() #List of temporary data frames
        length(ltempdata)<-length(iDate) #List should be as long as input dates
        
        iNumObsDay <- 24 * 60 #number of observations equal to 24 hours x 60 minutes = 1440
        
        for (iDay in 1:length(iDate)){
                #Finding out which rows correspond to the inputs date
                ifirstrow<-grep(iDate[iDay], readLines(iData))[1]
                ltempdata[[iDay]]<-read.table(iData, header = FALSE, sep = ";", skip = ifirstrow-1, nrows = iNumObsDay)    
                names(ltempdata[[iDay]])<-vNamesVariables
                dHouPwrCon <- merge(dHouPwrCon, ltempdata[[iDay]], all = TRUE)
        }
        ####### Converting the Date and Time column into a single column #######
        
        dHouPwrCon[[1]]<-as.character(dHouPwrCon[[1]])
        dHouPwrCon[[2]]<-as.character(dHouPwrCon[[2]])
        cNewDateTimeCol<-paste(dHouPwrCon[[1]], dHouPwrCon[[2]])
        dHouPwrCon[[1]]<-strptime(cNewDateTimeCol, "%d/%m/%Y %H:%M:%S") #Changing the Class to Date/Time
        
        dHouPwrCon[[2]]<-NULL #Deleting the second column
        names(dHouPwrCon)[1]<-"Date_Time" #Changing the Name to the New Date/Time column
        
        ####### Making the Plot4 #######
        
        png(filename = "plot4.png", width = 480, height = 480)
        par(mfcol=c(2,2))
        plot(dHouPwrCon$Date_Time, dHouPwrCon$Global_active_power, type = "l", xlab = "",ylab = "Global Active Power (kilowatts)")
        plot(dHouPwrCon$Date_Time, dHouPwrCon$Sub_metering_1, type = "l", xlab = "",ylab = "Energy sub metering")
        lines(dHouPwrCon$Date_Time, dHouPwrCon$Sub_metering_2, type = "l", col = "red")
        lines(dHouPwrCon$Date_Time, dHouPwrCon$Sub_metering_3, type = "l", col = "blue")
        legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1,1,1),col = c("black", "red", "blue"))
        plot(dHouPwrCon$Date_Time, dHouPwrCon$Voltage, type = "l", xlab = "datetime",ylab = "Voltage")
        plot(dHouPwrCon$Date_Time, dHouPwrCon$Global_reactive_power, type = "l", xlab = "datetime",ylab = "Global_reactive_power")
        dev.off()
}
