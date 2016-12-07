root_directory <- "/home/mashrin/Workarea/Bearings/1st_test/"
data <- read.table(paste0(root_directory, "2003.10.22.12.06.24"), header=FALSE, sep="\t")
head(data)
colnames(data) <- c("bearing1.x", "bearing1.y", "bearing2.x", "bearing2.y", "bearing3.x", "bearing3.y", "bearing4.x", "bearing4.y")
summary(data$bearing1.x)
plot(data$bearing1.x, t="l") # using lineplot

bearing1.x.fft <- fft(data$bearing1.x)
# Ignore the 2nd half, which are complex conjugates of the 1st half and calculate the Mod (magnitude of each complex number)
amplitude <- Mod(bearing1.x.fft[1:(length(bearing1.x.fft)/2)])

# Calculate the frequencies
frequency <- seq(0, 10000, length.out=length(bearing1.x.fft)/2)

# Plot

plot(amplitude ~ frequency, t="l")
plot(amplitude ~ frequency, t="l", xlim=c(0,1000), ylim=c(0,500))
axis(1, at=seq(0,1000,100), labels=FALSE)  # add more ticks
sorted <- sort.int(amplitude, decreasing=TRUE, index.return=TRUE)
top15 <- sorted$ix[1:15] # indexes of the largest 15
top15f <- frequency[top15] # convert indexes to frequencies

fft.profile <- function (dataset, n)
{
  fft.data <- fft(dataset)
  amplitude <- Mod(fft.data[1:(length(fft.data)/2)])
  frequencies <- seq(0, 10000, length.out=length(fft.data)/2)

  sorted <- sort.int(amplitude, decreasing=TRUE, index.return=TRUE)
  top <- sorted$ix[1:n] # indexes of the largest n components
  return (frequencies[top]) # convert indexes to frequencies
}

# How many FFT components should I grab as features?
n <- 5

# Set up storage for bearing-grouped data
bearing1 <- matrix(nrow=0, ncol=(2*n+1))
bearing2 <- matrix(nrow=0, ncol=(2*n+1))
bearing3 <- matrix(nrow=0, ncol=(2*n+1))
bearing4 <- matrix(nrow=0, ncol=(2*n+1))

for (filename in list.files(root_directory))
{
  cat("Processing file ", filename, "\n")

  timestamp <- as.character(strptime(filename, format="%Y.%m.%d.%H.%M.%S"))

  data <- read.table(paste0(root_directory, filename), header=FALSE, sep="\t")
  colnames(data) <- c("bearing1.x", "bearing1.y", "bearing2.x", "bearing2.y", "bearing3.x", "bearing3.y", "bearing4.x", "bearing4.y")

  # Bind the new rows to the bearing matrices
  bearing1 <- rbind(bearing1, c(timestamp, fft.profile(data$bearing1.x, n), fft.profile(data$bearing1.y, n)))
  bearing2 <- rbind(bearing2, c(timestamp, fft.profile(data$bearing2.x, n), fft.profile(data$bearing2.y, n)))
  bearing3 <- rbind(bearing3, c(timestamp, fft.profile(data$bearing3.x, n), fft.profile(data$bearing3.y, n)))
  bearing4 <- rbind(bearing4, c(timestamp, fft.profile(data$bearing4.x, n), fft.profile(data$bearing4.y, n)))

}

write.table(bearing1, file=paste0(root_directory, "../bearing1.csv"), sep=",", row.names=FALSE, col.names=FALSE)
write.table(bearing2, file=paste0(root_directory, "../bearing2.csv"), sep=",", row.names=FALSE, col.names=FALSE)
write.table(bearing3, file=paste0(root_directory, "../bearing3.csv"), sep=",", row.names=FALSE, col.names=FALSE)
write.table(bearing4, file=paste0(root_directory, "../bearing4.csv"), sep=",", row.names=FALSE, col.names=FALSE)

## Plotting patterns

root_directory <- "/home/mashrin/Workarea/Bearings/1st_test/"
bearing1 <- read.table(file=paste0(root_directory, "../bearing1.csv"), sep=",", header=FALSE)
bearing2 <- read.table(file=paste0(root_directory, "../bearing2.csv"), sep=",", header=FALSE)
bearing3 <- read.table(file=paste0(root_directory, "../bearing3.csv"), sep=",", header=FALSE)
bearing4 <- read.table(file=paste0(root_directory, "../bearing4.csv"), sep=",", header=FALSE)


par(mfrow=c(5,2))

# x axis components
plot(bearing1[, 2], pch=4, col="dodgerblue2", ylab="Frequency", main="Degraded x", ylim=c(0, 2))
points(bearing2[, 2], pch=3, col="darkorchid2")
points(bearing4[, 2], pch=1, col="chartreuse")
points(bearing3[, 2], pch=20, col="coral2")

legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))

plot(bearing1[, 7], pch=4, col="dodgerblue2", ylab="Frequency", main="Degraded y", ylim=c(0, 2))
points(bearing2[, 7], pch=3, col="darkorchid2")
points(bearing4[, 7], pch=1, col="chartreuse")
points(bearing3[, 7], pch=20, col="coral2")

legend("topleft", c("bearing 1", "bearing 2", "bearing 3", "bearing 4"), col=c("dodgerblue2", "darkorchid2", "coral2", "chartreuse"), pch=c(4, 3, 20, 1))


plot(bearing1[, 3], pch=4, col="dodgerblue2", ylab="Frequency", main="Needs replacement x", ylim=c(0,10000))
points(bearing2[, 3], pch=3, col="darkorchid2")
points(bearing4[, 3], pch=1, col="chartreuse")
points(bearing3[, 3], pch=20, col="coral2")

plot(bearing1[, 8], pch=4, col="dodgerblue2", ylab="Frequency", main="Needs replacement y", ylim=c(0,10000))
points(bearing2[, 8], pch=3, col="darkorchid2")
points(bearing4[, 8], pch=1, col="chartreuse")
points(bearing3[, 8], pch=20, col="coral2")

plot(bearing1[, 4], pch=4, col="dodgerblue2", ylab="Frequency", main="Level one degradation x", ylim=c(0,10000))
points(bearing2[, 4], pch=3, col="darkorchid2")
points(bearing4[, 4], pch=1, col="chartreuse")
points(bearing3[, 4], pch=20, col="coral2")

plot(bearing1[, 9], pch=4, col="dodgerblue2", ylab="Frequency", main="Level one degradation y", ylim=c(0,10000))
points(bearing2[, 9], pch=3, col="darkorchid2")
points(bearing4[, 9], pch=1, col="chartreuse")
points(bearing3[, 9], pch=20, col="coral2")

plot(bearing1[, 5], pch=4, col="dodgerblue2", ylab="Frequency", main="Level two degradation x", ylim=c(0,10000))
points(bearing2[, 5], pch=3, col="darkorchid2")
points(bearing4[, 5], pch=1, col="chartreuse")
points(bearing3[, 5], pch=20, col="coral2")

plot(bearing1[, 10], pch=4, col="dodgerblue2", ylab="Frequency", main="Level two degradation y", ylim=c(0,10000))
points(bearing2[, 10], pch=3, col="darkorchid2")
points(bearing4[, 10], pch=1, col="chartreuse")
points(bearing3[, 10], pch=20, col="coral2")

plot(bearing1[, 6], pch=4, col="dodgerblue2", ylab="Frequency", main="Lubrication Required x", ylim=c(0,10000))
points(bearing2[, 6], pch=3, col="darkorchid2")
points(bearing4[, 6], pch=1, col="chartreuse")
points(bearing3[, 6], pch=20, col="coral2")

plot(bearing1[, 11], pch=4, col="dodgerblue2", ylab="Frequency", main="Lubrication Required y", ylim=c(0,10000))
points(bearing2[, 11], pch=3, col="darkorchid2")
points(bearing4[, 11], pch=1, col="chartreuse")
points(bearing3[, 11], pch=20, col="coral2")
