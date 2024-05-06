data <- read.table("c:/Users/sarah/Documents/VUB/master_2/thesis/own-code/maf-hs/maf2-slicer/results/arbitrary.csv", header=TRUE, sep=";", dec=",")

plot(data$size, data$decrease, type="p", xlab="program size", ylab="% decrease after slicing")

mean(data$decrease)