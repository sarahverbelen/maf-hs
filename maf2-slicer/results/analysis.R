arbitrary <- read.table("c:/Users/sarah/Documents/VUB/master_2/thesis/own-code/maf-hs/maf2-slicer/results/arbitrary.csv", header=TRUE, sep=";", dec=",")


arbitrary$diff <- arbitrary$sign.size - arbitrary$parity.size

shapiro.test(arbitrary$diff)

t.test(arbitrary$sign.size, arbitrary$parity.size, paired = TRUE)


manySets <- read.table("c:/Users/sarah/Documents/VUB/master_2/thesis/own-code/maf-hs/maf2-slicer/results/manySets.csv", header=TRUE, sep=";", dec=",")

manySets$diff <- manySets$sign.size - manySets$parity.size
manySets
shapiro.test(manySets$diff)

t.test(manySets$sign.size, manySets$parity.size, paired = TRUE)