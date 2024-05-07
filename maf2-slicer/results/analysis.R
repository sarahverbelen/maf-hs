#############
# ARBITRARY #
#############
arbitrary <- read.table("c:/Users/sarah/Documents/VUB/master_2/thesis/own-code/maf-hs/maf2-slicer/results/arbitrary.csv", header=TRUE, sep=";", dec=",")

arbitrary$diff <- arbitrary$sign.size - arbitrary$concrete.size

shapiro.test(arbitrary$diff)


t.test(arbitrary$sign.size, arbitrary$parity.size, paired = TRUE)
t.test(arbitrary$sign.size, arbitrary$concrete.size, paired = TRUE)

boxplot(arbitrary$sign.diff, arbitrary$parity.diff, arbitrary$concrete.diff)

#############
# MANY SETS #
#############
manySets <- read.table("c:/Users/sarah/Documents/VUB/master_2/thesis/own-code/maf-hs/maf2-slicer/results/manySets.csv", header=TRUE, sep=";", dec=",")

manySets$diff <- manySets$sign.size - manySets$concrete.size
manySets
shapiro.test(manySets$diff)

t.test(manySets$sign.size, manySets$parity.size, paired = TRUE)
t.test(manySets$sign.size, manySets$concrete.size, paired = TRUE)

boxplot(manySets$sign.diff, manySets$parity.diff, manySets$concrete.diff)