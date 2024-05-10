#############
# ARBITRARY #
#############
arbitrary <- read.table("c:/Users/sarah/Documents/VUB/master_2/thesis/own-code/maf-hs/maf2-slicer/results/arbitrary-clean.csv", header=TRUE, sep=";", dec=",")

# normality test
arbitrary$diff <- arbitrary$sign.size - arbitrary$concrete.size
shapiro.test(arbitrary$diff)

# test significant difference in size
t.test(arbitrary$sign.size, arbitrary$parity.size, paired = TRUE) # not significant
t.test(arbitrary$sign.size, arbitrary$concrete.size, paired = TRUE, alternative = "l") # significant

# size difference boxplot
boxplot(arbitrary$sign.diff, arbitrary$parity.diff, arbitrary$concrete.diff, names=c("sign", "parity", "concrete"), xlab="Kind of slice", ylab="% of original size", main="Size decrease after slicing (arbitrary programs)")

# time boxplot
boxplot(arbitrary$concrete.time.diff, arbitrary$abstract.time.diff, names=c("concrete slice", "abstract slice"), xlab="Kind of slice", ylab="% of original analysis time", main="Analysis time per kind of slice (arbitrary programs)")

# test significant difference in time 
t.test(arbitrary$concrete.time.diff, arbitrary$abstract.time.diff, paired = TRUE, alternative = "g") # not significant (almost..)

# plot size difference to original size 
plot(arbitrary$size, arbitrary$sign.diff, ylab="% from original", xlab="program size (AST nodes)", main="slice size vs program size (arbitrary programs)")

# plot time difference to original size
plot(arbitrary$size, arbitrary$analysis.time, xlab="program size (AST nodes)", ylab="Analysis time (ns)", main="analysis time vs program size (original program) (arbitrary)")
plot(arbitrary$size, arbitrary$abstract.analysis.time, xlab="original program size (AST nodes)", ylab="Analysis time (ns)", main="analysis time vs program size (abstract slice) (arbitrary)")

#############
# MANY SETS #
#############
manySets <- read.table("c:/Users/sarah/Documents/VUB/master_2/thesis/own-code/maf-hs/maf2-slicer/results/manySets-clean.csv", header=TRUE, sep=";", dec=",")

# normality test
shapiro.test(manySets$parity.diff)
shapiro.test(manySets$sign.diff)
shapiro.test(manySets$concrete.diff)

# test significant difference in size
t.test(manySets$sign.size, manySets$parity.size, paired = TRUE) # not significant
t.test(manySets$sign.size, manySets$concrete.size, paired = TRUE) # significant

# size difference boxplot
boxplot(manySets$sign.diff, manySets$parity.diff, manySets$concrete.diff, names=c("sign", "parity", "concrete"), xlab="Kind of slice", ylab="% of original size", main="Size after slicing (programs with > 5 set!s)")

# time boxplot
boxplot(manySets$concrete.time.diff, manySets$abstract.time.diff, names=c("concrete slice", "abstract slice"), xlab="Kind of slice", ylab="% of original analysis time)", main="Analysis time per kind of slice (programs with > 5 set!s)")

# test significant difference in time 
t.test(manySets$concrete.time.diff, manySets$abstract.time.diff, paired = TRUE, alternative = "g") # significant

# plot size difference to original size 
plot(manySets$size, manySets$sign.diff, ylab="% of original size", xlab="program size (AST nodes)", main="slice size vs program size (programs with > 5 set!s)")

# plot time difference to original size
plot(manySets$size, manySets$analysis.time, xlab="program size (AST nodes)", ylab="Analysis time (ns)", main="analysis time vs program size (original program) (> 5 set!s)")
plot(manySets$size, manySets$abstract.analysis.time, xlab="original program size (AST nodes)", ylab="Analysis time (ns)", main="analysis time vs program size (abstract slice) (> 5 set!s)")

#########################
# ARBITRARY VS MANYSETS #
#########################

# test significant difference in abstract slice size 
t.test(arbitrary$sign.diff, manySets$sign.diff, paired = FALSE) # significant 

# size difference boxplot
boxplot(arbitrary$sign.diff, manySets$sign.diff, names=c("arbitrary programs", "programs with >5 set!s"), xlab="kind of slice", ylab="% removed", main="Difference in % sliced")