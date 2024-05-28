#############
# ARBITRARY #
#############
arbitrary <- read.table("c:/Users/sarah/Documents/VUB/master_2/thesis/own-code/maf-hs/maf2-slicer/results/arbitrary-clean.csv", header=TRUE, sep=";", dec=",")


# test significant difference in size
wilcox.test(arbitrary$sign.size, arbitrary$parity.size, paired = TRUE) # significant
wilcox.test(arbitrary$sign.size, arbitrary$concrete.size, paired = TRUE, alternative = "l") # significant

# size difference boxplot
boxplot(arbitrary$sign.diff, arbitrary$parity.diff, arbitrary$concrete.diff, names=c("sign", "parity", "concrete"), xlab="Kind of slice", ylab="% of original size", main="Size decrease after slicing (arbitrary programs)")

# time boxplot
boxplot(arbitrary$concrete.time.diff, arbitrary$abstract.time.diff, names=c("concrete slice", "abstract slice"), xlab="Kind of slice", ylab="% of original analysis time", main="Analysis time per kind of slice (arbitrary programs)")

# test significant difference in time 
wilcox.test(arbitrary$concrete.time.diff, arbitrary$abstract.time.diff, paired = TRUE, alternative = "g") # not significant

# plot size difference to original size 
plot(arbitrary$size, arbitrary$sign.diff, ylab="% from original", xlab="program size (AST nodes)", main="slice size vs program size (arbitrary programs)")

# plot time difference to original size
plot(arbitrary$size, arbitrary$analysis.time, xlab="program size (AST nodes)", ylab="Analysis time (ns)", main="analysis time vs program size (original program) (arbitrary)")
plot(arbitrary$size, arbitrary$abstract.analysis.time, xlab="original program size (AST nodes)", ylab="Analysis time (ns)", main="analysis time vs program size (abstract slice) (arbitrary)")

#############
# MANY SETS #
#############
manySets <- read.table("c:/Users/sarah/Documents/VUB/master_2/thesis/own-code/maf-hs/maf2-slicer/results/manySets-clean.csv", header=TRUE, sep=";", dec=",")

# test significant difference in size
wilcox.test(manySets$sign.size, manySets$parity.size, paired = TRUE) # not significant
wilcox.test(manySets$sign.size, manySets$concrete.size, paired = TRUE) # significant

# size difference boxplot
boxplot(manySets$sign.diff, manySets$parity.diff, manySets$concrete.diff, names=c("sign", "parity", "concrete"), xlab="Kind of slice", ylab="% of original size", main="Size after slicing (programs with > 5 set!s)")

# time boxplot
boxplot(manySets$concrete.time.diff, manySets$abstract.time.diff, names=c("concrete slice", "abstract slice"), xlab="Kind of slice", ylab="% of original analysis time)", main="Analysis time per kind of slice (programs with > 5 set!s)")

# test significant difference in time 
wilcox.test(manySets$concrete.time.diff, manySets$abstract.time.diff, paired = TRUE, alternative = "g") # significant

# plot size difference to original size 
plot(manySets$size, manySets$sign.diff, ylab="% of original size", xlab="program size (AST nodes)", main="slice size vs program size (programs with > 5 set!s)")

# plot time difference to original size
plot(manySets$size, manySets$analysis.time, xlab="program size (AST nodes)", ylab="Analysis time (ns)", main="analysis time vs program size (original program) (> 5 set!s)")
plot(manySets$size, manySets$abstract.analysis.time, xlab="original program size (AST nodes)", ylab="Analysis time (ns)", main="analysis time vs program size (abstract slice) (> 5 set!s)")

#########################
# ARBITRARY VS MANYSETS #
#########################

# test significant difference in abstract slice size  
wilcox.test(arbitrary$sign.diff, manySets$sign.diff, paired = FALSE) # significant 

# size difference boxplot
boxplot(arbitrary$sign.diff, manySets$sign.diff, names=c("arbitrary programs", "programs with >5 set!s"), xlab="kind of slice", ylab="% removed", main="Difference in % sliced")


#############
# FORCED SETS #
#############
forcedSets <- read.table("c:/Users/sarah/Documents/VUB/master_2/thesis/own-code/maf-hs/maf2-slicer/results/forcedSets-clean.csv", header=TRUE, sep=";", dec=",")

# normality test
shapiro.test(forcedSets$parity.diff)
shapiro.test(forcedSets$sign.diff)
shapiro.test(forcedSets$concrete.diff)

# test significant difference in size
wilcox.test(forcedSets$sign.size, forcedSets$parity.size, paired = TRUE) # significant
wilcox.test(forcedSets$sign.size, forcedSets$concrete.size, paired = TRUE) # significant

# size difference boxplot
boxplot(forcedSets$sign.diff, forcedSets$parity.diff, forcedSets$concrete.diff, names=c("sign", "parity", "concrete"), xlab="Kind of slice", ylab="% of original size", main="Size after slicing")

# time boxplot
boxplot(forcedSets$concrete.time.diff, forcedSets$abstract.time.diff, names=c("concrete slice", "abstract slice"), xlab="Kind of slice", ylab="analysis time (% of original time)", main="Analysis time per kind of slice (parity analysis)")

# test significant difference in time 
wilcox.test(forcedSets$concrete.time.diff, forcedSets$abstract.time.diff, paired = TRUE, alternative = "g") # significant

# plot size difference to original size 
plot(forcedSets$size, forcedSets$sign.diff, ylab="% of original size", xlab="program size (AST nodes)", main="slice size vs program size")

# plot size difference to % of set!s
plot(forcedSets$sets, forcedSets$sign.diff, xlab="% of set!s", ylab="slice size (% of original size)", main="slice size vs % of set!s")

# plot time difference to original size
plot(forcedSets$size, forcedSets$concrete.time.diff, xlab="original program size (AST nodes)", ylab="Analysis time (% of original)", main="analysis time difference vs program size (concrete slice)")
plot(forcedSets$size, forcedSets$abstract.time.diff, xlab="original program size (AST nodes)", ylab="Analysis time (% of original)", main="analysis time difference vs program size (abstract slice)")


# geometric mean of sizes
exp(mean(log(forcedSets$sign.diff))) # 0.3526975
exp(mean(log(forcedSets$parity.diff))) # 0.3178308
exp(mean(log(forcedSets$concrete.diff))) # 0.4126053

# geometric mean of time 
exp(mean(log(forcedSets$concrete.time.diff))) # 0.4344993
exp(mean(log(forcedSets$abstract.time.diff))) # 0.3101495

# plot means per % of set!s
groupedSignSize <- aggregate(forcedSets$sign.diff, list(forcedSets$sets), FUN=geometric_mean) 
plot(groupedSignSize, xlab="% of set!s", ylab="slice size (% of original)", main="Slice size vs % of set!s (sign slice)")

groupedConcreteSize <- aggregate(forcedSets$concrete.diff, list(forcedSets$sets), FUN=geometric_mean) 
plot(groupedConcreteSize, xlab="% of set!s", ylab="slice size (% of original)", main="Slice size vs % of set!s (concrete slice)")

groupedConcreteTime <- aggregate(forcedSets$concrete.time.diff, list(forcedSets$sets), FUN=geometric_mean) 
plot(groupedConcreteTime, xlab="% of set!s", ylab="analysis time (% of original)", main="Analysis time vs % of set!s (concrete slice)")

groupedAbstractTime <- aggregate(forcedSets$abstract.time.diff, list(forcedSets$sets), FUN=geometric_mean) 
plot(groupedAbstractTime, xlab="% of set!s", ylab="analysis time (% of original)", main="Analysis time vs % of set!s (abstract slice)")

geometric_mean <- function(x){
  exp(mean(log(x))) 
}


# find out at what percentage the difference becomes meaningful
forcedSetsReduced <- forcedSets[forcedSets$sets == 5, ]
wilcox.test(forcedSetsReduced$sign.size, forcedSetsReduced$parity.size, paired = TRUE) # significant
