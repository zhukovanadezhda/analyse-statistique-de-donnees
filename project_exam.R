#Lecture et Description du jeu de données

df <- read.table('exams.csv', header = TRUE, dec = '.', sep = ',')
df[, "gender"] <- as.factor(df[, "gender"])
df[, "race.ethnicity"] <- as.factor(df[, "race.ethnicity"])
df[, "parental.level.of.education"] <- as.factor(df[, "parental.level.of.education"])
df[, "lunch"] <- as.factor(df[, "lunch"])
df[, "test.preparation.course"] <- as.factor(df[, "test.preparation.course"])

summary(df)
str(df)

apply(X = df[, -(1:5)], MARGIN = 2, FUN = mean)
apply(X = df[, -(1:5)], MARGIN = 2, FUN = median)
apply(X = df[, -(1:5)], MARGIN = 2, FUN = var)
apply(X = df[, -(1:5)], MARGIN = 2, FUN = sd)

#Représentations graphiques


sexe <- table(df$gender)
pie(sexe)

boxplot(df[, -(1:5)])

par(mfrow = c(1, 3))
hist(df$math.score)
hist(df$reading.score)
hist(df$writing.score)
par(mfrow = c(1, 1))

par(mfrow = c(1, 3))
hist(df[, "math.score"], freq = FALSE,
     main = "hitogramme de la variable math.score",
     xlab = "math.score", ylab = "% d'étudiants")
curve(dnorm(x, mean = mean(df[, "math.score"]), sd = sd(df[, "math.score"])), 
      col = "red", add = T)

hist(df[, "reading.score"], freq = FALSE,
     main = "hitogramme de la variable reading.score",
     xlab = "reading.score", ylab = "% d'étudiants")
curve(dnorm(x, mean = mean(df[, "reading.score"]), sd = sd(df[, "reading.score"])), 
      col = "red", add = T)

hist(df[, "writing.score"], freq = FALSE,
     main = "hitogramme de la variable writing.score",
     xlab = "writing.score", ylab = "% d'étudiants")
curve(dnorm(x, mean = mean(df[, "writing.score"]), sd = sd(df[, "writing.score"])), 
      col = "red", add = T)
par(mfrow = c(1, 1))

#Verefication de la normalité

# H0 : X suit une loi normale pour le niveau du facteur
# H1 : X ne suit pas une loi normale pour le niveau du facteur

shapiro.test(df$math.score)
shapiro.test(df$reading.score)
shapiro.test(df$writing.score)

by(data = df$math.score, INDICES = df$gender, FUN = shapiro.test)
by(data = df$reading.score, INDICES = df$gender, FUN = shapiro.test)
by(data = df$writing.score, INDICES = df$gender, FUN = shapiro.test)

by(data = df$math.score, INDICES = df$parental.level.of.education, FUN = shapiro.test)
by(data = df$reading.score, INDICES = df$parental.level.of.education, FUN = shapiro.test)
by(data = df$writing.score, INDICES = df$parental.level.of.education, FUN = shapiro.test)

by(data = df$math.score, INDICES = df$test.preparation.course, FUN = shapiro.test)
by(data = df$reading.score, INDICES = df$test.preparation.course, FUN = shapiro.test)
by(data = df$writing.score, INDICES = df$test.preparation.course, FUN = shapiro.test)

by(data = df$math.score, INDICES = df$lunch, FUN = shapiro.test)
by(data = df$reading.score, INDICES = df$lunch, FUN = shapiro.test)
by(data = df$writing.score, INDICES = df$lunch, FUN = shapiro.test)

bartlett.test(formula = math.score ~ parental.level.of.education, data = df)
bartlett.test(formula = reading.score ~ parental.level.of.education, data = df)
bartlett.test(formula = writing.score ~ parental.level.of.education, data = df)

bartlett.test(formula = math.score ~ gender, data = df)
bartlett.test(formula = reading.score ~ gender, data = df)
bartlett.test(formula = writing.score ~ gender, data = df)

bartlett.test(formula = math.score ~ test.preparation.course, data = df)
bartlett.test(formula = reading.score ~ test.preparation.course, data = df)
bartlett.test(formula = writing.score ~ test.preparation.course, data = df)

bartlett.test(formula = math.score ~ lunch, data = df)
bartlett.test(formula = reading.score ~ lunch, data = df)
bartlett.test(formula = writing.score ~ lunch, data = df)

#La corrélation entre les 3 notes existe-elle ? 

par(mfrow = c(1, 3))
plot(df[,"reading.score"], df[,"writing.score"])
cor(df[,"reading.score"], df[,"writing.score"])
cor.test(df[,"reading.score"], df[,"writing.score"], method = "spearman")
mon.modele <- lm(df[,"writing.score"] ~ df[,"reading.score"])
summary(mon.modele)
abline(mon.modele, col = "red")

plot(df[,"reading.score"], df[,"math.score"])
cor(df[,"reading.score"], df[,"math.score"])
cor.test(df[,"reading.score"], df[,"math.score"], method = "spearman")
mon.modele <- lm(df[,"math.score"] ~ df[,"reading.score"])
summary(mon.modele)
abline(mon.modele, col = "red")

plot(df[,"math.score"], df[,"writing.score"])
cor(df[,"math.score"], df[,"writing.score"])
cor.test(df[,"math.score"], df[,"writing.score"], method = "spearman")
mon.modele <- lm(df[,"writing.score"] ~ df[,"math.score"])
summary(mon.modele)
abline(mon.modele, col = "red")
par(mfrow = c(1, 1))


#Le math.score est-il dépendant du niveau d’education des parents ?

aggregate(df[, "math.score"], 
          by = list(df$parental.level.of.education), 
          FUN = length)
aggregate(df[, "math.score"], 
          by = list(df$parental.level.of.education), 
          FUN = mean)
aggregate(df[, "math.score"], 
          by = list(df$parental.level.of.education),
          FUN = var)
aggregate(df[, "math.score"], 
          by = list(df$parental.level.of.education), 
          FUN = sd)

par(mfrow = c(2, 3))
for (i in unique(df[, "parental.level.of.education"])) {
  hist(df[df[, "parental.level.of.education"] == i, "math.score"], breaks = 6, 
       xlim = c(min(df[, "math.score"]), max(df[, "math.score"])),
       xlab = "math.score", 
       main = paste(i))
}
par(mfrow = c(1, 1))

boxplot(df[, "math.score"] ~ df[, "parental.level.of.education"], 
        xlab = "F : parental.level.of.education",
        ylab = "X : math.score",
        main = "math.score et parental education")

my_aov <- aov(math.score ~ parental.level.of.education, data = df)
summary(my_aov)

TukeyHSD((my_aov))
par(mar=c(6,4,4,2))
plot(TukeyHSD(my_aov), las = 2)

pairwise.t.test(df$math.score, df$parental.level.of.education, 
                p.adjust.method = "bonferroni")

# Le sexe et le fait de suivre un cours préparatoire sont-ils indépendants ?

tbl_prep_gen <- table(df$test.preparation.course, df$gender)
chisq.test(tbl_prep_gen)$expected
chisq.test(tbl_prep_gen)
chisq.test(tbl_prep_gen)$residuals^2

#Y a-t-il une différence significative entre les notes des garçons et des filles ?

par(mfrow = c(1, 3))
boxplot(math.score ~ gender, data = df)
boxplot(reading.score ~ gender, data = df)
boxplot(writing.score ~ gender, data = df)
par(mfrow = c(1, 1))

wilcox.test(math.score ~ gender, data = df)
wilcox.test(reading.score ~ gender, data = df)
wilcox.test(writing.score ~ gender, data = df)

# Quelle est l'efficacité du cours de préparation aux examens ?

par(mfrow = c(1, 3))
boxplot(math.score ~ test.preparation.course, data = df)
boxplot(reading.score ~ test.preparation.course, data = df)
boxplot(writing.score ~ test.preparation.course, data = df)
par(mfrow = c(1, 1))

wilcox.test(math.score ~ test.preparation.course, data = df)
wilcox.test(reading.score ~ test.preparation.course, data = df)
wilcox.test(writing.score ~ test.preparation.course, data = df)

#Le déjeuner affecte-t-il les notes des élèves ?

par(mfrow = c(1, 3))
boxplot(math.score ~ lunch, data = df)
boxplot(reading.score ~ lunch, data = df)
boxplot(writing.score ~ lunch, data = df)
par(mfrow = c(1, 1))

wilcox.test(math.score ~ lunch, data = df)
wilcox.test(reading.score ~ lunch, data = df)
wilcox.test(writing.score ~ lunch, data = df)


# C'est pas tres interessant pour nous

df_pca <- PCA(df[,(6:8)])
factoextra::fviz_pca_biplot(df_pca)

dist_raw <- dist(df[,(6:8)])
dist_norm <- dist(scale(df[,(6:8)]))
img_raw <- factoextra::fviz_dist(dist_raw, show_labels = FALSE)
img_norm <- factoextra::fviz_dist(dist_norm, show_labels = FALSE)
cowplot::plot_grid(img_raw, img_norm, align = "h")

dtf_hclust <- hclust(dist_norm, method = "ward.D2")
plot(dtf_hclust, hang = -1)
rect.hclust(dtf_hclust, k = 4)
dtf_cut_4 <- cutree(dtf_hclust, k = 4)
table(dtf_cut_4)

dtf_visu <- data.frame(df[,(6:8)], cluster = as.factor(dtf_cut_4))
dtf_visu_pca <- PCA(dtf_visu, graph = FALSE, quali.sup = 4)
plot(dtf_visu_pca, habillage = 4)

(cluster_description_moyenne <- aggregate(. ~ cluster, FUN = mean, 
                                          data = dtf_visu))
aggregate(. ~ cluster, FUN = var, data = dtf_visu)
mes_couleurs <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c')
par(mar = c(2,2,2,2))
fmsb::radarchart(cluster_description_moyenne[, -1], maxmin = FALSE, 
                 plwd = 3, plty = 4, pcol = mes_couleurs,
                 title = "caractéristiques des classes")
legend(x = 1, y = 1, legend = rownames(cluster_description_moyenne), 
       col = mes_couleurs,
       bty = "n", pch = 20 , text.col = "grey", cex =  1.2, pt.cex = 2)
pheatmap(df[,(6:8)], scale = "column", clustering_method = "ward.D2")
mon_dtf_corr <- cor(df[,(6:8)])
corrplot(mon_dtf_corr)
corrplot.mixed(mon_dtf_corr)
