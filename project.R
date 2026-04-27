setwd("C:/Users/AZA/OneDrive - Institut National de Statistique et d'Economie Appliquee/Desktop/ISEFAR/stat")

# ── Libraries ────────────────────────────────────────────────────────────────
library(lmtest)
library(sandwich)
library(AER)
library(survival)
library(MASS)   # glm.nb for Negative Binomial

source("assurance_complet.R")

# ── 1. Exploration de Sinistre0 ───────────────────────────────────────────────
summary(dat$Sinistre0)
sum(dat$Sinistre0 == 0)

par(mfrow = c(2, 2))
hist(dat$Sinistre0, breaks = 40,
     main = "Distribution brute de Sinistre0",
     xlab = "Sinistre0", ylab = "Fréquence")
hist(exp(dat$Sinistre0), breaks = 40,
     main = "Montants réels exp(Sinistre0)",
     xlab = "Montant (€)", ylab = "Fréquence")
hist(log(dat$Sinistre0), breaks = 40,
     main = "log(Sinistre0)",
     xlab = "log(Sinistre0)", ylab = "Fréquence")
boxplot(Sinistre0 ~ cs, data = dat,
        main = "Sinistre0 par niveau de vie",
        xlab = "cs", ylab = "Sinistre0")
par(mfrow = c(1, 1))

# Cohérence de l'interprétation log-montant
cat("Médiane en euros :", exp(median(dat$Sinistre0)), "\n")
cat("Moyenne en euros :", exp(mean(dat$Sinistre0)), "\n")
cor(dat$Sinistre0, dat$reves)

# Boxplots par variable catégorielle
par(mfrow = c(1, 3))
boxplot(Sinistre0 ~ pcs,    data = dat, main = "Sinistre0 ~ pcs",    xlab = "pcs",    ylab = "Sinistre0")
boxplot(Sinistre0 ~ agecat, data = dat, main = "Sinistre0 ~ agecat", xlab = "agecat", ylab = "Sinistre0")
boxplot(Sinistre0 ~ Acompm, data = dat, main = "Sinistre0 ~ Acompm", xlab = "Acompm", ylab = "Sinistre0")
par(mfrow = c(1, 1))

# ── 2. Modèles MCO ────────────────────────────────────────────────────────────
mod_ols <- lm(Sinistre0 ~ cs + agecat + pcs + Atyph + Ahabi + Acompm + Bauto,
              data = dat)
summary(mod_ols)

mod_ols2 <- lm(Sinistre0 ~ cs + Acompm, data = dat)
summary(mod_ols2)

# Comparaison des deux modèles OLS
anova(mod_ols2, mod_ols)
AIC(mod_ols, mod_ols2)

# Boxplot Acompm — sauvegardé
png("box_acompm.png", width = 800, height = 600)
boxplot(Sinistre0 ~ Acompm, data = dat,
        main = "Distribution de Sinistre0 par composition de ménage",
        xlab = "Composition du ménage",
        ylab = "Sinistre0",
        col = "lightgray")
dev.off()
shell.exec("box_acompm.png")

# Écarts-types OLS classiques vs robustes (White)
coeftest(mod_ols2)
coeftest(mod_ols2, vcov = sandwich)

# ── 3. Test d'endogénéité (Blundell-Robin) ────────────────────────────────────
dat$cs_num <- as.numeric(dat$cs)

# Instrument 1 : reves
cat("\n--- Première étape : instrument = reves ---\n")
first_stage_reves <- lm(cs_num ~ reves + Acompm, data = dat)
summary(first_stage_reves)
cat("F-statistic (étape 1, reves) :", summary(first_stage_reves)$fstatistic[1],
    "— seuil recommandé : > 10\n")

dat$eta_hat_reves <- residuals(first_stage_reves)
mod_augmente_reves <- lm(Sinistre0 ~ cs + Acompm + eta_hat_reves, data = dat)
summary(mod_augmente_reves)

# Instrument 2 : RUC
cat("\n--- Première étape : instrument = RUC ---\n")
first_stage_RUC <- lm(cs_num ~ RUC + Acompm, data = dat)
summary(first_stage_RUC)
cat("F-statistic (étape 1, RUC) :", summary(first_stage_RUC)$fstatistic[1],
    "— seuil recommandé : > 10\n")

dat$eta_hat_RUC <- residuals(first_stage_RUC)
mod_augmente_RUC <- lm(Sinistre0 ~ cs + Acompm + eta_hat_RUC, data = dat)
summary(mod_augmente_RUC)

# Instrument 3 : crevpp
dat$crevpp_num <- as.numeric(dat$crevpp)
first_stage_crevpp <- lm(cs_num ~ crevpp_num + Acompm, data = dat)
summary(first_stage_crevpp)
cat("F-statistic (étape 1, crevpp) :", summary(first_stage_crevpp)$fstatistic[1], "\n")

# ── 4. Régression IV (Variables Instrumentales) ────────────────────────────────
mod_iv <- ivreg(Sinistre0 ~ cs + Acompm | RUC + Acompm, data = dat)
summary(mod_iv, diagnostics = TRUE)

# ── 5. Exploration Sinistre1 / 2 / 3 ─────────────────────────────────────────
# Proportion de zéros
cat("Proportion de zéros — Sinistre1 :", mean(dat$Sinistre1 == 0), "\n")
cat("Proportion de zéros — Sinistre2 :", mean(dat$Sinistre2 == 0), "\n")
cat("Proportion de zéros — Sinistre3 :", mean(dat$Sinistre3 == 0), "\n")

# Distribution des valeurs positives
summary(dat$Sinistre1[dat$Sinistre1 > 0])
summary(dat$Sinistre2[dat$Sinistre2 > 0])
summary(dat$Sinistre3[dat$Sinistre3 > 0])

cat("N positifs — Sinistre1 :", sum(dat$Sinistre1 > 0), "\n")
cat("N positifs — Sinistre2 :", sum(dat$Sinistre2 > 0), "\n")
cat("N positifs — Sinistre3 :", sum(dat$Sinistre3 > 0), "\n")

par(mfrow = c(1, 2))
hist(dat$Sinistre1[dat$Sinistre1 > 0], breaks = 40,
     main = "Sinistre1 (valeurs positives)",
     xlab = "Sinistre1", ylab = "Fréquence")
hist(log(dat$Sinistre1[dat$Sinistre1 > 0]), breaks = 40,
     main = "log(Sinistre1) (valeurs positives)",
     xlab = "log(Sinistre1)", ylab = "Fréquence")
par(mfrow = c(1, 1))

# ── 6. Modèles Tobit ──────────────────────────────────────────────────────────
mod_tobit <- tobit(Sinistre1 ~ cs + agecat + Acompm + Atyph, left = 0, data = dat)
summary(mod_tobit)

mod_tobit2 <- tobit(Sinistre1 ~ agecat + Acompm + Atyph, left = 0, data = dat)
summary(mod_tobit2)

AIC(mod_tobit, mod_tobit2)
lrtest(mod_tobit2, mod_tobit)

# ── 7. Modèles de comptage (NSin) ─────────────────────────────────────────────
summary(dat$NSin)
table(dat$NSin)
cat("Ratio de dispersion (var/mean) :", var(dat$NSin) / mean(dat$NSin), "\n")

# Poisson
mod_poisson <- glm(NSin ~ cs + agecat + Acompm + Atyph + pcs,
                   family = poisson(link = "log"), data = dat)
summary(mod_poisson)

# Quasi-Poisson (corrige la surdispersion)
mod_qpoisson <- glm(NSin ~ cs + agecat + Acompm + Atyph + pcs,
                    family = quasipoisson(link = "log"), data = dat)
summary(mod_qpoisson)
cat("Paramètre de dispersion (quasi-Poisson) :", summary(mod_qpoisson)$dispersion, "\n")

# Quasi-Poisson sans Atyph
mod_qpoisson2 <- glm(NSin ~ cs + agecat + Acompm + pcs,
                     family = quasipoisson(link = "log"), data = dat)
summary(mod_qpoisson2)
anova(mod_qpoisson2, mod_qpoisson, test = "F")

# Binomiale Négative — alternative à quasi-Poisson pour la surdispersion
mod_nb <- glm.nb(NSin ~ cs + agecat + Acompm + Atyph + pcs, data = dat)
summary(mod_nb)
cat("Theta (BN) :", mod_nb$theta, "— plus theta est petit, plus la surdispersion est forte\n")

# Comparaison Poisson vs BN (test du rapport de vraisemblance)
lrtest(mod_poisson, mod_nb)

# ── 8. Analyse de survie ───────────────────────────────────────────────────────
summary(dat$Duree)
cat("Proportion de censurés :", mean(dat$censure), "\n")
table(dat$censure)

dat$event <- 1 - dat$censure
surv_obj <- Surv(dat$Duree, dat$event)

# Kaplan-Meier global
km_global <- survfit(surv_obj ~ 1, data = dat)
summary(km_global)$table
summary(km_global, times = c(30, 90, 180, 365, 730))

png("km_global.png", width = 800, height = 600)
plot(km_global,
     main = "Kaplan-Meier — Durée de souscription",
     xlab = "Durée (jours)", ylab = "S(t)",
     conf.int = TRUE)
dev.off()
shell.exec("km_global.png")

# KM par niveau de vie + test du log-rank
km_cs <- survfit(surv_obj ~ cs, data = dat)
logrank_cs <- survdiff(surv_obj ~ cs, data = dat)
print(logrank_cs)
cat("p-value log-rank (cs) :", 1 - pchisq(logrank_cs$chisq, df = length(logrank_cs$n) - 1), "\n")

png("km_cs.png", width = 800, height = 600)
plot(km_cs,
     main = "Kaplan-Meier par niveau de vie (cs)",
     xlab = "Durée (jours)", ylab = "S(t)",
     col = 1:nlevels(dat$cs), lty = 1:nlevels(dat$cs))
legend("topright", levels(dat$cs),
       col = 1:nlevels(dat$cs), lty = 1:nlevels(dat$cs))
dev.off()
shell.exec("km_cs.png")

# ── 9. Modèle de Cox ──────────────────────────────────────────────────────────
mod_cox <- coxph(surv_obj ~ cs + agecat + Acompm + Atyph + pcs, data = dat)
summary(mod_cox)

mod_cox2 <- coxph(surv_obj ~ agecat + Acompm + Atyph + pcs, data = dat)
summary(mod_cox2)

# Test des risques proportionnels (Schoenfeld)
zph_test <- cox.zph(mod_cox2)
print(zph_test)

# Visualisation des résidus de Schoenfeld par covariable
png("cox_zph.png", width = 800, height = 600)
par(mfrow = c(2, 2))
plot(zph_test)
par(mfrow = c(1, 1))
dev.off()
shell.exec("cox_zph.png")
