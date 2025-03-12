set.seed(123)
france_tax <- read.csv('/Users/aleksamilovanovic/Downloads/France Tax.csv')
n <- 300

pilot_sam <- sample(france_tax$`Taxe.d.habitation`, n)
var_tax <- var(pilot_sam)
var_pop <- var(sample(france_tax$Population, n))

z_alpha <- qnorm(0.975)
r_tax <- 0.02
e_pop <- 300

n_tax <- ceiling((z_alpha^2 * var_tax) / (r_tax * mean(pilot_sam))^2)
n_pop <- ceiling((z_alpha^2 * var_pop) / e_pop^2)

tax_samp <- sample(france_tax$`Taxe.d.habitation`, min(n_tax, nrow(france_tax)))
pop_sam <- sample(france_tax$Population, min(n_pop, nrow(france_tax)))

mean_tax_sam <- mean(tax_samp)
sd_tax_sam <- sd(tax_samp)
mean_pop_sam <- mean(pop_sam)
sd_pop_sam <- sd(pop_sam)

se_tax <- sd_tax_sam / sqrt(n_tax)
CI_tax <- mean_tax_sam + c(-1, 1) * z_alpha * se_tax

se_pop <- sd_pop_sam / sqrt(n_pop)
CI_pop_mean <- mean_pop_sam + c(-1, 1) * z_alpha * se_pop

N <- nrow(france_tax)
total_pop <- mean_pop_sam * N
se_total_pop <- se_pop * N
CI_pop_total <- total_pop + c(-1, 1) * z_alpha * se_total_pop

northern_regions <- subset(france_tax, Region %in% c("BRETAGNE", "HAUTS-DE-FRANCE", "ILE-DE-FRANCE", "NORMANDIE"))

Nh <- aggregate(`Taxe.d.habitation` ~ Region, northern_regions, length)
y_hU <- aggregate(`Taxe.d.habitation` ~ Region, northern_regions, mean)
s2_h <- aggregate(`Taxe.d.habitation` ~ Region, northern_regions, var)

s_h <- sqrt(s2_h$`Taxe.d.habitation`)
nh_opt <- round(n * Nh$`Taxe.d.habitation` * s_h / sum(Nh$`Taxe.d.habitation` * s_h))

samples <- lapply(split(northern_regions, northern_regions$Region), function(df, size) sample(df$`Taxe.d.habitation`, min(size, nrow(df))), nh_opt)
ybar_h <- sapply(samples, mean)
ybar_str <- sum(Nh$`Taxe.d.habitation` * ybar_h) / N

fpc <- 1 - nh_opt / Nh$`Taxe.d.habitation`
vhat_ybar_str <- sum(fpc * Nh$`Taxe.d.habitation`^2 * s2_h$`Taxe.d.habitation` / nh_opt) / N^2
SE_ybar_str <- sqrt(vhat_ybar_str)
CI_tax <- ybar_str + c(-1, 1) * z_alpha * SE_ybar_str

sample_indices <- sample(1:N, n)
sample_data <- france_tax[sample_indices, ]
y <- sample_data$`Taxe.d.habitation`
x <- sample_data$`Taxe.sur.le.foncier.bati`

B_hat <- mean(y) / mean(x)
t_x <- sum(france_tax$`Taxe.sur.le.foncier.bâti`)
y_hat_r <- B_hat * mean(x)
t_hat_r <- B_hat * t_x
s2_e <- var(y - B_hat * x)

V_hat_y_hat_r <- fpc * s2_e / (n * mean(x)^2)
SE_y_hat_r <- sqrt(V_hat_y_hat_r)
CI_y <- y_hat_r + c(-1, 1) * z_alpha * SE_y_hat_r

V_hat_B_hat <- fpc * s2_e / (n * mean(x)^2)
SE_B_hat <- sqrt(V_hat_B_hat)
CI_B <- B_hat + c(-1, 1) * z_alpha * SE_B_hat

izracunaj_bias <- function(obim_uzoraka, podaci, n_uzorak=3000) {
  replicate(obim_uzoraka, mean(sample(podaci$`Taxe.d.habitation`, min(n_uzorak, nrow(podaci)))) - mean(podaci$`Taxe.d.habitation`))
}

bias_1 <- izracunaj_bias(200, france_tax)
bias_2 <- izracunaj_bias(20000, france_tax)