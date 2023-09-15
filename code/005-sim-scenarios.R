library(epiDisplay)
library(magrittr)
library(purrr)
library(dplyr)
library(ggplot2)

output_directory <- here::here("output", "figs/")

# Sim data for opt-outs ---------------------------------------------------
set.seed <- 7
n <- 1e7

df <- data.frame(
  id = 1:n, 
  anxiety = sample(c(T, F), size = n, replace = TRUE, prob = c(0.1, 0.9)), 
  rare_cancer = sample(c(T, F), size = n, replace = TRUE, prob = c(0.01, 0.99)),
  age = sample(5:95, size = n, replace = TRUE)
)

prob_optout_likely <- 0.05
prob_optout_unlikely <- 0.01

more_optouts_prob <- c(prob_optout_likely, 1-prob_optout_likely)
less_optouts_prob <- c(prob_optout_unlikely, 1-prob_optout_unlikely)

# function define ---------------------------------------------------------
bodge_2x2tab <- function (row,
                          column,
                          decimal = 1){
  tab <- table(row, column, deparse.level = 1, dnn = list(deparse(substitute(row)), 
                                                          deparse(substitute(column))))
  cpercent <- tab
  for (i in 1:ncol(tab)) {
    cpercent[, i] <- paste("(", format(round(tab[, i]/colSums(tab)[i] * 
                                               100, digits = decimal), trim = TRUE), ")", sep = "")
  }
  cpercent <- rbind(cpercent, rep("(100)", ncol(tab)))
  col.1.1 <- cbind(format(c(tab[, 1], sum(tab[, 1])), trim = TRUE), 
                   cpercent[, 1])
  for (i in 2:ncol(tab)) {
    col.1.1 <- cbind(col.1.1, c(format(tab[, i], trim = TRUE), 
                                format(sum(tab[, i]), trim = TRUE)), cpercent[, 
                                                                              i])
  }
  cpercent <- col.1.1
  cnames <- character(0)
  for (i in 1:ncol(tab)) {
    cnames <- c(cnames, colnames(tab)[i], "%")
  }
  colnames(cpercent) <- cnames
  rownames(cpercent)[nrow(cpercent)] <- "Total"
  return(cpercent)
}

get_stats <- function(data_in){
  or_ci <- data_in %$% epiDisplay::cc(anxiety, rare_cancer, graph = FALSE, design = "case-control")
  or_est <- bind_rows(or = or_ci$or, lci = or_ci$ci.or[1], uci = or_ci$ci.or[2], se = or_ci$se.ln.or)
  tab2x2 <- data_in %$% bodge_2x2tab(row = anxiety, column = rare_cancer)
  return(list(or_est = or_est, tab2x2 = tab2x2))
}

# opt-outs associated with anxiety ----------------------------------------------
set.seed <- 3513
df$opt_outs <- NA
df$opt_outs[df$anxiety] <- sample(c(T,F), size = sum(df$anxiety), replace = TRUE, prob = more_optouts_prob)
df$opt_outs[!df$anxiety] <- FALSE

df_optout1 <- df[!df$opt_outs, ]

# opt-outs associated with rare_cancer ----------------------------------------------
set.seed <- 3
df$opt_outs <- NA
df$opt_outs[df$rare_cancer] <- sample(c(T,F), size = sum(df$rare_cancer), replace = TRUE, prob = more_optouts_prob)
df$opt_outs[!df$rare_cancer] <- FALSE 

df_optout2 <- df[!df$opt_outs, ]

# opt-outs associated with anxiety and rare_cancer ----------------------------------------
set.seed <- 79797
df$opt_outs <- NA

df$opt_outs[df$rare_cancer | df$anxiety] <- sample(c(T,F), size = sum(df$rare_cancer | df$anxiety), replace = TRUE, prob = less_optouts_prob)
df$opt_outs[!df$rare_cancer & !df$anxiety] <- sample(c(T,F), size = sum(!df$rare_cancer & !df$anxiety), replace = TRUE, prob = more_optouts_prob)

df_optout3 <- df[!df$opt_outs, ]

# opt-outs negatively associated with anxiety, positive with rare_ --------
set.seed <- 454
df$opt_outs <- NA

df$opt_outs[df$rare_cancer | df$anxiety] <- sample(c(T,F), size = sum(df$rare_cancer | df$anxiety), replace = TRUE, prob = more_optouts_prob)
df$opt_outs[!df$rare_cancer & !df$anxiety] <- sample(c(T,F), size = sum(!df$rare_cancer & !df$anxiety), replace = TRUE, prob = less_optouts_prob)

df_optout4 <- df[!df$opt_outs, ]

# estimate ORs and store results ------------------------------------------
sim <- get_stats(df)
sim1 <- get_stats(df_optout1)
sim2 <- get_stats(df_optout2)
sim3 <- get_stats(df_optout3)
sim4 <- get_stats(df_optout4)

# get outputs -------------------------------------------------------------
models <- c(
  "A: True association",
  "B: 5% opt-outs ~ anxiety",
  "C: 5% opt-outs ~ rare cancer",
  "D: 1% opt-out if anxiety OR rare cancer, 5% if neither",
  "E: 5% opt-out if anxiety OR rare cancer, 1% if neither"
)
plot_df <- tibble(x = rep("OR", 5),
                  model = factor(models,
                                 levels = models))
or_estimates <- bind_rows(
  sim$or_est,
  sim1$or_est,
  sim2$or_est,
  sim3$or_est,
  sim4$or_est
)
plot_df <- bind_cols(plot_df, or_estimates)
write.csv(plot_df, paste0(output_directory,"opt-out-plot-data.csv"))

p1 <- ggplot(plot_df, aes(x = x, y = or, ymin = lci, ymax = uci)) +
  geom_hline(yintercept = 1, lty = 2, col = "gray60") +
  geom_linerange() +
  facet_grid(rows = vars(model), switch = "y") +
  geom_point(aes(size = se), pch = 1) +
  labs(size = "Std. Error",
       title = "Observed association between anxiety and rare cancer",
       subtitle = "Comparison of different hypothetical opt-out scenarios",
       x = "Opt-out behaviour in data",
       y = "OR (95% CI)") +
  scale_y_log10() +
  coord_flip() +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(size = 10, angle = 0, hjust = 0, vjust = 0.5),
        axis.text.y = element_blank(),
        plot.title.position = "plot")
tiff(paste0(output_directory, "opt-out-plot.tiff"), width = 8, height = 3, units = "in", res = 800)
  p1
dev.off()
jpeg(paste0(output_directory, "opt-out-plot.tiff"), width = 8, height = 3, units = "in", res = 800)
  p1
dev.off()

tab <- sim$tab2x2
tab1 <- sim1$tab2x2
tab2 <- sim2$tab2x2
tab3 <- sim3$tab2x2
tab4 <- sim4$tab2x2

ii <-  1
capture.output(
  for (XX in list(tab, tab1, tab2, tab3, tab4)) {
    cat("------------------------------ \n")
    cat(models[ii], "\n")
    cat("Column percent", "\n")
    names(attr(XX, "dimnames")) <- c("Anxiety", "Rare cancer")
    print.table(XX, right = TRUE, print.gap = 2)
    cat("\n")
    ii <- ii+1
  },
  file = paste0(output_directory, "opt_out_tables.txt")
)
data_for_csv <- rbind.data.frame(tab, tab1, tab2, tab3, tab4)
data_for_csv$false_pc <- paste(prettyNum(data_for_csv$`FALSE`, big.mark = ","), data_for_csv[,2])
data_for_csv$true_pc <- paste(prettyNum(data_for_csv$`TRUE`, big.mark = ","), data_for_csv[,4])

write.csv(data_for_csv, paste0(output_directory, "opt_out_tables_rawnumbers.csv"))

# sample size calc for a single rate --------------------------------------
samplesize_n_for_singlerate <- function(n = 2e7,
                                        mu = 1.22,
                                        mu0 = 1,
                                        alpha = 0.05){
  # all done assuming rates per 100,000 person-years so convert n to n-years
  nyears <- n/1e5
  v <- qnorm(alpha/2,mean=0, sd=1, lower.tail = FALSE)
  
  # n <- ((u + v)^2*mu) / ((mu-mu0)^2)
  # rearranged gives:
  u <- sqrt((nyears * ((mu - mu0)^2)) / mu) - v
  power <- (1-pnorm(u, lower.tail = FALSE))*100
  power
}
samplesize_n_for_singlerate() # ~80% power to detect change to 1.22

base_n <- 2e7
opt_out_line <- data.frame(
  opt_outs = seq(0, 20, by = 0.01)
)
opt_out_line$opt_out_pwr = sapply(opt_out_line$opt_outs, function(xx){samplesize_n_for_singlerate(n = base_n*(1-(xx/100)))})

p2 <- ggplot(opt_out_line, aes(x=opt_outs, y = opt_out_pwr)) +
  geom_line() +
  xlim(c(0, 20)) +
  labs(title = "Power of sample size as opt-out percentage increases",
       subtitle = "Test of a single rate",
       x = "% of people with opt-out",
       y = "Power of comparison of 2 proportions", 
       caption = "Population size = 20 million, Observed rate = 1.22/100,000 person-years, null hypothesis value = 1, sig. level = 5%") + 
  theme_classic()
tiff( paste0(output_directory, "opt-out-power-singlerate.tiff"), width = 7, height = 3, units = "in", res = 800)
  p2
dev.off()

# numbers quoted int he results section
samplesize_n_for_singlerate(base_n*1) # at 100%
samplesize_n_for_singlerate(base_n*0.95) # at 5% opt-out
samplesize_n_for_singlerate(base_n*0.8) # at 20% opt-out

