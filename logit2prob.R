library(ggplot2)
#visualization
logit_seq <- seq(-10, 10, by = .1)

prob_seq <- logit2prob(logit_seq)

rm(df)

df <- data.frame(Logit = logit_seq,
                 Probability = prob_seq)

ggplot(df) +
  aes(x = logit_seq, y = prob_seq) +
  geom_point(size = 2, alpha = .3) +
  labs(x = "logit", y = "probability of success")


coef(model_nbt)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

pro_df <- as.data.frame(logit2prob(coef(md.multinom)))
head(pro_df)

odds.prob <- function(x){
  prob <- x / (1+x)
  return(prob)
}

list.coef <- list(-0.178, 0.144, -0.0220, 0.153, 0.00610, 0.0646, -0.0476, 0.0675)
list.prob <- sapply(list.coef, logit2prob)

logit2prob()