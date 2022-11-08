pred_clmm <- function(eta, theta, cat = 1:(length(theta)+1), inv.link = plogis)
{
  Theta <- c(-1e3, theta, 1e3)
  sapply(cat, function(j)
    inv.link(Theta[j+1] - eta) - inv.link(Theta[j] - eta) )
}
