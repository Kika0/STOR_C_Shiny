# dependence between r.v. for husler reiss dist
# use this to block out the upper part
upper_corner = tibble(x = c(0, 1, 1),
                      y = c(1, 1, 0))
# generate with coefficients with limiting point distributions ---
# adapt function form GEV.qmd
# try another
a1 <- c(17,3,0,1,4)
a <- a1/sum(a1)
b1 <- c(0,11,6,2,1)
b <- b1/sum(b1)
c1 <- c(0,0,3,3,4)
c <- c1/sum(c1)
abc <- data.frame(a,b,c)
generate_dependent_X_Y_Z <- function(N,abc=abc,dep) {
  set.seed(12)
  d <- 5
  U <- runif(d*N)
  # a <- c(2/3,1/12,0,1/12,1/6)
  # b <- c(0,1/3,1/3,1/6,1/6)
  # c <- c(0,0,1/3,1/3,1/3)
  a <- abc[,1]
  b <- abc[,2]
  c <- abc[,3]
  
  # generate Y
  # asy<-list(.4,.1,.6,c(.3,.2),c(.1,.1),c(.4,.1),c(.2,.3,.2))
  # Y <- rmvevd(n=N*d,dep=c(0.6,0.5,0.2,0.9),d=3,asy=asy,model="alog",mar=c(1,1,1)) 
  Y <-  exp(rmvevd(n=N*d,dep=dep,model="log",d=3))
  # generate X
  X_1 <- c()
  X_2 <- c()
  X_3 <- c()
  for (j in 1:N) {
    X_1[j] <- max(Y[(d*(j-1)+1):(d*j),1]*a)
    X_2[j] <- max(Y[(d*(j-1)+1):(d*j),2]*b)
    X_3[j] <- max(Y[(d*(j-1)+1):(d*j),3]*c)
  }
  return(data.frame(X_1,X_2,X_3))
}

plot_clusters <- function(sims,u=0.9,dep) {
  sims_low_dependence <- sims
  dat = tibble(x1 = sims_low_dependence[,1], x2 = sims_low_dependence[,2], x3 = sims_low_dependence[,3])%>%
    mutate(R = x1 + x2 + x3)  %>% 
    mutate(u = quantile(R, u)) %>%
    filter(R>u) %>%
    mutate(w1 = x1/R, 
           w2 = x2/R,
           w3 = x3/R) 
  
  dat %>%
    ggplot()+ 
    geom_density_2d_filled(aes(w1,w2),bins=10,contour_var = "ndensity") + 
    geom_polygon(data = upper_corner, aes(x,y),col = 'white', fill = "white")+
    theme_minimal()+
    scale_y_continuous(limits = c(0, 1))+
    scale_x_continuous(limits = c(0, 1))+
    theme(panel.grid.major = element_blank(), 
          # legend.position = 'none',
          panel.grid.minor = element_blank())+
    labs(x = "", y = "") + ggtitle(TeX(paste0("$u=\\hat{F}_R^{-1}($",u,"$)$",","," $\\alpha=$",dep))) +
    guides(fill=guide_legend(title="Density estimate")) 
}

# plot for different thresholds
#p1 <- generate_dependent_X_Y_Z(N=50000,abc=abc,dep=0.1) %>% plot_clusters(u=0.9,dep=0.99)

# for assymetric case
generate_dependent_X_Y_Y_Z <- function(N,abc=abc,dep) {
  set.seed(12)
  d <- 5
  U <- runif(d*N)
  # a <- c(2/3,1/12,0,1/12,1/6)
  # b <- c(0,1/3,1/3,1/6,1/6)
  # c <- c(0,0,1/3,1/3,1/3)
  a <- abc[,1]
  b <- abc[,2]
  c <- abc[,3]
  x_y <- evd::rbvevd(N*d,dep=dep[1],model="log")
  x <- exp(x_y[,1])
  Y <- exp(x_y[,2])
  a <- dep[2]
  # generate z
  to_opt <- function(z) {
    (  (  y^(-(1/a)+1)*(y^(-1/a)+z^(-1/a))^(a-1)*exp(-(y^(-1/a)+z^(-1/a))^a)*exp(1/y)  )-Unif)^2
  }
  z <- c()
  for (i in 1:nrow(x_y)){
    # generate U
    Unif <- runif(1)
    y <- Y[i]
    # F_Y_Z <- function(z) {
    #   -a *y^(-(1/a)+1)*(y^(-1/a)+z^(-1/a))^(a-1)*exp(-(y^(-1/a)+z^(-1/a))^a)
    # }
    z[i] <- optim(par=1,fn=to_opt)$par
  }
  Y <- data.frame(X_1=x,X_2=Y,X_3=z)
 
  # generate X
  X_1 <- c()
  X_2 <- c()
  X_3 <- c()
  for (j in 1:N) {
    X_1[j] <- max(Y[(d*(j-1)+1):(d*j),1]*a)
    X_2[j] <- max(Y[(d*(j-1)+1):(d*j),2]*b)
    X_3[j] <- max(Y[(d*(j-1)+1):(d*j),3]*c)
  }
  return(data.frame(X_1,X_2,X_3))
}
plot_clusters_2dep <- function(sims,u=0.9,dep) {
  sims_low_dependence <- sims
  dat = tibble(x1 = sims_low_dependence[,1], x2 = sims_low_dependence[,2], x3 = sims_low_dependence[,3])%>%
    mutate(R = x1 + x2 + x3)  %>% 
    mutate(u = quantile(R, u)) %>%
    filter(R>u) %>%
    mutate(w1 = x1/R, 
           w2 = x2/R,
           w3 = x3/R) 
  a <- dep[1]
  b <- dep[2]
  
  dat %>%
    ggplot()+ 
    geom_density_2d_filled(aes(w1,w2),bins=10,contour_var = "ndensity") + 
    geom_polygon(data = upper_corner, aes(x,y),col = 'white', fill = "white")+
    theme_minimal()+
    scale_y_continuous(limits = c(0, 1))+
    scale_x_continuous(limits = c(0, 1))+
    theme(panel.grid.major = element_blank(), 
          # legend.position = 'none',
          panel.grid.minor = element_blank())+
    labs(x = "", y = "") + ggtitle(TeX(paste0("$u=\\hat{F}_R^{-1}($",u,"$)$",","," $\\alpha_1=$",a,", $\\alpha_2=$",b))) +
    guides(fill=guide_legend(title="Density estimate")) 
}
#generate_dependent_X_Y_Y_Z(N=5000,abc=abc,dep=c(0.1,0.1)) %>% plot_clusters_2dep(u=0.9,dep=c(1/2,1/2))
all_comb <- array(dim=c(95,95,500,3))
dep_range <- seq(0.05,0.99,0.01)
for (i in 1:length(dep_range)) {
  for (j in 1:length(dep_range)) {
    dep <- c(dep_range[i],dep_range[j])
   all_comb[i,j,,] <-  array(unlist(generate_dependent_X_Y_Y_Z(N=500,abc=abc,dep=dep)),dim=c(50,3))
  }
}
#saveRDS(all_comb,"all_comb.rda")
