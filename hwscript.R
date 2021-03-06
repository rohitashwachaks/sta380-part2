
#---- Question - 1 ----
gbuilds <- read.csv("./data/greenbuildings.csv")
#View(gbuilds)

str(gbuilds)

# Data conversion
gbuilds$cluster <- as.factor(gbuilds$cluster) # 693 clusters numbered randomly
gbuilds$renovated <- as.factor(gbuilds$renovated)
gbuilds$class_a <- as.factor(gbuilds$class_a)
gbuilds$class_b <- as.factor(gbuilds$class_b)
gbuilds$LEED <- as.factor(gbuilds$LEED)
gbuilds$Energystar <- as.factor(gbuilds$Energystar)
gbuilds$green_rating <- as.factor(gbuilds$green_rating)
gbuilds$net <- as.factor(gbuilds$net)
gbuilds$amenities <- as.factor(gbuilds$amenities)

attach(gbuilds)

slinreg <- lm(Rent-cluster_rent~green_rating, data = gbuilds)
cat(paste(summary(slinreg)))
# Coefficient of Green_Rating: 2.4125; P-Value: 8e-10

confoundingParams <- colnames(gbuilds)[-c(5,12,13,14,23)]#"Rent","green_rating","LEED","Energystar", Cluster Rent)]
cat(paste(confoundingParams))
significant <- NULL
i <- 0
for(col in confoundingParams)
{
  i <- i+1
  cat(paste(i,") MLR with",col),": ")
  mdl <- lm(paste("Rent-cluster_rent~ green_rating+",col), data = gbuilds)
  smry <- summary(mdl)
  pval <- smry$coefficients["green_rating1","Pr(>|t|)"]
  
  if(pval > 0.05)
  {
    cat("Confounding\n")
    significant <- c(significant,col)
  }
  else
  {
    cat("Non Confounding\n")
  }
}

cat(paste(significant))

mlr <- lm(Rent-cluster_rent~ green_rating+class_a, data = gbuilds)
smry <- summary(mlr)
cat(paste(smry))

## Therefore the class of the building is a confounding variable
plot(class_a,Rent-cluster_rent)

detach(gbuils)
