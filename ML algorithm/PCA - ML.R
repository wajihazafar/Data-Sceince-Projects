
ml1 <- as.data.frame(matrix(round(rnorm(100, 1, 1)), ncol = 5))
ml2 <- as.data.frame(matrix(round(rnorm(100, 5, 2)), ncol = 5))
ml3 <- as.data.frame(matrix(round(rnorm(100, 4, 2)), ncol = 5))
ml4<- as.data.frame(matrix(round(rnorm(100, 6, 3)), ncol = 5))
ml5 <- as.data.frame(matrix(round(rnorm(100, 8, 4)), ncol = 5))

#PCA computation for dataset 1:
p1 <- ncol(ml1) # no on variables in dataset
R1 <- cor(ml1) # correlation matrix for ml1 data set
R1
e1 <- eigen(R1) #Solving for eigenvalues and eigen vectors from correlation matrix
str(e1)
L <- e1$values # placing the eigenvalues in L
Vm1 <- matrix(0, nrow = p1, ncol = p1) # creating a PxP matrix with zeros.
#Vm1 is an orthogonal matrix since all correlations between variable are 0
diag(Vm1) <- L # putting the eigenvalues in the diagonal.
Vm1 # print the matrix with eigenvalues on the diagonals
e1$vectors #these are the eigenvectors-- these are the standardized regression weights
loadings <- e1$vectors %*% sqrt(Vm1) #these are the loadings or the correlation of the component variables with the original variables-- 
#sometimes referred to as the P matrix. 

loadings <- e1$vectors %*% Vm1 %*% t(e1$vectors) # by putting eigenvectors.
loadings
L/length(L) #This is the proportion of variance accounted for by each PC
# To compute the PCA scores (using matrices), zValues x eigenvectors:
zml1 <- scale(ml1)
pca_scores <- zml1 %*% e1$vectors
colnames(pca_scores) <- c('pca1', 'pca2', 'pca3', 'pca4')
head(pca_scores)
round(colMeans(pca_scores), 2) # mean calculation for PCA
apply(pca_scores, 2, var) # the variance PER column
e1$values
plot(L, main = "Scree Plot", ylab="Eigenvalues",xlab="Component number",type='b')
abline(h=1, lty=2)

#PCA computation for dataset 2:
p2 <- ncol(ml2) # no on variables in dataset
R2 <- cor(ml2) # correlation matrix for ml1 data set
R1
e2 <- eigen(R2) #Solving for eigenvalues and eigen vectors from correlation matrix
str(e2)
L2 <- e2$values # placing the eigenvalues in L
Vm2 <- matrix(0, nrow = p2, ncol = p2) # creating a PxP matrix with zeros.
#Vm2 is an orthogonal matrix since all correlations between variable are 0
diag(Vm2) <- L2 # putting the eigenvalues in the diagonal.
Vm2 # print the matrix with eigenvalues on the diagonals
e2$vectors #these are the eigenvectors-- these are the standardized regression weights
loadings <- e2$vectors %*% sqrt(Vm2) #these are the loadings or the correlation of the component variables with the original variables-- 
#sometimes referred to as the P matrix. 

loadings <- e2$vectors %*% Vm2 %*% t(e2$vectors) # by putting eigenvectors.
loadings
L2/length(L2) #This is the proportion of variance accounted for by each PC
# To compute the PCA scores (using matrices), zValues x eigenvectors:
zml2 <- scale(ml2)
pca_scores2 <- zml2 %*% e2$vectors
colnames(pca_scores) <- c('pca1', 'pca2', 'pca3', 'pca4', 'pca5', 'pca6')
head(pca_scores)
round(colMeans(pca_scores), 2) # mean calculation for PCA
apply(pca_scores, 2, var) # the variance PER column
e2$values
plot(L2, main = "Scree Plot", ylab="Eigenvalues",xlab="Component number",type='b')
abline(h=1, lty=2)

#PCA computation for dataset 3:
p3 <- ncol(ml3) # no on variables in dataset
R3 <- cor(ml3) # correlation matrix for ml1 data set
R3
e3 <- eigen(R3) #Solving for eigenvalues and eigen vectors from correlation matrix
str(e3)
L3 <- e3$values # placing the eigenvalues in L
Vm3 <- matrix(0, nrow = p3, ncol = p3) # creating a PxP matrix with zeros.
#Vm2 is an orthogonal matrix since all correlations between variable are 0
diag(Vm3) <- L3 # putting the eigenvalues in the diagonal.
Vm3 # print the matrix with eigenvalues on the diagonals
e3$vectors #these are the eigenvectors-- these are the standardized regression weights
loadings <- e3$vectors %*% sqrt(Vm3) #these are the loadings or the correlation of the component variables with the original variables-- 
#sometimes referred to as the P matrix. 
loadings <- e3$vectors %*% Vm3 %*% t(e3$vectors) # by putting eigenvectors.
loadings
L3/length(L3) #This is the proportion of variance accounted for by each PC
# To compute the PCA scores (using matrices), zValues x eigenvectors:
zml3 <- scale(ml3)
pca_scores3 <- zml3 %*% e3$vectors
colnames(pca_scores3) <- c('pca1', 'pca2', 'pca3', 'pca4', 'pca5', 'pca6')
head(pca_scores3)
round(colMeans(pca_scores3), 2) # mean calculation for PCA
apply(pca_scores3, 2, var) # the variance PER column
e3$values
plot(L3, main = "Scree Plot", ylab="Eigenvalues",xlab="Component number",type='b')
abline(h=1, lty=2)

#PCA computation for dataset 4:
p4 <- ncol(ml4) # no on variables in dataset
R4 <- cor(ml4) # correlation matrix for ml1 data set
R4
e4 <- eigen(R4) #Solving for eigenvalues and eigen vectors from correlation matrix
str(e4)
L4 <- e4$values # placing the eigenvalues in L
Vm4 <- matrix(0, nrow = p4, ncol = p4) # creating a PxP matrix with zeros.
#Vm2 is an orthogonal matrix since all correlations between variable are 0
diag(Vm4) <- L4 # putting the eigenvalues in the diagonal.
Vm4 # print the matrix with eigenvalues on the diagonals
e4$vectors #these are the eigenvectors-- these are the standardized regression weights
loadings <- e4$vectors %*% sqrt(Vm4) #these are the loadings or the correlation of the component variables with the original variables-- 
#sometimes referred to as the P matrix. 
loadings <- e4$vectors %*% Vm4 %*% t(e4$vectors) # by putting eigenvectors.
loadings
L4/length(L4) #This is the proportion of variance accounted for by each PC
# To compute the PCA scores (using matrices), zValues x eigenvectors:
zml4 <- scale(ml4)
pca_scores4 <- zml4 %*% e4$vectors
colnames(pca_scores4) <- c('pca1', 'pca2', 'pca3', 'pca4', 'pca5', 'pca6')
head(pca_scores4)
round(colMeans(pca_scores4), 2) # mean calculation for PCA
apply(pca_scores4, 2, var) # the variance PER column
e4$values
plot(L4, main = "Scree Plot", ylab="Eigenvalues",xlab="Component number",type='b')
abline(h=1, lty=2)

#PCA computation for dataset 3:
p5 <- ncol(ml5) # no on variables in dataset
R5 <- cor(ml5) # correlation matrix for ml1 data set
R5
e5 <- eigen(R5) #Solving for eigenvalues and eigen vectors from correlation matrix
str(e5)
L5 <- e5$values # placing the eigenvalues in L
Vm5 <- matrix(0, nrow = p5, ncol = p5) # creating a PxP matrix with zeros.
#Vm2 is an orthogonal matrix since all correlations between variable are 0
diag(Vm5) <- L5 # putting the eigenvalues in the diagonal.
Vm5 # print the matrix with eigenvalues on the diagonals
e5$vectors #these are the eigenvectors-- these are the standardized regression weights
loadings <- e5$vectors %*% sqrt(Vm5) #these are the loadings or the correlation of the component variables with the original variables-- 
#sometimes referred to as the P matrix. 

loadings <- e5$vectors %*% Vm5 %*% t(e5$vectors) # by putting eigenvectors.
loadings
L5/length(L5) #This is the proportion of variance accounted for by each PC
# To compute the PCA scores (using matrices), zValues x eigenvectors:
zml5 <- scale(ml5)
pca_scores5 <- zml5 %*% e5$vectors
colnames(pca_scores5) <- c('pca1', 'pca2', 'pca3', 'pca4', 'pca5', 'pca6')
head(pca_scores5)
round(colMeans(pca_scores5), 2) # mean calculation for PCA
apply(pca_scores5, 2, var) # the variance PER column
e5$values
plot(L5, main = "Scree Plot", ylab="Eigenvalues",xlab="Component number",type='b')
abline(h=1, lty=2)

e1$values/p1  #To show the percentage of variance accounted for by each variable.
#First component accounts for 31% of the variance, second 25%, etc.A scree plot is used to access 
#components or factors which explains the most of variability in the data. 
#It represents values in descending order.

e2$values/p2 #To show the percentage of variance accounted for by each variable.
#First component accounts for 32% of the variance, second 31%, etc.A scree plot is used to access 
#components or factors which explains the most of variability in the data. 
#It represents values in descending order.

e3$values/p3 #To show the percentage of variance accounted for by each variable.
#First component accounts for 36% of the variance, second 25%, etc.A scree plot is used to access 
#components or factors which explains the most of variability in the data. 
#It represents values in descending order.

e4$values/p4 #To show the percentage of variance accounted for by each variable.
#First component accounts for 33% of the variance, second 25%, etc.A scree plot is used to access 
#components or factors which explains the most of variability in the data. 
#It represents values in descending order.

e5$values/p5 #To show the percentage of variance accounted for by each variable.
#First component accounts for 28% of the variance, second 23%, etc.A scree plot is used to access 
#components or factors which explains the most of variability in the data. 
#It represents values in descending order.
