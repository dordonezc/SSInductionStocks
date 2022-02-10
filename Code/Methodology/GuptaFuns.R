## Hyperparameters 

## carries out the SVLM procedure proposed in Gupta S, Kanchinadam T, Conathan D
# and Fung G (2020) Task-Optimized Word Embeddings for Text Classification Representations.
# Front. Appl. Math. Stat. 5:67. doi: 10.3389/fams.2019.00067

directed_ls_weights <- function(wts_mat, embed_mat, label_vct, cost = 1, regul = 0.001,
                             max_iter = 500, max_tol = 0.01){
  ## Drop NA's
  drop <- which(is.na(label_vct))
  if(length(drop) != 0){
    label_vct <- label_vct[-drop]
    wts_mat <- wts_mat[-drop,]
  }

  ## It is possible that the code fails when the weight matrix becomes non invertible
  ## when excluding testing samples. That is there are words that don't appear in the training
  ## corpus. It is important to only use words that appear in at least a certain number of docs
  
  ## Length of the corpus
  k <- dim(wts_mat)[2]
  col_len <- dim(embed_mat)[2] + 1
  
  ## Starting weights for lambda
  lambda <- rep(1, k)
  
  ## Calculate everyting that doesn't get updated in the loop 
  get_slowinv <- function(x, regul){
    ## Function used to get inverse
    prod <- x %*% t(x)
    diag(prod) <- diag(prod) + regul
    inv_prod <- chol2inv(chol(prod))
    aux_mat <- (t(x) %*% inv_prod %*% x)*(-1)
    diag(aux_mat) <- diag(aux_mat) + 1
    end <- aux_mat/regul
    end
  }
  
  fixed_mat <- get_slowinv(wts_mat, regul = regul)
  constant_mat <- fixed_mat %*% (t(wts_mat) %*% label_vct)
  
  ## Hyperparameters from the while 
  iter <- 0
  tol_crit <- 1
  conv_crit <- c()
  
  while(iter < max_iter & tol_crit > max_tol){
    
    iter <- iter + 1
    ##-------------------------------------##
    ## 1st opt problem
    ## Define A_bar
    A_bar <- cbind(wts_mat %*% (embed_mat * lambda), 1)
    
    ## Inverse
    inv_part <- (t(A_bar) %*% A_bar + diag(col_len)/cost)
    
    ## Get coefficients
    cof <- solve(inv_part) %*% t(A_bar) %*% label_vct
    
    ## Clean 
    rm(inv_part, A_bar)
    
    ##--------------------------------------##
    ## 2nd opt problem
    
    ## Get W
    W <- embed_mat %*% cof[1:(col_len - 1)]
    
    ## Define new lambda
    old_lambda <- lambda
    lambda <- W[,,drop=T] * constant_mat[,,drop=T] * (1-cof[col_len])
    
    ## Good thing here is the inverse that is used gets calculated only once
    ## outside the loop
    
    ##--------------------------------------##
    
    ## Convergence criteria 
    tol_crit <- sqrt(sum((lambda - old_lambda)^2))
    cat("Iteration : ", iter, "\n")
    conv_crit <- c(conv_crit, tol_crit)
  }
  list("Lambda"=lambda, "Convergence"=conv_crit)
}


##-----------------------------------------------------------------------------------##

directed_svm_weights <- function(wts_mat, embed_mat, label_vct,
                             max_iter = 500, max_tol = 0.01, ...){
  
  ## Length of the corpus
  k <- dim(wts_mat)[2]

  ## Starting weights for lambda
  lambda <- rep(1, k)
  
  ## Hyperparameters from the while 
  iter <- 0
  tol_crit <- 1
  conv_crit <- c()
  
  while(iter < max_iter & tol_crit > max_tol){
    
    iter <- iter + 1
    ##-------------------------------------##
    ## 1st opt problem
    ## Define A_bar
    A_bar <- wts_mat %*% (embed_mat * lambda)
    
    ## Get coefficients
    cof_mod <- svm(x=A_bar, y = label_vct, kernel = "linear")
    cof <- coef(cof_mod)
    
    ##--------------------------------------##
    ## 2nd opt problem
    
    ## Get W
    W <- embed_mat %*% cof[-1]
    M <- t(t(wts_mat) * as.numeric(W))
    
    ## Get coefficients
    cof_mod <- svm(x=M, y = label_vct, kernel = "linear")
    lambda_aux <- coef(cof_mod)
    
    
    ## Define new lambda
    old_lambda <- lambda
    lambda <- lambda_aux[-1]
    
    ## Good thing here is the inverse that is used gets calculated only once
    ## outside the loop
    
    ##--------------------------------------##
    
    ## Convergence criteria 
    tol_crit <- sqrt(sum((lambda - old_lambda)^2))
    cat("Iteration : ", iter, "\n")
    conv_crit <- c(conv_crit, tol_crit)
  }
  list("Lambda"=lambda, "Convergence"=conv_crit)
}
