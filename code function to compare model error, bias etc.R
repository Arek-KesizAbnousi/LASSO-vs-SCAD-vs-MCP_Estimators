#-------------------------------------------------------------------------------------------
#function to compare model error, bias etc.
#-------------------------------------------------------------------------------------------
library(ncvreg)
penalty=function(s,gamma_MCP,gamma_SCAD){
  Lasso_data=matrix(0,nrow=50,ncol=7)
  MCP_data=matrix(0,nrow=50,ncol=7)
  SCAD_data=matrix(0,nrow=50,ncol=7)
  for(i in 1:50){
    X=matrix(rnorm(n*p), nrow=n, ncol=p)%*%SigmaXsqrt
    y=X%*%beta + rnorm(n, sd = s)
    cv_fit_Lasso=cv.glmnet(X, y, alpha = 1, lambda = lambda_values)
    lasso_opt_lambda=cv_fit_Lasso$lambda.min
    lasso_reg=glmnet(X, y,alpha = 1,lambda=lasso_opt_lambda)
    beta_hat=matrix(coef(lasso_reg),nrow = 301)
    beta_hat=beta_hat[-1,]
    Lasso_data[i,1]=t(beta_hat-beta)%*%SigmaX%*%(beta_hat-beta)
    Lasso_data[i,2:5]=c(abs(beta_hat[1]-beta[1]), abs(beta_hat[3]-beta[3]),
                        abs(beta_hat[5]-beta[5]), abs(beta_hat[6]-beta[6]))
    Lasso_data[i,6]=length(which(beta_hat!=0 & beta!=0))/length(which(beta!=0))
    Lasso_data[i,7]=length(which(beta_hat==0 & beta==0))/length(which(beta==0))
    cv_fit_MCP=cv.ncvreg(X, y, alpha = 1, lambda = lambda_values)
    MCP_opt_lambda=cv_fit_MCP$lambda.min
    MCP_reg=ncvreg(X, y,alpha = 1,lambda=MCP_opt_lambda,type="MCP", gamma = gamma_MCP)
    beta_hat=matrix(coef(MCP_reg),nrow=301)
    beta_hat=beta_hat[-1,]
    MCP_data[i,1]=t(beta_hat-beta)%*%SigmaX%*%(beta_hat-beta)
    MCP_data[i,2:5]=c(abs(beta_hat[1]-beta[1]),abs(beta_hat[3]-beta[3]),
                      abs(beta_hat[5]-beta[5]), abs(beta_hat[6]-beta[6]))
    MCP_data[i,6]=length(which(beta_hat!=0 & beta!=0))/length(which(beta!=0))
    MCP_data[i,7]=length(which(beta_hat==0 & beta==0))/length(which(beta==0))
    cv_fit_SCAD=cv.ncvreg(X, y, alpha = 1, lambda = lambda_values)
    SCAD_opt_lambda=cv_fit_SCAD$lambda.min
    SCAD_reg=ncvreg(X, y,alpha = 1,lambda=SCAD_opt_lambda,type="SCAD", gamma = gamma_SCAD)
    beta_hat=matrix(coef(SCAD_reg),nrow=301)
    beta_hat=beta_hat[-1,]
    SCAD_data[i,1]=t(beta_hat-beta)%*%SigmaX%*%(beta_hat-beta)
    SCAD_data[i,2:5]=c(abs(beta_hat[1]-beta[1]),abs(beta_hat[3]-beta[3]),
                       abs(beta_hat[5]-beta[5]), abs(beta_hat[6]-beta[6]))
    SCAD_data[i,6]=length(which(beta_hat!=0 & beta!=0))/length(which(beta!=0))
    SCAD_data[i,7]=length(which(beta_hat==0 & beta==0))/length(which(beta==0))
  }
  mean_data=cbind(colMeans(Lasso_data),colMeans(MCP_data),colMeans(SCAD_data))
  rownames(mean_data)=c("Error","Bias1","Bias3","Bias5","Bias6","TPR","TNR")
  colnames(mean_data)=c("LASSO","MCP","SCAD")
  return(mean_data)
}