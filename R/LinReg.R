#' Linear regression by least squares
#' 
#' Computes linear regression by matrix multiplication of least squares.
#' 
#' @param Data A data.frame
#' @param formula A formula

#' @return
#' \code{print()} Returns the function call and estimated coefficients
#'
#' \code{plot()} Plots two ggplots. Residuals vs fitted and scale-location
#'
#' \code{resid()} Returns a vector of residuals
#' 
#' \code{pred()} Returns a vector of predicted values
#' 
#' \code{coef()} Returns a vector of estimated coefficients
#' 
#' \code{summary()} Returns a short summary of the model estimation
#' 
#' @examples
#' example <- linreg(data=iris, formula=Petal.Length~Species)
#' example$summary()
#' example$print()
#' 
#' @references Edlund, O. (2011). Some Notes on Least Squares, OR-factorization, SVD and Fitting. \url{http://staff.www.ltu.se/~jove/courses/c0002m/least_squares.pdf}
#' @export linreg
#' @import ggplot2 methods

linreg <- setRefClass("linreg",
                      fields = list(result ="list",
                                    data = "data.frame",
                                    df_name ="character",
                                    formula = "formula",
                                    beta_hat = "matrix",
                                    y_hat ="matrix",
                                    e_hat = "matrix",
                                    df = "numeric",
                                    sigma_2_hat = "matrix",
                                    var_beta_hat = "matrix",
                                    t_beta = "matrix",
                                    p_value = "matrix",
                                    X = "matrix",
                                    y = "numeric"),
                      methods = list(
                          initialize = function(formula, data){
                              formula <<-formula
                              df_name <<- as.character(substitute(data))
                              X <<- model.matrix(formula, data)                # Independen variable(s)
                              y <<- data[,all.vars(formula)[1]]                     # Dependent variable
                              
                              # Beta (Regression coefficients)
                              result$beta_hat <<- solve(t(X)%*%X)%*%t(X)%*%y
                              
                              # Fitted values 
                              result$y_hat <<- X%*%result$beta_hat
                              
                              # Residuals
                              result$e_hat <<- y-result$y_hat
                              
                              # Degrees of freedom
                              n <- nrow(X)       # Number of obs
                              p <- ncol(X)       # Number of variables
                              result$df <<- n-p  # Degrees of freedom
                              
                              # The residual variance
                              result$sigma_2_hat <<- t(result$e_hat)%*%result$e_hat/result$df
                              
                              # The variance of the regression coefficients
                              result$var_beta_hat <<- result$sigma_2_hat[1]*solve(t(X)%*%X)
                              
                              # The t-values for each coefficient
                              result$t_beta <<- result$beta_hat/sqrt(diag(result$var_beta_hat))
                              
                              # p-values
                              result$p_value <<- 2*(1-pt(result$t_beta, result$df))
                          },
                          print = function(){
                              
                              cat("Call:\nlinreg(formula = ", format(formula), ", data = ", df_name,")\n\n", sep="")
                              temp <- as.vector(result$beta_hat)
                              names(temp) <- colnames(X)
                              cat("Coefficients:\n")
                              print.data.frame(temp)
                          },
                          plot = function(){
                              # Data preperation for the first plot:
                              # A data.frame with x (fitted) and y (residuals) variables
                              plot_data1 <- data.frame(e_hat=result$e_hat,
                                                       y_hat=result$y_hat)
                              # Calculate the median residual for each fitted value
                              plot_median_data <- aggregate(plot_data1$e_hat, list(plot_data1$y_hat), FUN=median)
                              colnames(plot_median_data) <- c("y_hat", "median")
                              # Creating a data.frame with: Residuals, fitted values and the median for each fitted value
                              plot_data <- merge(plot_data1, plot_median_data, by="y_hat")
                              
                              # Finding outliers
                              qunts <- quantile(plot_data$e_hat, probs = c(0.02, 0.98))
                              # Criteras
                              smaller <- qunts[[1]]
                              larger <- qunts[[2]]
                              out <- which(plot_data$e_hat > larger | plot_data$e_hat < smaller)
                              # If not outlier -> NA
                              plot_data$outlier <-NA
                              # Saves the obs numbers of the outliers
                              plot_data$outlier[out] <- out
                              
                              
                              # Plot 1 - Residuals Vs fitted
                              p1 <- ggplot(data=plot_data, aes(y=e_hat, x=y_hat)) + 
                                  geom_point(shape=1, size=3) + 
                                  geom_hline(aes(yintercept=0), linetype='dotted', color="gray") +
                                  geom_line(color='red', data = plot_data, aes(x=y_hat, y=median)) +
                                  geom_text(aes(label = outlier), na.rm = TRUE, hjust=-0.3) +
                                  labs(y="Residuals",
                                       x = paste("Fitted values\nlm(",format(formula),")", sep=""),
                                       title="Residuals Vs Fitted") +
                                  theme_bw() +
                                  theme(plot.title = element_text(hjust=0.5),
                                        panel.grid.major = element_blank(),
                                        panel.grid.minor = element_blank())
                              
                              # Data preperation for the second plot:
                              # Uses the data.frame with x (fitted) and y (residuals) variables
                              plot_data2 <- plot_data1
                              # Transforming the residuals
                              plot_data2$e_hat <- sqrt(abs(scale(plot_data1$e_hat)))
                              # Calculate the median residual for each fitted value
                              plot_median_data2 <- aggregate(plot_data2$e_hat, list(plot_data$y_hat), FUN=median)
                              colnames(plot_median_data2) <- c("y_hat", "median")
                              # Creating a data.frame with: Residuals, fitted values and the median for each fitted value
                              plot_data_stan <- merge(plot_data2, plot_median_data2, by="y_hat")
                              
                              # Finding outliers
                              qunts <- quantile(plot_data_stan$e_hat, probs = c(0.02, 0.98))
                              # Criteras
                              smaller <- qunts[[1]]
                              larger <- qunts[[2]]
                              out <- which(plot_data_stan$e_hat > larger | plot_data_stan$e_hat < smaller)
                              # If not outlier -> NA
                              plot_data_stan$outlier <-NA
                              # Saves the obs numbers of the outliers
                              plot_data_stan$outlier[out] <- out
                              
                              # Plot 2 - Scale-Location
                              p2 <- ggplot(data=plot_data_stan, aes(y=e_hat, x=y_hat)) + 
                                  geom_point(shape=1, size=3) + 
                                  geom_hline(aes(yintercept=0), linetype='dotted', color="gray") +
                                  geom_line(color='red', data = plot_data_stan, aes(x=y_hat, y=median)) +
                                  geom_text(aes(label = outlier), na.rm = TRUE, hjust=-0.3) +
                                  labs(y=bquote(sqrt("|Standardized Residuals|")),
                                       x = paste("Fitted values\nlm(",format(formula),")", sep=""),
                                       title="Scale-Location") +
                                  theme_bw() +
                                  theme(plot.title = element_text(hjust=0.5),
                                        panel.grid.major = element_blank(),
                                        panel.grid.minor = element_blank())
                              return(list(p1,p2))
                          },
                          pred = function(){
                              return(result$y_hat)
                          },
                          coef = function(){
                              return(result$beta_hat)
                          },
                          resid = function(){
                              return(result$e_hat)
                          },
                          summary = function(){
                              sig_level <- rep(" ", length(result$p_value))
                            
                              for(i in 1:length(result$p_value)){
                                if(result$p_value[i]<0.1){sig_level[i]<-"."}
                                if(result$p_value[i]<0.05){sig_level[i]<-"*"}
                                if(result$p_value[i]<0.01){sig_level[i]<-"**"}
                                if(result$p_value[i]<0.001){sig_level[i]<-"***"}
                              }
                              data_frame <- data.frame(Estimate = as.vector(result$beta_hat),
                                                       "Std. Error" = as.vector(diag(sqrt(abs(result$var_beta_hat)))),
                                                       t_value = result$t_beta,
                                                       p_value = result$p_value,
                                                       sig_level = sig_level)
                              rownames(data_frame)<-rownames(result$beta_hat)
                              colnames(data_frame)<-c("Estimate", "Std. Error", "t value", "p value", "")
                              vec<-as.vector(sqrt(result$sigma_2_hat)[1])
                              cat("Coefficients:\n")
                              print.data.frame(data_frame)
                              cat("\nResidual standard error:", vec, "on", result$df,  "degrees of freedom.")
                          }
                      )
)

