# writing your own functions

fn_pythag(a=c(1,2,3))



fn_sum <- function(A,B,C){
  if(!is.numeric(A)){
    stop("hey, A needs to be numeric")
  }
  D <- sum(A, B, C)
  return(D)
}

fn_sum(3,3,'4')

#source(filepath)

library(modelr)
library(tidyverse)


#set.seed(12345)
# before for loop make empty list

function(data=dat,n=100,form){
x <- list()
for(i in 1:n){
train <- mpg %>% 
  slice_sample(prop = .25)

 
test <- anti_join(data,train)

#mpg model
mod <- train %>% 
  glm(data = .,
      formula = cty ~ displ)

x[[i]] <- test %>% 
  add_residuals(model = mod) %>%  #show difference between expected and actual values
 pluck("resid") %>% 
  abs() %>% 
  mean()
}

return(x %>% unlist())

}

  
x_val(data = mpg,n = 100,form = formula(cty ~ displ))
x_val(data = mpg,n = 100,form = formula(cty ~ cyl + displ))
