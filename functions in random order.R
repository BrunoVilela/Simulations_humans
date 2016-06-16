
## Run functions in random order


func_1 <- function(){print("1")}
func_2 <- function(){print("2")}
func_3 <- function(){print("3")}
func_4 <- function(){print("4")}
func_5 <- function(){print("5")}


rand_order_func_run <- list(func_1 , func_2 , func_3 , func_4 , func_5 )


rand_order <- sample(1:5,5)
do.call(rand_order_func_run[[rand_order[1]]], args=list(), quote=FALSE)
do.call(rand_order_func_run[[rand_order[2]]], args=list(), quote=FALSE)
do.call(rand_order_func_run[[rand_order[3]]], args=list(), quote=FALSE)
do.call(rand_order_func_run[[rand_order[4]]], args=list(), quote=FALSE)
do.call(rand_order_func_run[[rand_order[5]]], args=list(), quote=FALSE)