find_point <- function(b, data){
        column <- ceiling(b/nrow(data))
        row  <- b - (column-1) * nrow(data)
        c(row, column)
}