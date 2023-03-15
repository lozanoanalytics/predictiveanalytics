function (x) 
{
  newx = .Call(Ccopy, x)
  if (!is.data.table(x)) {
    if (is.list(x)) {
      anydt = vapply(x, is.data.table, TRUE, USE.NAMES = FALSE)
      if (sum(anydt)) {
        newx[anydt] = lapply(newx[anydt], function(x) {
          setattr(x, ".data.table.locked", NULL)
          alloc.col(x)
        })
      }
    }
    return(newx)
  }
  setattr(newx, ".data.table.locked", NULL)
  alloc.col(newx)
}