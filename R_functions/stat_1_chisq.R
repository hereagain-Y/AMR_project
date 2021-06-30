# @TITLE
#' a function loop over all levels and do stat test

do_chisq_test=function(data,var1,var2,levels,gender){
  if(gender=="1"){ # if male
  
    cat("this is fro male comparison")
    return(sapply(1:levels, function(x)chisq.test(table(data[[var1]],data[[var2]])[x,1:2])$p.val))
  }else{# if female 
   
    cat("this is for female comparison")
    return(sapply(1:levels, function(x)chisq.test(table(data[[var1]],data[[var2]])[x,3:4])$p.val))
  }
}
 #example
		  do_chisq_test(table.dat,"edu_level_BL","group","1")
