#' @title
#' Classify the antiobiotics from our AST system into 8 categories.
#'

#' @references
#'
#' @export
#'

#'
ast<-read.csv("~/Desktop/ast0617.csv",sep=",",header=T)
dim(ast)
antibiotics=ast%>%
  select(1:5,contains("result"))
dim(antibiotics)
Abx_name=
  sapply(strsplit(colnames(antibiotics)[6:35],"_"),function(x)x[2])


cat("-----Abx category list---------------")
#https://card.mcmaster.ca/ontology/36981
#"Gen500"=gentamicin, st1000=Streptomycin, trisul=Sulfamethizole/trimethoprim:
#chlora: chloramphenicol 
#foxscr: cefoxitin;
#synerc: quninupristin

lactams=c("amocla")
penicllin=c("ampicillin")
cephalosporin=c("cefoxitin","ceftazidime","ceftriaxone","cefepime")
#cefepimepi: 4th generation cephalosporin antibiotic.
#cefoxition, foxscr: 2nd
#ceftazidime: 3rd 
#ceftriaxone: 3rd;
Tetracycline=c("tetracyclines")
quinolones=c("ciprofloxacin")
aminoglycosides=c("gentamicin","tobramycin")
macrolides=c("azithromycin")
carbapenems=c("merope","imipenem")
sulfonamide=c("trisul")
phenicol=c("chlora")
rifamycins=c("rifampin")
#uncoventional=c("trisul","chlora","clindamycin","nitrofurantoin","rifampin")
reserve=c("colistin","aztreonam")
lincosamides=c("clindamycin")
nitro=c("nitrofurantoin")



#others=c(carbapenems,antituber)

# synerc
