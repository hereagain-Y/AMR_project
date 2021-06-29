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


lactams=c("amocla","ampicillin","cefoxitin","ceftazidime","ceftriaxone","cefepime")#,"penicillin","oxacillin")
#cefepimepi: 4th generation cephalosporin antibiotic.
#cefoxition, foxscr: 2nd
#ceftazidime: 3rd 
#ceftriaxone: 3rd;

aminoglycosides=c("gentamicin","tobramycin")#"gen500","st1000","vancomycin"

macrolides=c("azithromycin")#"erythromycin")
quinolones=c("ciprofloxacin")#"levofloxacin","moxifloxacin")
carbapenems=c("merope","imipenem")
#rifamycins=c("rifampin")
Tetracycline=c("tetracyclines")
uncoventional=c("trisul","chlora")#"clindamycin","nitrofurantoin","rifampin")
reserve=c("colistin","aztreonam")#"daptomycin","linezolid","aztreonam","synerc")
#lipopeptide antibiotic:daptomycin /red
#trisul: sulfonamide antibiotic
#chlora: phenicol antibiotic 
#cindamycin: lincosamides
#nitrofuran: nitrofuran antibiotic 
#linezolid: oxazokidinones/ red
#monobactams: aZtreonam / red 

#clindamycin:lincosamide antibiotic


#others=c(carbapenems,antituber)

# synerc
