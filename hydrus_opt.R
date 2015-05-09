#hydrus-1d multiparameter optimization script
#MODELID = 5

obs_node_fname="S:\\Andrew\\Code\\alfalfa_gb_git\\Simulations\\99\\OBS_NODE.OUT"
con=file(obs_node_fname, open="r")
buf=""
while (length(li <- readLines(con, n=1,warn=FALSE))>0) {
 if(grepl("          ",li,fixed=TRUE)) {
   baz=gsub("\\s+",",",li,perl=TRUE)
   buf=paste(buf,baz[3:length(baz)],sep="\n")
 }
}
lapply(strsplit(buf,"\n",fixed=TRUE), strsplit, split=",")
