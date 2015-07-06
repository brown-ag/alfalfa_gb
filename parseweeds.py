import re
filename="27-05-15_SV_bio.csv"
dat = open(filename)
data=dat.read()
lines=data.split("\n")
wvars={}
i=0
for l in lines[2:len(lines)]:
	chunks=l.split(",")
	#for c in chunks:
	species=chunks[4].split(";")
	for s in species:
		#print s
		m=re.search('(\d+)(.*)',s)
		print m.group(1)+' '+m.group(2)
		wvars[m.group(2)]=
