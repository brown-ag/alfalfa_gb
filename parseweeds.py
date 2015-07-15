import re
filename="27-05-15_SV_bio.csv"
dat = open(filename)
data=dat.read()
lines=data.split("\n")
wvars={}
i=0
for l in lines[1:len(lines)-1]:
	chunks=l.split(",")
	#for c in chunks:
	if len(chunks) > 1:
                species=chunks[4].split(";")
                for s in species:
                        #print s
                        m=re.search('(\d+)(.*)',s)
                        print m.group(1)+' '+m.group(2)
                        if not m.group(2) in wvars:
                                wvars[m.group(2)]=[]
                        wvars[m.group(2)].append((i,m.group(1)))
                i+=1

print wvars

columns=wvars.keys()
rows=[]
header=['id']
for c in columns:
        header.append(c)
rows.append(header)
nlines=lines[0:len(lines)-1]
for j in range(0,len(nlines)-1):
        row=[]
        row.append(j+1)
        for w in wvars:
                fudge=-1
                for k in wvars[w]:
                        #print k[0]
                        if(j in k):  
                                fudge=(k[1])
                if fudge==-1:
                        row.append(0)
                else:
                        row.append(int(fudge))
        rows.append(row)

print rows

outer=open("27-05-15_weedsout.csv",'w')
for r in rows:
        outer.write(",".join(map(str,r))+"\n")
outer.close()
