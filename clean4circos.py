
read="/Volumes/JORDAN 1/SUTD/Sembcorp/Correlation/corr121314A.txt" 
read1="/Volumes/JORDAN 1/SUTD/Sembcorp/Correlation/matB.1.Correff.txt"
write="/Volumes/JORDAN 1/SUTD/Sembcorp/Correlation/corr1234B.txt" 

# with open(read1,'r')as h:
# 	for line in h:
# 		line=line.split("\t")
# 		print line
# with open(write,'a') as g:
# 	g.write("data\tdata\tdata\t153,0,153\n")

with open(read1,'r') as f:
	newstr=''
	for line in f:
		print line
		line=line.split("\t")
		if line[0]!='data':
			line[1]=line[1][1:-1]
			# line[2]=line[2][1:-1]
			# print type(line[3])
			line[3]=str(int(float(line[3][:-2])))
		else:
			# line[1]=line[1][1:-1]
			# line[2]=line[2]
			line[3]=line[3][1:-2]
			# line[0]=line[0][1:-1]
		# print line
		newstr+='\t'.join(line)+"\n"
		# newstr+="\n"
	with open(write,'a') as g:
		print newstr
		g.write(newstr[:-2])