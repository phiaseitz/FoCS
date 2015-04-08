nums = ["0","1"]

highest_num = 10

for w1 in range(highest_num+1):
	for w2 in range(highest_num+1):
		for w3 in range (2*(highest_num +1)):
			print ("run binary_sum \"" + '{0:08b}'.format(w3) + "#" + 
				'{0:08b}'.format(w1) + "#"+ '{0:08b}'.format(w2) + "\";;")
			print (w3 == (w1 + w2))