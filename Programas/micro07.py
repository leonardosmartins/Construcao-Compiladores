def micro07():
	numero ,programa= 0,1
	opc = ""
	
	while programa ==1:
		print("Digite um numero: ",end="")
		numero = int(input())
		
		if numero>0:
			print("Positivo")
		else:
			if numero==0:
				print("O numero e igual a 0")
			if numero <0:
				print("Negativo")
			
		print("Deseja Finalizar? (S/N) ",end="")
		opc = input()
		if opc == "S":
			programa = 0