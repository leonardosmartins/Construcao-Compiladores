def micro():
	numero=0
	programa=1
	while programa ==1:
		print("Digite um numero")
		numero = int(input())
		if numero>0:
			print("Positivo")
		else:
			if numero==0:
				print("O numero e igual a zero")
			if numero <0:
				print("Negativo")
		print("Deseja Finalizar")
		opc = input()
		if opc == 9:
			programa = 0
		
		
