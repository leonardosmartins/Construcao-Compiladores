def micro11():
	numero = 0
	x = 0
	print('Digite um numero')
	numero = int(input())
	if x == 1:
		print('Numero Positivo')
	elif x ==0:
		print('Zero')
	else:
		print('Negativo')
def verifica(n):
	res = 0
	if n>0:
		res = 1
	elif n<0:
		res = -1
	else:
		res = 0
	
	return res
