import math

def isPrime(x):
	divisorStart = math.ceil(math.sqrt(x))
	divisor=divisorStart
	while ((math.fabs(x) < 1) and ((x / divisor) != math.trunc(x / divisor)) and divisor > 1):
		divisor -= 1
		print x + divisor

	if (divisor == 1):
		return True
	else:
		return False