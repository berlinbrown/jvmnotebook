#
# http://basildoncoder.com/blog/2008/04/07/project-euler-problem-3/
def primeFactors(n, factor):
    factors = []
    while (n % factor != 0):
        factor = factor + 1
        
    factors.append(factor)
    if n > factor:
        factors.extend(primeFactors(n / factor, factor))
    return factors

print max(primeFactors(600851475143, 2))

# End of script
