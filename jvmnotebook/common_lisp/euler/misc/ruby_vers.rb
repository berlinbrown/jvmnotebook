# Project Euler Problem #2
# This code from:
# http://www.absorbeo.net/2008/01/18/project-euler-problem-3/
# References:
# [1] http://www.ruby-doc.org/core/
def sieve(n)
  # Create two arrays (l and empty store)
  l = Array.new(n, 1)
  store = Array.new
  for i in 2 .. n
    next unless l[i]
    # num.step(limit, step ) {|i| block } => num
    # Start at num to limit of step:
    # E.g. 4.step(100, 2) = 4, 6, 8
    (i**2).step(n, i) {|x| 
      l[x] = nil 
    }
    store << i    
  end
  store
end
def determine
  primes = sieve(600851475143)
  #num = 317584931803
  num = 600851475143
  max = 0
  primes.each { |prime| 
    a, b = num.divmod(prime)
    if b == 0 then max = prime end
  }
  max
end

600,851,475,143

puts "-----"
puts determine
# End of File

