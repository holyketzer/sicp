def cons(x, y)
  ->(m) { m.call(x, y) }
end

def car(z)
  z.call(->(p, q) { p })
end

def cdr(z)
  z.call(->(p, q) { q })
end

puts car(cons(3, 10))
puts cdr(cons(3, 10))
