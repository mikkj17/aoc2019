
def solve_first(inp)
  inp.lines.map { |num| num.to_i / 3 - 2 }.sum
end


def fuel_required(mod)
  remaining = mod / 3 - 2
  return 0 if remaining <= 0
  remaining + fuel_required(remaining)
end


def solve_second(inp)
  inp.lines.map { |num| fuel_required num.to_i }.sum
end


test = "\
14
1969
100756\
"
input = File.open('input.txt').read

puts solve_first input
puts solve_second input
