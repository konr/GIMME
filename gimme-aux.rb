$debug = 1

def debug(m)
  puts m if $debug
end

def message (t)
  puts [:message, t].to_sexp
end

