$debug = 1

def debug(m)
  puts m if $debug
end

def sexp text
  # TODO: ESCAPE text
  "(message \"#{text}\")"
end
