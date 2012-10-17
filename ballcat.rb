require 'strscan'

class String

  def parse_sexp
    s = StringScanner.new(self)
    parse_aux(s).first
  end

  private

  def parse_aux(s)
    exp = []
    while true
      token = fetch_token(s)
      case token
      when '('
        exp << parse_aux(s)
      when ')'
        break
      when :"'"
        token = fetch_token(s)
        case token
        when '(' then exp << [:quote].concat([parse_aux(s)])
        else exp << [:quote, token]
        end
      when String, Fixnum, Float, Symbol
        exp << token
      when nil
        break
      end
    end
    exp
  end

  def fetch_token(s)
    s.skip(/\s+/)
    return nil if(s.eos?)

    token =
      # Match parentheses
      if s.scan(/[\(\)]/)
        s.matched
        # Match a string literal
      elsif s.scan(/"([^"\\]|\\.)*"/)
        eval(s.matched)
        # Match a float literal
      elsif s.scan(/[\-\+]? [0-9]+ ((e[0-9]+) | (\.[0-9]+(e[0-9]+)?))/x)
        s.matched.to_f
        # Match an integer literal
      elsif s.scan(/[\-\+]?[0-9]+/)
        s.matched.to_i
        # Match a comma (for comma quoting)
      elsif s.scan(/'/)
        s.matched.to_sym
        # Match a symbol
      elsif s.scan(/[^\(\)\s]+/)
        s.matched.to_sym
        # If we've gotten here then we have an invalid token
      else
        near = s.scan %r{.{0,20}}
        raise "Invalid character at position #{pos} near '#{near}'."
      end
    return token
  end
end

class Array

  def to_sexp
    mapped = self.map do |x|
      if(x.is_a?(Array))
        x.to_sexp
      elsif (x.is_a?(String))
        "\"" + x.to_s + "\""
      else
        x.to_s
      end
    end
    "(" + mapped.join(" ") + ")"
  end
end
