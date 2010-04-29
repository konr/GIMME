
class GIMME
  def test
    puts '(message "Hello, Cleveland!")'
  end
end

client = GIMME.new

while true
  # print "> "
  gets.strip.each do | command |
    if client.respond_to?(command.split.first)
      client.send(*command.split)
    else
      puts ["wat", "does not compute", "can you explain with oranges?"][rand(3)]
    end
  end
end
