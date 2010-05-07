require 'xmmsclient'
require 'xmmsclient_glib'
require 'glib2'
require 'gimme-aux'

class GIMME


  def initialize
    @async = Xmms::Client.new('GIMMEasync').connect(ENV['XMMS_PATH'])
    @async.add_to_glib_mainloop

  end

  def self.gen_methods
    m = {
      'play' => 'playback_start',
      'pause' => 'playback_pause'
    }

    m.each do |k,v|
      define_method(k) do
        @async.send(v).notifier {|id| puts sexp(k)}
      end
    end
  end

  gen_methods

end



$ml = GLib::MainLoop.new(nil, false)
client = GIMME.new
$channel = GLib::IOChannel.new(STDIN)

Thread.new do
  puts $channel
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
end

$ml.run
