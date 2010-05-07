require 'xmmsclient'
require 'xmmsclient_glib'
require 'glib2'
require 'gimme-aux'
require 'rubygems'
require 'sexp'
require 'pp'

class GIMME

  def initialize
    @async = Xmms::Client.new('GIMMEasync').connect(ENV['XMMS_PATH'])
    @async.add_to_glib_mainloop
  end

  def self.gen_methods
    {
      'play' => 'playback_start',
      'pause' => 'playback_pause'
    }.each do |k,v|
      define_method(k) do
        @async.send(v).notifier {|id| message (k)}
      end
    end
  end

  gen_methods

  def list
    print_col("Default",["id","artist","album","title"])
  end

  private

  def print_col(name,atrib)
    @async.coll_get(name).notifier do |coll|
      @async.coll_query_info(coll,atrib).notifier do |wrapperdict|
        wrapperdict.each do |dict|
          print "(" # FIXME not sure how to send these alists to Emacs
          dict.each {|key,val| print [key, ".".to_sym, val].to_sexp } # FIXME ".".to_sym :P
          print ")\n"
        end
        42 # FIXME: For some reason, an integer is required
      end
    end
  end

end



$ml = GLib::MainLoop.new(nil, false)
client = GIMME.new
$channel = GLib::IOChannel.new(STDIN)

Thread.new do
  while true
    gets.strip.each do | command |
      parsed = command.parse_sexp.first
      if (parsed.class == Array && client.respond_to?(parsed.first))
        client.send(*parsed)
      else
        message ["wat", "does not compute", "can you explain with oranges?"][rand(3)]
      end
    end
  end
end

$ml.run
