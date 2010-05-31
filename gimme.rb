require 'xmmsclient'
require 'xmmsclient_glib'
require 'glib2'
require 'rubygems'
require 'sexp'
require 'pp'
require 'gimme-aux'


class GIMME

  def initialize
    @async = Xmms::Client.new('GIMMEasync').connect(ENV['XMMS_PATH'])
    @async.add_to_glib_mainloop

    @async.broadcast_playback_current_id.notifier do |res|
      print_current(res)
      true
    end

    @async.broadcast_playlist_changed.notifier do |res|
      # puts ["gimme-update-playlist".to_sym].to_sexp FIXME: broken
      # FIXME: Solve the problem of having ghost gimme processes on emacs
      true
    end
  end

  def self.gen_methods
    {
      'play' => 'playback_start',
      'pause' => 'playback_pause',
      'stop' => 'playback_stop',
      'tickle' => 'playback_tickle',
    }.each do |k,v|
      define_method(k) { @async.send(v).notifier {|id| message (k)} }
    end
  end

  gen_methods

  def prev; @async.playlist_set_next_rel(-1).notifier { tickle }; end
  def next; @async.playlist_set_next_rel( 1).notifier { tickle }; end


  def list
    # FIXME: Clean up this mess
    @async.playback_current_id.notifier do |id|
      atrib=["id","artist","album","title"]
      bdict={}
      @async.coll_get("_active").notifier do |coll|
        @async.coll_query_info(coll,atrib).notifier do |wrapperdict|
          wrapperdict.each do |dict|
            adict = {}
            dict.each {|key,val| adict[key] = val }
            adict[:face] = :highlight if (adict[:id] == id)
            bdict[adict[:id]]=adict
          end
          @async.playlist("_active").entries.notifier do |list|
            list.each_with_index do |el,i|
              bdict[el][:pos] = i
              puts ["gimme-append-to-buffer".to_sym,[:quote, bdict[el].to_a.flatten]].to_sexp
            end
            42 # FIXME: For some reason, an integer is required
          end
          42 # FIXME: For some reason, an integer is required
        end
      end
    end
  end

  def inc_vol; change_volume 5; end
  def dec_vol; change_volume -5; end

  def toggle
    @async.playback_status.notifier do |s|
      s == Xmms::Client::PAUSE ? play : pause
    end
  end


  def playn (id)
    @async.playback_status.notifier do |s|
      @async.playlist_set_next(id).notifier do
        case s
        when Xmms::Client::PLAY then tickle
        when Xmms::Client::STOP then play
        when Xmms::Client::PAUSE then tickle # FIXME: Doesn't actually work
        end
      end
    end
  end

  private

  def print_current (id)
    puts ["gimme-set-playing".to_sym, id].to_sexp
  end

  def change_volume (inc)
    @async.playback_volume_get.notifier do |vol|
      @async.playback_volume_set(:left, vol[:left]+inc).notifier {}
      @async.playback_volume_set(:right, vol[:right]+inc).notifier {}
      # Not exactly right, but who in this world uses left/right channels?!
      message "Volume set to " + [0,[100,(vol[:left] + inc)].min].max.to_s
    end
  end

  def print_col(name,atrib)
    @async.coll_get(name).notifier do |coll|
      @async.coll_query_info(coll,atrib).notifier do |wrapperdict|
        wrapperdict.each do |dict|
          adict = {}
          dict.each {|key,val| adict[key] = val }
          puts ["gimme-append-to-buffer".to_sym,[:quote, adict.to_a.flatten]].to_sexp
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
