$: << File.join(File.dirname(__FILE__))

require 'xmmsclient'
require 'xmmsclient_glib'
require 'glib2'
require 'rubygems'
require 'sexp'
require 'pp'
require 'gimme-aux'


NOTHING = "nil"
$stderr.reopen('/dev/null') # To prevent the library from FIXME: Won't work on Windows
$atribs=["title","id","artist","album"]

class GIMME

  def initialize
    @async = Xmms::Client.new('GIMMEasync').connect(ENV['XMMS_PATH'])
    @async.add_to_glib_mainloop

    @async.broadcast_playback_current_id.notifier do |res|
      @async.playlist("_active").current_pos.notifier do |pos|
        puts ["gimme-set-playing".to_sym, pos[:position]].to_sexp
      end
      true
    end

    @async.broadcast_medialib_entry_changed.notifier do |id|
      dict = {}
      @async.medialib_get_info(id).notifier do |res|
        $atribs.map{|i| i.to_sym}.each do |e|
          dict[e] = res[e] ? res[e].first.at(1) : NOTHING
        end
        puts [:"gimme-update-tags", [:quote, dict.to_a.flatten]].to_sexp
      end
      true
    end
    @async.broadcast_playlist_changed.notifier do |res|
      dict = {}; res.each {|key,val| dict[key] = val }
      dict[:pos] = dict[:position]
      dict.delete(:position); dict.delete(:name)
      dict[:type] = case dict[:type]
                    when Xmms::Playlist::ADD then :add
                    when Xmms::Playlist::INSERT then :insert
                    when Xmms::Playlist::SHUFFLE then :shuffle
                    when Xmms::Playlist::REMOVE then :remove
                    when Xmms::Playlist::CLEAR then :clear
                    when Xmms::Playlist::MOVE then :move
                    when Xmms::Playlist::SORT then :sort
                    when Xmms::Playlist::UPDATE then :update
                    end
      if (dict[:type] == :add or dict[:type] == :insert) then
        @async.medialib_get_info(dict[:id]).notifier do |res2|
          [:title, :artist, :album].each do |e|
            dict[e] = res2[e] ? res2[e].first.at(1) : NOTHING
          end if res2
          puts ["gimme-update-playlist".to_sym, [:quote, dict.to_a.flatten]].to_sexp
          42 # 3 freaking hours debugging because I forgot about this :(
        end
      else
        puts ["gimme-update-playlist".to_sym, [:quote, dict.to_a.flatten]].to_sexp
      end

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

  def clear; @async.playlist("_active").clear.notifier; end
  def shuffle; @async.playlist("_active").shuffle.notifier; end

  # FIXME: Generate these automatically
  def remove (pos); @async.playlist("_active").remove_entry(pos).notifier; end
  def add (id); @async.playlist("_active").add_entry(id).notifier; end
  def insert (id,pos); @async.playlist("_active").insert_entry(pos,id).notifier {|id| message id}; end

  def gimme (id, pos)
    @async.medialib_get_info(id).notifier do |res|
      if res then
        dict = {}; dict[:pos] = pos; dict[:id] = id
        [:title, :artist, :album].each {|e| dict[e] = res[e][:"plugin/id3v2"]}
        puts ["gimme-insert-song".to_sym, [:quote, dict.to_a.flatten], nil].to_sexp
      end
    end
  end

  def list
    # FIXME: Clean up this mess
    @async.playlist("_active").current_pos.notifier do |pos|
      pos = pos ? pos : {:name => NOTHING, :position => -1} #FIXME: In case the it isn't on the playlist
      puts [:"gimme-set-title", "GIMME - Playlist View (" + pos[:name] + ")"].to_sexp
      bdict={}
      @async.coll_get("_active").notifier do |coll|
        @async.coll_query_info(coll,$atribs).notifier do |wrapperdict|
          wrapperdict.each do |dict|
            adict = {}
            dict.each {|key,val| adict[key] = val }
            bdict[adict[:id]]=adict
          end
          @async.playlist("_active").entries.notifier do |list|
            list.each_with_index do |el,i|
              bdict[el][:pos] = i
              bdict[el][:face] = :highlight if (i == pos[:position])
              puts ["gimme-insert-song".to_sym,[:quote, bdict[el].to_a.flatten],:t].to_sexp
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
        when Xmms::Client::PAUSE then tickle; play # FIXME: Doesn't actually work
        end
      end
    end
  end

  def subcol (col,pattern)
    pattern = col == "*" ? pattern : "\"in:#{col}\" AND \"#{pattern}\""
    pattern = "in:foo"
    puts pattern
    puts Xmms::Collection.parse(pattern).idlist.each { |id| puts id }
    puts "ok"
  end

  def update_tags (alist)
    dict = {}; alist.each {|key,val| dict[key] = val }
    ($atribs-["id"]).each do |key|
      @async.medialib_entry_property_set(dict[:id], key.to_sym, dict[key.to_sym]).notifier {}
    end
  end

  def pcol (name)
    puts [:"gimme-set-title", "GIMME - Filter View (" +name + ")"].to_sexp
    @async.coll_get(name).notifier do |coll|
      coll = Xmms::Collection.universe if name == "*"
      @async.coll_query_info(coll,$atribs).notifier do |wrapperdict|
        wrapperdict.each do |dict|
          adict = {}
          dict.each {|key,val| adict[key] = val }
          puts ["gimme-insert-song".to_sym,[:quote, adict.to_a.flatten],:t].to_sexp
        end
        42 # FIXME: For some reason, an integer is required
      end
    end
  end

  private


  def change_volume (inc)
    @async.playback_volume_get.notifier do |vol|
      @async.playback_volume_set(:left, vol[:left]+inc).notifier {}
      @async.playback_volume_set(:right, vol[:right]+inc).notifier {}
      # Not exactly right, but who in this world uses left/right channels?!
      message "Volume set to " + [0,[100,(vol[:left] + inc)].min].max.to_s
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
