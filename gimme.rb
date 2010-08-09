# -*- coding: utf-8 -*-
$: << File.join(File.dirname(__FILE__))

require 'xmmsclient'
require 'xmmsclient_glib'
require 'glib2'
require 'rubygems'
require 'sexp'
require 'pp'
require 'gimme-aux'


DEBUG = false
NOTHING = "nil"
$stderr.reopen('/dev/null') # To prevent the library from FIXME: Won't work on Windows
$atribs=["title","id","artist","album","duration","starred"]

class GIMME

  def initialize
    begin
      @async = Xmms::Client.new('GIMMEasync').connect(ENV['XMMS_PATH'])
    rescue
      message "Failed to connect to the core. Make sure xmms2d is running"
    end
    @async.add_to_glib_mainloop

    last_time = 0
    @async.signal_playback_playtime.notifier do |time|
      if time == 0
      elsif ((time / 1000) % 60) != ((last_time / 1000) % 60)
        last_time = time
        @async.playback_current_id.notifier do |id|
          if id != 0
            @async.medialib_get_info(id).notifier do |res|
              duration = res[:duration] ? res[:duration].first.at(1) : NOTHING
              puts [:"gimme-update-playtime",time,duration].to_sexp
            end
          end
        end
      end
      true
    end

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
          $atribs.map{|x| x.to_sym}.each do |e|
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

  def seek_rel (x); @async.playback_seek_ms_rel(x.to_i).notifier; end
  def seek     (x); @async.playback_seek_ms(x.to_i).notifier; end

  # FIXME: Generate these automatically
  def remove (pos); @async.playlist("_active").remove_entry(pos).notifier; end
  def add (id); @async.playlist("_active").add_entry(id).notifier; end

  def url (id)
    @async.medialib_get_info(id).notifier do |res|
      message res[:url][:server]
    end
  end

  def addplay (id)
    @async.playlist("_active").add_entry(id).notifier do
      puts '(message "not yet implemented! appending to the playlist...")'
    end
  end

  def addcol (name)
    @async.coll_get(name).notifier do |c|
      # FIXME: Doesn't work :(
      @async.playlist("_active").add_collection(c).notifier { message "did it work?" }
    end
  end


  def insert (id,pos); @async.playlist("_active").insert_entry(pos,id).notifier {|id| message id}; end

  def list (session)
    # FIXME: Clean up this mess
    @async.playlist("_active").current_pos.notifier do |pos|
      pos = pos ? pos : {:name => NOTHING, :position => -1} #FIXME: In case the it isn't on the playlist
      puts [:"gimme-set-title", "GIMME - Playlist View (" + pos[:name] + ")"].to_sexp
      bdict={}
      @async.coll_get("_active").notifier do |coll|
        @async.coll_query_info(coll,$atribs).notifier do |wrapperdict|
          wrapperdict.each do |dict|
            adict = {}
            dict.each {|key,val| adict[key] = val.class == NilClass ? "nil" : val} #FIXME make this a function
            bdict[adict[:id]]=adict
          end
          @async.playlist("_active").entries.notifier do |list|
            list.each_with_index do |el,i|
              bdict[el][:pos] = i
              if (i == pos[:position])
                bdict[el][:face] = :highlight
              else
                bdict[el].delete(:face)
              end
              puts ["gimme-insert-song".to_sym,session,[:quote, bdict[el].to_a.flatten],:t].to_sexp
              #puts bdict.class
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

  def subcol (data,pattern)
    # FIXME: Complex patterns!
    #
    # Collections will follow this pattern:
    #
    # "*"
    # "0-tracknr:1"
    # "00-artist:Foo"
    @async.coll_get(data.to_s).notifier do |parent|
      if data.class == String and data == "*"
        parent = Xmms::Collection.universe
      elsif data.class == Symbol and data == :nil
        parent = Xmms::Collection.new(Xmms::Collection::TYPE_IDLIST)
        parent.idlist=[]
      elsif data.class == Array
        parent = Xmms::Collection.new(Xmms::Collection::TYPE_IDLIST)
        parent.idlist=data
      end
      match = Xmms::Collection.new(Xmms::Collection::TYPE_MATCH)
      match = Xmms::Collection.parse(pattern)

      intersection = Xmms::Collection.new(Xmms::Collection::TYPE_INTERSECTION)
      intersection.operands.push(parent)
      intersection.operands.push(match)
      @async.coll_query_ids(intersection).notifier do |list|
        puts [:"gimme-filter-set-current-col", [:quote, list]].to_sexp
      end
    end
  end

  def sort (criteria)
    @async.playlist("_active").sort(criteria.map{|x| x.to_s}).notifier {}
  end

  def update_tags (alist)
    dict = {}; alist.each {|key,val| dict[key] = val }
    ($atribs-["id"]).each do |key|
      key=key.to_sym; dict[key] = dict[key].class == Symbol ? dict[key].to_s : dict[key]
      @async.medialib_entry_property_set(dict[:id], key.to_sym, dict[key.to_sym]).notifier {}
    end
  end

  def pcol (data, session)
    @async.coll_get(data.to_s).notifier do |coll|
      if data.class == String and data == "*"
        coll = Xmms::Collection.universe
      elsif data.class == Symbol and data == :nil
        coll = Xmms::Collection.new(Xmms::Collection::TYPE_IDLIST)
        coll.idlist=[]
      elsif data.class == Array
        coll = Xmms::Collection.new(Xmms::Collection::TYPE_IDLIST)
        coll.idlist=data
      end
      @async.coll_query_info(coll,$atribs).notifier do |wrapperdict|
        wrapperdict.each do |dict|
          adict = {}
          dict.each {|key,val| adict[key] = val.class == NilClass ? "nil" : val}
          puts ["gimme-insert-song".to_sym,session,[:quote, adict.to_a.flatten],:t].to_sexp
        end
        42 # FIXME: For some reason, an integer is required
      end
    end
  end

  def set_atribs (l)
    $atribs |= l.map{|n| n.to_s}
  end

  def colls (session)
    @async.coll_list.notifier do |res|
      children = {}; name={}
      res.delete_if {|x| !x.match("^[0-9]\+-")}
      res.each {|x| x = x.split("-",2); name[x[0]]=x[1]}
      name[""]="Root"

      name.each_key do |k|
        some = name.reject {|k2,v2| !k2.match("^#{k}[0-9]$")}.each{|k2,v2| children[k]=children[k].to_a+[[k2,v2]]}
      end

      name.sort { |x,y| y[0].length <=> x[0].length }.each do |i|
        name[i[0]] = children.has_key?(i[0]) ? children[i[0]].map{|j| [j,*name[j[0]]]} : ""
      end

      puts [:"gimme-tree-colls",session,[:quote, name[""].to_a]].to_sexp
    end
  end

  def colls (session)
    @async.coll_list.notifier do |res|
      puts [:"gimme-tree-colls",session,[:quote, res.to_a]].to_sexp
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
