# -*- coding: utf-8 -*-
$: << File.join(File.dirname(__FILE__))


['xmmsclient', 'glib2', 'xmmsclient_glib', 'rubygems', 'sexp'].each do |lib|
  begin
    require lib
  rescue LoadError
    warn "(message \"Oops! Something didn't go right. \
I bet 3 internets that '#{lib}' is missing\")"
    exit
  end
end


DEBUG = false
NOTHING = "nil"
$stderr.reopen('/dev/null') # To prevent the library from FIXME: Won't work on Windows

$atribs=["title","id","artist","album","duration","starred"]

class GIMME

  def initialize
    begin
      @async = Xmms::Client.new('GIMMEasync').connect(ENV['XMMS_PATH'])
    rescue
      to_emacs [:message, "Failed to connect to the core. Make sure xmms2d is running"]
    end
    @async.add_to_glib_mainloop

    ##################
    ### Broadcasts ###
    ##################

    last_time = 0
    @async.signal_playback_playtime.notifier do |time|
      if time == 0
      elsif ((time / 1000) % 60) != ((last_time / 1000) % 60)
        last_time = time
        @async.playback_current_id.notifier do |id|
          if id != 0
            @async.medialib_get_info(id).notifier do |res|
              duration = res[:duration] ? res[:duration].first.at(1) : NOTHING
              to_emacs [:"gimme-update-playtime",time,duration]
            end;end;end;end
      true;end

    @async.broadcast_coll_changed.notifier do |res|
      dict = {}; res.each{|k,v| dict[k]=v}
      dict[:type] = case dict[:type]
                    when Xmms::Collection::ADD then :add
                    when Xmms::Collection::UPDATE then :update
                    when Xmms::Collection::RENAME then :rename
                    when Xmms::Collection::REMOVE then :remove
                    end
      to_emacs [:"gimme-coll-changed", [:quote, dict.to_a.flatten]]
      true;end

    @async.broadcast_playback_current_id.notifier do |res|
      @async.playlist("_active").current_pos.notifier do |pos|
        to_emacs [:"gimme-set-playing", pos[:position]]
      end
      true;end

    @async.broadcast_medialib_entry_changed.notifier do |id|
      dict = {}
      @async.medialib_get_info(id).notifier do |res|
        $atribs.map{|i| i.to_sym}.each do |e|
          dict[e] = res[e] ? res[e].first.at(1) : NOTHING
        end
        to_emacs [:"gimme-update-tags", [:quote, dict.to_a.flatten]]
      end
      true;end

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
          to_emacs [:"gimme-update-playlist", [:quote, dict.to_a.flatten]]
          true
        end
      else
        to_emacs [:"gimme-update-playlist", [:quote, dict.to_a.flatten]]
      end
      true
    end
  end

  #########################
  ### Shorter Functions ###
  #########################

  def self.gen_methods

    # Client

    { 'play'   => 'playback_start',
      'pause'  => 'playback_pause',
      'stop'   => 'playback_stop',
      'tickle' => 'playback_tickle',
      'dcol'   => 'coll_remove'}.each do |k,v|
      define_method(k) { |*args| @async.send(v,*args).notifier };end

    # Playlist

    { 'add'     => 'add_entry',
      'remove'  => 'remove_entry',
      'insert'  => 'insert_entry',
      'clear'   => 'clear',
      'shuffle' => 'shuffle'}. each do |k,v|
      define_method(k) { |*args| @async.playlist("_active").send(v,*args).notifier}; end

    # Misc

    { 'seek'     => 'playback_seek_ms',
      'seek_rel' => 'playback_seek_ms_rel'}. each do |k,v|
      define_method(k) { |x| @async.send(v,x.to_s.to_i).notifier}; end

    { 'prev' => -1, 'next' => 1}. each do |k,v|
      define_method(k) { @async.playlist_set_next_rel(v).notifier { tickle }}; end
  end; gen_methods

  ########################
  ### Longer Functions ###
  ########################

  def sort (crit); @async.playlist("_active").sort(crit.map{|x| x.to_s}).notifier; end

  def addplay (id)
    @async.playlist("_active").add_entry(id).notifier do
      @async.playlist("_active").entries.notifier { |l| playn l.length-1 }; end; end

  def toggle
    @async.playback_status.notifier {|s| s == Xmms::Client::PAUSE ? play : pause}; end


  def playn (id)
    @async.playback_status.notifier do |s|
      @async.playlist_set_next(id).notifier do
        case s
        when Xmms::Client::PLAY then tickle
        when Xmms::Client::STOP then play
        when Xmms::Client::PAUSE then play; tickle
        end; end; end; end

  def list (session)
    @async.playlist("_active").current_pos.notifier do |pos|
      bdict={}; pos = pos || {:name => NOTHING, :position => -1}
      to_emacs [:"gimme-set-title", "GIMME - Playlist View (#{pos[:name]})"]
      @async.coll_get("_active").notifier do |coll|
        @async.coll_query_info(coll,$atribs).notifier do |wrapperdict|
          wrapperdict.each do |dict|
            adict = {}
            dict.each {|key,val| adict[key] = val.class == NilClass ? NOTHING : val}
            bdict[adict[:id]]=adict
          end
          @async.playlist("_active").entries.notifier do |list|
            list.each_with_index do |el,i|
              bdict[el][:pos] = i; bdict[el].delete(:face)
              bdict[el][:face] = :highlight if (i == pos[:position])
              data = [:quote, bdict[el].to_a.flatten]
              to_emacs [:"gimme-insert-song",session.to_i,data,:t]
            end
            true; end
          true;end;end;end;end

  def update_tags (alist)
    dict = {}; alist.each {|key,val| dict[key] = val }
    ($atribs-["id"]).each do |key|
      key=key.to_sym; dict[key] = dict[key].class == Symbol ? dict[key].to_s : dict[key]
      @async.medialib_entry_property_set(dict[:id], key, dict[key]).notifier;end;end

  def vol (inc)
    @async.playback_volume_get.notifier do |vol|
      inc = inc.to_s.to_i # The sexp library translates -5 into a symbol :P
      new = [0,[100,(vol[:left] + inc)].min].max
      @async.playback_volume_set(:left, new).notifier {}
      @async.playback_volume_set(:right, new).notifier {}
      to_emacs [:message, "Volume set to " + new.to_s]; end; end


  ###################
  ### Collections ###
  ###################

  def subcol (data,pattern)
    with_col(data) do |parent|
      match = Xmms::Collection.new(Xmms::Collection::TYPE_MATCH)
      match = Xmms::Collection.parse(pattern)
      intersection = Xmms::Collection.new(Xmms::Collection::TYPE_INTERSECTION)
      intersection.operands.push(parent)
      intersection.operands.push(match)
      @async.coll_query_ids(intersection).notifier do |list|
        to_emacs [:"gimme-filter-set-current-col", [:quote, list]]
      end;end;end

  def pcol (data, session)
    with_col(data) do |coll|
      @async.coll_query_info(coll,$atribs).notifier do |wrapperdict|
        wrapperdict.each do |dict|
          adict = {}
          dict.each {|key,val| adict[key] = val.class == NilClass ? NOTHING : val}
          to_emacs [:"gimme-insert-song",session,[:quote, adict.to_a.flatten],:t]
        end
        true;end;end;end

  def rcol (old, new)
    @async.coll_rename(old,new,Xmms::Collection::NS_COLLECTIONS).notifier {|res|}
  end

  def scol (data,name)
    coll = Xmms::Collection.new(Xmms::Collection::TYPE_IDLIST)
    coll.idlist=data
    @async.coll_save(coll,name,Xmms::Collection::NS_COLLECTIONS)
  end

  def colls (session)
    @async.coll_list.notifier do |res|
      to_emacs [:"gimme-tree-colls",session,[:quote, res.to_a]]
    end
  end

  ########################
  ### Misc and Private ###
  ########################

  def set_atribs (l); $atribs |= l.map{|n| n.to_s}; end

  private

  def to_emacs (array); puts array.to_sexp; end

  def with_col (data)
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
      yield coll
    end;end
end



$ml = GLib::MainLoop.new(nil, false)
$channel = GLib::IOChannel.new(STDIN)
client = GIMME.new

Thread.new do
  while true
    p = gets.strip.parse_sexp.first
    client.send(*p) if (p.class == Array && client.respond_to?(p.first))
  end;end

$ml.run
