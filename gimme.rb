# -*- coding: utf-8 -*-

dir=File.dirname(__FILE__)
$LOAD_PATH << dir

['rparsec', 'sexp'].each {|gem| Dir["#{dir}/gems/gems/#{gem}-*/**/"].each {|d| $LOAD_PATH << d}}
['xmmsclient', 'glib2', 'xmmsclient_glib', 'rubygems', 'crawlyr','freebase','sexp'].each do |lib|
  begin
    require lib
  rescue LoadError
    warn "(message \"Oops! Something didn't go right. I bet that '#{lib}' is missing\")"
    exit
  end
end


NOTHING = "nil"

$stderr.reopen(((RUBY_PLATFORM =~ /win/) ? 'NUL' : '/dev/null'), 'w')
$atribs=["title","id","artist","album", "duration","starred","url","tracknr",
         "timesplayed"]


##################################
### Extending existing objects ###
##################################

module Xmms
  class Collection
    def longtype
      {Xmms::Collection::TYPE_REFERENCE => :reference,
        Xmms::Collection::TYPE_UNION => :union,
        Xmms::Collection::TYPE_INTERSECTION => :intersection,
        Xmms::Collection::TYPE_COMPLEMENT => :complement,
        Xmms::Collection::TYPE_HAS => :has,
        Xmms::Collection::TYPE_EQUALS => :equals,
        Xmms::Collection::TYPE_MATCH => :match,
        Xmms::Collection::TYPE_SMALLER => :smaller,
        Xmms::Collection::TYPE_GREATER => :greater,
        Xmms::Collection::TYPE_IDLIST => :idlist,
        Xmms::Collection::TYPE_QUEUE => :queue,
        Xmms::Collection::TYPE_PARTYSHUFFLE => :partyshuffle} [self.type]
    end

    def to_a
      [self.type,
       self.attributes.map {|k,v| [k,v]}.flatten,
       self.operands.map {|op| op.to_a}]
    end

    def Collection.from_a (array)
      array.map! { |el| el == :nil ? [] : el}

      new = Xmms::Collection.new(array[0])
      array[1].each_slice(2).each { |key,val| new.attributes[key.to_s] = val.to_s }
      array[2].each { |child| new.operands.push(Xmms::Collection.from_a(child))}

      new
    end

    def to_sexp; self.to_a.to_sexp; end
    def Collection.from_sexp (sexp); Collection.from_a(sexp.parse_sexp.first); end
  end
end

class Array
  def rmap
    self.map {|x| x.respond_to?(:rmap) ? x.rmap(&block) : yield(x) }
  end
  def rmap2 (fun)
    self.map {|x| x.respond_to?(:rmap2) ? x.rmap2(fun) : fun.call(x)}
  end
  def collect_every(n, fill=false, offset = 0)

    if block_given?
      while  offset < size
        ret = []

        if fill
          n.times do |x|
            if offset + x > size - 1 then ret << nil
            else  ret << self[offset + x] end
          end
        else
          n.times { |x| ret << self[offset + x] unless offset + x > size - 1}
        end

        offset += n
        yield ret
        ret = nil
      end

    else

      ret = []
      while  offset < size
        ret << []
        if fill
          n.times do |x|
            if offset + x > size - 1 then ret.last << nil
            else ret.last << self[offset + x]; end
          end
        else
          n.times { |x| ret.last << self[offset + x] unless offset + x > size - 1 }
        end

        offset += n
      end
      return ret
    end
  end
end


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
        to_emacs [:"gimme-set-playing", pos[:name], pos[:position]]
      end
      true;end

    @async.broadcast_medialib_entry_changed.notifier do |id|
      dict = {}
      @async.medialib_get_info(id).notifier do |res|
        $atribs.map{|i| i.to_sym}.each do |e|
          dict[e] = res[e] ? res[e].first.at(1) : NOTHING
        end
        dict[:timesplayed] = Hash[res[:timesplayed].to_a][:server].to_s
        to_emacs [:"gimme-update-tags", [:quote, dict.to_a.flatten]]
      end
      true;end

    @async.broadcast_playlist_changed.notifier do |res|
      dict = {}; res.each {|key,val| dict[key] = val }
      dict[:pos] = dict[:position]
      dict.delete(:position);
      #dict.delete(:name) Can't use 'name': reserved word or lousy code? you decide
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
          to_emacs [:"gimme-broadcast-playlist", [:quote, dict.to_a.flatten]]
          true
        end
      else
        to_emacs [:"gimme-broadcast-playlist", [:quote, dict.to_a.flatten]]
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

  def remove_many (first, times)
    @async.playlist("_active").remove_entry(first).notifier do
      remove_many(first, times-1)
      true
    end if times > 0
  end

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

  def list (playlist,gen=nil)
    plist = [:quote, [:"gimme-buffer-type", :playlist,
                      :"gimme-playlist-name", playlist]]
    to_emacs [:"gimme-gen-buffer",plist] unless gen
    @async.playlist(playlist).current_pos.notifier do |pos|
      bdict={}; pos = pos || {:name => NOTHING, :position => -1}
      @async.coll_get(playlist).notifier do |coll|
        @async.coll_query_info(coll,$atribs).notifier do |wrapperdict|
          wrapperdict.each do |dict|
            adict = {}
            dict.each do |key,val|
              adict[key] = val.class == NilClass ? NOTHING : val
            end
            bdict[adict[:id]]=adict
          end
          @async.playlist(playlist).entries.notifier do |list|
            list.each_with_index do |el,i|
              bdict[el][:pos] = i; bdict[el].delete(:face)
              bdict[el][:face] = :highlight if (i == pos[:position])
              data = [:quote, bdict[el].to_a.flatten]
              to_emacs [:"gimme-insert-song",plist,data,:t]
            end
            true; end
          true;end;end;end;end

  def coll_overview(name = nil)
    Thread.new do
      sleep 0.5 # Feels smoother than without the sleep
      message = !name
      to_emacs [:"message", "Caching data from the Mlib..."] if message
      perc = [-1]; i = 0
      with_coll(name) do |coll|
        atribs = $atribs - ["url","duration","id","tracknr","starred","timesplayed"]
        hash = Hash[]
        $atribs.each {|at| hash[at] = Hash[]}
        @async.coll_query_info(coll,atribs).notifier do |all|
          n = all.count
          all.each do |raw|
            i += 1
            perc = (perc+[i*100/n])[-2..-1]
            to_emacs [:"message", "Caching data from the Mlib... #{i*100/n}%%"] if perc[0] != perc[1] and message
            dict = Hash[raw.to_a]
            atribs.each {|at| val = dict[at.to_sym]; hash[at][val] = 0 if !hash[at][val]; hash[at][val] += 1}
          end
          to_emacs [:"gimme-coll-overview", name, [:quote, hash.to_a.map{|x| [x[0],x[1].to_a.map{|k,_| k}]}]]
        end; end; end; end

  def update_tags (alist)
    dict = {}; alist.each {|key,val| dict[key] = val }
    ($atribs-["id","url","font-lock-face"]).each do |key|
      key=key.to_sym; dict[key] = dict[key].class == Symbol ? dict[key].to_s : dict[key]
      @async.medialib_entry_property_set(dict[:id], key, dict[key]).notifier;end;end

  def vol (inc)
    @async.playback_volume_get.notifier do |vol|
      if vol; then
        inc = inc.to_s.to_i # The sexp library translates -5 into a symbol :P
        new = [0,[100,(vol[:left] + inc)].min].max
        @async.playback_volume_set(:left, new).notifier {}
        @async.playback_volume_set(:right, new).notifier {}
        to_emacs [:message, "Volume changed to #{new}"]
      else
        to_emacs [:message, "Error while trying to change the volume"]
      end; end; end

  #####################
  ### Configuration ###
  #####################

  def track_conf (id)
    @async.medialib_get_info(id).notifier do |res|
      alist = res.to_a
      alist = alist.map {|k,v| [k,v.to_a.last[1]]}
      to_emacs [:"gimme-track-conf",[:quote, alist]]
    end
  end

  def conf
    @async.config_list_values.notifier do |x|
      y=x.to_a
      to_emacs [:"gimme-print-conf",[:quote, y]]
    end; end

  def conf_save (alist, silent = nil)
    to_emacs [:message, "Changing options..."] if !silent
    dict = {}; alist.each {|key,val| dict[key] = val }
    alist.each do |k,v|
      @async.config_set_value(k, v).notifier
    end;
    to_emacs [:message, "Changing options... DONE!"] if !silent
  end

  def eqgain
    @async.config_list_values.notifier do |x|
      y=x.to_a.keep_if {|k,v| k =~ /equalizer.gain/}
      to_emacs [:"gimme-eq-print",[:quote, y.map{|k,v| v.to_f}]]
    end
  end

  def eqchange (min, max, change)
    change = change.to_s.to_f # The sexp library translates -5 into a symbol :P
    @async.config_list_values.notifier do |x|
      y = Hash[x.to_a]
      (max+1-min).times do |t|
        key = ("equalizer.gain%02d" % (min+t))
        val = (y[key.to_sym].to_f + change)
        val = [[20.0,val].min,-20.0].max.to_s
        @async.config_set_value(key,val).notifier
      end
    end
    Thread.new do
      # There is a delay in the server, for some reason
      sleep 0.1
      eqgain
    end
  end


  ###################
  ### Collections ###
  ###################

  def append_coll (data)
    with_coll(data) do |coll|
      @async.coll_query_info(coll,["id"]).notifier do |adict|
        adict.each do |dict|
          add dict.to_a[0][1]
        end
        true; end; end; end

  def combine (op, colls, raw)
    if raw.empty?
      type = {"or" => Xmms::Collection::TYPE_UNION,
        "and" => Xmms::Collection::TYPE_INTERSECTION,
        "not" => Xmms::Collection::TYPE_COMPLEMENT}[op]
      combined = Xmms::Collection.new(type)
      colls.each { |coll| combined.operands.push(coll) }
      title = (colls.each.map { |c| Hash[c.attributes.to_a]["title"] }).join(" #{op} ")
      title = "not #{title}" if type == Xmms::Collection::TYPE_COMPLEMENT
      combined.attributes["title"] = title
      to_emacs [:"gimme-bookmark-add-child", [:quote, combined.to_a],
                [:quote, colls.first.to_a]]
      #pcol combined
    else
      with_coll(raw[0]) do |coll|
        combine(op, colls.to_a + [coll], raw[1..-1])
      end
    end

  end


  def supcol (child, faceted = nil)
    child =   Xmms::Collection.from_a(child).to_a
    if (child[0] == Xmms::Collection::TYPE_INTERSECTION)
      parent =  Xmms::Collection.from_a(child[2][0])
      faceted ? faceted_pcol(parent) : pcol(parent)
    else
      to_emacs [:message, "Couldn't find a parent collection!"]
    end
  end

  def subcol (data,pattern)
    with_coll(data) do |parent|
      match = Xmms::Collection.new(Xmms::Collection::TYPE_MATCH)
      match = Xmms::Collection.parse(pattern)
      intersection = Xmms::Collection.new(Xmms::Collection::TYPE_INTERSECTION)
      intersection.operands.push(parent)
      intersection.operands.push(match)
      intersection.attributes["title"] = pattern
      to_emacs [:"gimme-bookmark-add-child", [:quote, intersection.to_a],
                [:quote, parent.to_a]]
      pcol(intersection)
    end;end

  def pcol (data=nil)
    with_coll(data) do |coll|
      title = coll.attributes["title"] || data.to_s
      plist = [:quote, [:"gimme-buffer-type", :collection,
                        :"gimme-collection-name", data,
                        :"gimme-collection-title", title]]
      to_emacs [:"gimme-gen-buffer",plist]
      @async.coll_query_info(coll,$atribs).notifier do |wrapperdict|
        wrapperdict.each do |dict|
          adict = {}
          dict.each {|key,val| adict[key] = val.class == NilClass ? NOTHING : val}
          to_emacs [:"gimme-insert-song",plist,[:quote, adict.to_a.flatten],:t]
        end
        coll_overview(data)
        true;end;end; end

  def rcol (old, new)
    @async.coll_rename(old,new,Xmms::Collection::NS_COLLECTIONS).notifier {|res|}
  end

  def savecol (data,name)
    with_coll(data) do |coll|
      @async.coll_save(coll,name,Xmms::Collection::NS_COLLECTIONS)
      true
    end
  end

  def colls (session)
    @async.coll_list.notifier do |res|
      to_emacs [:"gimme-bookmark-colls",session,[:quote, res.to_a]]
    end
  end

  #######################
  ### Faceted Viewing ###
  #######################

  def faceted_pcol (data=nil, facet=nil)
    with_coll(data) do |coll|
      title = coll.attributes["title"] || data.to_s
      candidates = facet ? [facet] : ["album","artist","genre"]
      count = Hash[]
      candidates.each {|f| count[f] = Hash[]}
      @async.coll_query_info(coll,$atribs).notifier do |wrapperdict|
        wrapperdict.each do |dict|
          adict = {}
          dict.each {|key,val| adict[key] = val.class == NilClass ? NOTHING : val}
          candidates.each do |f|
            key = adict[f.to_sym]
            count[f][key] = 0 if !count[f][key]
            count[f][key] = count[f][key]+1
          end
        end

        # Either receives a facet or tries to guess a good one
        chosen = count.min_by{|_,v| v.length == 1 ? 100000000 : (30-v.length).abs}
        count = chosen[1]; chosen = chosen[0]
        plist = [:quote, [:"gimme-buffer-type", :collection,
                          :"gimme-collection-name", data,
                          :"gimme-collection-facet", chosen,
                          :"gimme-collection-title", title]]
        to_emacs [:"gimme-gen-buffer",plist]

        # Regroup
        count = count.to_a.sort
        count.each do |k,v|
          to_emacs [:"gimme-faceted-insert-group", plist, k,v]
        end
        coll_overview(data)
        true;end;end;end

  def faceted_subcol (data,pattern)
    with_coll(data) do |parent|
      match = Xmms::Collection.new(Xmms::Collection::TYPE_MATCH)
      match = Xmms::Collection.parse(pattern)
      intersection = Xmms::Collection.new(Xmms::Collection::TYPE_INTERSECTION)
      intersection.operands.push(parent)
      intersection.operands.push(match)
      intersection.attributes["title"] = pattern
      to_emacs [:"gimme-bookmark-add-child", [:quote, intersection.to_a],
                [:quote, parent.to_a]]
      faceted_pcol(intersection,nil)
    end;end

  def subcol_change_tags (data, pattern, key, val)
    with_coll(data) do |parent|
      pattern="#{key}:'#{pattern}'"
      match = Xmms::Collection.new(Xmms::Collection::TYPE_MATCH)
      match = Xmms::Collection.parse(pattern)
      intersection = Xmms::Collection.new(Xmms::Collection::TYPE_INTERSECTION)
      intersection.operands.push(parent)
      intersection.operands.push(match)
      @async.coll_query_ids(intersection).notifier do |ids|
        to_emacs [:"message", "Changing over 1000 tracks. Please wait for a couple of seconds..."] if ids.count > 1000
        ids.each { |id| @async.medialib_entry_property_set(id, key.to_sym, val).notifier }
        faceted_pcol(parent,key)
      end
    end;end

  def append_subcol (data,pattern)
    with_coll(data) do |parent|
      match = Xmms::Collection.new(Xmms::Collection::TYPE_MATCH)
      match = Xmms::Collection.parse(pattern)
      intersection = Xmms::Collection.new(Xmms::Collection::TYPE_INTERSECTION)
      intersection.operands.push(parent)
      intersection.operands.push(match)
      intersection.attributes["title"] = pattern
      to_emacs [:"gimme-bookmark-add-child", [:quote, intersection.to_a],
                [:quote, parent.to_a]]
      append_coll(intersection)

    end;end

  ###############################
  ### Augmented mode and such ###
  ###############################

  def fetch_lyrics (plist)
    Thread.new do
      dict = Hash[plist.collect_every(2)]
      # FIXME: Isn't it better to leave to the lib the method
      # selection?  Apparently so, but what if the track depends on
      # its context to be identified?
      if dict[:artist] == "Johann Sebastian Bach"
        bwv = dict[:title].gsub(/[^0-9]/,"")
        lyrics = Crawlyr.get_bachcantata(bwv)
      else
        lyrics = Crawlyr.get_lyrics(dict)
      end
      plist = plist + [:source,lyrics[1]]
      lyrics = lyrics[0]
      to_emacs [:"gimme-augmented-lyrics-display", [:quote, plist], lyrics.encode('UTF-8')]
    end
  end

  def get_influencees (artist)
    Thread.new do
      to_emacs [:"message", "Querying Freebase... [0/1]"]
      query = [{"type"=>"/music/artist", "name"=>[], "/influence/influence_node/influenced_by"=> [{"type"=>"/music/artist", "name"=> artist}]}]

      req = Freebase.mqlread query
      artists = req["result"].map {|x| x["name"][0]}
      if artists.empty?
      then
        to_emacs [:"message", "Querying Freebase... Nothing found!"]
      else
        coll =  Xmms::Collection.new(Xmms::Collection::TYPE_UNION)
        artists.each do |artist|
          match = Xmms::Collection.new(Xmms::Collection::TYPE_MATCH)
          match = Xmms::Collection.parse("artist:'#{artist}'")
          coll.operands.push match
        end
        coll.attributes["title"] = "#{artist}'s influencees #{artists.to_s}"
        faceted_pcol(coll,"artist")
      end
    end
  end

  def get_influences (artist)
    Thread.new do
      to_emacs [:"message", "Querying Freebase... [0/1]"]
      query = [{"type"=>"/music/artist", "name"=>[], "/influence/influence_node/influenced"=> [{"type"=>"/music/artist", "name"=> artist}]}]

      req = Freebase.mqlread query
      artists = req["result"].map {|x| x["name"][0]}
      if artists.empty?
      then
        to_emacs [:"message", "Querying Freebase... Nothing found!"]
      else
        to_emacs [:"message", "Querying Freebase... OK!"]
        coll =  Xmms::Collection.new(Xmms::Collection::TYPE_UNION)
        artists.each do |artist|
          match = Xmms::Collection.new(Xmms::Collection::TYPE_MATCH)
          match = Xmms::Collection.parse("artist:'#{artist}'")
          coll.operands.push match
        end
        coll.attributes["title"] = "#{artist}'s influences #{artists.to_s}"
        faceted_pcol(coll,"artist")
      end
    end
  end

  def get_similar (artist)
    # Tecnically, I'm just looking for artists with the same,
    # according to Freebase, genre. However there are so many silly
    # subgenres, that this should in practice work to get a very broad
    # cluster of songs.
    Thread.new do
      to_emacs [:"message", "Querying Freebase... [0/1]"]
      query = [{"type"=>"/music/artist", "name"=>[], "/music/artist/genre"=>[{"type"=>"/music/genre", "id"=>[], "artists"=>[{"name"=>artist}]}]}]

      req = Freebase.mqlread query
      artists = req["result"].map {|x| x["name"][0]}
      if artists.empty?
      then
        to_emacs [:"message", "Querying Freebase... Nothing found!"]
      else
        to_emacs [:"message", "Querying Freebase... OK!"]
        coll =  Xmms::Collection.new(Xmms::Collection::TYPE_UNION)
        artists.each do |artist|
          match = Xmms::Collection.new(Xmms::Collection::TYPE_MATCH)
          match = Xmms::Collection.parse("artist:'#{artist}'")
          coll.operands.push match
        end
        coll.attributes["title"] = "Similar to #{artist}"
        faceted_pcol(coll,"artist")
      end
    end
  end

  def get_artist_info (artist)
    Thread.new do
      to_emacs [:"message", "Querying Freebase... [0/1]"]
      query = {"type"=>"/music/artist", "name"=>artist, "/common/topic/article"=>[{"text"=>{"maxlength"=>16384, "chars"=>nil}}]}

      res = Freebase.mqlread(query)["result"]
      if !(res) # FIXME: Check why it's not getting just an empty list
      then
        to_emacs [:"message", "Querying Freebase... Nothing found!"]
      else
        to_emacs [:"message", "Querying Freebase... OK!"]
        text = res["/common/topic/article"][0]["text"]["chars"]
        to_emacs [:"gimme-augmented-show-info", text, artist]
      end
    end
  end


  ############
  ### Help ###
  ############

  def get_help_info
    Thread.new do
      url = "http://gimmeplayer.org/wiki/Special:Ask/-5B-5BCategory:Song-20properties-5D-5D/-3FName/-3FDescription/-3FExample/-3FDisplay-20function/mainlabel%3D/format%3Djson"
      agent = Mechanize.new
      page = agent.get(url)
      j = JSON page.body
      to_emacs [:"gimme-help-set-properties", [:"quote", j["items"].map {|x| x.to_a.flatten}]]
    end
  end

  ########################
  ### Misc and Private ###
  ########################

  def set_atribs (l); $atribs |= l.map{|n| n.to_s}; end

  private

  def to_emacs (array)
    # Because this is a feature, not a bug!
    # http://www.gnu.org/s/emacs/manual/html_node/elisp/Non_002dASCII-in-Strings.html
    puts array.to_sexp.gsub(/(\\x[0-9A-F][0-9A-F])([0-9A-Fa-f])/,'\1\\\\ \2')
  end

  def with_coll (data)
    @async.coll_get(data.to_s).notifier do |coll|
      if data == nil or data == :nil
        coll = Xmms::Collection.universe
        coll.attributes["title"] = "All Media"
      elsif data.class == Array
        coll = Xmms::Collection.from_a(data)
      elsif data.class == Xmms::Collection
        coll = data
      end
      yield coll
    end;end

end



$ml = GLib::MainLoop.new(nil, false)
$channel = GLib::IOChannel.new(STDIN)
client = GIMME.new

Thread.new do; while true
                 p = gets
                 if (RUBY_VERSION =~ /1.8/)
                 then
                   p = p.parse_sexp.first
                 else
                   # FIXME FIXME FIXME
                   # The lib doesn't play well with accented chars :(
                   # Tired of wasting hours on this, so I'm saving their unicode position
                   # Will fix it when preparing GIMME for Ruby 1.9
                   p = p.strip.scan(/./).map {|x| x.ord > 127 ? "fix#{x.ord}me" : x}.join
                   p = p.parse_sexp.first
                   p = p.rmap2(lambda {|x| x.class == String ? x.gsub(/fix[0-9][0-9][0-9]me/) {|m| m.gsub(/[a-z]/,"").to_i.chr(Encoding::UTF_8)} : x})
                 end
                 client.send(*p) if (p.class == Array && client.respond_to?(p.first))
               end;end

$ml.run
