#!/usr/bin/ruby

require 'glib2'
require 'yaml'

require 'xmmsclient'
require 'xmmsclient/async'
require 'xmmsclient_glib'


$0 = "xmms2-weighted-shuffle-client"

def debug(*arg)
#  puts(*arg)
end

module WeightedShuffle

  # {{{ class Config
  class Config

    CONF_PATH = Xmms.userconfdir + "/clients/WeightedShuffle.yaml"

    # {{{ Default Playlist Consts
    DEFAULT_PLAYLIST_CONF = {
      "colls" => [
                  { "name" => "1-rated", "expr" => "rating:*1", "mult" => 1 },
                  { "name" => "2-rated", "expr" => "rating:*2", "mult" => 2 },
                  { "name" => "3-rated", "expr" => "rating:*3", "mult" => 3 },
                  { "name" => "4-rated", "expr" => "rating:*4", "mult" => 4 },
                  { "name" => "5-rated", "expr" => "rating:*5", "mult" => 5 },
                  { "name" => "not-rated", "expr" => "NOT +rating", "mult" => 2 }
                 ],
      "history" => 3,
      "upcoming" => 18,
    }

    DEFAULT_PLAYLIST_NAME = "weighted_shuffee_playlist"
    # }}}

    # {{{ class Playlist
    class Playlist
      attr_reader :conf, :colls, :name, :history, :upcoming


      # {{{ def initialize
      def initialize(name,playlist_conf)
        @conf = DEFAULT_PLAYLIST_CONF.merge(playlist_conf)
        @conf["playlist"] ||= name

        @colls = conf["colls"]
        debug("collections:\n #{colls.to_yaml}")
        @name = conf["playlist"]
        debug("playlist: #{name}")
        @history = conf["history"]
        debug("history: #{history}")
        @upcoming = conf["upcoming"]
        debug("upcoming: #{upcoming}")
      end
      # }}}
    end
    # }}}

    # {{{ def initialize
    def initialize
      @mtime = File.mtime(CONF_PATH)
      begin
        config_file=YAML.load_file(CONF_PATH)
      rescue Errno::ENOENT => x
        config_file={ DEFAULT_PLAYLIST_NAME => DEFAULT_PLAYLIST_CONF }
        File.open(CONF_PATH, 'w') do |out|
          YAML.dump(DEFAULT_CONF,out)
        end
      end

      @playlists = { }

      config_file.each_pair { |name,config| @playlists[name] = Playlist.new(name, config) }
    end
    # }}}

    def newer_conf?
      @mtime < File.mtime(CONF_PATH)
    end

    def each(&body)
      @playlists.each(&body)
    end

    def [] name
      @playlists[name]
    end
  end
  # }}}

  # {{{ class Playlists
  class Playlists
    # {{{ def initialize(xc, config)
    def initialize(xc, config)
      @xc = xc
      @config = config
      @pos = 0
      @length = 0
      @adding = false
      @removing = false
      @name = @config.name

      @colls = []

      @config.colls.each do |v|
        add_coll v
      end

      @playlist = @xc.playlist(@name)
    end
    # }}}

    # {{{ def add_coll
    def add_coll v
      if v["expr"] then
        coll=Xmms::Collection.parse(v["expr"])
        load_coll(v["name"], coll, v["mult"])
      else
        @xc.coll_get(v["name"]) do |coll|
          if(coll.is_a?(Xmms::Collection)) then
            load_coll(v["name"], coll, v["mult"])
          else
            puts "Problem with collection #{v["name"]}"
            puts "Please make sure it exists."
            exit
          end
          true
        end
      end
    end
    # }}}

    # {{{ def load_coll
    def load_coll(name,coll,mult)
      @xc.coll_query_ids(coll) do |ids_list|
        if ids_list then
          @colls.push({:name => name, :coll => coll, :mult => mult, :size => ids_list.length})
        else
          puts "Problem with collection #{name}"
          puts "Please make sure it exists, or that its expression is correct"
          exit
        end
        false
      end
    end
    # }}}

    # {{{ def initialize_playlist
    def initialize_playlist
      update_length

      @playlist.current_pos do |cur|
        set_pos cur[:position] if cur[:name] == @name
        true
      end
    end
    # }}}

    # {{{ def update_length
    def update_length
      @playlist.entries do |entries|
        set_length entries.length
        true
      end
    end
    # }}}

    # {{{ def set_length
    def set_length new_length
      debug "set_length #{new_length}"
      @length = new_length
      may_add_song
    end
    # }}}

    # {{{ def set_pos
    def set_pos new_pos
      debug "set_pos #{new_pos}"
      @pos = new_pos || 0
      may_add_song
      may_remove_song
    end
    # }}}

    # {{{ def rand_colls
    def rand_colls
      # look for the total number
      max = @colls.inject(0) do |acc,coll|
        acc + coll[:mult] * coll[:size]
      end
      num = rand(max)
      coll = @colls.find do |coll|
        num = num - coll[:mult] * coll[:size]
        num < 0
      end
      return coll
    end
    # }}}

    # {{{ def rand_song(&block)
    def rand_song(&block)
      coll = rand_colls()
      debug "song from #{coll[:name]}"
      num = rand(coll[:size])
      @xc.coll_query_ids(coll[:coll], ["id"], num, 1, &block)
    end
    # }}}

    # {{{ def may_add_song
    def may_add_song
      debug "adding: #{@adding}, cur pos: #{@pos}, cur length: #{@length}"
      unless @adding or @length - @pos + 1 >= @config.upcoming
        @adding = true
        rand_song do |ids|
          unless ids.empty?
            debug "will add #{ids[0]}"
            @playlist.add_entry(ids[0]) do |res|
              debug "#{ids[0]} added"
              @adding = false
              true
            end
          else
            @adding = false
          end
          true
        end
      end
    end
    # }}}

    # {{{ def may_remove_song
    def may_remove_song
      if not @removing and @pos > @config.history then
        debug "will remove"
        @removing = true
        @playlist.remove_entry(0) do |res|
          debug "has removed"
          @removing = false
          may_remove_song       # pos is updated before deletion is confirmed,
                                # so we have to check if the pos is still a problem
          false
        end
      end
    end
    # }}}
  end
  # }}}

  # {{{ class Client
  class Client
    # {{{ def initialize
    def initialize
      srand
      begin
        @xc = Xmms::Client::Async.new('WeightedShuffle').connect(ENV['XMMS_PATH'])
      rescue Xmms::Client::ClientError
        puts 'Failed to connect to XMMS2 daemon.'
        puts 'Please make sure xmms2d is running and using the correct IPC path.'
        exit
      end

      @xc.on_disconnect do
        exit(0)
      end

      @xc.broadcast_quit do |res|
        exit(0)
      end

      @xc.add_to_glib_mainloop
      @ml = GLib::MainLoop.new(nil, false)

      read_config()

      @xc.playback_status do |res|
        # Here all stage 1 for colls are done
        @xc.playback_status do |res|
          # here all stage 2 for colls are done,
          # and stage 3 will be done before the callback of the next command
          @xc.broadcast_playlist_current_pos do |cur|
            cur_list = @playlists[cur[:name]]
            cur_list.set_pos(cur[:position]) if cur_list
            true
          end

          @xc.broadcast_playlist_changed do |cur|
            cur_list = @playlists[cur[:name]]
            cur_list.update_length if cur_list
            true
          end
          true
        end
        true
      end
    end
    # }}}

    # {{{ def read_config()
    def read_config()
      @config = Config.new()
      @playlists = {}
      @config.each { |id,conf| @playlists[ conf.name ] = Playlists.new(@xc, conf) }
      @xc.playback_status do |res|
        #Here all stage 1 for colls are done
        @xc.playback_status do |res|
          #here all stage 2 for colls are done, and stage 3 will be done before the callback of the next command
          @playlists.each do |n,list|
            list.initialize_playlist
            true
          end
          true
        end
        true
      end
    end
    # }}}

    def may_reread_config()
      read_config if @config.newer_conf?
    end

    def run()
      @ml.run
    end
  end
  # }}}

  Client.new.run()
end

#File layout controlled by Emacs folding.el available at:
#Latest folding is available at http://cvs.xemacs.org/viewcvs.cgi/XEmacs/packages/xemacs-packages/text-modes/
