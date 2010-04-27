require 'xmmsclient'
require 'xmmsclient_glib'
require 'glib2'

# Temporary Silly name
# I also have to deal with errors and stuff, this is a pre-alpha sketch :)
class GimmeRuby
  attr_accessor :xmms

  def initialize
    begin
      @xmms = Xmms::Client.new('GIMMEsync').connect(ENV['XMMS_PATH'])
    rescue Xmms::Client::ClientError
      puts 'Failed to connect to XMMS2 daemon.'
      puts 'Please make sure xmms2d is running and using the correct IPC path.'
      exit
    end
    @xmmsa = Xmms::Client.new('GIMMEasync').connect(ENV['XMMS_PATH'])
    @xmmsa.add_to_glib_mainloop
    @xmmsa.playback_current_id.notifier(&method(:on_playback_cur_id))
  end

  def play
    res = @xmms.playback_start
    res.wait
  end

  def pause
    res = @xmms.playback_pause
    res.wait
  end

  def info
    id=@xmms.playback_current_id.wait.value
    @xmms.medialib_get_info(id).wait.value.each do |src,keyval|
      keyval.each_pair { |key, val| puts "[#{src}] #{key} = #{val}" }
    end
  end

  def on_playback_cur_id(id)
    return if id.zero?
    ainfo
  end

  def ainfo
    @xmmsa.playback_current_id.notifier do |id|
      if(id == 0)
        puts 'There is no current ID. XMMS2 is probably not playing anything.'
      else
        puts "Currently playing ID: #{id}"
      end
    end
  end

  def ls
    @xmms.playlist.entries.wait.value.each_with_index do |id, index|
      if(id.zero?)
        puts "#{index + 1} Invalid ID!"
        next
      end
      begin
        puts "#{index + 1}. #{@xmms.medialib_get_info(id).wait.value[:url][:server]}"
      rescue Xmms::Result::ValueError
        puts "There was an error retrieving mediainfo for ID: #{id}."
      end
    end
  rescue Xmms::Result::ValueError
    puts 'There was an error retrieving mediainfo for a playlist entry.'
  end

  def colls
    @xmms.coll_list("Collections").wait.value.each do |key|
      puts "- #{key}"
    end
  end

  def coll(name)
    coll = @xmms.coll_get(name).wait.value
    # This seems to be a bug: "Failed in file ../src/lib/xmmstypes/value.c on  row 303"
    @xmms.coll_query_info(coll,["artist","title"]).wait.value.each do |info|
      puts "- #{info[:artist]} > #{info[:title]}"
    end
  end
end


$ml = GLib::MainLoop.new(nil, false)
gr = GimmeRuby.new


if __FILE__ == $0
  Thread.new do
    while true
      # Buggy :(
      print "\n> "
      gets.strip.each do | command |
        if gr.respond_to?(command.split.first)
          gr.send(*command.split)
        else
          puts ["wat", "does not compute", "can you explain with oranges?"][rand(3)]
        end
      end
    end
  end
end

$ml.run


# Todo:
# - Deal with errors

# - Fix'd tutorial examples
# - I didn't work with a large sample
