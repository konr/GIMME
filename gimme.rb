require 'xmmsclient'
require 'xmmsclient_glib'
require 'glib2'

def message text
  # TODO: ESCAPE text
  "(message \"#{text}\")"
end

class GIMME

  def initialize
    begin
      @async = Xmms::Client.new('GIMMEasync').connect(ENV['XMMS_PATH'])
    rescue Xmms::Client::ClientError
      puts 'Failed to connect to XMMS2 daemon.'
      puts 'Please make sure xmms2d is running and using the correct IPC path.'
      exit
    end
    @async.add_to_glib_mainloop
    #@async.playback_current_id.notifier(&method(:on_playback_cur_id))
  end

  def ainfo
    @async.playback_current_id.notifier do |id|
      if(id == 0)
        puts 'There is no current ID. XMMS2 is probably not playing anything.'
      else
        puts "Currently playing ID: #{id}"
      end
    end
  end

  def play
    @async.playback_start.notifier do |id|
      puts message "Now playing."
    end
  end

  def pause
    @async.playback_pause.notifier do |id|
      puts message "Now paused."
    end
  end

  def test
    puts '(message "Hello, Cleveland!")'
  end
end

$ml = GLib::MainLoop.new(nil, false)
client = GIMME.new

Thread.new do
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
