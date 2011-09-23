#  <konr> I'm writing a client that adds an 'md5' property to tracks on mlib and,
#         for every missing file, verifies if there is an existing track with its
#         md5, concluding that it was just moved, updating the tags and deleting
#         the old entry. Any suggestion/request?  [21:25]
#  *** nesciens (~nesciens@ip82-139-88-64.lijbrandt.net) has quit: Ping timeout:
#      252 seconds  [21:33]
#  <CareBear\> konr : watch out for metadata tag editors  [21:41]
#  *** nesciens (~nesciens@ip82-139-88-64.lijbrandt.net) has joined channel
#      #xmms2  [21:43]
#  <nesciens> That is, what happens to properties set on the song for the old
#             entry, when it is "moved"? For instance it might have cover art
#             added by xmms2-covers?
#  <nesciens> is it only local files?
#  <nesciens> ignore headers?
#  <nesciens> I think Amarok has something like this, might be worth reading
#           their website on it.



dir=File.dirname(__FILE__)
$LOAD_PATH << dir

['cgi', 'xmmsclient', 'glib2', 'xmmsclient_glib','digest/md5'].each do |lib|
  begin; require lib
  rescue LoadError; warn "Oops! Something didn't go right. I bet that '#{lib}' is missing."; exit; end
end

module Xmms
  class Fixer
    def initialize
      @atribs = ["md5","url","id"]
      begin; @sync = Xmms::Client.new('Fixer').connect(ENV['XMMS_PATH'])
      rescue; puts "Failed to connect to the core. Make sure xmms2d is running"; end
      @sync.add_to_glib_mainloop
    end

    def gettracks
      coll = Xmms::Collection.universe
      wrapperdict = @sync.coll_query_info(coll,@atribs).wait
      tracks = []
      wrapperdict.value.each do |rawdict|
        dict = {}; rawdict.each { |key,val| dict[key] = val }
        if dict[:url] =~ /^file/; then; dict[:url] = dict[:url] [7..-1]; tracks += [dict]; end
      end
      tracks; end

    def genmd5 (tracks)
      dict = {}; dict[nil] = []
      length = tracks.length; last = -1; i = 0
      tracks.each do |track|
        i += 1; cur = i * 100 / length; print "#{cur}%... " if cur > last; last = cur
        f = CGI.unescape(track[:url])
        if File.exists?(f)
        then
          if track[:md5]; then; digest=track[:md5]
          else
            digest = Digest::MD5.hexdigest(File.read(f))
            @sync.medialib_entry_property_set(track[:id], :md5, digest).wait
          end
          dict[digest] = track[:id]
        else
          dict[nil] = dict[nil] + [track]
        end
      end
      puts
      dict; end

    def recover (dict)
      atribs = ['album','artist','genre','timesplayed','title','tracknr','starred']
      pairs = []; recoverable = non_recoverable = 0
      unrec = []
      dict[nil].each do |track|
        if track[:md5]
        then; pairs += [[track[:id], dict[track[:md5]]]]  if dict[track[:md5]]; recoverable += 1
        else; unrec += [track[:id]]; non_recoverable += 1;
        end
      end
      puts "#{recoverable} recoverable and #{non_recoverable} non-recoverable tracks."
      print "Recoving tracks." if recoverable > 0
      pairs.each do |pair|
        res = Hash[@sync.medialib_get_info(pair[0]).wait.value.to_a]
        atribs.each do |atrib|
          val = res[atrib.to_sym]
          @sync.medialib_entry_property_set(pair[1], atrib.to_sym, val.first.to_s).wait if val
        end
        @sync.medialib_entry_remove(pair[0]).wait
        print "."
      end
      puts
      unrec; end

    def delete_track (id)
      @sync.medialib_entry_remove(id).wait
      print "."
    end
  end
end

$ml = GLib::MainLoop.new(nil, false)
$channel = GLib::IOChannel.new(STDIN)
client = Xmms::Fixer.new


if ARGV[0] == "-u"; then
  puts "Getting media information..."
  media = client.gettracks
  puts "Calculating MD5s..."
  media = client.genmd5(media)
  puts "Recovering lost files"
  media = client.recover(media)
elsif ARGV[0] == "-p"; then
  media = client.gettracks
  media.map! {|track| CGI.unescape(track[:url])}
  media.delete_if {|f| File.exists?(f)}
  media.each {|url| puts url}
elsif ARGV[0] == "-d"; then
  media = client.gettracks
  media.delete_if {|t| File.exists?(CGI.unescape(t[:url]))}
  print "Deleting #{media.length} tracks from Mlib."
  media.each {|t| client.delete_track t[:id]}
  puts
else
  puts "./fix-mlib.rb <-u|-p|-d>"
  [["u","Updates the collection by calculating MD5s and recovering moved filed."],
   ["p","Prints unrecoverable files"],
   ["d","Removes unrecoverable files"]].each {|k,v| puts "\t#{k}\t#{v}"}
end



