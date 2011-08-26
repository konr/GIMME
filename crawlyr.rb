require 'rubygems'
require 'nokogiri'
require 'mechanize'
require 'sexp'
require 'cgi'

  def to_emacs (array)
    # Because this is a feature, not a bug!
    # http://www.gnu.org/s/emacs/manual/html_node/elisp/Non_002dASCII-in-Strings.html
    puts array.to_sexp.gsub(/(\\x[0-9A-F][0-9A-F])([0-9A-Fa-f])/,'\1\\\\ \2')
  end

module Crawlyr
  class Analyzer
    attr_accessor :freq, :text, :node, :html

    def initialize (node)
      freq = Hash[]; i = 1
      ('a'..'z').each {|c| freq[c] = 0}; ('A'..'Z').each {|c| freq[c] = 0}; ('0'..'9').each {|c| freq[c] = 0}

      node.inner_text.each_char do |x|
        if (x =~ /[A-Za-z0-9]/); then; freq[x] = freq[x]+1; i += 1; end; end
      freq.each { |k,v| freq[k] = v.to_f / i }

      self.freq = i == 1 ? nil : freq
      self.text = node.inner_text
      self.node = node
    end

    def distance (node)
      i = 0
      self.freq.each { |k,v| i += (v - node.freq[k]) ** 2}
      return i
    end
  end

  def Crawlyr.get_bachcantata (bwv, part=nil)
    begin
      url = "http://www.bach-cantatas.com/Texts/BWV#{bwv}-Eng3.htm"
      agent = Mechanize.new
      to_emacs [:message, "Fetching lyrics... [0/1]"]
      page = agent.get(url)
      to_emacs [:message, "Fetching lyrics... [1/1]"]
      page = Nokogiri::HTML(page.body, 'UTF-8')
      trs = page.css("table tr").to_a
      trs.delete_if {|x| !(x.to_s =~ /<b>/ && x.to_s =~ /<i>/)}
      trs = trs[2..-1]
      html = part ? trs[part+1] : trs.inject{|a,z| a.to_s + "<hr>" + z.to_s}
      return [html, url]
    rescue
      to_emacs [:message, "Fetching lyrics... ERROR!"]
    end; end

  def Crawlyr.get_lyricwiki (tags)
    to_emacs [:message, "Fetching lyrics... [0/3]"]
    agent = Mechanize.new
    page = agent.get('http://google.com/')
    to_emacs [:message, "Fetching lyrics... [1/3]"]

    google_form = page.form('f')
    string = "site:lyrics.wikia.com #{tags}"
    google_form.q = string

    page = agent.submit(google_form, google_form.buttons.first)
    to_emacs [:message, "Fetching lyrics... [2/3]"]

    page = Nokogiri::HTML(page.body, 'UTF-8')
    links = page.css('li a').to_a
    links.delete_if {|x| !(x['href'] =~ /^http:\/\/lyrics/)}

    if links.empty?
    then
      to_emacs [:message, "No matches :("]
      return nil

    end
    chosen = links[0]['href']
    page = Nokogiri::HTML(agent.get(chosen).body, 'UTF-8')
    to_emacs [:message, "Fetching lyrics... [3/3]"]

    text = page.css('div.lyricbox').to_a[0]
    page.css('div').each {|node| node.remove}
    text = Nokogiri::HTML(text.to_s.gsub("<br>","\n")).inner_text

    return text
  end


  def Crawlyr.get_lyrics (dict)
    # 0. Initialize
    terms = "\"#{dict[:artist]}\" \"#{dict[:title]}\""
    to_emacs [:message, "Fetching lyrics... [0/11]"]
    agent = Mechanize.new
    terms = CGI.escape("lyrics #{terms}")

    # 1. Get links
    page = agent.get("http://google.com/search?q=#{terms}")
    page = Nokogiri::HTML(page.body, 'UTF-8')
    links = page.css('h3.r a').to_a.map {|l| l['href']}
    to_emacs [:message, "Fetching lyrics... [1/11]"]

    # 2. Get all nodes and remove non-markup children
    threads = []; i = 2; nodes = []
    links.each do |l|
      threads << Thread.new do
        begin; page = agent.get(l); encoding = page.detected_encoding; page = page.body
        rescue; page = ""; encoding = "UTF-8"; end
        page = Nokogiri::HTML(page, encoding).css("*")
        to_emacs [:message, "Fetching lyrics... [#{i}/11]"]
        i += 1
        nodes = nodes + page.to_a
      end
    end
    threads.each {|t| t.join}
    nodes.each { |node| node.children.each {|x| x.remove if !(["b", "br", "text", "i"].include?(x.name))}}

    # 3. Do some preselection to make the algorithm faster
    nodes.delete_if do |node|
      # Removes 95% of the nodes
      (node.inner_text.length < 280) or \
      # SEO stuff, disclaimers, copyright laws etc
      (node.inner_text =~ /Music Videos Wizard/) or \
      (node.inner_text =~ /Search and Download/) or \
      (node.inner_text =~ /9610\/98/) or \
      # Short biographies stolen from wikipedia are sometimes popular
      (node.inner_text =~ /#{dict[:artist]}/)
    end

    # 4. Cross the data by comparing the character distribution and clusterize into a graph.
    nodes = nodes.map { |node| Analyzer.new(node)}.delete_if {|node| !node.freq}
    adj = Hash[]; fair = 0.001; n = nodes.length - 1
    (0..n).each do |i|; (0..n).each do |j|
        adj[[i,j]] = nodes[i].distance(nodes[j]) <= fair ? 1 : 0
      end; end

    # 5. Get the largest connected subgraph
    (0..n).each do |i|; (0..n).each do |j|
        (0..n).each {|k| adj[[i,k]] = 1 if adj[[j,k]] == 1} if (adj [[i,j]] == 1)
      end; end
    size = (0..n).map {|i| m=0; (0..n).each {|j| m += adj[[i,j]]}; [i,m]}.sort{|a,b| a[1] <= b[1] ? 1 : -1}
    max = size[0][1]
    group = size.delete_if{|x| x[1] != max}.map{|x| x[0]}

    # 6. Reconstruct the text. Let's trust Google and get the topmost result
    group.sort{|a,b| a[0] > b[0] ? 1 : -1}
    return [nodes[group[0]].node.to_s, links[group[0]]]
  end
end

