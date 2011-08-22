require 'rubygems'
require 'mechanize'
require 'uri'
require 'cgi'
require 'json'

module Freebase

  def Freebase.mqlread (input)
    extended = {"extended" => 1, "query" => input}
    json = extended.to_json
    escaped = CGI.escape(json)
    url = "http://api.freebase.com/api/service/mqlread?query=#{escaped}"

    agent = Mechanize.new
    res = agent.get(url).content

    output = JSON res

    return output
  end
end

