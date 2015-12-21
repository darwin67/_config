# Awesomeprint replaces pry's default pretty print
begin
  require 'awesome_print'
rescue LoadError
  puts 'Please install awesome_print'
else
  AwesomePrint.pry!
end

if ENV['RAILS_ENV'] == 'development' || ENV['RAILS_ENV'] == 'test'
  ActiveRecord::Base.logger = ActiveSupport::Logger.new(STDOUT)
end
