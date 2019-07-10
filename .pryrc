puts 'Loading ~/.pryrc'

# Awesomeprint replaces pry's default pretty print
begin
  require 'awesome_print'
rescue LoadError
  puts 'Please install awesome_print'
else
  AwesomePrint.pry!
end

def clear
  system('clear')
end

if ENV['RAILS_ENV'] || defined?(Rails)
  # Print ActiveRecord SQL to the screen
  ActiveRecord::Base.logger = Logger.new(STDOUT)

  # Custom prompt for Rails Console
  red     = "\033[0;31m"
  yellow  = "\033[0;33m"
  blue    = "\033[0;34m"
  default = "\033[0;39m"

  color = Rails.env =~ /production/ ? red : blue
  Pry.config.prompt_name = "#{yellow}#{File.basename Rails.root}#{default} - #{color}#{Rails.env}#{default}"
end
