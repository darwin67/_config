# Auto-complete for method names and such
require 'irb/completion'

# Awesomeprint replaces irb's default pretty printing with fancier formatting
begin
  require 'awesome_print'
rescue LoadError
  puts 'Please install awesome_print'
else
  AwesomePrint.irb!
end
# Loads simple IRB (without RVM notice)
IRB.conf[:PROMPT_MODE] = :SIMPLE

IRB.conf[:AUTO_INDENT] = true

# A method for clearing the screen
def clear
  system('clear')
end

puts 'Loading ~/.irbrc'
