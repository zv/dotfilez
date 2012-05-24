require 'rake'

desc "Hook our dotfiles into system-standard positions."
task :install => [:submodules] do
  puts "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
  puts "Ladies and Gentlemen, Step Right up, it's that time again"
  puts "                      Thats right"
  puts "  It's time for DOTFILEZ OF THE OWNED AND THE LAMEST"
  puts "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
  linkables = []
  linkables += Dir.glob('**/*') 

  linkables.each do |linkable|
    file = linkable.split('/').last
    source = "#{ENV["PWD"]}/#{linkable}"
    target = "#{ENV["HOME"]}/.#{file}"

    puts "EL8EL8EL8EL8EL8EL8EL8"
    puts "file:   #{file}"
    puts "source: #{source}"
    puts "target: #{target}"
    run %{ mv "$HOME/.#{file}" "$HOME/.#{file}.backup" }
    FileUtils.rm_rf(target) # sorry 
    run %{ ln -s "#{source}" "#{target}" }
  end
  success_msg("jumping jeepers nothing fucked up that badly")
end

task :submodules do
  sh('git submodule update --init')
end

task :default => 'install'

private

def success_msg()
  puts ""
  puts "Feel the now!" 
  puts "Shits alright!"
end
