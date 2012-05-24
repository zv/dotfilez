require 'rake'

desc "Hook our dotfiles into system-standard positions."
task :install => [:submodules] do
  puts "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
  puts "Ladies and Gentlemen, Step Right up, it's that time again"
  puts "                      Thats right"
  puts "  It's time for DOTFILEZ OF THE OWNED AND THE LAMEST"
  puts "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
  linkables = []
  # This is elite, as you can see.
  linkables += Dir.glob('{vim,vimrc}') 
  linkables += Dir.glob('aprc')
  linkables += Dir.glob('gemrc')
  linkables += Dir.glob('gitconfig')
  linkables += Dir.glob('id_rsa.pub')
  linkables += Dir.glob('zshrc')
  linkables += Dir.glob('erlang')
  linkables += Dir.glob('gitignore')
  linkables += Dir.glob('id_rsa.gpg')
  linkables += Dir.glob('pryrc')

  linkables.each do |linkable|
    file = linkable.split('/').last
    if File.exists?(file) || File.symlink?(file)
      source = "#{ENV["PWD"]}/#{linkable}"
      target = "#{ENV["HOME"]}/.#{file}"

      puts "***************************"
      puts "file:   #{file}"
      puts "source: #{source}"
      puts "target: #{target}"
      run %{ mv "$HOME/.#{file}" "$HOME/.#{file}.backup" }
      FileUtils.rm_rf(target) # sorry 
      run %{ ln -s "#{source}" "#{target}" }
    end
  end

  run %{ mkdir ~/.ssh }
  run %{ mv ~/.id_rsa.gpg ~/.ssh/id_rsa.gpg }
  run %{ mv ~/.id_rsa.pub ~/.ssh/id_rsa.pub }
  run %{ gpg ~/.ssh/id_rsa.gpg }
  success_msg("whoa, totally radical!")
end

desc 'wut'
task :submodules do
  sh('git submodule update --init')
end

task :default => 'install'

private

def run(cmd)
  puts
  puts "[Installing] #{cmd}"
  `#{cmd}` unless ENV['DEBUG']
end

def success_msg()
  puts ""
  puts "Feel the now!" 
  puts "Shits alright!"
end
