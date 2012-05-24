# Dotfiles of the Owned and the Lamest

# Just another dotfile repository.

```
git clone https://github.com/skwp/dotfiles ~/.yadr
cd ~/.yadr && rake install
```

### Use pry

  * as irb: `pry`
  * as rails console: `script/console --irb=pry`

### Pry Customizations:

 * `clear` command to clear screen
 * `sql` command to execute something (within a rails console)
 * all objects displayed in readable format (colorized, sorted hash keys) - via awesome_print
 * a few color modifications to make it more useable
 * type `help` to see all the commands


## Git
### Git Customizations:

  * `git l` - a much more usable git log
  * `git b` - a list of branches with summary of last commit
  * `git r` - a list of remotes with info
  * `git t` - a list of tags with info
  * `git nb` - a (n)ew (b)ranch - like checkout -b
  * `git cp` - cherry-pick -x (showing what was cherrypicked)
  * `git changelog` - a nice format for creating changelogs
  * Some sensible default configs, such as improving merge messages, push only pushes the current branch, removing status hints, and using mnemonic prefixes in diff: (i)ndex, (w)ork tree, (c)ommit and (o)bject
  * Slightly improved colors for diff
  * `git unstage` (remove from index) and `git uncommit` (revert to the time prior to the last commit - dangerous if already pushed) aliases

## RubyGems

A .gemrc is included. Never again type `gem install whatever --no-ri --no-rdoc`. `--no-ri --no-rdoc` is done by default.


## Vim Configuration

The .vimrc is well commented and broken up by settings. I encourage you
to take a look and learn some of my handy aliases, or comment them out
if you don't like them, or make your own.


### Vim Keymaps

The files in vim/plugin/settings are customizations stored on a per-plugin
basis. The main keymap is available in skwp-keymap.vim, but some of the vim
files contain key mappings as well (TODO: probably will move them out to skwp-keymap.vim)

#### Navigation

 * `,z` - go to previous buffer (:bp)
 * `,x` - go to next buffer (:bn)
 * `Cmd-j` and `Cmd-k` to move up and down roughly by functions
 * `Ctrl-o` - Old cursor position - this is a standard mapping but very useful, so included here
 * `Ctrl-i` - opposite of Ctrl-O (again, this is standard)

#### LustyJuggler

 * `,lj` - show buffers (LustyJuggler buffer search), just type to fuzzy match a buffer name
 * `,lf` - file system browser

#### Rails

 * `,ss` to run specs, `,ll` to run a given spec on a line - using my [vim-ruby-conque plugin](https://github.com/skwp/vim-ruby-conque)
 * `Cmd-Shift-R` to use vim-ruby-conque to run a spec file. `Cmd-Shift-L` to run from a line (individual it block), `,Cmd-Shift-R` to rerun the last run command (great for re-running specs)

#### Surround.vim customizations

 * in plugin/settings/surround.vim (this folder contains all my customizations)
 * the `#` key now surrounds with `#{}`, so `ysaw#` (surround around word) `#{foo}`
 * `=` surrounds with `<%= erb tag %>`; `-` for `<% this %>`. So, `yss=` or `yss-` to wrap code

#### Search/Code Navigation

 * `,f` - instantly Find definition of class (must have exuberant ctags installed)
 * `,F` - same as ,f but in a vertical split
 * `,gf` - same as vim normal gf (go to file), but in a vertical split
 * `K` - GitGrep the current word under the cursor and show results in quickfix window
 * `,K` - GitGrep the current word up to next exclamation point (useful for ruby foo! methods)
 * `Cmd-*` - highlight all occurrences of current word (similar to regular `*` except doesn't move)
 * `,hl` - toggle search highlight on and off
 * `,gg` - GitGrep command line, type between quotes
 * `,gd` - GitGrep def (greps for 'def [function name]') when cursor is over the function name
 * `,gcp` - GitGrep Current Partial to find references to the current view partial
 * `,gcf` - GitGrep Current File to find references to the current file
 * `//` - clear the search
 * `,q/` -  quickfix window with last search (stolen from Steve Losh)
 * `,qa/` - quickfix Ack last search (Steve Losh)
 * `,qg/` - quickfix GitGrep last search
 * `,T` - Tag list (list of methods in a class)
 * `Ctrl-s` - Open related spec in a split. Similar to :A and :AV from rails.vim but is also aware of the fast_spec dir and faster to type

#### File Navigation

 * `,t` - CtrlP fuzzy file selector
 * `,b` - CtrlP buffer selector
 * `,m` - jump to method - CtrlP tag search within current buffer
 * `,M` - jump to any Method - CtrlP tag search within all buffers
 * `,jm` jump (via CtrlP) to app/models
 * `,jc` app/controllers
 * `,jv` app/views
 * `,jh` app/helpers
 * `,jl` lib
 * `,jp` public
 * `,js` spec
 * `,jf` fast_spec
 * `,jt` test
 * `,jd` db
 * `,jC` config
 * `,jV` vendor
 * `,jF` factories
 * `Cmd-Shift-P` - Clear CtrlP cache
 * `:Bopen [gem name]` to navigate to a gem (@tpope/vim-bundler)

#### RSI-reduction

 * `Cmd-k` and `Cmd-d` to type underscores and dashes (use Shift), since they are so common in code but so far away from home row
 * `Cmd-k` and `Cmd-d` to type underscores and dashes (use Shift), since they are so common in code but so far away from home row
 * `Ctrl-l` to insert a => hashrocket (thanks @garybernhardt)
 * `,.` to go to last edit location (same as `'.`) because the apostrophe is hard on the pinky
 * `Cmd-'` and `Cmd-"` to change content inside quotes
 * Cmd-Space to autocomplete. Tab for snipmate snippets.
 * `,ci` to change inside any set of quotes/brackets/etc
 * `,#` `,"` `,'` `,]` `,)` `,}` to surround a word in these common wrappers. the # does #{ruby interpolation}. works in visual mode (thanks @cj)

#### Tab Navigation

 * `Ctrl-H` and `Ctrl-L` - left an right on tabs
 * Use `Cmd-1` thru `Cmd-9` to switch to a specific tab number (like iTerm) - and tabs have been set up to show numbers

#### Window Navigation

 * `Ctrl-h,l,j,k` - to move left, right, down, up between windows
 * `Q` - Intelligent Window Killer. Close window `wincmd c` if there are multiple windows to same buffer, or kill the buffer `bwipeout` if this is the last window into it.
 * Cmd-Arrow keys - resize windows (up/down for vertical, left=make smaller horizontally, right=make bigger horizontally)

#### Splits

 * `vv` - vertical split (`Ctrl-w,v`)
 * `ss` - horizontal split (`Ctrl-w,s`)
 * `,qo` - open quickfix window (this is where output from GitGrep goes)
 * `,qc` - close quickfix
 * `,gz` - zoom a window to max size and again to unzoom it (ZoomWin plugin, usually `C-w,o`)

#### NERDTree Project Tree

 * `Cmd-Shift-N` - NERDTree toggle
 * `Ctrl-\` - Show current file tree

#### Utility

 * `,ig` - toggle visual indentation guides
 * `,cf` - Copy Filename of current file (full path) into system (not vi) paste buffer
 * `,cn` - Copy Filename of current file (name only, no path)
 * `,vc` - (Vim Command) copies the command under your cursor and executes it in vim. Great for testing single line changes to vimrc.
 * `,vr` - (Vim Reload) source current file as a vim file
 * `,yw` - yank a word from anywhere within the word (so you don't have to go to the beginning of it)
 * `,ow` - overwrite a word with whatever is in your yank buffer - you can be anywhere on the word. saves having to visually select it
 * `,ocf` - open changed files (stolen from @garybernhardt). open all files with git changes in splits
 * `,w` - strip trailing whitespaces
 * `sj` - split a line such as a hash {:foo => {:bar => :baz}} into a multiline hash (j = down)
 * `sk` - unsplit a link (k = up)
 * `,he` - Html Escape
 * `,hu` - Html Unescape
 * `Cmd-Shift-A` - align things (type a character/expression to align by, works in visual mode or by itself)
 * `:ColorToggle` - turn on #abc123 color highlighting (useful for css)
 * `:gitv` - Git log browsers
 * `,hi` - show current Highlight group. if you don't like the color of something, use this, then use `hi! link [groupname] [anothergroupname]` in your vimrc.after to remap the color. You can see available colors using `:hi`

#### Comments

 * `Cmd-/` - toggle comments (usually gcc from tComment)
 * `gcp` (comment a paragraph)

 **Wrapping**

 * :Wrap - wrap long lines (e.g. when editing markdown files).
 * Cmd-[j, k, $, 0, ^] - navigate display lines.

### Included vim plugins

#### Navigation

 * NERDTree - everyone's favorite tree browser
 * NERDTree-tabs - makes NERDTree play nice with MacVim tabs so that it's on every tab
 * ShowMarks - creates a visual gutter to the left of the number column showing you your marks
 * EasyMotion - hit ,,w (forward) or ,,b (back) and watch the magic happen. just type the letters and jump directly to your target - in the provided vimrc the keys are optimized for home and upper row, no pinkies
 * LustyJuggler/Explorer - hit B, type buf name to match a buffer, or type S and use the home row keys to select a buffer
 * TagBar - hit ,T to see a list of methods in a class (uses ctags)
 * CtrlP - Ctrl-p or ,t to find a file

#### Git

 * fugitive - "a git wrapper so awesome, it should be illegal...". Try Gstatus and hit `-` to toggle files. Git `d` to see a diff. Learn more: http://vimcasts.org/blog/2011/05/the-fugitive-series/
 * gitv - use :gitv for a better git log browser
 * GitGrep - much better than the grep provided with fugitive; use :GitGrep or hit K to grep current word

#### Colors

 * AnsiEsc - inteprets ansi color codes inside log files. great for looking at Rails logs
 * solarized - a color scheme scientifically calibrated for awesomeness (including skwp mods for ShowMarks)
 * csapprox - helps colors to be represented correctly on terminals (even though we expect to use MacVim)
 * Powerline - beautiful vim status bar. Requires patched fonts (install from fonts/ directory)

#### Coding

 * tComment - gcc to comment a line, gcp to comment blocks, nuff said
 * sparkup - div.foo#bar - hit `ctrl-e`, expands into `<div class="foo" id="bar"/>`, and that's just the beginning
 * rails.vim - syntax highlighting, gf (goto file) enhancements, and lots more. should be required for any rails dev
 * rake.vim - like rails.vim but for non-rails projects. makes `:Rtags` and other commands just work
 * ruby.vim - lots of general enhancements for ruby dev
 * necomplcache - intelligent and fast complete as you type, and added Command-Space to select a completion (same as Ctrl-N)
 * snipMate - offers textmate-like snippet expansion + scrooloose-snippets . try hitting TAB after typing a snippet
 * jasmine.vim - support for jasmine javascript unit testing, including snippets for it, before, etc..
 * vim-coffeescript - support for coffeescript, highlighting
 * vim-stylus - support for stylus css language
 * vim-bundler - work with bundled gems

#### TextObjects

 The things in this section provide new "objects" to work with your standard verbs such as yank/delete/change/=(codeformat), etc

 * textobj-rubyblock - ruby blocks become vim textobjects denoted with `r`. try var/vir to select a ruby block, dar/dir for delete car/cir for change, =ar/=ir for formatting, etc
 * vim-indentobject - manipulate chunks of code by indentation level (great for yaml) use vai/vii to select around an indent block, same as above applies
 * argtextobj - manipulation of function arguments as an "a" object, so vaa/via, caa/cia, daa/dia, etc..
 * textobj-datetime - gives you `da` (date), `df` (date full) and so on text objects. useable with all standard verbs
 * vim-textobj-entire - gives you `e` for entire document. so vae (visual around entire document), and etc
 * vim-textobj-rubysymbol - gives you `:` textobj. so va: to select a ruby symbol. da: to delete a symbol..etc
 * vim-textobj-function - gives you `f` textobj. so vaf to select a function
 * next-textobject - from Steve Losh, ability to use `n` such as vinb (visual inside (n)ext set of parens)

#### Utils

 * SplitJoin - easily split up things like ruby hashes into multiple lines or join them back together. Try :SplitjoinJoin and :SplitjoinSplit or use the bindings sj(split) and sk(unsplit) - mnemonically j and k are directions down and up
 * tabularize - align code effortlessly by using :Tabularize /[character] to align by a character, or try the keymaps
 * yankring - effortless sanity for pasting. every time you yank something it goes into a buffer. after hitting p to paste, use ctrl-p or ctrl-n to cycle through the paste options. great for when you accidentally overwrite your yank with a delete
 * surround - super easy quote and tag manipulation - ysiw" - sourround inner word with quotes. ci"' - change inner double quotes to single quotes, etc
 * greplace - use :Gsearch to find across many files, replace inside the changes, then :Greplace to do a replace across all matches
 * ConqueTerm - embedded fully colorful shell inside your vim
 * vim-ruby-conque - helpers to run ruby,rspec,rake within ConqueTerm
 * vim-markdown-preview - :Mm to view your README.md as html
 * html-escape - ,he and ,hu to escape and unescape html
 * ruby-debug-ide - not quite working for me, but maybe it will for you. supposedly a graphical debugger you can step through
 * Gundo - visualize your undos - pretty amazing plugin. Hit ,u with my keymappings to trigger it, very user friendly
 * slime - use ctrl-c,ctrl-c to send text to a running irb/pry/console. To start the console, you must use screen with a named session: "screen -S [name] [cmd]", ex: "screen -S pry pry"
 * vim-indent-guides - visual indent guides, off by default
 * color_highlight - use :ColorCodes to see hex colors highlighted
 * change-inside-surroundings - change content inside delimiters like quotes/brackets
 * Specky - used for color highlighting rspec correctly even if specs live outside of spec/ (rails.vim doesn't handle this)

#### General enhancements that don't add new commands

 * Arpeggio - allows you to define key-chord combinations
 * IndexedSearch - when you do searches will show you "Match 2 of 4" in the status line
 * delimitMate - automatically closes quotes
 * syntastic - automatic syntax checking when you save the file
 * repeat - adds `.` (repeat command) support for complex commands like surround.vim. i.e. if you perform a surround and hit `.`, it will Just Work (vim by default will only repeat the last piece of the complex command)
 * endwise - automatically closes blocks (if/end)
 * autotag - automatically creates tags for fast sourcecode browsing. use ctrl-[ over a symbol name to go to its definition
 * matchit - helps with matching brackets, improves other plugins
 * sass-status - decorates your status bar with full nesting of where you are in the sass file


### Overriding vim settings

You may use `~/.vimrc.before` for settings like the __leader__ setting. You may `~/.vimrc.after` for any additional overrides/settings.

### Credits

First, I'd like to thank the state of Colorado for being boring enough for me to spend years in front of a computer rather than do something
worthwhile with my youth.
Secondly I'd like to thank Janus for being who actually put this together.
