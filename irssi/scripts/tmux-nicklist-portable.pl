# based on the nicklist.pl script
################################################################################
#                               tmux_nicklist.pl                               
# This script integrates tmux and irssi to display a list of nicks in a       
# vertical right pane with 20% width. Right now theres no configuration
# or setup, simply initialize the script with irssi and by default you
# will get the nicklist for every channel(customize by altering 
# the regex in /set nicklist_channel_re)
#
# /set nicklist_channel_re <regex>
# * only show on channels matching this regular expression
#
# /set nicklist_max_users <num>
# * only show when the channel has so many users or less (0 = always)
#
# /set nicklist_smallest_main <num>
# * only show when main window is larger than this (0 = always)
#
# /set nicklist_pane_width <num>
# * width of the nicklist pane
#
# /set nicklist_color <ON|OFF>
# * colourise the nicks in the nicklist (required nickcolor script
#   with get_nick_color2 and debug_ansicolour functions)
# 
#
# It supports mouse scrolling and the following keys:
# k/up arrow: up one line
# j/down arrow: down one line
# pageup: up 20 lines
# pagedown: down 20 lines
# gg: go to top
# G: go to bottom
#
# For better integration, unrecognized sequences will be sent to irssi and
# its pane will be focused.
################################################################################

use strict;
use warnings;
use IO::Handle;
use IO::Select;
use POSIX;
use File::Temp qw/ :mktemp  /;
use File::Basename;
our $VERSION = '0.1.2'; # e945fd92c94e4ab
our %IRSSI = (
  authors     => 'Thiago de Arruda',
  contact     => 'tpadilha84@gmail.com',
  name        => 'tmux-nicklist',
  description => 'displays a list of nicks in a separate tmux pane',
  license     => 'WTFPL',
);

# "other" prefixes by danielg4 <daniel@gimpelevich.san-francisco.ca.us>

{ package Irssi::Nick }

if ($#ARGV == -1) {
require Irssi;

my $enabled = 0;
my $script_path = __FILE__;
my $tmpdir;
my $fifo_path;
my $fifo;
my $just_launched;
my $resize_timer;

sub enable_nicklist {
  return if ($enabled);
  $tmpdir = mkdtemp Irssi::get_irssi_dir()."/nicklist-XXXXXXXX";
  $fifo_path = "$tmpdir/fifo";
  POSIX::mkfifo($fifo_path, 0600) or die "can't mkfifo $fifo_path: $!";
  my $cmd = "perl $script_path $fifo_path $ENV{TMUX_PANE}";
  my $width = Irssi::settings_get_int('nicklist_pane_width');
  system('tmux', 'split-window', '-dh', '-l', $width, '-t', $ENV{TMUX_PANE}, $cmd);
  open_fifo();
  Irssi::timeout_remove($just_launched) if defined $just_launched;
  $just_launched = Irssi::timeout_add_once(300, sub { $just_launched = undef; }, '');
}

sub open_fifo {
  # The next system call will block until the other pane has opened the pipe
  # for reading, so synchronization is not an issue here.
  open $fifo, ">", $fifo_path or do {
    if ($! == 4) {
      Irssi::timeout_add_once(300, \&open_fifo, '');
      $enabled = -1 unless $enabled;
      return;
    }
    die "can't open $fifo_path: $!";
  };
  $fifo->autoflush(1);
  if ($enabled < -1) {
    $enabled = 1;
    disable_nicklist();
  } elsif ($enabled == -1) {
    $enabled = 1;
    reset_nicklist();
  } else {
    $enabled = 1;
  }
}

sub disable_nicklist {
  return unless ($enabled);
  if ($enabled > 0) {
    print $fifo "EXIT\n";
    close $fifo;
    $fifo = undef;
    unlink $fifo_path;
    rmdir $tmpdir;
  }
  $enabled--;
}

sub reset_nicklist {
  my $active = Irssi::active_win();
  my $channel = $active->{active};
  my ($colourer, $ansifier);
  if (Irssi::settings_get_bool('nicklist_color')) {
    for my $script (sort map { my $z = $_; $z =~ s/::$//; $z } grep { /^nickcolor|nm/ } keys %Irssi::Script::) {
      if ($colourer = "Irssi::Script::$script"->can('get_nick_color2')) {
        $ansifier = "Irssi::Script::$script"->can('debug_ansicolour');
        last;
      }
    }
  }
  my $channel_pattern = Irssi::settings_get_str('nicklist_channel_re');
  { local $@;
    $channel_pattern = eval { qr/$channel_pattern/ };
    $channel_pattern = qr/(?!)/ if $@;
  }

  if (!$channel || !ref($channel)
      || !$channel->isa('Irssi::Channel')
      || !$channel->{'names_got'}
      || $channel->{'name'} !~ $channel_pattern) {
    disable_nicklist;
  } else {
    my %colour;
    my @nicks = $channel->nicks();
    my $max_nicks = Irssi::settings_get_int('nicklist_max_users');
    if ($max_nicks && @nicks > $max_nicks) {
      disable_nicklist;
    } else {
      enable_nicklist;
      return unless $enabled > 0;
      foreach my $nick (sort { $a->{_irssi} <=> $b->{_irssi} } @nicks) {
        $colour{$nick->{nick}} = ($ansifier && $colourer) ? $ansifier->($colourer->($active->{active}{server}{tag}, $channel->{name}, $nick->{nick}, 0)) : '';
      }
      print($fifo "BEGIN\n");
      foreach my $nick (sort {(($a->{'op'}?'1':$a->{'halfop'}?'2':$a->{'voice'}?'3':$a->{'other'}>32?'0':'4').lc($a->{'nick'}))
        cmp (($b->{'op'}?'1':$b->{'halfop'}?'2':$b->{'voice'}?'3':$b->{'other'}>32?'0':'4').lc($b->{'nick'}))} @nicks) {
        my $colour = $colour{$nick->{nick}} || "\e[39m";
	$colour = "\e[37m" if $nick->{'gone'};
        print($fifo "NICK");
        if ($nick->{'op'}) {
          print($fifo "\e[32m\@$colour$nick->{'nick'}\e[39m");
        } elsif ($nick->{'halfop'}) {
          print($fifo "\e[34m%$colour$nick->{'nick'}\e[39m");
        } elsif ($nick->{'voice'}) {
          print($fifo "\e[33m+$colour$nick->{'nick'}\e[39m");
        } elsif ($nick->{'other'}>32) {
          print($fifo "\e[31m".(chr $nick->{'other'})."$colour$nick->{'nick'}\e[39m");
        } else {
          print($fifo " $colour$nick->{'nick'}\e[39m");
        }
        print($fifo "\n");
      }
      print($fifo "END\n");
    }
  }
}

sub switch_channel {
  print $fifo "SWITCH_CHANNEL\n" if $fifo;
  reset_nicklist;
}

sub resized_timed {
  Irssi::timeout_remove($resize_timer) if defined $resize_timer;
  return if defined $just_launched;
  $resize_timer = Irssi::timeout_add_once(1100, \&resized, '');
  #resized();
}
sub resized {
  $resize_timer = undef;
  return if defined $just_launched;
  return unless $enabled > 0;
  disable_nicklist;
  Irssi::timeout_add_once(200, \&reset_nicklist, '');
}

Irssi::settings_add_str('tmux_nicklist', 'nicklist_channel_re', '.*');
Irssi::settings_add_int('tmux_nicklist', 'nicklist_max_users', 0);
Irssi::settings_add_int('tmux_nicklist', 'nicklist_smallest_main', 0);
Irssi::settings_add_int('tmux_nicklist', 'nicklist_pane_width', 13);
Irssi::settings_add_bool('tmux_nicklist', 'nicklist_color', 1);
Irssi::signal_add_last('window item changed', \&switch_channel);
Irssi::signal_add_last('window changed', \&switch_channel);
Irssi::signal_add_last('channel joined', \&switch_channel);
Irssi::signal_add('nicklist new', \&reset_nicklist);
Irssi::signal_add('nicklist remove', \&reset_nicklist);
Irssi::signal_add('nicklist changed', \&reset_nicklist);
Irssi::signal_add_first('nick mode changed', \&reset_nicklist);
Irssi::signal_add('gui exit', \&disable_nicklist);
Irssi::signal_add_last('terminal resized', \&resized_timed);

} else {
my $fifo_path = $ARGV[0];
my $irssi_pane = $ARGV[1];
# array to store the current channel nicknames
my @nicknames = ();

# helper functions for manipulating the terminal
# escape sequences taken from
# http://www.tldp.org/HOWTO/Bash-Prompt-HOWTO/x361.html
sub enable_mouse { print "\e[?1000h"; }
# recognized sequences
my $MOUSE_SCROLL_DOWN="\e[Ma";
my $MOUSE_SCROLL_UP="\e[M`";
my $ARROW_DOWN="\e[B";
my $ARROW_UP="\e[A";
my $UP="k";
my $DOWN="j";
my $PAGE_DOWN="\e[6~";
my $PAGE_UP="\e[5~";
my $GO_TOP="gg";
my $GO_BOTTOM="G";

my $current_line = 0;
my $sequence = '';
my ($rows, $cols);

sub term_size {
  split ' ', `stty size`;
}

sub redraw {
  my $last_nick_idx = @nicknames;
  my $last_idx = $current_line + $rows;
  # normalize last visible index
  if ($last_idx > ($last_nick_idx)) {
    $last_idx = $last_nick_idx;
  }
  # redraw visible nicks
  for my $i (reverse 1..$rows) {
    print "\e[$i;1H\e[K";
    my $idx = $current_line + $i - 1;
    if ($idx < $last_idx) {
      my $z = 0; my $col = $cols;
      for (split /(\e\[(?:\d|;|:|\?|\s)*.)/, $nicknames[$idx]) {
	if ($z ^= 1) {
          print +(substr $_, 0, $col) if $col > 0;
	  $col -= length;
	} else {
	  print
	}
      }
    }
  }
}

sub move_down {
  $sequence = '';
  my $count = int $_[0];
  my $nickcount = $#nicknames;
  return if ($nickcount <= $rows);
  if ($count == -1) {
    $current_line = $nickcount - $rows - 1;
    redraw;
    return;
  }
  my $visible = $nickcount - $current_line - $count;
  if ($visible > $rows) {
    $current_line += $count;
    redraw;
  } elsif (($visible + $count) > $rows) {
    # scroll the maximum we can
    $current_line = $nickcount - $rows - 1;
    redraw;
  }
}

sub move_up {
  $sequence = '';
  my $count = int $_[0];
  if ($count == -1) {
    $current_line = 0;
    redraw;
    return;
  }
  return if ($current_line == 0);
  $count = 1 if $count == 0;
  $current_line -= $count;
  $current_line = 0 if $current_line < 0;
  redraw;
}

$SIG{INT} = 'IGNORE';

STDOUT->autoflush(1);
# setup terminal so we can listen for individual key presses without echo
`stty -icanon -echo`;

# open named pipe and setup the 'select' wrapper object for listening on both
# fds(fifo and sdtin)
open my $fifo, "<", $fifo_path or die "can't open $fifo_path: $!";
my $select = IO::Select->new();
my @ready;
$select->add($fifo);
$select->add(\*STDIN);

enable_mouse;
system('tput', 'smcup');
print "\e[?7l"; #system('tput', 'rmam');
system('tput', 'civis');
MAIN: {
  while (@ready = $select->can_read) {
    foreach my $fd (@ready) {
      ($rows, $cols) = term_size;
      if ($fd == $fifo) {
        while (<$fifo>) {
          my $line = $_;
          if ($line =~ /^BEGIN/) {
            @nicknames = ();
          } elsif ($line =~ /^SWITCH_CHANNEL/) {
            $current_line = 0;
          } elsif ($line =~ /^NICK(.+)$/) {
            push @nicknames, $1;
          } elsif ($line =~ /^END$/) {
            redraw;
            last; 
          } elsif ($line =~ /^EXIT$/) {
            last MAIN;
          }
        }
      } else {
        my $key = '';
        sysread(STDIN, $key, 1);
        $sequence .= $key;
        if ($MOUSE_SCROLL_DOWN =~ /^\Q$sequence\E/) {
          if ($MOUSE_SCROLL_DOWN eq $sequence) {
            move_down 3; 
            # mouse scroll has two more bytes that I dont use here
            # so consume them now to avoid sending unwanted bytes to
            # irssi
            sysread(STDIN, $key, 2);
          }
        } elsif ($MOUSE_SCROLL_UP =~ /^\Q$sequence\E/) {
          if ($MOUSE_SCROLL_UP eq $sequence) {
            move_up 3; 
            sysread(STDIN, $key, 2);
          }
        } elsif ($ARROW_DOWN =~ /^\Q$sequence\E/) {
          move_down 1 if ($ARROW_DOWN eq $sequence);
        } elsif ($ARROW_UP =~ /^\Q$sequence\E/) {
          move_up 1 if ($ARROW_UP eq $sequence);
        } elsif ($DOWN =~ /^\Q$sequence\E/) {
          move_down 1 if ($DOWN eq $sequence);
        } elsif ($UP =~ /^\Q$sequence\E/) {
          move_up 1 if ($UP eq $sequence);
        } elsif ($PAGE_DOWN =~ /^\Q$sequence\E/) {
          move_down $rows/2 if ($PAGE_DOWN eq $sequence);
        } elsif ($PAGE_UP =~ /^\Q$sequence\E/) {
          move_up $rows/2 if ($PAGE_UP eq $sequence);
        } elsif ($GO_BOTTOM =~ /^\Q$sequence\E/) {
          move_down -1 if ($GO_BOTTOM eq $sequence);
        } elsif ($GO_TOP =~ /^\Q$sequence\E/) {
          move_up -1 if ($GO_TOP eq $sequence);
        } else {
          # Unrecognized sequences will be send to irssi and its pane
          # will be focused
          system('tmux', 'send-keys', '-t', $irssi_pane, $sequence);
          system('tmux', 'select-pane', '-t', $irssi_pane);
          $sequence = '';
        }
      }
    }
  }
}

close $fifo;

}
