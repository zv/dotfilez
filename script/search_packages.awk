# Search our list of installable packages by 'tag'

BEGIN { FS="|" }
# skip until header (a line of ------------) is done
NR == 1, /---+/ { next }
$2 ~ tag { print $1 }

