NR == 1, /---+/ { next } # skip until header is done
file ~ $1 {
    dst=$3
    sub(/%skip/, "", dst)
    sub(/%filename/, file, dst)
    print dst
    exit
}
