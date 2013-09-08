############################################
#  Node 
#############################################

eval "$(npm completion 2>/dev/null)"

function node-docs {
  open "http://nodejs.org/docs/$(node --version)/api/all.html#all_$1"
}
