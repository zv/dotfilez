if exists(":Tabularize")
  " pretty ridiculous but nessasary
  AddTabularPattern 1=  /^[^=]*\zs
  AddTabularPattern 1=> /^[^=>]*\zs
  AddTabularPattern 1:  /^[^:]*\zs
endif
