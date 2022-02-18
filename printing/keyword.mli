open Source_parsing

val decorate
  :  Document.t
  -> extension:string Location.loc option
  -> Source_tree.attributes
  -> later:Document.t
  -> Document.t
