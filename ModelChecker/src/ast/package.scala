package object ast {
    /**
     * Implicit declaration that enables to split the 'data' field of the ConcreteASTNode(s)
     */
    implicit class DataProcessor(data: String) {
        val dataList = DataProcessor.splitReg.findAllIn(data).map(_.replaceAll("'", "")).toSeq
    }
    
    implicit object DataProcessor {
        private val splitReg = "(\\'.+?\\'|\\S+)".r
    }
    
    implicit class SeqFetcher[T](seq: Seq[T]) {
        def get(idx: Int) = if (idx >= 0) seq(idx) else seq(seq.length + idx)
    }
    
    implicit class FindSome(a: Option[SourceCodeNode]) {
        def or(b: Option[SourceCodeNode]) = (a,b) match {
            case (Some(_),Some(_)) => a
            case (Some(_),None)    => a
            case (None,Some(_))    => b
            case (None,None)       => None
        }
        def or(b: SourceCodeNode) = (a,b) match {
            case (Some(_),_) => a
            case (None,_)    => Some(b)
        }
    }
    
}