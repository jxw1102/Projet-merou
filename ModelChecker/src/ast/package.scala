package object ast {
    /**
     * Implicit declaration that enables to split the 'data' field of the ConcreteASTNode(s)
     */
    implicit class DataProcessor(data: String) {
        val dataList = DataProcessor.splitReg.findAllIn(data).map(_.replaceAll("'", "")).toSeq
    }
    
    implicit object DataProcessor {
        private val splitReg = "<[^>]+>|('[^']+'|[^\\s']+)+".r
    }
    
    implicit class SeqFetcher[T](seq: Seq[T]) {
        def get(idx: Int) = if (idx >= 0) seq(idx) else seq(seq.length + idx)
    }
}