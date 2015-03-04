package object ast {
    /**
     * enables to split the 'data' field of the ConcreteASTNode(s)
     * */
    implicit class DataProcessor(data: String) {
        val dataList = DataProcessor.splitReg.findAllIn(data).map(_.replaceAll("'", "")).toSeq
    }
    
    /**
     * separates a data string into several fragments
     * */
    implicit object DataProcessor {
        private val splitReg = "<[^>]+>+|\\(.+\\)|(\"[^\"]+\")|('[^']+'|[^\\s']+)+".r
    }
    
    /**
     * enables negative indexes for look-up on a Seq
     * */
    implicit class SeqFetcher[T](seq: Seq[T]) {
        def get(idx: Int) = if (idx >= 0) seq(idx) else seq(seq.length + idx)
    }
    
}