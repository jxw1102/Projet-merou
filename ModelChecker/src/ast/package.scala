/**
 * This package contains all the classes needed to model the Clang AST (well, only a small
 * part of it but it is sufficient to play with purely imperative C++).
 */
package object ast {
    /**
     * Enables to split the 'data' field of the ConcreteASTNode(s)
     * */
    implicit class DataProcessor(data: String) {
        val dataList = DataProcessor.splitReg.findAllIn(data).map(_.replaceAll("'", "")).toSeq
    }
    
    /**
     * Separates a data string into several fragments
     * */
    implicit object DataProcessor {
        private val splitReg = "<[^>]+>+|\\(.+\\)|(\"[^\"]+\")|('[^']+'|[^\\s']+)+".r
    }
    
    /**
     * Enables negative indexes for look-up on a Seq
     * */
    implicit class SeqFetcher[T](seq: Seq[T]) {
        def get(idx: Int) = if (idx >= 0) seq(idx) else seq(seq.length + idx)
    }
    
}