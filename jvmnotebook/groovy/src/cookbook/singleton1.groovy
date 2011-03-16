class VoteCollector {
    def votes = 0
    private static final INSTANCE = new VoteCollector()
    static getInstance(){ return INSTANCE }
    private VoteCollector() {}
    def display() { println "Collector:${hashCode()}, Votes:$votes" }
}

def collector = VoteCollector.instance
collector.display()
collector.votes++
collector = null

Thread.start{
    def collector2 = VoteCollector.instance
    collector2.display()
    collector2.votes++
    collector2 = null
}.join()

def collector3 = VoteCollector.instance
collector3.display()