def download(address)
{
    //def file = new FileOutputStream(address.tokenize("/")[-1])
    def file = System.out
    def out = new BufferedOutputStream(file)
    out << new URL(address).openStream()
    out.flush()    
    out.close()   
}

System.properties.putAll( )


download("http://www.botnode.com")
