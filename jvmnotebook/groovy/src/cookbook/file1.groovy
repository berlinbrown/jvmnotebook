// Cookbook example - recursively print files
// Create a ref for closure
def simplClos

// Define the closure
simplClos = {
  println "Dir ${it.canonicalPath}";
  it.eachDir( simplClos);
  it.eachFile {
    println "File ${it.canonicalPath}";
  }
}

// Apply the closure
simplClos(new File("C:/"))

