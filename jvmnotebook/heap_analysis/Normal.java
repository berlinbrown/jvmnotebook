/**
 * The JVM has a set amount of system memory available defined by the maximum heap size.
 * A class to help analyze when garbage collection occurs, also when garbage collection
 * will fail and generate out of memory errors.

 * Date: 12/21/2008
 * @author Berlin Brown
 */
public class Normal {

	private static final Runtime runtime = Runtime.getRuntime();
	
	private String data_small1 = "BerlinBrown Test, this is a small string (1)";

	private long s_time = 0;
	private long e_time = 0;

	public String toString() {
		return data_small1;
	}
	
	public void stime() {
		s_time = System.currentTimeMillis();
	}
	public void etime() {
		e_time = System.currentTimeMillis();
	}
	public void printDiffTime() {
		long diff = (e_time - s_time);
		if (diff > 0) {
			System.out.println("\n* Time Diff in ms, t=" + diff);
		}
	}
			
	public void printMemoryStats(final String header) {
		
		System.out.println("************************");
		System.out.println("* Memory Stats");
		System.out.println("## " + header);
		System.out.println("************************");

		final double mb_size = 1024.0 * 1024.0;

		final long freeMemory = runtime.freeMemory();
		final long maxMemory = runtime.maxMemory();
		final long totalMemory = runtime.totalMemory();

		final double freeMemory_mb= (double)freeMemory / mb_size;
		final double maxMemory_mb = (double)maxMemory / mb_size;
		final double totalMemory_mb = (double)totalMemory / mb_size;
		
		System.out.println(" Free Memory = " + freeMemory);
		System.out.println(" Max Memory = " + maxMemory);
		System.out.println(" Total Memory = " + totalMemory);

		System.out.println(" Free Memory(mb) = " + freeMemory_mb);
		System.out.println(" Max Memory(mb) = " + maxMemory_mb);
		System.out.println(" Total Memory(mb) = " + totalMemory_mb);
		printDiffTime();
	}

	public static void waitForGarbageCollection(Normal printer) throws Exception {
		for (int i = 0; i < 80; i++) {
			Thread.sleep(500);
			printer.printMemoryStats("wait for garbage collection, iter=" + i);
		}
		
	}

	public static void main(final String [] args) throws Exception {
		System.out.println("Running Normal");	   
		Thread.sleep(10000);
		Normal test1 = new Normal();
		waitForGarbageCollection(test1);
		Thread.sleep(2000);
		test1.printMemoryStats("End of program");
		Thread.sleep(20000);
		System.out.println("Done");
	}

}
