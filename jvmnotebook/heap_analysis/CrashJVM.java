/**
 * The JVM has a set amount of system memory available defined by the maximum heap size.
 * A class to help analyze when garbage collection occurs, also when garbage collection
 * will fail and generate out of memory errors.

 * Date: 12/21/2008
 * @author Berlin Brown
 */
public class CrashJVM {

	private static final Runtime runtime = Runtime.getRuntime();
	
	private String data_small1 = "BerlinBrown Test, this is a small string (1)";
	private String data_small2 = "BerlinBrown Test, this is a small string (2)";
	private String data_small3 = "BerlinBrown Test, this is a small string (3)";

	private String data_append = "BerlinBrownTest";

	private long s_time = 0;
	private long e_time = 0;

	public String toString() {
		return data_small1 + data_small2;
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

	public CrashJVM test_SmallString1() throws Exception {

		final int size = 1000;
		stime();
		printMemoryStats("test small string, begin");
		for(int i = 0; i < size; i++) {
			this.data_append += data_small1;
		}
		
		etime();
		printMemoryStats("test small string, done");
		return this;

	}
	
	public CrashJVM test_SmallString_silent() throws Exception {

		final int size = 1000;
		for(int i = 0; i < size; i++) {
			this.data_append += data_small1 + "Random Str";
		}	   
		return this;

	}

	public CrashJVM [] test_SmallObjects1() throws Exception {
		stime();
		printMemoryStats("test small objects, begin");
		final int size = 100000;
		CrashJVM [] items = new CrashJVM[size];
		for(int i = 0; i < size; i++) {
			items[i] = new CrashJVM();
		}
		Thread.sleep(100);
		printMemoryStats("test small objects, end");
		for(int i = 0; i < size; i++) {
			//System.out.println(items[i]);
		}
		etime();
		printMemoryStats("null");
		return items;
	}
	
	/**
	 * Delay for around 20 seconds, hoping garbage collections.
	 */
	public static void waitForGarbageCollection(CrashJVM printer) throws Exception {
		for (int i = 0; i < 20; i++) {
			Thread.sleep(500);
			printer.printMemoryStats("wait for garbage collection, iter=" + i);
		}
		
	}

	public static void main(final String [] args) throws Exception {
		System.out.println("Running");	   
		Thread.sleep(20000);
		CrashJVM test1 = new CrashJVM();
		CrashJVM [] tmp1 = test1.test_SmallObjects1();
		CrashJVM [] tmp2 = test1.test_SmallObjects1();
		CrashJVM [] tmp3 = test1.test_SmallObjects1();
		// Pretend that we need to reference this array
		// Hopefully fool the garbage collector
		tmp1[0].toString();
		Thread.sleep(200);
		
		tmp3[0].test_SmallString1();
		for (int j = 0; j < 10;j++) {
			tmp1[j].test_SmallString_silent();
		}

		waitForGarbageCollection(tmp3[0]);
		
		Thread.sleep(2000);
		System.out.println("size of objs=" + tmp3.length);
		tmp1[0].printMemoryStats("End of program");
		Thread.sleep(20000);
		System.out.println("Done");
	}

}
