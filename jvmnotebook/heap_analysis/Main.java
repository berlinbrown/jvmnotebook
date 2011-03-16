import server.BetterReportBean;


public class Main {

	public static void main(String [] args) throws Exception {

		System.out.println("Running");
		BetterReportBean bean [] = new BetterReportBean[3000];
		for (int i = 0; i < 3000; i++) {		
			bean[i] = new BetterReportBean();
		}
		Thread.sleep(2000);
		for (int i = 0; i < 3000; i++) {		
			bean[i].toString();
		}

		System.out.println("Done");

	} 
	

}
