package org.berlin.test;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;

public class Test2 extends JFrame {
	private static final long serialVersionUID = 1555304764686313742L;

	private JButton button;
	private JProgressBar progress;

	public Test2() {
		init();
	}

	private void init() {
		progress = new JProgressBar();		
		button = new JButton();
		button.setText("A button");

		button.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				printOK();
			}
		});

		getContentPane().add(progress, BorderLayout.CENTER);
		getContentPane().add(button, BorderLayout.PAGE_END);

		pack();
		setVisible(true);
	}

	public void printOK() {
		System.out.println("OK");
		progress.setMaximum(100);		
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				final LongAction la = new LongAction();				
				new Thread(la).start();
			}
		});
	}
	
	public class LongAction implements Runnable {
		@Override
		public void run() {
			// do something which take some time
			try {
				for (int i = 0; i < 100; i++) {
					Thread.sleep(50);
					progress.setValue(i);					
					System.out.println("Updating action..." + i);
				}
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		new Test2();
	}	
}


