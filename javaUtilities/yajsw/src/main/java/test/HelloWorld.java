package test;

import java.awt.AWTException;
import java.awt.BorderLayout;
import java.awt.Image;
import java.awt.SystemTray;
import java.awt.Toolkit;
import java.awt.TrayIcon;
import java.awt.image.ImageProducer;
import java.awt.image.MemoryImageSource;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JLabel;

import org.apache.commons.collections.map.CaseInsensitiveMap;
import org.rzo.yajsw.app.WrapperJVMMain;

public class HelloWorld
{
	Map			m			= new CaseInsensitiveMap();
	static Map	outOfMem	= new HashMap();

	static class MyWriter implements Runnable
	{
		public void run()
		{
			Thread.currentThread().setName("writer");

			int i = 0;
			while (i < 10)
			{
				System.out.println(i++);
				try
				{
					Thread.sleep(100);
				}
				catch (InterruptedException e)
				{
					e.printStackTrace();
				}
			}
		}

	}

	public static void simulateDeadlock()
	{
		// These are the two resource objects we'll try to get locks for
		final Object resource1 = "resource1";
		final Object resource2 = "resource2";
		// Here's the first thread. It tries to lock resource1 then resource2
		Thread t1 = new Thread()
		{
			public void run()
			{
				Thread.currentThread().setName("simulate deadlock");

				// Lock resource 1
				synchronized (resource1)
				{
					System.out.println("Thread 1: locked resource 1");

					// Pause for a bit, simulating some file I/O or something.
					// Basically, we just want to give the other thread a chance
					// to
					// run. Threads and deadlock are asynchronous things, but
					// we're
					// trying to force deadlock to happen here...
					try
					{
						Thread.sleep(50);
					}
					catch (InterruptedException e)
					{
					}

					// Now wait 'till we can get a lock on resource 2
					synchronized (resource2)
					{
						System.out.println("Thread 1: locked resource 2");
					}
				}
			}
		};

		// Here's the second thread. It tries to lock resource2 then resource1
		Thread t2 = new Thread()
		{
			public void run()
			{
				Thread.currentThread().setName("simulate deadlock 2");
				// This thread locks resource 2 right away
				synchronized (resource2)
				{
					System.out.println("Thread 2: locked resource 2");

					// Then it pauses, for the same reason as the first thread
					// does
					try
					{
						Thread.sleep(50);
					}
					catch (InterruptedException e)
					{
					}

					// Then it tries to lock resource1. But wait! Thread 1
					// locked
					// resource1, and won't release it 'till it gets a lock on
					// resource2. This thread holds the lock on resource2, and
					// won't
					// release it 'till it gets resource1. We're at an impasse.
					// Neither
					// thread can run, and the program freezes up.
					synchronized (resource1)
					{
						System.out.println("Thread 2: locked resource 1");
					}
				}
			}
		};

		// Start the two threads. If all goes as planned, deadlock will occur,
		// and the program will never exit.
		t1.start();
		t2.start();
	}

	// test for application main.
	public static void main(final String[] args) throws Exception
	{
		final FileWriter fw = new FileWriter("test.txt");
		Runtime.getRuntime().addShutdownHook(new Thread()
		{

			public void run()
			{
				Thread.currentThread().setName("shutdown hook");
				if (WrapperJVMMain.WRAPPER_MANAGER != null)
					System.out.println("stop reason: " + WrapperJVMMain.WRAPPER_MANAGER.getStopReason());
				if (args.length > 0 && args[0].equals("exception"))
				{
					System.out.println("Exception 1");
					System.out.println("Exception 2");
					System.out.println("Exception 3");
				}

				int i = 1;
				// while (i>0)
				// System.out.println("asdfasd");
				// Runtime.getRuntime().halt(0);
				System.out.println("You wanna quit, hey?");
				try
				{
					fw.close();
					if (args.length > 0 && args[0].equals("signalStopping"))
					{
						System.out.println("+ sleeping");
						if (WrapperJVMMain.WRAPPER_MANAGER != null)
							WrapperJVMMain.WRAPPER_MANAGER.signalStopping(35000);
						Thread.sleep(60000);
						System.out.println("- sleeping");
					}
					else if (args.length > 0 && args[0].equals("sleepStop"))
					{
					 Thread.sleep(180000);
					 Runtime.getRuntime().halt(0);
					}
				}
				catch (Exception e)
				{
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				// while(true);
			}

		});
		
		System.out.println("java.library.path: "+System.getProperty("java.library.path"));

		if (args.length >= 1 && "crash".equals(args[0]))
		{
			Thread.sleep(5000);
			Runtime.getRuntime().halt(99);
		}
		if (args.length >= 1 && "outofmem-thread".equals(args[0]))
		{
			int x = 0;
			while (true)
			{
				x++;
				new Thread(new Runnable()
				{

					public void run()
					{
						try
						{
							// System.out.println("thread up");
							Thread.sleep(Long.MAX_VALUE);
							System.out.println("thread down");
						}
						catch (InterruptedException e)
						{
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}

				}).start();
				if (x % 100 == 0)
					System.out.println("outofmem-thread " + x);
				// Thread.sleep(10);
			}
		}
		if (args.length >= 1 && "outofmem-heap".equals(args[0]))
		{
			new Thread(new Runnable()
			{

				public void run()
				{
					int i = 0;
					while (true)
					{
						i++;
						outOfMem.put(i, "aaaaaaaaaaaaaaaaaaaaa" + i);

						if (i % 1000 == 0)
							System.out.println("outofmem-heap " + i);
						// Thread.sleep(10);
					}
				}
			}).start();
		}

		if (args.length >= 1 && "appready".equals(args[0]))
		{
			Thread.sleep(5000);
			System.out.println("calling report service startup");
			if (WrapperJVMMain.WRAPPER_MANAGER != null)
				WrapperJVMMain.WRAPPER_MANAGER.reportServiceStartup();
			else
				System.out.println("missing wrapper manager");
		}

		System.out.println("myenv " + System.getProperty("myenv"));
		if (WrapperJVMMain.WRAPPER_MANAGER != null)
			System.out.println("wrapper property: " + WrapperJVMMain.WRAPPER_MANAGER.getProperties().getProperty("wrapper.debug"));
		/*
		 * try { Process p = Runtime.getRuntime().exec("../set.bat");
		 * BufferedReader in1 = new BufferedReader(new
		 * InputStreamReader(p.getInputStream())); String line; while ((line =
		 * in1.readLine()) != null) System.out.println(line); } catch (Exception
		 * ex) { ex.printStackTrace(); } DocumentBuilderFactory factory =
		 * DocumentBuilderFactory.newInstance();
		 * System.out.println(factory.getClass());
		 */
		// try
		// {
		// Configuration config = new BaseConfiguration();
		// }
		// catch (Throwable ex)
		// {
		// System.out.println("all ok we cannot access commons configuration");
		// ex.printStackTrace();
		// }
		System.out.println("args:");
		for (int i = 0; i < args.length; i++)
			System.out.println(args[i]);
		final Vector v = new Vector();
		new File("test.txt").delete();
		final BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		new Thread(new Runnable()
		{
			public void run()
			{
				Thread.currentThread().setName("input reader");
				try
				{
					int i = 0;
					byte[] buf = new byte[256];
					while (true)
					{
						i++;
						String line = in.readLine();
						System.out.println("in > " + line);
						if (line.contains("exit 0"))
						{
							System.out.println("exiting 0");
							System.exit(0);
						}
						if (line.contains("exit 1"))
						{
							System.out.println("exiting 1");
							System.exit(1);
						}
						if (line.contains("exit 257"))
						{
							System.out.println("exiting 1");
							System.exit(257);
						}
					}
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
				}
				System.out.println("terminated");
			}
		}).start();

		ArrayList list = new ArrayList();

		// System.out.println(Scheduler.class.getClassLoader());
		// System.out.println(Configuration.class.getClassLoader());
		// System.out.flush();
		int i = 0;
		// org.rzo.yajsw.WrapperMain.WRAPPER_MANAGER.threadDump();
		try
		{
			// Thread.sleep(10000);
		}
		catch (Exception e2)
		{
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}
		new Thread(new MyWriter()).start();
		new Thread(new MyWriter()).start();
		new Thread(new MyWriter()).start();
		// System.out.println(new BufferedReader(new
		// InputStreamReader(System.in)).readLine());
		// for (; i < 10;)
		if (args.length > 0 && "reportStartup".equals(args[0]))
			if (WrapperJVMMain.WRAPPER_MANAGER != null)
				WrapperJVMMain.WRAPPER_MANAGER.reportServiceStartup();

		if (args.length >= 1 && "deadlock".equals(args[0]))
			simulateDeadlock();
		if (args.length >= 1 && "tray".equals(args[0]))
			startTray();

		while (true)
		{
			i++;
			System.out.println("a" + i);
			System.out.flush();
			// simulate jvm crash
			// while (i>3)
			// list.add("asfdasffsadfdsdfsaadfsasdasf");

			// if (i ==20)
			// org.rzo.yajsw.app.WrapperJVMMain.WRAPPER_MANAGER.restart();

			if (fw != null)
				try
				{
					// v.add(new byte[1000]);
					// fw.write("" + i + "\n");
					// fw.flush();
				}
				catch (Throwable e1)
				{
					// TODO Auto-generated catch block
					e1.printStackTrace();
					System.exit(0);
				}
			if (i % 2 == 0)
				try
				{
					// WrapperJVMMain.WRAPPER_MANAGER.stop();
					Thread.sleep(500);
					// System.out.println("Exception");
					// System.out.flush();
					// Runtime.getRuntime().halt(0);
				}
				catch (Exception e)
				{
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		}

		/*
		 * WrapperManager.instance.restart(); try { Thread.sleep(10000); } catch
		 * (InterruptedException e) { // TODO Auto-generated catch block
		 * e.printStackTrace(); }
		 */
		// System.exit(0);
		// System.out.println("hello world. short test");
	}

	private static void startTray()
	{
		SystemTray tray = SystemTray.getSystemTray();
		int w = 80;
		int[] pix = new int[w * w];
		for (int i = 0; i < w * w; i++)
			pix[i] = (int) (Math.random() * 255);
		ImageProducer producer = new MemoryImageSource(w, w, pix, 0, w);
		Image image = Toolkit.getDefaultToolkit().createImage(producer);
		TrayIcon trayIcon = new TrayIcon(image);
		trayIcon.setImageAutoSize(true);
		startWindow();
		try
		{
			tray.add(trayIcon);
			System.out.println("installed tray");
		}
		catch (AWTException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private static void startWindow()
	{
		JFrame frame = new JFrame("Hellow World");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.getContentPane().add(new JLabel("hellow world test"), BorderLayout.CENTER);
		frame.pack();
		frame.setVisible(true);
	}

}
