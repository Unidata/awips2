package org.rzo.yajsw.ws;

import java.awt.TextArea;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.swing.JFileChooser;
import javax.swing.JFrame;

public class WebStartBooter
{

	static WSForm	wsform			= new WSForm();
	static String	source;
	static String	destination		= "";
	static String	action			= "-c";
	static boolean	start			= false;
	static boolean	tray			= false;
	static String	configuration	= "";
	static String	_wrapperJar;

	static String	TITLE			= "YAJSW-WS Rel 0.2";

	static boolean	_useProxies		= false;

	private static void showFile(String file)
	{
		// Create a TextArea to display the contents of the file in
		TextArea textarea = new TextArea("", 24, 80);
		textarea.setEditable(false);
		final JFrame frame = new JFrame();
		frame.setSize(530, 450);
		frame.setLocation(100, 100);
		frame.getContentPane().add(textarea);
		frame.setVisible(true);

		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent evt)
			{
				frame.dispose();
			}
		});
		textarea.setText(getFile(file));

	}

	private static boolean downloadFile(String source, String destination) throws IOException
	{

		System.out.println("checking " + source);
		URL url = new URL(source);
		URLConnection con = url.openConnection();

		File outFile = new File(destination);
		if (outFile.exists() && outFile.lastModified() < con.getLastModified())
			System.out.println("file changed -> overwrite " + destination);
		else if (outFile.exists())
		{
			System.out.println("file unchanged -> continue");
			return false;
		}
		if (!outFile.getParentFile().exists())
			outFile.getParentFile().mkdirs();
		System.out.println("loading " + source + " -> " + destination);
		BufferedInputStream in = new BufferedInputStream(url.openStream());
		BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(outFile), 1024);
		copyStream(in, out);
		out.close();
		in.close();
		return true;
	}

	private static void copyStream(BufferedInputStream in, BufferedOutputStream out) throws IOException
	{
		byte data[] = new byte[1024];
		int count = 0;
		long total = 0;
		long startTime = System.currentTimeMillis();
		long duration = startTime;
		while ((count = in.read(data, 0, 1024)) != -1)
		{
			out.write(data, 0, count);
			total += count;
			duration = (System.currentTimeMillis() - startTime) / 1000;
			if (duration > 0)
				showSpeed(total / (duration));
		}
	}

	private static void loadWrapperJar(String base, String destination) throws IOException
	{
		downloadFile(base + "/wrapper.jar", destination + "/wrapper.jar");
		_wrapperJar = destination + "/wrapper.jar";
	}

	public static void main(String[] args) throws Exception
	{
		source = args[0];
		destination = args[1];
		action = args[2];
		configuration = args[3];
		if (!args[args.length - 1].contains("nogui"))
			;
		doGui(args);
		File destF = new File(destination);
		if (!destF.exists())
		{
			System.out.println("creating installation folder");
			destF.mkdirs();
		}
		try
		{
			downloadWrapper();
			showStep("Loading & Executing application ...");
			doActions(destination, action, configuration);
		}
		catch (Exception ex)
		{
			showStep("Error -> aborted");
			System.out.println("Error : " + ex.getMessage());
		}

	}

	private static void downloadWrapper() throws IOException
	{
		// System.out.println("source: "+source);
		if (source.endsWith(".zip"))
		{
			URL sourceURL = new URL(source);
			String sourceName = new File(sourceURL.getPath()).getName();
			showStep("Downloading " + sourceName + " ...");
			if (downloadFile(source, destination + "/" + sourceName))
			{
				showStep("Unzip " + sourceName + " ...");
				unzipWrapper(destination, destination + "/" + sourceName);
			}
		}
		else
		{
			showStep("Loading wrapper.jar ...");
			loadWrapperJar(source, destination);
			showStep("Loading wrapper libs ...");
			loadManifestFiles(source, destination);
			showStep("Loading wrapper resources ...");
			loadResources(source, destination);
		}
		findWrapperJar(new File(destination));
	}

	private static void findWrapperJar(File file)
	{
		if (_wrapperJar == null)
		{
			if (file.isDirectory())
			{
				for (File f : file.listFiles())
				{
					findWrapperJar(f);
				}
			}
			else if (file.getName().equals("wrapper.jar"))
				try
				{
					_wrapperJar = file.getCanonicalPath();
				}
				catch (IOException e)
				{
					e.printStackTrace();
				}
		}
	}

	private static void unzipWrapper(String destination, String zipFileName) throws IOException, IOException
	{
		ZipFile zipFile = new ZipFile(zipFileName);

		Enumeration<? extends ZipEntry> entries = zipFile.entries();

		while (entries.hasMoreElements())
		{
			ZipEntry entry = (ZipEntry) entries.nextElement();

			if (entry.isDirectory())
			{
				// Assume directories are stored parents first then children.
				System.err.println("Extracting wrapper folder: " + entry.getName());
				// This is not robust, just for demonstration purposes.
				(new File(destination + "/" + entry.getName())).mkdir();
				continue;
			}

			if (entry.getName().endsWith("wrapper.jar"))
				_wrapperJar = destination + "/" + entry.getName();
			// skip java source file
			if (entry.getName().endsWith(".java"))
				continue;
			System.err.println("Extracting wrapper file: " + entry.getName());
			BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(destination + "/" + entry.getName()));
			copyStream(new BufferedInputStream(zipFile.getInputStream(entry)), out);
			out.close();
		}
		zipFile.close();
	}

	private static void doActions(String destination, String action, String configuration) throws IOException
	{
		if (!action.startsWith("-"))
			action = "-" + action;
		startAppl(destination, action, configuration);
	}

	private static void showStep(String txt)
	{
		if (wsform != null)
			wsform._STATE.setText(txt);
	}

	private static void showSpeed(long speed)
	{
		if (wsform != null)
			wsform._SPEED.setText(speed / 1024 + " kB/s");
	}

	private static void doGui(String[] args) throws IOException, Exception
	{

		// set data in form
		wsform._APPLICATION.setText(configuration);
		wsform._INSTALL_FOLDER.setText(new File(destination).getAbsolutePath());
		if (action.contains("c"))
			wsform._CONSOLE_OPTION.setSelected(true);
		if (action.contains("i"))
			wsform._INSTALL_OPTION.setSelected(true);
		if (action.contains("y"))
			wsform._TRAY_ICON_OPTION.setSelected(true);
		if (action.contains("t"))
			wsform._START_OPTION.setSelected(true);

		// pipe output to log text area in form
		PrintStream aPrintStream = new PrintStream(new FilteredStream(new ByteArrayOutputStream()));

		System.setOut(aPrintStream); // catches System.out messages
		System.setErr(aPrintStream); // catches error messages

		// signal condition if continue button hit
		final Lock lock = new ReentrantLock();
		final Condition cont = lock.newCondition();

		wsform._GO_BUTTON.addActionListener(new ActionListener()
		{

			public void actionPerformed(ActionEvent e)
			{
				lock.lock();
				cont.signal();
				lock.unlock();
			}

		});

		wsform._SHOW_CONF_BUTTON.addActionListener(new ActionListener()
		{

			public void actionPerformed(ActionEvent e)
			{
				showFile(configuration);
			}

		});

		wsform._SELECT_FOLDER_BUTTON.addActionListener(new ActionListener()
		{

			public void actionPerformed(ActionEvent e)
			{
				JFileChooser fc = new JFileChooser();
				fc.setCurrentDirectory(new File("."));
				fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				fc.setAcceptAllFileFilterUsed(false);
				int retval = fc.showOpenDialog(wsform);

				if (retval == JFileChooser.APPROVE_OPTION)
				{
					// ... The user selected a file, get it, use it.
					File file = fc.getSelectedFile();

					// ... Update user interface.
					try
					{
						wsform._INSTALL_FOLDER.setText(file.getCanonicalPath());
					}
					catch (IOException e1)
					{
						e1.printStackTrace();
					}
				}
			}

		});

		// exit if cancel button hit
		wsform._CANCEL_BUTTON.addActionListener(new ActionListener()
		{

			public void actionPerformed(ActionEvent e)
			{
				System.exit(0);
			}

		});

		JFrame frame = new JFrame();
		frame.setTitle(TITLE);
		frame.setSize(530, 450);
		frame.setLocation(100, 100);
		frame.getContentPane().add(wsform);
		frame.setVisible(true);

		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent evt)
			{
				System.exit(0);
			}
		});

		showStep("Click button to continue");

		// wait for continue button
		lock.lock();
		cont.await();
		lock.unlock();

		// get user data
		destination = wsform._INSTALL_FOLDER.getText();
		if (wsform._INSTALL_OPTION.isSelected())
		{
			action = "i";
			if (wsform._START_OPTION.isSelected())
				action += "t";
		}
		if (wsform._CONSOLE_OPTION.isSelected())
			action = "c";
		if (wsform._TRAY_ICON_OPTION.isSelected())
			action += "y";
	}

	private static void startAppl(String destination, String func, String conf) throws IOException
	{
		String useProxies = _useProxies ? " -Djava.net.useSystemProxies=true" : "";
		String wrapperJar = _wrapperJar.contains(" ") ? "\"" + _wrapperJar + "\"" : _wrapperJar;
		String cmd = getJava() + useProxies + " -jar " + wrapperJar + " " + func + " " + conf;
		System.out.println("executing " + cmd);
		final Process p = Runtime.getRuntime().exec(cmd);
		new Thread(new Runnable()
		{
			public void run()
			{
				BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));
				try
				{
					String line = null;
					while ((line = in.readLine()) != null)
						System.out.println(line);
				}
				catch (Exception ex)
				{
				}
			}

		}).start();
		new Thread(new Runnable()
		{
			public void run()
			{
				BufferedReader in = new BufferedReader(new InputStreamReader(p.getErrorStream()));
				try
				{
					String line = null;
					while ((line = in.readLine()) != null)
						System.err.println(line);
				}
				catch (Exception ex)
				{
				}
			}

		}).start();
	}

	// TODO
	private static String getJava()
	{
		return "java";
	}

	private static void loadManifestFiles(String base, String destination) throws IOException
	{
		String manifest = getManifest(destination);
		Set<String> jars = getJars(manifest);
		for (String jar : jars)
			if (!jar.contains("wrapper.jar"))
				try
				{
					downloadFile(base + "/" + jar, destination + "/" + jar);
				}
				catch (Exception ex)
				{
					System.out.println("error loading " + base + "/" + jar);
					System.out.println(ex.getMessage());
				}
	}

	private static void loadResources(String base, String destination) throws IOException
	{
		for (String r : getResources(base))
		{
			r = r.trim();
			downloadFile(base + "/" + r, destination + "/" + r);
		}
	}

	private static Set<String> getJars(String manifest)
	{
		Set<String> result = new HashSet<String>();
		String[] s = manifest.split(" ");
		for (String x : s)
			if (x.endsWith(".jar"))
				result.add(x.trim());
		return result;
	}

	private static String[] getResources(String base)
	{
		return getFile(base + "/resources.txt").split("\r\n");
	}

	public static String getFile(String file)
	{
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		URL url;
		try
		{
			url = new URL(file);
		}
		catch (MalformedURLException e1)
		{
			System.out.println(e1.getMessage());
			return "";
		}
		try
		{
			URLConnection con = url.openConnection();
		}
		catch (IOException e1)
		{
			System.out.println(e1.getMessage());
			return "";
		}
		BufferedInputStream in;
		try
		{
			in = new BufferedInputStream(url.openStream());
		}
		catch (IOException e1)
		{
			System.out.println(e1.getMessage());
			return "";
		}
		byte data[] = new byte[1024];
		int count = 0;
		try
		{
			while ((count = in.read(data, 0, 1024)) != -1)
			{
				out.write(data, 0, count);
			}
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		try
		{
			in.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		return new String(out.toByteArray());

	}

	private static String getManifest(String destination) throws IOException
	{
		ZipFile z = new ZipFile(destination + "/wrapper.jar");
		ZipEntry ze = z.getEntry("META-INF/MANIFEST.MF");
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		BufferedInputStream in = new BufferedInputStream(z.getInputStream(ze));
		byte data[] = new byte[1024];
		int count = 0;
		while ((count = in.read(data, 0, 1024)) != -1)
		{
			out.write(data, 0, count);
		}
		in.close();
		z.close();
		return new String(out.toByteArray()).replaceAll("\r\n ", "");
	}

	static class FilteredStream extends FilterOutputStream
	{
		public FilteredStream(OutputStream aStream)
		{
			super(aStream);
		}

		public void write(byte b[]) throws IOException
		{
			String aString = new String(b);
			wsform._LOG_AREA.append(aString);
		}

		public void write(byte b[], int off, int len) throws IOException
		{
			String aString = new String(b, off, len);
			wsform._LOG_AREA.append(aString);
			if (wsform._LOG_AREA.getText().contains("Exception"))
				showStep("Error found");
			if (wsform._LOG_AREA.getText().contains("configuration file not found"))
				showStep("Error: Configuration file not found");

			wsform._LOG_AREA.setCaretPosition(wsform._LOG_AREA.getDocument().getLength());
		}
	}

}
