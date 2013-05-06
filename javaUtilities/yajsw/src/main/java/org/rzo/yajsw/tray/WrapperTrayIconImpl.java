/* This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * <p/>
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.  
 */
package org.rzo.yajsw.tray;

import java.awt.AWTException;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.SystemTray;
import java.awt.Toolkit;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import org.rzo.yajsw.boot.WrapperLoader;
import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.os.Mouse;
import org.rzo.yajsw.os.OperatingSystem;
import org.rzo.yajsw.os.Process;
import org.rzo.yajsw.tools.JCLParser;
import org.rzo.yajsw.util.DaemonThreadFactory;
import org.rzo.yajsw.wrapper.AbstractWrappedProcessMBean;
import org.rzo.yajsw.wrapper.WrappedProcess;


// TODO: Auto-generated Javadoc
/**
 * The Class WrapperTrayIconImpl.
 */
public class WrapperTrayIconImpl implements WrapperTrayIcon
{
	volatile boolean						_dialogDisplayed;

	/** The icon running. */
	Image									iconRunning;

	/** The icon idle. */
	Image									iconIdle;

	Image									iconWaitForApp;
	/** The icon else. */
	Image									iconElse;

	/** The icon offline. */
	Image									iconOffline;

	/** The ti. */
	TrayIcon								ti;
	Image 									tiImage;
	
	final JPopupMenu popup = new JPopupMenu();


	/** The current image. */
	Image									currentImage			= iconIdle;

	/** The tool tip prefix. */
	String									toolTipPrefix;

	/** The current tool tip. */
	String									currentToolTip;

	/** The tray. */
	final SystemTray						tray					= SystemTray.getSystemTray();

	/** The init. */
	boolean									init					= false;

	/** The _console. */
	Console									_console				= null;

	/** The _process. */
	volatile AbstractWrappedProcessMBean	_process;
	protected static final Executor			executor				= Executors.newCachedThreadPool(new DaemonThreadFactory("console"));

	/** The stop. */
	volatile boolean						stop					= false;

	/** The _current state. */
	int										_currentState			= WrappedProcess.STATE_IDLE;

	/** The _stop item. */
	JMenuItem								_stopItem				= new JMenuItem();

	/** The _close item. */
	JMenuItem								_closeItem				= new JMenuItem();

	/** The _start item. */
	JMenuItem								_startItem				= new JMenuItem();

	/** The _restart item. */
	JMenuItem								_restartItem			= new JMenuItem();

	/** The _console item. */
	JMenuItem								_consoleItem			= new JMenuItem();

	/** The _stop timer item. */
	JMenuItem								_stopTimerItem			= new JMenuItem();

	/** The _thread dump item. */
	JMenuItem								_threadDumpItem			= new JMenuItem();
	JMenuItem								_gcItem					= new JMenuItem();
	JMenuItem								_dumpHeapItem					= new JMenuItem();

	/** The _exit item. */
	JMenuItem								_exitItem				= new JMenuItem();

	/** The _exit wrapper item. */
	JMenuItem								_exitWrapperItem		= new JMenuItem();

	/** The _thread dump wrapper item. */
	JMenuItem								_threadDumpWrapperItem	= new JMenuItem();

	/** The _close console item. */
	JMenuItem								_closeConsoleItem		= new JMenuItem();

	/** The _start service item. */
	JMenuItem								_startServiceItem		= new JMenuItem();

	/** The _response item. */
	JMenuItem								_responseItem			= new JMenuItem();

	JMenuItem								_updateItem			= new JMenuItem();

	/** The _inquire message. */
	String									_inquireMessage			= null;

	boolean									_waitForAppReady		= false;
	
	volatile boolean						_trayDialog				= false;

	private YajswConfigurationImpl			_config;
	
	static Mouse m = OperatingSystem.instance().mouseInstance();

	/**
	 * Instantiates a new wrapper tray icon impl.
	 * 
	 * @param name
	 *            the name
	 * @param icon
	 *            the icon
	 */
	public WrapperTrayIconImpl(String name, String icon, YajswConfigurationImpl	config)
	{
		try
		{
			Class cl = this.getClass().getClassLoader().loadClass("java.awt.GraphicsEnvironment");
			Method m = cl.getMethod("isHeadless", null);
			Boolean b = (Boolean) m.invoke(null, null);
			if (b)
			{
				System.out.println("SystemTray not supported on this platform: headless");
				return;
			}
		}
		catch (Exception ex)
		{
			System.out.println("SystemTray not supported on this platform: "+ex.getMessage() + " error getting java.awt.GraphicsEnvironment");
			return;			
		}
		if (!SystemTray.isSupported())
		{
			System.out.println("SystemTray not supported on this platform");
			return;
		}
		
		_config = config;
		if (_config != null)
		{
		_waitForAppReady = _config.getBoolean("wrapper.ntservice.autoreport.waitready", false);
		_trayDialog = _config.getBoolean("wrapper.tray.dialog", true);
		String lookAndFeel = _config.getString("wrapper.tray.look_and_feel", null);
		try
		{
		if (lookAndFeel != null && lookAndFeel.length() > 0)
		{
			UIManager.setLookAndFeel(lookAndFeel);
		}
		}
		catch (Throwable ex)
		{
			ex.printStackTrace();
		}
		}
		
		_dialogDisplayed = new Boolean(false);
		
		toolTipPrefix = name + " - ";

		InputStream f = null;
		try
		{
			f = getImage(icon);
			ti = new TrayIcon(createColorImage(f, null, null));
			ti.setImageAutoSize(true);

			Dimension d = ti.getSize();
			f = getImage(icon);
			iconRunning = createColorImage(f, Color.GREEN, d);
			f = getImage(icon);
			iconIdle = createColorImage(f, Color.RED, d);
			f = getImage(icon);
			iconElse = createColorImage(f, Color.ORANGE, d);
			f = getImage(icon);
			iconOffline = createColorImage(f, Color.BLACK, d);
			f = getImage(icon);
			iconWaitForApp = createColorImage(f, Color.BLUE.brighter(), d);
		}
		catch (Exception ex)
		{
			System.out.println("System Tray: file type not supported -> abort");
			return;
		}

		ti = new TrayIcon(iconIdle);
		/*
		 * process.addStateChangeListener(new StateChangeListener() { public
		 * void stateChange(int newState, int oldState) { if (newState ==
		 * WrappedProcess.STATE_SHUTDOWN) { synchronized (tray) {
		 * tray.remove(ti); }
		 * 
		 * if (!_process.getType().endsWith("Service"))
		 * Runtime.getRuntime().halt(0); return; } showState(newState); } });
		 */
		ti.setImageAutoSize(true);

		
		try
		{
			SwingUtilities.windowForComponent(popup.getInvoker()).setAlwaysOnTop(true); 
		}catch(Throwable tr){
			
		}
		
		_exitItem.setAction(new AbstractAction("Stop Tray", createImageIcon("/resources/exit.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
					try
					{
						_dialogDisplayed = true;
						closePopup();
						int userChoice = JOptionPane.OK_OPTION;
						if (_trayDialog)
						userChoice = JOptionPane.showConfirmDialog(null, _config.getString("wrapper.tray.text.dialog_exit_tray", "Terminate wrapper Tray ?"),
                                UIManager.getString("OptionPane.titleText"),
                                JOptionPane.YES_NO_OPTION);
				
						if(JOptionPane.OK_OPTION == userChoice)
						{
							closePopup();
							stop = true;
							synchronized (tray)
							{
								tray.remove(ti);
							}
							System.exit(0);
						}
					}
					finally
					{
						_dialogDisplayed = false;
					}
					
				
			}

		});
		_stopItem.setAction(new AbstractAction("Stop", createImageIcon("/resources/stop.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				if (_process != null)
				{
					SwingUtilities.invokeLater(new Runnable() 
					{
						public void run() 
						{
								try
								{
									_dialogDisplayed = true;
									closePopup();
									int userChoice = JOptionPane.OK_OPTION;
									if (_trayDialog)
									userChoice = JOptionPane.showConfirmDialog(null, 
											_config.getString("wrapper.tray.text.dialog_stop", "Stop the application ?"),
											UIManager.getString("OptionPane.titleText"),
			                                JOptionPane.YES_NO_OPTION);
									if(JOptionPane.OK_OPTION == userChoice)
									{
										executor.execute(new Runnable()
										{
											public void run()
											{
												try
												{
													_process.stop("TRAY");	
												}
												catch (Throwable ex)
												{
													ex.printStackTrace();
												}
												
											}
										});
									}
								}
								finally
								{
									
									_dialogDisplayed = false;
								}
							}
					});


				}

			}

		});
		_closeItem.setAction(new AbstractAction("Close Popup", createImageIcon("/resources/close.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				closePopup();
			}
		});

		_startItem.setAction(new AbstractAction("Start", createImageIcon("/resources/start.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				System.out.println("start");
				if (_process != null)
					try
					{
						_process.start();
					}
					catch (Throwable ex)
					{
						ex.printStackTrace();
					}
					closePopup();
			}
		});
		
		_restartItem.setAction(new AbstractAction("Restart", createImageIcon("/resources/restart.png"))
		{

			public void actionPerformed(ActionEvent e)
			{
				if (_process != null)
				{
					SwingUtilities.invokeLater(new Runnable() 
					{
						public void run() 
						{
								try
								{
									_dialogDisplayed = true;
									closePopup();
									int userChoice = JOptionPane.OK_OPTION;
									if (_trayDialog)
									userChoice = JOptionPane.showConfirmDialog(null, 
											_config.getString("wrapper.tray.text.dialog_restart", "Restart the application ?"),
											UIManager.getString("OptionPane.titleText"),
			                                JOptionPane.YES_NO_OPTION);
									if(JOptionPane.OK_OPTION == userChoice)
									{
										executor.execute(new Runnable()
										{
											public void run()
											{
												try
												{
													_process.restart();	
												}
												catch (Throwable ex)
												{
													ex.printStackTrace();
												}
												
											}
										});
									}
								}
								finally
								{
									
									_dialogDisplayed = false;
								}
							}
					});


				}
			}

});

_consoleItem.setAction(new AbstractAction("Console", createImageIcon("/resources/console.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				if (_process != null)
					openConsole();
				closePopup();
			}

		});
		_threadDumpItem.setAction(new AbstractAction("Thread Dump", createImageIcon("/resources/lightning.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				if (_process != null)
					try
					{
						_process.threadDump();
					}
					catch (Throwable ex)
					{
						ex.printStackTrace();
					}

					closePopup();
			}
		});

		_gcItem.setAction(new AbstractAction("GC", createImageIcon("/resources/recycle.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				if (_process != null)
					try
					{
						_process.gc();
					}
					catch (Throwable ex)
					{
						ex.printStackTrace();
					}

					closePopup();
			}
		});

		_dumpHeapItem.setAction(new AbstractAction("Dump Heap", createImageIcon("/resources/document-save.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				if (_process != null)
					try
					{
						String s = (String) JOptionPane.showInputDialog("Dump File Name (Empty == default) ?", "");
						_process.dumpHeap(s);
					}
					catch (Throwable ex)
					{
						ex.printStackTrace();
					}

					closePopup();
			}
		});


		_stopTimerItem.setAction(new AbstractAction("Stop Timer/Condition", createImageIcon("/resources/clock_stop.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				if (_process != null)
				{
					try
					{
						_process.stopTimerCondition();
						if (_console != null)
						{
							_console.setTimer(_process.isTimerActive());
							_console.setCondition(_process.isConditionActive());
						}
					}
					catch (Throwable ex)
					{
						ex.printStackTrace();
					}

				}
				closePopup();
			}
		});
		_exitWrapperItem.setAction(new AbstractAction("Stop Wrapper", createImageIcon("/resources/exitWrapper.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				if (_process != null)
				{
					SwingUtilities.invokeLater(new Runnable() 
					{
						public void run() 
						{
								try
								{
									_dialogDisplayed = true;
									closePopup();
									int userChoice = JOptionPane.OK_OPTION;
									if (_trayDialog)
									userChoice = JOptionPane.showConfirmDialog(null, 
											_config.getString("wrapper.tray.text.dialog_exit_wrapper", "Stop the wrapper ?"),
											UIManager.getString("OptionPane.titleText"),
			                                JOptionPane.YES_NO_OPTION);
									if(JOptionPane.OK_OPTION == userChoice)
									{
										executor.execute(new Runnable()
										{
											public void run()
											{
												try
												{
													_process.stopWrapper();	
												}
												catch (Throwable ex)
												{
													ex.printStackTrace();
												}
												
											}
										});
									}
								}
								finally
								{
									
									_dialogDisplayed = false;
								}
							}
					});


				}

			}

		});

		_threadDumpWrapperItem.setAction(new AbstractAction("TDump Wrapper", createImageIcon("/resources/lightning.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				if (_process != null)
					try
					{
						_process.wrapperThreadDump();
					}
					catch (Throwable ex)
					{
						ex.printStackTrace();
					}

					closePopup();
			}

		});

		_closeConsoleItem.setAction(new AbstractAction("Close Console")
		{
			public void actionPerformed(ActionEvent e)
			{
				closeConsole();
				closePopup();
			}

		});

		_startServiceItem.setAction(new AbstractAction("Start Service", createImageIcon("/resources/startService.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				if (_process == null)
				{
					startService();
				}
				closePopup();
			}

		});

		_responseItem.setAction(new AbstractAction("Response", createImageIcon("/resources/Help16.gif"))
		{
			public void actionPerformed(ActionEvent e)
			{
				if (_process != null && _inquireMessage != null)
				{
					String message = _inquireMessage;
					String s = (String) JOptionPane.showInputDialog(message, "");
					if (s != null && _process != null)
					{
						try
						{
							_process.writeInquireResponse(s);
							_inquireMessage = null;
						}
						catch (Throwable ex)
						{
							ex.printStackTrace();
						}

					}

				}
				closePopup();
			}

		});
		
		_updateItem.setAction(new AbstractAction("Update", createImageIcon("/resources/update.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				if (_process != null)
				{
					String s = (String) JOptionPane.showInputDialog("Update configuration file", "");
					if (s != null && _process != null)
					{
						try
						{
							_process.update(s);
						}
						catch (Throwable ex)
						{
							ex.printStackTrace();
						}

					}

				}
				closePopup();
			}

		});
		
		
		List menueList = _config == null ? Arrays.asList("start", "stop") : _config.getList("wrapper.tray.commands", Arrays.asList(new Object[]{"close", "start", "stop", "restart", "console", "response", "exitWrapper", "startService", "updateService", "exitTray"}));

		if (menueList.contains("start"))
			popup.add(_startItem);
		if (menueList.contains("stop"))
			popup.add(_stopItem);
		if (menueList.contains("restart"))
			popup.add(_restartItem);
		if (menueList.contains("console"))
			popup.add(_consoleItem);
		if (menueList.contains("response"))
			popup.add(_responseItem);
		// popup.add(_threadDumpWrapperItem);
		if (menueList.contains("exitWrapper"))
			popup.add(_exitWrapperItem);
		if (menueList.contains("startService"))
			popup.add(_startServiceItem);
		if (menueList.contains("exitTray"))
			popup.add(_exitItem);
		if (menueList.contains("updateService"))
			popup.add(_updateItem);
		popup.add(_closeItem);
		popup.validate();
			ti.addMouseListener(new MouseListener()
		{

			public void mouseClicked(MouseEvent e)
			{
				System.out.println("mouse clicked");
			}

			public void mouseEntered(MouseEvent e)
			{
				System.out.println("mouse entered");

			}

			public void mouseExited(MouseEvent e)
			{
				System.out.println("mouse exited");
			}

			public void mousePressed(MouseEvent e)
			{

				if (!OperatingSystem.instance().getOperatingSystemName().toLowerCase().contains("mac"))
					return;
				System.out.println("mouse rleased");
				if(_dialogDisplayed == true)
				{

				}
				else
				{
					if(!_dialogDisplayed)
					{
						System.out.println("X"+e.getXOnScreen()+"/"+ popup.getWidth());
						System.out.println("Y"+e.getYOnScreen()+"/"+ popup.getHeight());
						int xPos = e.getXOnScreen() > popup.getWidth() ? e.getXOnScreen() - popup.getWidth() : e.getXOnScreen();
						int yPos = e.getYOnScreen() > popup.getHeight() ? e.getYOnScreen() - popup.getHeight() : e.getYOnScreen();
					popup.show(e.getComponent(), xPos, yPos);
					if (m != null)
					m.registerMouseUpListner(new Runnable()
					{

						public void run()
						{
							closePopup();
						}
						
					}, executor);
					}
				}

			}

			public void mouseReleased(MouseEvent e)
			{
				System.out.println("mouse rleased");
				if(_dialogDisplayed == true)
				{

				}
				else
				{
					if(!_dialogDisplayed)
					{
						System.out.println("X"+e.getXOnScreen()+"/"+ popup.getWidth());
						System.out.println("Y"+e.getYOnScreen()+"/"+ popup.getHeight());
						int xPos = e.getXOnScreen() > popup.getWidth() ? e.getXOnScreen() - popup.getWidth() : e.getXOnScreen();
						int yPos = e.getYOnScreen() > popup.getHeight() ? e.getYOnScreen() - popup.getHeight() : e.getYOnScreen();
					popup.show(e.getComponent(), xPos, yPos);
					if (m != null)
					m.registerMouseUpListner(new Runnable()
					{

						public void run()
						{
							closePopup();
						}
						
					}, executor);
					}
				}

			}

		});
		

		Runtime.getRuntime().addShutdownHook(new Thread()
		{
			public void run()
			{
				stop = true;
				synchronized (tray)
				{
					tray.remove(ti);
				}
			}
		});

		try
		{
			tray.add(ti);
		}
		catch (AWTException e1)
		{
			// TODO Auto-generated catch block
			e1.printStackTrace();
			System.exit(0);
		}

		init = true;

	}
	
	private void startService()
	{
		try
		{
			
			/*
		WrappedService w = new WrappedService();
			if((!w.isRunning())&&(!w.isStarting()))
			{
				w.init();
				w.start();
			}
			else
			{
				System.out.println("Already in Running || starting state.");
			}
			*/
			// start in a separate process so that we can handle windows uac elevation if necessary
			int myPid = OperatingSystem.instance().processManagerInstance().currentProcessId(); 
			Process me = OperatingSystem.instance().processManagerInstance().getProcess(myPid);
			String cmd = me.getCommand();
			me.destroy();
			JCLParser parser = JCLParser.parse(cmd);
			String[] startCmd = new String[5];
			startCmd[0] = parser.getJava();
			startCmd[1] = "-jar";
			startCmd[2] = new File(WrapperLoader.getWrapperJar()).getCanonicalPath();
			startCmd[3] = "-t"; 
			startCmd[4] = parser.getArgs().get(1);
			Process startProcess = OperatingSystem.instance().processManagerInstance().createProcess();
			startProcess.setCommand(startCmd);
			startProcess.setDebug(false);
			startProcess.start();
			startProcess.waitFor();
			startProcess.destroy();
		}
		catch (Throwable ex)
		{
			ex.printStackTrace();
		}


	}
	
	private void closePopup()
	{
		executor.execute(new Runnable()
		{
			public void run()
			{
				try
				{
					// give event some time to get to java.
					// in case we clicked in a java ui
					Thread.sleep(50);
				}
				catch (InterruptedException e)
				{
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				SwingUtilities.invokeLater(new Runnable() 
				{
					public void run()
					{
					popup.setVisible(false);
					if (m != null)
					m.unregisterMouseUpListner();
					}
				});
			}
		});

	}

	private InputStream getImage(String icon)
	{
		InputStream f = null;
		if (icon == null)
			f = findFile("/resources/console.png");
		else
		{
			f = findFile(icon);
			if (f == null)
			{
				try
				{
					System.out.println("System Tray: " + new File(icon).getCanonicalPath() + " not found -> default icon");
				}
				catch (IOException e)
				{
					e.printStackTrace();
				}
				f = findFile("/resources/console.png");
			}
		}
		if (f == null)
		{
			System.out.println("System Tray: no icon found -> abort");
			return null;
		}
		return f;
	}

	/**
	 * Gets the state image.
	 * 
	 * @param state
	 *            the state
	 * 
	 * @return the state image
	 */
	public Image getStateImage(int state)
	{
		switch (state)
		{
		case WrappedProcess.STATE_RUNNING:
			return iconRunning;
		case WrappedProcess.STATE_IDLE:
			return iconIdle;
		case WrappedProcess.STATE_APP_WAIT:
			return iconWaitForApp;
		default:
			return iconElse;
		}
	}
	
	Color _currentUserColor = null;
	Image _baseImage;
	public Image getColorImage(Color color)
	{
		if (_currentUserColor == null || !_currentUserColor.equals(color))
		{
			if (color != null)
			{
				return createColorImage(iconElse, color, ti.getSize());
			}
			else
				return getStateImage(_currentState);
		}
		return null;
	}

	/**
	 * Gets the state tool tip.
	 * 
	 * @param state
	 *            the state
	 * 
	 * @return the state tool tip
	 */
	public String getStateToolTip(int state)
	{
		switch (state)
		{
		case WrappedProcess.STATE_RUNNING:
			return "Running";
		case WrappedProcess.STATE_IDLE:
			return "Idle";
		case WrappedProcess.STATE_RESTART:
		case WrappedProcess.STATE_RESTART_START:
		case WrappedProcess.STATE_RESTART_STOP:
		case WrappedProcess.STATE_RESTART_WAIT:
			return "Restarting";
		case WrappedProcess.STATE_STARTING:
			return "Starting";
		case WrappedProcess.STATE_USER_STOP:
		case WrappedProcess.STATE_STOP:
			return "Stopping";
		case WrappedProcess.STATE_APP_WAIT:
			return "Waiting";
		default:
			return "Other";
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#showState(int)
	 */
	synchronized public void showState(int state)
	{
		int oldState = _currentState;

		if (_waitForAppReady && state == WrappedProcess.STATE_RUNNING && (!_process.isAppReportedReady()))
		{
			state = WrappedProcess.STATE_APP_WAIT;
		}

		_currentState = state;
		String strState = getStateToolTip(state);
		if (oldState != _currentState)
			this.message("STATE CHANGED", getStateToolTip(oldState) + " -> " + getStateToolTip(_currentState));
		if (_console != null && _process != null)
		{
			_console.setState(strState);
			_console.setAppRestartCount(_process.getTotalRestartCount(), _process.getRestartCount());
			_console.setAppPid(_process.getAppPid());
			_console.setAppStarted(_process.getAppStarted());
			_console.setAppStopped(_process.getAppStopped());
			_console.setExitCode(_process.getExitCode());
			_console.setTimer(_process.isTimerActive());
			_console.setCondition(_process.isConditionActive());
		}

		Image image = getStateImage(state);
		if (image != currentImage)
		{
			_currentUserColor = null;
			currentToolTip = toolTipPrefix + strState;
			showImage(image);
		}

	}
	
	private void showImage(Image image)
	{
		if (image != currentImage)
		{
			_currentUserColor = null;
			ti.setImage(image);
			currentImage = image;
			ti.setToolTip(currentToolTip);
		}

	}
	
	public void showColor(Color color)
	{
		Image image = getColorImage(color);
		if (image != null)
			showImage(image);
		
	}

	/**
	 * Returns an ImageIcon, or null if the path was invalid.
	 * 
	 * @param path
	 *            the path
	 * 
	 * @return the image icon
	 */
	static ImageIcon createImageIcon(String path)
	{
		Image image = createImage(path);
		if (image == null)
			return null;
		return new ImageIcon(image);
	}

	static Image createImage(String path)
	{
		java.net.URL imgURL = WrapperTrayIconImpl.class.getResource(path);
		if (imgURL != null)
		{
			return Toolkit.getDefaultToolkit().getImage(imgURL);
		}
		else
		{
			if (new File(path).exists())
				return Toolkit.getDefaultToolkit().getImage(path);
			return null;
		}
	}

	private InputStream findFile(String path)
	{
		InputStream result = null;
		try
		{
			result = getClass().getResourceAsStream(path);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		if (result != null)
			return result;
		File f = null;
		if (result == null)
			f = new File(path);
		if (f.exists())
			try
			{
				result = new FileInputStream(f);
				return result;
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
		return null;

	}
	
	private Image createColorImage(Image image, Color color, Dimension d)
	{
		if (d != null)
		{
			BufferedImage bufferedResizedImage = new BufferedImage(d.width, d.height, BufferedImage.TYPE_INT_RGB);
			Graphics2D g2d = bufferedResizedImage.createGraphics();
			g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
			g2d.drawImage(image, 0, 0, d.width, d.height, null);
			g2d.dispose();
			image = bufferedResizedImage;

		}
		if (color != null)
		{
			Graphics g = image.getGraphics();
			int w = image.getWidth(null);
			int h = image.getHeight(null);
			int rw = w / 2;
			int rh = h / 2;
			Color c = new Color(color.getRed(), color.getGreen(), color.getBlue(), 200);
			g.setColor(c);
			g.fillRoundRect(0, h - rh, rw, rh, rw, rh);
		}
		
		return image;
		
	}

	private Image createColorImage(InputStream imageFile, Color color, Dimension d) throws Exception
	{
		BufferedImage image = ImageIO.read(imageFile);
		imageFile.close();


		return createColorImage(image, color, d);
	}

	/**
	 * The main method.
	 * 
	 * @param args
	 *            the arguments
	 * 
	 * @throws InterruptedException
	 *             the interrupted exception
	 */
	public static void main(String[] args) throws InterruptedException
	{
		WrapperTrayIconImpl t = new WrapperTrayIconImpl("test", null, null);// "tomcat.gif");
		//while (true)
		{
			Thread.sleep(2000);
			t.showState(WrappedProcess.STATE_RUNNING);
			Thread.sleep(2000);
			t.showState(WrappedProcess.STATE_IDLE);
			Thread.sleep(2000);
			t.showState(WrappedProcess.STATE_RESTART);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#isInit()
	 */
	public boolean isInit()
	{
		return init;
	}

	/**
	 * Open console.
	 */
	public void openConsole()
	{
		if (_console != null)
			return;
		_console = new Console(this);
		this.showState(_currentState);
		_console.setWrapperPid(_process.getWrapperPid());
		_console.setWrapperStarted(_process.getWrapperStarted());
		_console.setWrapperType(_process.getType());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#closeConsole()
	 */
	public void closeConsole()
	{
		if (_console == null)
			return;
		_console.close();
		_console = null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#error(java.lang.String,
	 * java.lang.String)
	 */
	public void error(String caption, String message)
	{
		ti.displayMessage(toolTipPrefix + caption, message, TrayIcon.MessageType.ERROR);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#info(java.lang.String,
	 * java.lang.String)
	 */
	public void info(String caption, String message)
	{
		ti.displayMessage(toolTipPrefix + caption, message, TrayIcon.MessageType.INFO);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#message(java.lang.String,
	 * java.lang.String)
	 */
	public void message(String caption, String message)
	{
		ti.displayMessage(toolTipPrefix + caption, message, TrayIcon.MessageType.NONE);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#warning(java.lang.String,
	 * java.lang.String)
	 */
	public void warning(String caption, String message)
	{
		ti.displayMessage(toolTipPrefix + caption, message, TrayIcon.MessageType.WARNING);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.rzo.yajsw.tray.WrapperTrayIcon#setProcess(org.rzo.yajsw.wrapper.
	 * AbstractWrappedProcessMBean)
	 */
	public void setProcess(AbstractWrappedProcessMBean proxy)
	{
		_process = proxy;
		if (_process == null)
		{
			Image image = iconOffline;
			if (image != currentImage)
			{
				ti.setImage(image);
				currentImage = image;
				currentToolTip = toolTipPrefix + "OFFLINE";
				ti.setToolTip(currentToolTip);
				message("STATE CHANGED", currentToolTip);
			}

			// Enable stop menu.
			if (SwingUtilities.isEventDispatchThread())
			{
				_exitWrapperItem.setEnabled(true);
			}
			else
			{
				SwingUtilities.invokeLater(new Runnable()
				{
					public void run()
					{
						_exitWrapperItem.setEnabled(true);
					}

				});
			}

		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.rzo.yajsw.tray.WrapperTrayIcon#isStop()
	 */
	public boolean isStop()
	{
		return stop;
	}

}
