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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.locks.ReentrantLock;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;

import org.rzo.netty.ahessian.utils.MyReentrantLock;
import org.rzo.yajsw.util.DaemonThreadFactory;

/**
 * The YAJSW System Tray Console User interface
 */
public class Console extends JFrame
{

	/** The _tray icon. */
	WrapperTrayIconImpl				_trayIcon;

	/** true if the console has been shut down */
	boolean							stop;
	protected static final Executor	executor			= Executors.newCachedThreadPool(new DaemonThreadFactory("console"));

	/** The max lines in the output window */
	int								maxLines			= 1500;

	/** The console UI. */
	ConsoleForm						_consoleForm		= new ConsoleForm();

	/** The date time format. */
	SimpleDateFormat				_dateTimeFormat		= new SimpleDateFormat();

	/** The byte format. */
	ByteFormat						_byteFormat			= new ByteFormat();

	/** The ok icon. */
	Icon							_okIcon;

	JMenuItem						_startOutputItem	= new JMenuItem();

	JMenuItem						_pauseOutputItem	= new JMenuItem();

	JMenuItem						_clearOutputItem	= new JMenuItem();

	volatile boolean				_outputPaused		= false;
	volatile String					_outputFilter		= null;
	volatile LinkedList<String>		_outputLines		= new LinkedList<String>();
	ReentrantLock					_outputLock			= new MyReentrantLock();

	/**
	 * Instantiates a new console.
	 * 
	 * @param trayIcon
	 *            the tray icon
	 */
	public Console(WrapperTrayIconImpl trayIcon)
	{
		_trayIcon = trayIcon;
		this.setTitle(_trayIcon.toolTipPrefix + "Console");
		this.addWindowListener(new WindowEventHandler());
		this.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		_okIcon = _trayIcon.createImageIcon("/resources/tick.png");

		_clearOutputItem.setAction(new AbstractAction(null, WrapperTrayIconImpl.createImageIcon("/resources/edit-clear.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				clearOutput();
			}

		});

		_pauseOutputItem.setAction(new AbstractAction(null, WrapperTrayIconImpl.createImageIcon("/resources/pause.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				_outputPaused = true;
				_consoleForm._START_OUTPUT_BUTTON.setEnabled(true);
				_consoleForm._PAUSE_OUTPUT_BUTTON.setEnabled(false);
			}

		});

		_startOutputItem.setAction(new AbstractAction(null, WrapperTrayIconImpl.createImageIcon("/resources/start.png"))
		{
			public void actionPerformed(ActionEvent e)
			{
				_outputPaused = false;
				_consoleForm._START_OUTPUT_BUTTON.setEnabled(false);
				_consoleForm._PAUSE_OUTPUT_BUTTON.setEnabled(true);
			}

		});

		_consoleForm.__OUTPUT_FILTER.addActionListener(new ActionListener()
		{

			public void actionPerformed(ActionEvent e)
			{
				String txt = _consoleForm.__OUTPUT_FILTER.getText();
				_outputFilter = "".equals(txt) ? null : txt;
				filterOutput();
			}

		});

		initOutput();
		initInput();
		initPerformance();
		initButtons();
		_consoleForm._START_OUTPUT_BUTTON.setEnabled(false);

		this.getContentPane().add(_consoleForm);

		this.pack();
		this.setVisible(true);
		stop = false;
	}

	private void initButtons()
	{
		initButton(_consoleForm._EXIT_TRAY_ICON_BUTTON, _trayIcon._exitItem);
		initButton(_consoleForm._EXIT_WRAPPER_BUTTON, _trayIcon._exitWrapperItem);
		initButton(_consoleForm._THREAD_DUMP_WRAPPER_BUTTON, _trayIcon._threadDumpWrapperItem);
		initButton(_consoleForm._RESTART_BUTTON, _trayIcon._restartItem);
		initButton(_consoleForm._START_BUTTON, _trayIcon._startItem);
		initButton(_consoleForm._STOP_BUTTON, _trayIcon._stopItem);
		initButton(_consoleForm._STOP_TIMER_BUTTON, _trayIcon._stopTimerItem);
		initButton(_consoleForm._THREAD_DUMP_BUTTON, _trayIcon._threadDumpItem);
		initButton(_consoleForm._GC_BUTTON, _trayIcon._gcItem);
		initButton(_consoleForm._DUMP_HEAP_BUTTON, _trayIcon._dumpHeapItem);
		initButton(_consoleForm._jbutton1, _trayIcon._closeConsoleItem);
		initButton(_consoleForm._PAUSE_OUTPUT_BUTTON, _pauseOutputItem);
		initButton(_consoleForm._START_OUTPUT_BUTTON, _startOutputItem);
		initButton(_consoleForm._CLEAR_OUTPUT_BUTTON, _clearOutputItem);

	}

	private void initButton(JButton button, JMenuItem item)
	{
		button.setAction(item.getAction());
	}

	private void initPerformance()
	{
		executor.execute(new Runnable()
		{

			public void run()
			{
				while (!stop)
				{
					setAppCpu(_trayIcon._process.getAppCpu());
					setAppHandles(_trayIcon._process.getAppHandles());
					setAppMemory(_trayIcon._process.getAppPMemory(), _trayIcon._process.getAppVMemory());
					setAppThreads(_trayIcon._process.getAppThreads());
					try
					{
						Thread.sleep(1000);
					}
					catch (InterruptedException e)
					{
						e.printStackTrace();
						Thread.currentThread().interrupt();
					}
				}
			}
		});
	}

	/**
	 * Sets the state.
	 * 
	 * @param state
	 *            the new state
	 */
	void setState(String state)
	{
		if (state != null)
			_consoleForm._state.setText(state);
		_consoleForm._state.repaint();
	}

	/**
	 * Sets the app pid.
	 * 
	 * @param pid
	 *            the new app pid
	 */
	void setAppPid(int pid)
	{
		if (pid > 0)
			_consoleForm._appPid.setText("" + pid);
		else
			_consoleForm._appPid.setText("?");

	}

	/**
	 * Sets the wrapper pid.
	 * 
	 * @param pid
	 *            the new wrapper pid
	 */
	void setWrapperPid(int pid)
	{
		if (pid > 0)
			_consoleForm._wPid.setText("" + pid);
		else
			_consoleForm._wPid.setText("?");
	}

	/**
	 * Sets the trigger.
	 * 
	 * @param trigger
	 *            the new trigger
	 */
	void setTrigger(String trigger)
	{
		if (trigger != null)
			_consoleForm._trigger.setText(trigger);
	}

	/**
	 * Sets the app started.
	 * 
	 * @param time
	 *            the new app started
	 */
	void setAppStarted(Date time)
	{
		if (time != null)
			_consoleForm._appStartTime.setText(_dateTimeFormat.format(time));
	}

	/**
	 * Sets the app stopped.
	 * 
	 * @param time
	 *            the new app stopped
	 */
	void setAppStopped(Date time)
	{
		if (time != null)
			_consoleForm._appStopTime.setText(_dateTimeFormat.format(time));
	}

	/**
	 * Sets the app restart count.
	 * 
	 * @param total
	 *            the total
	 * @param count
	 *            the count
	 */
	void setAppRestartCount(int total, int count)
	{
		if (count > 0)
			_consoleForm._count.setText(total + "[" + count + "]");
	}

	/**
	 * Sets the wrapper started.
	 * 
	 * @param time
	 *            the new wrapper started
	 */
	void setWrapperStarted(Date time)
	{
		if (time != null)
			_consoleForm._wStartTime.setText(_dateTimeFormat.format(time));
	}

	/**
	 * Sets the app threads.
	 * 
	 * @param count
	 *            the new app threads
	 */
	void setAppThreads(int count)
	{
		if (count > 0)
			_consoleForm._threads.setText("" + count);
		else
			_consoleForm._threads.setText("?");

	}

	/**
	 * Sets the app handles.
	 * 
	 * @param count
	 *            the new app handles
	 */
	void setAppHandles(int count)
	{
		if (count > 0)
			_consoleForm._handles.setText("" + count);
		else
			_consoleForm._handles.setText("?");
	}

	/**
	 * Sets the app memory.
	 * 
	 * @param bytes
	 *            the new app memory
	 */
	void setAppMemory(long pBytes, long vBytes)
	{
		String sPBytes = pBytes > 0 ? _byteFormat.format(pBytes) : "?";
		String sVBytes = vBytes > 0 ? _byteFormat.format(vBytes) : "?";
		_consoleForm._memory.setText(sPBytes+"["+sVBytes+"]");
	}

	/**
	 * Sets the app cpu.
	 * 
	 * @param count
	 *            the new app cpu
	 */
	void setAppCpu(int count)
	{
		if (count >= 0)
			_consoleForm._cpu.setText("" + count);
		else
			_consoleForm._cpu.setText("?");

	}

	/**
	 * Sets the exit code.
	 * 
	 * @param code
	 *            the new exit code
	 */
	void setExitCode(int code)
	{
		if (code >= 0)
			_consoleForm._exitCode.setText("" + code);
	}

	/**
	 * Sets the wrapper type.
	 * 
	 * @param type
	 *            the new wrapper type
	 */
	void setWrapperType(String type)
	{
		_consoleForm._wrapperType.setText(type);
	}

	/**
	 * Sets the condition.
	 * 
	 * @param active
	 *            the new condition
	 */
	void setCondition(boolean active)
	{
		if (active)
		{
			_consoleForm._condition.setText("");
			_consoleForm._condition.setIcon(_okIcon);
		}
		else
		{
			_consoleForm._condition.setText("-");
			_consoleForm._condition.setIcon(null);
		}

	}

	/**
	 * Sets the timer.
	 * 
	 * @param active
	 *            the new timer
	 */
	void setTimer(boolean active)
	{
		if (active)
		{
			_consoleForm._timer.setText("");
			_consoleForm._timer.setIcon(_okIcon);
		}
		else
		{
			_consoleForm._timer.setText("-");
			_consoleForm._timer.setIcon(null);
		}

	}

	private void initInput()
	{

		_consoleForm._input.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				// process may have not yet been started or it may have been
				// stopped
				try
				{
					if (_trayIcon._process == null || !_trayIcon._process.hasOutput())
					{
						_consoleForm._input.setText("No input possible");
						_consoleForm._input.selectAll();
						return;
					}
					String txt = _consoleForm._input.getText();
					_trayIcon._process.writeOutput(txt);
					_consoleForm._input.selectAll();
				}
				catch (Throwable ex)
				{
					ex.printStackTrace();
				}
			}
		});

	}

	private void initOutput()
	{

		executor.execute(new Runnable()
		{

			public void run()
			{
				_trayIcon._process.startDrain();
				while (!stop)
				{
					if (!_outputPaused)
					{
						_trayIcon.showState(_trayIcon._process.getState());
						String line;
						while ((line = _trayIcon._process.readDrainLine()) != null)
							addLine(line);
					}
					try
					{
						Thread.sleep(500);
					}
					catch (InterruptedException e)
					{
						e.printStackTrace();
					}
				}
				_trayIcon._process.stopDrain();

			}

		});

	}

	private void filterOutput()
	{
		executor.execute(new Runnable()
		{
			public void run()
			{
				_outputLock.lock();
				try
				{
					_consoleForm._output.getDocument().remove(0, _consoleForm._output.getDocument().getLength());
				}
				catch (BadLocationException e)
				{
					e.printStackTrace();
				}
				for (String line : _outputLines)
					if (_outputFilter == null || line.contains(_outputFilter))
						addToTextArea(line);
				_outputLock.unlock();
			}
		});
	}

	private void clearOutput()
	{
		_outputLock.lock();
		_outputLines.clear();
		filterOutput();
		_outputLock.unlock();

	}

	private void addLine(String line)
	{
		_outputLock.lock();
		_outputLines.addLast(line);
		if (_outputLines.size() > maxLines)
			_outputLines.removeFirst();
		if (_outputFilter == null || line.contains(_outputFilter))
			addToTextArea(line);
		_outputLock.unlock();
	}

	private void addToTextArea(String line)
	{
		JTextArea textArea = _consoleForm._output;
		textArea.append(line + "\n");
		if (textArea.getLineCount() > maxLines)
		{
			Element root = textArea.getDocument().getDefaultRootElement();
			Element firstLine = root.getElement(0);

			try
			{
				textArea.getDocument().remove(0, firstLine.getEndOffset());
			}
			catch (Exception ble)
			{
				System.out.println(ble.getMessage());
			}
		}
		textArea.setCaretPosition(textArea.getDocument().getLength());

	}

	/**
	 * Close.
	 */
	public void close()
	{
		stop = true;
		this.setVisible(false);
		this.dispose();
	}

	/**
	 * The Class WindowEventHandler.
	 */
	class WindowEventHandler extends WindowAdapter
	{

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent
		 * )
		 */
		public void windowClosing(WindowEvent evt)
		{
			close();
			_trayIcon.closeConsole();
		}
	}

}
