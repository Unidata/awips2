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
package org.rzo.yajsw.log;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import org.rzo.yajsw.log.MyFileHandler.FileChangeListner;

// TODO: Auto-generated Javadoc
/**
 * The Class DateFileHandler.
 */
public class DateFileHandler extends Handler
{

	/** The _handler. */
	volatile MyFileHandler			_handler;

	/** The _end date. */
	volatile long				_endDate;

	/** The _pattern. */
	volatile String				_pattern;

	/** The _limit. */
	volatile int					_limit;

	/** The _count. */
	volatile int					_count;

	/** The _append. */
	volatile boolean				_append;

	/** The format. */
	final SimpleDateFormat	format	= new SimpleDateFormat("yyyyMMdd");

	/** The _init. */
	volatile boolean				_init	= false;
	
	volatile boolean _rollDate = false;
	
	volatile long _startDate = System.currentTimeMillis();
	
	//volatile LinkedList<File> _previousFiles = new LinkedList<File>();
	//volatile LinkedList<File> _currentFiles = new LinkedList<File>();
	volatile LinkedList<String> _previousDates = new LinkedList<String>();
	volatile String _currentDate = null;
	volatile int _maxDays = -1;

	/**
	 * Instantiates a new date file handler.
	 * 
	 * @param pattern
	 *            the pattern
	 * @param limit
	 *            the limit
	 * @param count
	 *            the count
	 * @param append
	 *            the append
	 * 
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 * @throws SecurityException
	 *             the security exception
	 */
	public DateFileHandler(String pattern, int limit, int count, boolean append, boolean rollDate, PatternFormatter fileFormatter, Level logLevel, String encoding, int maxDays)
	{
		_pattern = pattern;
		_limit = limit;
		_count = count;
		_append = append;
		_rollDate = rollDate;
		_maxDays = maxDays;
		_init = true;
		if (encoding != null)
			try
			{
				setEncoding(encoding);
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
		setFormatter(fileFormatter);
		setLevel(logLevel);
		findPreviousDates();
		rotateDate();
		//checkFileCount();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.logging.Handler#close()
	 */
	@Override
	public void close() throws SecurityException
	{
		_handler.close();

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.logging.Handler#flush()
	 */
	@Override
	public void flush()
	{
		_handler.flush();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.logging.Handler#publish(java.util.logging.LogRecord)
	 */
	@Override
	public void publish(LogRecord record)
	{
		if (_rollDate)
		{
		if (_endDate < record.getMillis())
			rotateDate();
		if (System.currentTimeMillis() - _startDate > 25*60*60*1000)
		{
			String msg = record.getMessage();
			record.setMessage("missed file rolling at: "+new Date(_endDate)+"\n"+msg);
		}
		}
		_handler.publish(record);

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.logging.Handler#setFormatter(java.util.logging.Formatter)
	 */
	@Override
	public void setFormatter(Formatter newFormatter)
	{
		super.setFormatter(newFormatter);
		if (_handler != null)
			_handler.setFormatter(newFormatter);
	}

	/**
	 * Rotate date.
	 */
	private void rotateDate()
	{
		_startDate = System.currentTimeMillis();
		if (_handler != null)
			_handler.close();
		_previousDates.addLast(_currentDate);
		cleanupDates();
		_currentDate = format.format(new Date());
		String pattern = _pattern.replace("%d", _currentDate);
		try
		{
			File dd = MyFileHandler.generate(pattern, 0, 0, _count);
			if (!dd.getParentFile().exists())
				dd.getParentFile().mkdirs();
		}
		catch (IOException e1)
		{
		}
		Calendar next = Calendar.getInstance(); // current date
		// begin of next date
		next.set(Calendar.HOUR_OF_DAY, 0);
		next.set(Calendar.MINUTE, 0);
		next.set(Calendar.SECOND, 0);
		next.set(Calendar.MILLISECOND, 0);
		next.add(Calendar.DATE, 1);
		_endDate = next.getTimeInMillis();

		try
		{
			_handler = new MyFileHandler(pattern, _limit, _count, _append);
			if (_init)
			{
				_handler.setEncoding(this.getEncoding());
				_handler.setErrorManager(this.getErrorManager());
				_handler.setFilter(this.getFilter());
				_handler.setFormatter(this.getFormatter());
				_handler.setLevel(this.getLevel());
				//findFiles();
				//_currentFiles.clear();
				//addFiles(_handler.getCurrentFiles());
				/*
				_handler.setNewFileListner(new FileChangeListner()
				{

					public void fileChange(File file, boolean added)
					{
						System.out.println("file change: "+added+ " "+file.getName());
						if (added)
							addFile(file);
					}
					
				});
				*/
				
			}
		}
		catch (SecurityException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (IOException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private void cleanupDates()
	{
		if (_maxDays >= 0)
			while (_previousDates.size() > _maxDays)
			{
				String toCleanup = _previousDates.removeFirst();
				cleanupDate(toCleanup);
			}
	}
	
	private void cleanupDate(String date)
	{
		String pattern = _pattern.replace("%d", date);
		File f;
		for (int unique=0; unique<_count; unique++)
		{
			try
			{
				f = MyFileHandler.generate(pattern, 0, unique, _count);
				if (!f.exists())
					break;
			}
			catch (IOException e1)
			{
				// should not happen, but keep silent for now
			}
			for (int generation=0; generation<_count; generation++)
			{
			try
			{
				f = MyFileHandler.generate(pattern, generation, unique, _count);
				if (f.exists())
				{
					f.delete();
				}
				else
					break;
			}
			catch (IOException e)
			{
				// should not happen, but keep silent for now
			}
			}
		}
		// cleanup parent folder if necessary
		try
		{
			// remove old lock files
			f = MyFileHandler.generate(pattern+".lck", 0, 0, _count);
			f.delete();
			f = MyFileHandler.generate(pattern, 0, 0, _count);
			while (!f.getName().contains(date))
				f = f.getParentFile();
			if (f.isDirectory())
				f.delete();
		}
		catch (IOException e)
		{
			// should not happen, but keep silent for now
		}		
	}
	
	private void findPreviousDates()
	{
		if (_maxDays < 0)
			return;
		Calendar date = Calendar.getInstance();
		// service may not run daily
		// for performance: do not scan all files.
		// service may not run every day, but we assume it will run at least once a year
		int scanDays = Math.max(365, _maxDays);
		for (int i = 0; i<scanDays; i++)
		{
			date.add(Calendar.DAY_OF_MONTH, -1);
			String cDate = format.format(date.getTime());
			String pattern = _pattern.replace("%d", cDate);
			boolean dateFound = false;

				File f;
				try
				{
					f = MyFileHandler.generate(pattern, 0, 0, _count);
					if (f.exists())
					{
						dateFound = true;
					}
				}
				catch (IOException e)
				{
					// should not happen, but keep silent for now
				}
				
		
			if (dateFound)
				_previousDates.addFirst(cDate);
		}
		
	}
	
/*	private void checkFileCount()
	{
		while (_previousFiles.size() > 0 && _previousFiles.size()+_currentFiles.size()>_count)
		{
			File f = _previousFiles.removeLast();
			if (f.exists())
				f.delete();
		}
	}
	
	private void addFile(File f)
	{
		if (!_currentFiles.contains(f))
			_currentFiles.addFirst(f);
		checkFileCount();
	}
	
	private void addFiles(LinkedList<File> files)
	{
		while (!files.isEmpty())
			addFile(files.removeLast());			
	}
	
	private void findFiles() throws IOException
	{
		Calendar date = Calendar.getInstance();
		while (_previousFiles.size() < _count)
		{
			date.add(Calendar.DAY_OF_MONTH, -1); 
			String pattern = _pattern.replace("%d", format.format(date.getTime()));
			for (int unique=0; unique<_count; unique++)
			{
				for (int generation=0; generation<_count; generation++)
				{
				File f = MyFileHandler.generate(pattern, generation, unique, _count);
				if (f.exists() && !_previousFiles.contains(f))
				{
					_previousFiles.addLast(f);
				}
				else
				{
					break;
				}
				}
				File f = MyFileHandler.generate(pattern, 0, unique, _count);
				if (!f.exists() || _previousFiles.contains(f))
					break;
			}
			File f = MyFileHandler.generate(pattern, 0, 0, _count);
			if (!f.exists())
				break;
		}

	}
	*/

}
