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
package org.rzo.yajsw.quartz;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;

import org.quartz.CronExpression;
import org.quartz.CronTrigger;
import org.quartz.JobDataMap;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.SchedulerFactory;
import org.quartz.SimpleTrigger;
import org.quartz.Trigger;
import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.wrapper.WrappedProcess;

// TODO: Auto-generated Javadoc
/**
 * The Class Timer.
 */
public class Timer
{

	/** The _config. */
	YajswConfigurationImpl	_config;

	/** The _wp. */
	WrappedProcess			_wp;

	/** The _scheduler. */
	static Scheduler		_scheduler;

	/** The _cron start. */
	MyCronTrigger			_cronStart;

	/** The _cron stop. */
	MyCronTrigger			_cronStop;

	/** The _cron restart. */
	MyCronTrigger			_cronRestart;

	/** The _simple start. */
	MySimpleTrigger			_simpleStart;

	/** The _simple stop. */
	MySimpleTrigger			_simpleStop;

	/** The _simple restart. */
	MySimpleTrigger			_simpleRestart;

	/** The _has trigger. */
	boolean					_hasTrigger		= false;

	/** The _start immediate. */
	boolean					_startImmediate	= true;

	/** The _triggered. */
	boolean					_triggered		= false;

	/**
	 * Instantiates a new timer.
	 * 
	 * @param config
	 *            the config
	 * @param wp
	 *            the wp
	 */
	public Timer(YajswConfigurationImpl config, WrappedProcess wp)
	{
		_config = config;
		_wp = wp;
	}

	/**
	 * Inits the.
	 */
	public synchronized void init()
	{
		for (Iterator keys = _config.getKeys("wrapper.timer"); keys.hasNext();)
		{
			String key = (String) keys.next();
			if (key.contains(".simple."))
			{
				if (key.contains(".START."))
				{
					if (_simpleStart == null)
						_simpleStart = getSimpleTrigger(key);
				}
				else if (key.contains(".STOP."))
				{
					if (_simpleStop == null)
						_simpleStop = getSimpleTrigger(key);
				}
				else if (key.contains(".RESTART."))
				{
					if (_simpleRestart == null)
						_simpleRestart = getSimpleTrigger(key);
				}
				else
					System.out.println("Cannot interpret timer property: " + key);
			}
			else if (key.contains(".cron."))
			{
				if (key.contains(".START"))
					_cronStart = getCronTrigger(key);
				else if (key.contains(".STOP"))
					_cronStop = getCronTrigger(key);
				else if (key.contains(".RESTART"))
					_cronRestart = getCronTrigger(key);
				else
					System.out.println("Cannot interpret timer property: " + key);
			}
			else
			{
				System.out.println("Cannot interpret timer property: " + key);
			}
		}

	}

	/**
	 * Gets the simple trigger.
	 * 
	 * @param key
	 *            the key
	 * 
	 * @return the simple trigger
	 */
	private MySimpleTrigger getSimpleTrigger(String key)
	{
		JobDetail jobDetail = new JobDetail();
		JobDataMap jobDataMap = new JobDataMap();
		jobDataMap.put("process", _wp);
		jobDetail.setJobDataMap(jobDataMap);

		Class jobClass = getJobClass(key);
		if (jobClass == null)
			return null;
		jobDetail.setJobClass(jobClass);
		jobDetail.setName(key);

		MySimpleTrigger trigger = new MySimpleTrigger(jobDetail);
		Date startTime = getStartTime(key);
		if (startTime != null)
		{
			trigger.setStartTime(startTime);
		}
		int repeatCount = getRepeatCount(key);
		if (repeatCount > 0)
			trigger.setRepeatCount(repeatCount);
		int interval = getInterval(key);
		if (interval > 0)
			trigger.setRepeatInterval(interval * 1000);
		_hasTrigger = true;

		if (trigger != null)
			trigger.setName(key);
		_startImmediate = false; // getStartTime will always return a date.
		// per default the current time.
		trigger.setMisfireInstruction(SimpleTrigger.MISFIRE_INSTRUCTION_FIRE_NOW);
		return trigger;
	}

	/**
	 * Gets the interval.
	 * 
	 * @param key
	 *            the key
	 * 
	 * @return the interval
	 */
	private int getInterval(String key)
	{
		return _config.getInt(key.substring(0, key.lastIndexOf(".")) + ".INTERVAL", SimpleTrigger.REPEAT_INDEFINITELY);
	}

	/**
	 * Gets the repeat count.
	 * 
	 * @param key
	 *            the key
	 * 
	 * @return the repeat count
	 */
	private int getRepeatCount(String key)
	{
		return _config.getInt(key.substring(0, key.lastIndexOf(".")) + ".COUNT", -1);
	}

	/**
	 * Gets the start time.
	 * 
	 * @param key
	 *            the key
	 * 
	 * @return the start time
	 */
	private Date getStartTime(String key)
	{
		String str = _config.getString(key.substring(0, key.lastIndexOf(".")) + ".FIRST");
		if (str == null)
			return new Date();
		SimpleDateFormat df = null;
		if (str.contains(" "))
			df = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
		else
			df = new SimpleDateFormat("HH:mm:ss");
		try
		{
			return df.parse(str);
		}
		catch (ParseException e)
		{
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Gets the job class.
	 * 
	 * @param key
	 *            the key
	 * 
	 * @return the job class
	 */
	private Class getJobClass(String key)
	{
		if (key.contains(".RESTART"))
			return RestartJob.class;
		else if (key.contains(".STOP"))
			return StopJob.class;
		else if (key.contains(".START"))
			return StartJob.class;
		return null;
	}

	/**
	 * Gets the cron trigger.
	 * 
	 * @param key
	 *            the key
	 * 
	 * @return the cron trigger
	 */
	private MyCronTrigger getCronTrigger(String key)
	{
		JobDetail jobDetail = new JobDetail();
		JobDataMap jobDataMap = new JobDataMap();
		jobDataMap.put("process", _wp);
		jobDetail.setJobDataMap(jobDataMap);
		jobDetail.setName(key);

		Class jobClass = getJobClass(key);
		if (jobClass == null)
			return null;
		jobDetail.setJobClass(jobClass);

		MyCronTrigger trigger = new MyCronTrigger(jobDetail);
		CronExpression cronExpression = getCronExpression(key);
		if (cronExpression != null)
		{
			trigger.setCronExpression(cronExpression);
			if (jobClass.equals(StartJob.class))
				_startImmediate = false;
			_hasTrigger = true;
		}
		else
		{
			return null;
		}

		trigger.setName(key);
		trigger.setMisfireInstruction(trigger.MISFIRE_INSTRUCTION_FIRE_ONCE_NOW);
		return trigger;
	}

	/**
	 * Gets the cron expression.
	 * 
	 * @param key
	 *            the key
	 * 
	 * @return the cron expression
	 */
	private CronExpression getCronExpression(String key)
	{
		String str = _config.getString(key);
		if (str == null)
		{
			return null;
		}
		try
		{
			return new CronExpression(str);
		}
		catch (ParseException e)
		{
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Start.
	 */
	public synchronized void start()
	{
		if (!_hasTrigger)
			return;
		if (getScheduler() == null)
			return;
		try
		{
			if (!_scheduler.isStarted())
				_scheduler.start();
		}
		catch (SchedulerException e)
		{
			e.printStackTrace();
			return;
		}

		if (_cronStart != null)
			startTrigger(_cronStart, _cronStart.getJobDetail());
		if (_cronStop != null)
			startTrigger(_cronStop, _cronStop.getJobDetail());
		if (_cronRestart != null)
			startTrigger(_cronRestart, _cronRestart.getJobDetail());
		if (_simpleStart != null)
			startTrigger(_simpleStart, _simpleStart.getJobDetail());
		if (_simpleStop != null)
			startTrigger(_simpleStop, _simpleStop.getJobDetail());
		if (_simpleRestart != null)
			startTrigger(_simpleRestart, _simpleRestart.getJobDetail());
		_triggered = true;
	}

	/**
	 * Gets the scheduler.
	 * 
	 * @return the scheduler
	 */
	private Scheduler getScheduler()
	{
		if (_scheduler == null)
		{
			SchedulerFactory schedFact = new org.quartz.impl.StdSchedulerFactory();
			try
			{
				_scheduler = schedFact.getScheduler();
			}
			catch (SchedulerException e)
			{
				e.printStackTrace();
				_scheduler = null;
			}
		}
		return _scheduler;
	}

	/**
	 * Start trigger.
	 * 
	 * @param trigger
	 *            the trigger
	 * @param jobDetail
	 *            the job detail
	 */
	private void startTrigger(Trigger trigger, JobDetail jobDetail)
	{
		if (trigger != null)
			try
			{
				_scheduler.scheduleJob(jobDetail, trigger);
			}
			catch (SchedulerException e)
			{
				e.printStackTrace();
			}
	}

	/**
	 * Stop.
	 */
	public void stop()
	{
		if (!_hasTrigger)
			return;
		stopTrigger(_cronStart);
		stopTrigger(_cronStop);
		stopTrigger(_cronRestart);
		stopTrigger(_simpleStart);
		stopTrigger(_simpleStop);
		stopTrigger(_simpleRestart);

	}

	/**
	 * Stop trigger.
	 * 
	 * @param trigger
	 *            the trigger
	 */
	private synchronized void stopTrigger(Trigger trigger)
	{
		try
		{
			_scheduler.shutdown();
		}
		catch (SchedulerException e)
		{
			e.printStackTrace();
			return;
		}
		_triggered = false;
	}

	/**
	 * Checks if is triggered.
	 * 
	 * @return true, if is triggered
	 */
	public boolean isTriggered()
	{
		return _triggered;
	}

	/**
	 * Checks if is start immediate.
	 * 
	 * @return true, if is start immediate
	 */
	public boolean isStartImmediate()
	{
		return _startImmediate;
	}

	/**
	 * Checks if is checks for trigger.
	 * 
	 * @return true, if is checks for trigger
	 */
	public boolean isHasTrigger()
	{
		return _hasTrigger;
	}

	/**
	 * The Class MyCronTrigger.
	 */
	class MyCronTrigger extends CronTrigger
	{

		/** The _job detail. */
		JobDetail	_jobDetail;

		/**
		 * Instantiates a new my cron trigger.
		 * 
		 * @param jobDetail
		 *            the job detail
		 */
		MyCronTrigger(JobDetail jobDetail)
		{
			_jobDetail = jobDetail;
		}

		/**
		 * Gets the job detail.
		 * 
		 * @return the job detail
		 */
		JobDetail getJobDetail()
		{
			return _jobDetail;
		}
	}

	/**
	 * The Class MySimpleTrigger.
	 */
	class MySimpleTrigger extends SimpleTrigger
	{

		/** The _job detail. */
		JobDetail	_jobDetail;

		/**
		 * Instantiates a new my simple trigger.
		 * 
		 * @param jobDetail
		 *            the job detail
		 */
		MySimpleTrigger(JobDetail jobDetail)
		{
			_jobDetail = jobDetail;
		}

		/**
		 * Gets the job detail.
		 * 
		 * @return the job detail
		 */
		JobDetail getJobDetail()
		{
			return _jobDetail;
		}
	}

}
