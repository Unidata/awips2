package org.rzo.yajsw.condition;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import org.jboss.netty.logging.InternalLogger;
import org.rzo.yajsw.config.YajswConfigurationImpl;
import org.rzo.yajsw.script.Script;
import org.rzo.yajsw.script.ScriptFactory;
import org.rzo.yajsw.wrapper.WrappedProcess;

public class Condition
{
	Script					_script;

	/** The _config. */
	YajswConfigurationImpl	_config;

	/** The _wp. */
	WrappedProcess			_wp;

	/** The _has trigger. */
	boolean					_hasTrigger	= false;

	/** The _triggered. */
	boolean					_triggered	= false;

	long					_period		= -1;

	static Timer			_timer		= new Timer("yajsw.condition");

	InternalLogger					_log;
	
	boolean _debug = false;

	public Condition(YajswConfigurationImpl config, WrappedProcess wrappedProcess, InternalLogger log)
	{
		_config = config;
		_wp = wrappedProcess;
		_log = log;
	}

	public void init()
	{
		String fileName = _config.getString("wrapper.condition.script");
		if (fileName == null)
			return;
		File f = new File(fileName);
		if (!f.exists() || !f.isFile())
		{
			try
			{
				System.out.println("file not found -> ignoring condition script " + f.getCanonicalPath());
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		}
		_debug = _config.getBoolean("wrapper.debug", false);
		List args = _config.getList("wrapper.condition.script.args", new ArrayList());
		String[] argsArr = new String[args.size()];
		for (int i = 0; i < argsArr.length; i++)
			argsArr[i] = args.get(i).toString();
		_script = ScriptFactory.createScript(fileName, "condition", _wp, argsArr, _log, 0, _config.getString("wrapper.script.encoding"), _config.getBoolean("wrapper.script.reload", false), _debug);
		_hasTrigger = _script != null;
		_period = _config.getLong("wrapper.condition.cycle", -1) * 1000;
	}

	public void stop()
	{
		_timer.cancel();
		_triggered = false;
	}

	public boolean isHasTrigger()
	{
		return _hasTrigger;
	}

	public boolean isTriggered()
	{
		return _triggered;
	}

	public void start()
	{
		_triggered = true;
		if (_period > 0)
			_timer.schedule(new TimerTask()
			{

				@Override
				public void run()
				{
					_script.execute();
				}

			}, new Date(), _period);
		else
			_timer.schedule(new TimerTask()
			{

				@Override
				public void run()
				{
					_script.execute();
				}

			}, new Date());

	}

}
