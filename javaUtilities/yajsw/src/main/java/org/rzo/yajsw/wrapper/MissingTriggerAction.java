package org.rzo.yajsw.wrapper;

import java.util.concurrent.Executor;
import java.util.logging.Logger;

import org.rzo.yajsw.util.Cycler;

class MissingTriggerAction implements TriggerAction
{
	volatile private Cycler	_cycler;
	volatile private int	_counter	= 0;
	private int				_count;
	private TriggerAction[]	_actions;
	Executor				_executor;

	MissingTriggerAction(Executor executor, long period, int count, TriggerAction[] actions, final boolean autoStop, final Logger logger)
	{
		_count = count;
		_executor = executor;
		_actions = actions;
		_cycler = new Cycler(period, period, executor, new Runnable()
		{
			public void run()
			{
				// System.out.println("missing trigger "+_counter + " "+_count);
				if (_counter < _count)
				{
					if (autoStop)
						_cycler.stop();
					for (final TriggerAction action : _actions)
						if (action != null)
						{
							// run the action in a separate thread, because on
							// restart the cycler thread will be interrupted
							_executor.execute(new Runnable()
							{
								public void run()
								{
									// TODO add logger
									logger.info("missing trigger executed, found # " + _counter + " triggers during check period");
									action.execute("");
								}
							});
						}
				}
				else
					_counter = 0;
			}
		});
	}

	void start()
	{
		_cycler.start();
	}

	void stop()
	{
		_cycler.stop();
	}

	public Object execute(String line)
	{
		_counter++;
		return null;
	}

}
