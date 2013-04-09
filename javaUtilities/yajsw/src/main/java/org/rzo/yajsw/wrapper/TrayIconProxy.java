package org.rzo.yajsw.wrapper;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TrayIconProxy
{
	List<TrayIconMessage>	messages	= Collections.synchronizedList(new ArrayList<TrayIconMessage>());
	volatile String			_inquireMessage;
	volatile String			_inquireResponse;
	volatile Color			_userColor;

	public enum Types
	{
		ERROR, INFO, WARNING, MESSAGE
	};

	public void error(String caption, String message)
	{
		synchronized (messages)
		{
		messages.add(new TrayIconMessage(Types.ERROR, caption, message));
		}
	}

	public void info(String caption, String message)
	{
		synchronized (messages)
		{
			messages.add(new TrayIconMessage(Types.INFO, caption, message));
		}
	}

	public void warning(String caption, String message)
	{
		synchronized (messages)
		{
			messages.add(new TrayIconMessage(Types.WARNING, caption, message));
		}
	}

	public void message(String caption, String message)
	{
		synchronized (messages)
		{
		messages.add(new TrayIconMessage(Types.MESSAGE, caption, message));
		}
	}

	public String inquire(String message)
	{
		String result = null;
		if (_inquireResponse != null)
		{
			result = _inquireResponse;
			System.out.println("got response for inquire ");
			_inquireResponse = null;
		}
		else
		{
			System.out.println("waiting for inquire " + message);
			_inquireMessage = message;
			result = null;
		}
		return result;
	}

	public String[][] toArrayAndClear()
	{
		String[][] result = null;
		synchronized (messages)
		{
			if (messages.size() == 0)
				return null;
			result = new String[messages.size()][];
			int i = 0;
			for (TrayIconMessage message : messages)
				result[i++] = message.toStringArray();
			messages.clear();
		}
		return result;
	}
	
	public void setUserColor(Color color)
	{
		_userColor = color;
	}
	
	public Color getUserColor()
	{
		return _userColor;
	}
	

}
