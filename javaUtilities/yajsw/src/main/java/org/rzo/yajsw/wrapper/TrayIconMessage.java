package org.rzo.yajsw.wrapper;

import org.rzo.yajsw.wrapper.TrayIconProxy.Types;

public class TrayIconMessage
{
	Types	_type;
	String	_caption;
	String	_message;

	public TrayIconMessage(Types type, String caption, String message)
	{
		_type = type;
		_caption = caption;
		_message = message;
	}

	public String[] toStringArray()
	{
		return new String[]
		{ _type.toString(), _caption, _message };
	}

}
