package org.rzo.yajsw.srvmgr.client;

import java.io.Serializable;

public class Host implements Serializable, Comparable<Host>
{
	String _name;
	int _port;
	boolean _included = false;
	String _state = "UNKNOWN";
	
	Host()
	{
		_state = "UNKOWN";
	}
	
	public Host(String name, int port)
	{
		_name = name;
		_port = port;
		_state = "UNKOWN";
	}

	public String getName()
	{
		return _name;
	}

	public void setName(String name)
	{
		//_name = name;
	}

	public boolean isIncluded()
	{
		return _included;
	}

	public String getState()
	{
		return _state;
	}

	public void setIncluded(boolean included)
	{
		_included = included;
	}

	public void setState(String state)
	{
		_state = state;
	}

	public int compareTo(Host o)
	{
		return _name.compareTo(o.getName());
	}

	public int getPort()
	{
		return _port;
	}

	public void setPort(int port)
	{
		_port = port;
	}
	

}
