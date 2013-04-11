package org.rzo.netty.ahessian.rpc.message;

import java.util.Map;

import org.jboss.netty.channel.Channel;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.io.OutputStreamEncoder;
import org.rzo.netty.ahessian.session.ServerSessionFilter;
import org.rzo.netty.ahessian.session.Session;


/**
 * message used for requesting a remote invocation
 */
public class HessianRPCCallMessage implements Constants, GroupedMessage
{
	
	/** The _method. */
	String _method;
	
	/** The _args. */
	Object[] _args;
	
	/** The _headers. */
	Map<Object, Object> _headers;
	transient OutputStreamEncoder _outputStreamEncoder;
	boolean _hasSessionFilter = false;
	transient boolean _isServer = false;
	transient Session _session;
	
	/**
	 * Gets the headers.
	 * 
	 * @return the headers
	 */
	public Map<Object, Object> getHeaders()
	{
		return _headers;
	}


	/**
	 * Instantiates a new hessian rpc call message.
	 * 
	 * @param method the method
	 * @param args the args
	 * @param headers the headers
	 */
	public HessianRPCCallMessage(String method, Object[] args, Map<Object, Object> headers, OutputStreamEncoder outputStreamEncoder)
	{
		_method = method;
		_args = args;
		_headers = headers;
		_outputStreamEncoder = outputStreamEncoder;
	}


	/**
	 * Gets the method.
	 * 
	 * @return the method
	 */
	public String getMethod()
	{
		return _method;
	}

	/**
	 * Gets the args.
	 * 
	 * @return the args
	 */
	public Object[] getArgs()
	{
		return _args;
	}
	
	public String toString()
	{
		StringBuffer sb = new StringBuffer();
		sb.append("HessianRPCCallMessage");
		if (_headers != null && _headers.get(CALL_ID_HEADER_KEY) != null)
		{
			sb.append('#');
			sb.append(_headers.get(CALL_ID_HEADER_KEY));
		}
		sb.append('[');
		sb.append(_method);
		sb.append('(');
		if (_args != null)
		for (int i=0; i<_args.length; i++)
		{
			sb.append(_args[i].toString());
			if (i != _args.length-1)
				sb.append(',');
		}
		sb.append(")]");
		return sb.toString();
	}
	
	public boolean isValid()
	{
		boolean result = false;
		if (!_hasSessionFilter || !_isServer)
			result = (_outputStreamEncoder != null && _outputStreamEncoder.getBuffer() != null && _outputStreamEncoder.getBuffer().getContext() != null && _outputStreamEncoder.getBuffer().getContext().getChannel().isConnected());
		else if (_outputStreamEncoder != null && _outputStreamEncoder.getBuffer() != null && _outputStreamEncoder.getBuffer().getContext() != null)
			{
				ServerSessionFilter session = ServerSessionFilter.getServerSessionFilter(_outputStreamEncoder.getBuffer().getContext());
				result =  session == null || session.isValid();
			}
		return result;
	}
	
	public Channel getChannel()
	{
		if (_outputStreamEncoder != null && _outputStreamEncoder.getBuffer() != null && _outputStreamEncoder.getBuffer().getContext() != null)
			return _outputStreamEncoder.getBuffer().getContext().getChannel();
		return null;
	}


	public void setHasSessionFilter(boolean hasSessionFilter)
	{
		_hasSessionFilter = hasSessionFilter;
		_headers.put(HAS_SESSION_FILTER_HEADER_KEY, _hasSessionFilter);
	}


	public void setServer(boolean isServer)
	{
		_isServer = isServer;
	}


	public Integer getGroup()
	{
		if (_headers == null || _headers.get(GROUP_HEADER_KEY) == null)
			return 0;
		return (Integer) _headers.get(GROUP_HEADER_KEY);
	}
	
	public void setSession(Session session)
	{
		_session = session;
	}
	
	public Session getSession()
	{
		return _session;
	}

}
