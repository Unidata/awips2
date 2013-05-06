package org.rzo.netty.ahessian.rpc.message;

import org.jboss.netty.channel.Channel;
import org.rzo.netty.ahessian.Constants;

/**
 * reply message for a remote invocation
 */
public class HessianRPCReplyMessage implements Constants, GroupedMessage
{
	
	/** The _value. */
	Object _value;
	
	/** The _fault. */
	Throwable _fault;
	
	Long _callId;
	Object[] _callbackArgs;
	Boolean _callbackDone;
	Long _callbackId;
	String _callbackMethod;
	Integer _group;
	Boolean _completed;
	int _headersCount = 0;
	
	
	
	transient HessianRPCCallMessage _call;
	
	/**
	 * Instantiates a new hessian rpc reply message.
	 * 
	 * @param value the value
	 * @param fault the fault
	 * @param headers the headers
	 * @param channel the channel
	 */
	public HessianRPCReplyMessage(Object value, Object fault, HessianRPCCallMessage call)
	{
		_value = value;
		_fault = (Throwable) fault;
		_call = call;
	}

	public HessianRPCReplyMessage()
	{
		// TODO Auto-generated constructor stub
	}

	/**
	 * Gets the value.
	 * 
	 * @return the value
	 */
	public Object getValue()
	{
		return _value;
	}

	/**
	 * Gets the fault.
	 * 
	 * @return the fault
	 */
	public Throwable getFault()
	{
		return _fault;
	}

	public String toString()
	{
		StringBuffer sb = new StringBuffer();
		sb.append("HessianRPCReplyMessage");
			sb.append('#');
			sb.append(_callId);
		return sb.toString();
	}
	
	public Channel getChannel()
	{
			return _call.getChannel();
	}
	
	public boolean isValid()
	{
		return _call.isValid();
	}

	public Long getCallId()
	{
		return _callId;
	}

	public Object[] getCallbackArgs()
	{
		return _callbackArgs;
	}

	public Boolean getCallbackDone()
	{
		return _callbackDone;
	}

	public Long getCallbackId()
	{
		return _callbackId;
	}

	public String getCallbackMethod()
	{
		return _callbackMethod;
	}

	public Integer getGroup()
	{
		return _group;
	}

	public void setCallId(Long callId)
	{
		if (callId != null)
		{
			_callId = callId;
			_headersCount++;
		}
	}

	public void setCallbackArgs(Object[] callbackArgs)
	{
		if (callbackArgs != null)
		{
			_callbackArgs = callbackArgs;
			_headersCount++;
		}
	}

	public void setCallbackDone(Boolean callbackDone)
	{
		if (callbackDone != null)
		{
			_callbackDone = callbackDone;
			_headersCount++;
		}
	}

	public void setCallbackId(Long callbackId)
	{
		if (callbackId != null)
		{
			_callbackId = callbackId;
			_headersCount++;
		}
	}

	public void setCallbackMethod(String callbackMethod)
	{
		if (callbackMethod != null)
		{
			_callbackMethod = callbackMethod;
			_headersCount++;
		}
	}

	public void setGroup(Integer group)
	{
		if (group != null)
		{
			_group = group;
			_headersCount++;
		}
	}

	public void setValue(Object value)
	{
		_value = value;
	}

	public void setFault(Throwable fault)
	{
		_fault = fault;
	}

	public Boolean getCompleted()
	{
		return _completed;
	}

	public void setCompleted(Boolean completed)
	{
		if (completed != null)
		{
			_completed = completed;
			_headersCount++;
		}
	}
	
	public int getHeadersCount()
	{
		return _headersCount;
	}
	
	


}
