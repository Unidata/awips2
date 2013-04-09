package org.rzo.netty.ahessian.rpc.io;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallMessage;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyMessage;

/**
 * The Class HessianOutput.
 */
public class Hessian2Output extends com.caucho.hessian4.io.Hessian2Output implements Constants
{

	/**
	 * Instantiates a new hessian output.
	 * 
	 * @param out
	 *            the out
	 */
	public Hessian2Output(OutputStream out)
	{
		super(out);
		setCloseStreamOnClose(true);
	}

	/**
	 * Write reply.
	 * 
	 * @param message
	 *            the message
	 * 
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public void writeReply(HessianRPCReplyMessage message) throws IOException
	{
		writeVersion();
		startEnvelope(HEADER_STRING);
		writeHeaders(message);
		if (message.getFault() == null)
		{
			startReply();
			writeObject(message.getValue());
			completeReply();
		}
		else
		{
			Throwable fault = message.getFault();
			if (fault instanceof InvocationTargetException)
			{
				InvocationTargetException invi = (InvocationTargetException) fault;
				Throwable inviFault = invi.getTargetException();
				if (inviFault != null)
					this.writeFault(inviFault.getClass().getSimpleName(), inviFault.getMessage(), inviFault);
				else
					this.writeFault(fault.getClass().getSimpleName(), fault.getMessage(), fault);
			}
			else
				this.writeFault(fault.getClass().getSimpleName(), fault.getMessage(), fault);
		}

		completeEnvelope();

	}

	/**
	 * Call.
	 * 
	 * @param message
	 *            the message
	 * 
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public void call(HessianRPCCallMessage message) throws IOException
	{
		String method = message.getMethod();
		Object[] args = message.getArgs();
		if (args == null)
			args = new Object[0];
		Map headers = message.getHeaders();
		int length = args != null ? args.length : 0;

		writeVersion();
		startEnvelope(HEADER_STRING);
		writeHeaders(message.getHeaders());
		// no packet read in Hessian2Input
		// startPacket();
		startCall(method, args.length);
		for (int i = 0; i < length; i++)
			writeObject(args[i]);
		completeCall();
		// endPacket();
		completeEnvelope();
	}
	
	private void writeHeaders(Map headers)
	{
		try
		{
			writeInt(headers.size());
			for (Iterator it = headers.entrySet().iterator(); it.hasNext(); )
			{
				Entry entry = (Entry) it.next();
				writeInt((Integer)entry.getKey());
				writeObject(entry.getValue());
			}
		}
			catch (Exception ex)
			{
				Constants.ahessianLogger.warn("", ex);
			}
		
	}

	private void writeHeaders(HessianRPCReplyMessage message)
	{
		try
		{
			writeInt(message.getHeadersCount());
			if (message.getCallbackArgs() != null)
			{
				writeInt(CALLBACK_ARGS_HEADER_KEY);
				writeObject(message.getCallbackArgs());
			}
			if (message.getCallbackDone() != null)
			{
				writeInt(CALLBACK_DONE_HEADER_KEY);
				writeObject(message.getCallbackDone());				
			}
			if (message.getCallbackId() != null)
			{
				writeInt(CALLBACK_ID_HEADER_KEY);
				writeObject(message.getCallbackId());				
			}
			if (message.getCallbackMethod() != null)
			{
				writeInt(CALLBACK_METHOD_HEADER_KEY);
				writeObject(message.getCallbackMethod());				
			}
			if (message.getCallId() != null)
			{
				writeInt(CALL_ID_HEADER_KEY);
				writeObject(message.getCallId());				
			}
			if (message.getCompleted() != null)
			{
				writeInt(COMPLETED_HEADER_KEY);
				writeObject(message.getCompleted());				
			}
			if (message.getGroup() != null)
			{
				writeInt(GROUP_HEADER_KEY);
				writeObject(message.getGroup());				
			}
		}
		catch (Exception ex)
		{
			Constants.ahessianLogger.warn("", ex);
		}
	}

}
