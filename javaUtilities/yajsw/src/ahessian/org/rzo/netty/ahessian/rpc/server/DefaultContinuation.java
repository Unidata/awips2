package org.rzo.netty.ahessian.rpc.server;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallMessage;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyMessage;
import org.rzo.netty.ahessian.session.Session;

/**
 * Default implementation of a {@link Continuation}.
 */
class DefaultContinuation implements Continuation, Constants
{
	
	/** The service. */
	private ContinuationService _service;
	
	/** The call request message. */
	private HessianRPCCallMessage _message;
	
	/** The headers for reply message. */
	private Map _headers;
	
	/** Indicates if the continuation has been completed. */
	private boolean _completed = false;
	
	/** Time to live. */
	private Date _ttl;
	
	private Session _session = null;
	

	/**
	 * Instantiates a new default continuation.
	 * 
	 * @param message the message
	 * @param service the service
	 */
	 DefaultContinuation(HessianRPCCallMessage message, ContinuationService service, Session session)
	{
		_service = service;
		_message = message;
		_headers = _message.getHeaders();
		if (_headers == null)
			_headers = new HashMap();
		Long ttl = (Long) _headers.get("TTL");
		if (ttl == null)
			_ttl = new Date(Long.MAX_VALUE);
		else
			_ttl = new Date(System.currentTimeMillis() + ttl.longValue());
		_headers.put(COMPLETED_HEADER_KEY, Boolean.FALSE);
		_session = session;
	}

	/* (non-Javadoc)
	 * @see org.rzo.netty.ahessian.rpc.server.Continuation#complete(java.lang.Object)
	 */
	
	public void complete(Object result)
	{
		checkCompleted();
		_completed = true;
		sendReply(result, null);
	}

	/* (non-Javadoc)
	 * @see org.rzo.netty.ahessian.rpc.server.Continuation#fault(java.lang.Throwable)
	 */
	
	public void fault(Throwable result)
	{
		checkCompleted();
		_completed = true;
		sendReply(null, result);
	}

	/* (non-Javadoc)
	 * @see org.rzo.netty.ahessian.rpc.server.Continuation#getTTL()
	 */
	
	public Date getTTL()
	{
		return _ttl;
	}

	/* (non-Javadoc)
	 * @see org.rzo.netty.ahessian.rpc.server.Continuation#send(java.lang.Object)
	 */
	
	public void send(Object result)
	{
		checkCompleted();
		sendReply(result, null);

	}

	private void checkCompleted()
	{
		if (_completed )
			throw new RuntimeException("Continuation already completed");
		if (System.currentTimeMillis() > _ttl.getTime())
		{
			_completed = true;
			throw new RuntimeException("Continuation already completed");
		}
	}
	
	private void sendReply(Object result, Object fault)
	{
		if (_completed)
			_headers.put(COMPLETED_HEADER_KEY, Boolean.TRUE);
		//TODO set reply headers
		_service.writeResult(new HessianRPCReplyMessage(result, fault, _message));
	}

	public Session getSession()
	{
		return _session;
	}
	



}
