package org.rzo.netty.ahessian.rpc.server;

import java.util.Date;

import org.rzo.netty.ahessian.session.Session;

/**
 * Continuation offers similar functionality as <a href="http://docs.codehaus.org/display/JETTY/Continuations">Jetty Continuations</a>
 * <br>
 * A continuation is a mechanism by which an RPC request can be suspended and restarted after a timeout or an asynchronous event has occured.
 * <br>
 * Typical usage within a service:
 * <br>
 * The service interface exposed to the client:
 * <br>
 * <pre>
 * public TableData getTableData(Filter filter)
 * </pre>
 * <br>
 * The service object implementation:
 * <br>
 * <pre>
 * public TableData getTableData(Continuation continuation, Filter filter)
 * {
 *      if (requestOk(filter))
 *      	// add client to client list
 * 			addClient(continuation, filter, context);
 *		else // or send an error
 *		    // continuation.fault(new Exception...());
 * }
 * 
 * void onTableDataChange()
 * {
 *   // when table data changes send the new data to all attached clients
 *   for (client : clients)
 *   {
 *     TableData newData = ...
 *     client.getContinuation().send(newData)
 *   }
 * }
 * 
 * void onTableClosed()
 * {
 *   // on shutdown inform all clients that the invocation is completed and no further
 *   // data will be sent.
 *   for (client : clients)
 *   {
 *     client.getContinuation().completed(null);
 *   }
 *   clients.reset();
 * }

 * </pre>
 * 
 */
public interface Continuation
{
	
	/**
	 * Send an invocation reply to the client
	 * 
	 * @param result the result
	 */
	public void send(Object result);
	
	/**
	 * Send the last reply to the client and inform that this is the last reply for the invocation request
	 * 
	 * @param result the result
	 */
	public void complete(Object result);
	
	/**
	 * Send an error to the client and inform that this is the last reply for the invocation request
	 * 
	 * @param result the result
	 */
	public void fault(Throwable result);
	
	/**
	 * If the client has given us a time out for the invocation calling send, complete or fault after the given 
	 * point in time will result in an exception
	 * 
	 * @return the tTL
	 */
	public Date getTTL();
	
	public Session getSession();
}
