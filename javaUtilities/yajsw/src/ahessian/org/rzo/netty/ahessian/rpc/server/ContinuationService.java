package org.rzo.netty.ahessian.rpc.server;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Executor;
import java.util.concurrent.LinkedBlockingQueue;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.rpc.message.HessianRPCCallMessage;
import org.rzo.netty.ahessian.session.ServerSessionFilter;

/**
 * Wraps an object as a {@link Service}. Call requests are added to a queue.
 * Calls are removed from the queue and invoked within a thread from the thread pool.
 * If no free threads are available in the pool, execution hangs until a thread is available.
 *  <br>
 * This type of service is used for long running invocations or for invocations 
 * which require {@link Continuation}.
 * They therefore allow for multiple transmissions as result for a single call.
 * <br>
 * A typical usage could be a request for table data. Each response will send just the rows which have 
 * changed. Only one call request is required to trigger the continuous table update.
 * <br>
 * Typical code:
 * <pre>
 * // The object to be wrapped, "kind of" implements MyServiceInterface
 * // This object implements for each method of the service-api a method with the same name and arguments
 * // However an extra argument as to be added to the signature.
 * // As first argument of the method an argument of type {@link Continuation} must be added.
 * // The continuation object is used to send the result to the client.
 * // The result of the method invocation is not sent to the client. 
 * Object myContinuationServiceObject = ...;
 * 
 * // the netty rpc service handler
 * HessianRPCServiceHandler handler = ...;
 * 
 * Service myService = new ContinuationService(myServiceObject, MyServiceInterface.class);
 * 
 * // Clients will access the service through the given name
 * handler.addService("myServiceName", myService);
 * 
 * </pre>
 */
public class ContinuationService extends HessianSkeleton
{
	private LinkedBlockingQueue<HessianRPCCallMessage>	_pendingCalls	= new LinkedBlockingQueue<HessianRPCCallMessage>();
	
	/** Thread pool for executing invocations. */
	private Executor											_executor;
	
	/** TODO indicates if execution is stopped */
	private boolean												_stop			= false;
	
	/** optimize search for service methods by name */
	private Map<String, Method> _methodMap = new HashMap<String, Method>();

	volatile private ChannelHandlerContext _ctx = null;
	/**
	 * Instantiates a new continuation service.
	 * 
	 * @param service the service object, "kind of" implementing the apiClass 
	 * @param apiClass the api of the object exposed to the clients
	 * @param factory the netty handler
	 * @param executor the thread pool for executing invocations
	 */
	public ContinuationService(Object service, Class apiClass, HessianRPCServiceHandler factory, Executor executor)
	{
		super(service, apiClass, factory);
		
	    Method []methodList = service.getClass().getMethods();

	    for (int i = 0; i < methodList.length; i++) {
	      Method method = methodList[i];

	      if (method.getParameterTypes().length > 0 && method.getParameterTypes()[0].equals(Continuation.class))
	      {
	    	  String mangeledName = method.getName()+"__"+(method.getParameterTypes().length-1);
	      		if (super.getMethod(mangeledName) != null)
	      			_methodMap.put(mangeledName, method);
	      }
	      
	     
	    }

		
		_executor = executor;
		_executor.execute(new Runnable()
		{
			public void run()
			{
				HessianRPCCallMessage message;
				while (!_stop)
				{
					message = null;
					try
					{
						message = _pendingCalls.take();
					}
					catch (InterruptedException e1)
					{
						Constants.ahessianLogger.warn("", e1);
					}
					if (message != null)
					{
						invoke(_ctx, message);
					}
				}

			}
		});

	}

	/* (non-Javadoc)
	 * @see org.rzo.netty.ahessian.rpc.server.HessianSkeleton#messageReceived(org.rzo.netty.ahessian.rpc.HessianRPCCallMessage)
	 */
	
	public void messageReceived(HessianRPCCallMessage message)
	{
		_pendingCalls.add(message);
	}

	private void invoke(ChannelHandlerContext ctx, final HessianRPCCallMessage message)
	{
		_executor.execute(new Runnable()
		{

			public void run()
			{
				Continuation continuation = new DefaultContinuation(message, ContinuationService.this, ServerSessionFilter.getSession(_ctx));
				try
				{
					Method method = getMethod(message);
					int l = message.getArgs() == null ? 1 : message.getArgs().length + 1;
					Object[] args = new Object[l];
					if (args.length > 1)
						System.arraycopy(message.getArgs(), 0, args, 1, message.getArgs().length);
					args[0] = continuation;
					method.invoke(_service, args);
				}
				catch (Throwable ex)
				{
					Constants.ahessianLogger.warn("", ex);
					continuation.fault(ex);
				}
			}
		});

	}
	
	/* (non-Javadoc)
	 * @see org.rzo.netty.ahessian.rpc.server.HessianSkeleton#getMethod(org.rzo.netty.ahessian.rpc.HessianRPCCallMessage)
	 */
	public Method getMethod(HessianRPCCallMessage message)
	{
  	  String mangeledName = message.getMethod()+"__"+(message.getArgs().length);
  	  return _methodMap.get(mangeledName);
	}
	

}
