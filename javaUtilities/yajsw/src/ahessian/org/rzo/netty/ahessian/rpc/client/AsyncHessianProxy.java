package org.rzo.netty.ahessian.rpc.client;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.Future;

import org.jboss.netty.channel.Channel;
import org.rzo.netty.ahessian.Constants;
import org.rzo.netty.ahessian.rpc.message.HessianRPCReplyMessage;

import com.caucho.hessian4.services.server.AbstractSkeleton;

/**
 * A proxy object implementing asynchronous invocations.
 * All invocations return a HessianProxyFuture
 */
public class AsyncHessianProxy implements InvocationHandler, Constants
{
	
	  private WeakHashMap<Method,String> _mangleMap = new WeakHashMap<Method,String>();
	  private HessianProxyFactory _factory;
	  private Class _api;
	  private boolean _valid = true;
	  Map _options;
	  private Map<String, Integer> _groups = new HashMap<String, Integer>();

	  
	/**
	 * Instantiates a new async hessian proxy.
	 * 
	 * @param factory the factory
	 * @param api the api
	 * @param options the options
	 */
	AsyncHessianProxy(HessianProxyFactory factory, Class api, Map options)
	{
		_factory = factory;
		_api = api;
		_options = options;
	}
	  
	  /**
  	 * Gets the channel.
  	 * 
  	 * @return the channel
  	 */
  	Channel getChannel()
	  {
  		if (!_valid)
  			throw new RuntimeException("invalidated proxy");
		  return _factory.getChannel();
	  }

	/* (non-Javadoc)
	 * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object, java.lang.reflect.Method, java.lang.Object[])
	 */
		  public Object invoke(Object proxy, Method method, Object []args)
		    throws Throwable
		  {
		    String mangleName;
		    Channel channel = getChannel();

		    synchronized (_mangleMap) {
		      mangleName = _mangleMap.get(method);
		    }

		    if (mangleName == null) {
		      String methodName = method.getName();
		      Class []params = method.getParameterTypes();

		      // equals and hashCode are special cased
		      if (methodName.equals("equals")
			  && params.length == 1 && params[0].equals(Object.class)) {
			Object value = args[0];
			if (value == null || ! Proxy.isProxyClass(value.getClass()))
			  return Boolean.FALSE;

			Object proxyHandler = Proxy.getInvocationHandler(value);

			if (! (proxyHandler instanceof AsyncHessianProxy))
			  return Boolean.FALSE;
			
			AsyncHessianProxy handler = (AsyncHessianProxy) proxyHandler;

			return new Boolean(this.equals(handler));
		      }
		      else if (methodName.equals("hashCode") && params.length == 0)
			return new Integer(System.identityHashCode(this));
		      else if (methodName.equals("getHessianType"))
			return proxy.getClass().getInterfaces()[0].getName();
		      else if (methodName.equals("getHessianURL"))
			return channel == null ? "?" : channel.toString();
		      else if (methodName.equals("toString") && params.length == 0)
			return "HessianProxy[" + _api + "]";
		      
		      if (! _factory.isOverloadEnabled())
			mangleName = method.getName();
		      else
		        mangleName = mangleName(method);

		      synchronized (_mangleMap) {
			_mangleMap.put(method, mangleName);
		      }
		    }
		    Integer group = getGroup(mangleName, method.getName());
		          return sendRequest(mangleName, args, group);
	}
		  
		  private Integer getGroup(String mangleName, String name)
		{
				Integer result = _groups.get(mangleName);
				if (result == null)
				{
					// get the group for the method
					result = (Integer) _options.get("method.group."+mangleName);
					if (result == null)
						result = (Integer) _options.get("method.group."+name);
					// if no group found set to default group 0
					if (result == null)
						result = 0;
					_groups.put(mangleName, result);
				}
				return result;
		}

		protected Future<Object> sendRequest(String methodName, Object []args, Integer group)
		    throws InterruptedException
		  {
		  		if (!_valid)
		  			throw new RuntimeException("invalidated proxy");
			 //Map options =  new HashMap(_options);
		  		Map options =  new HashMap();
		  		options.put(GROUP_HEADER_KEY, group);
		  		options.put(SERVICE_ID_HEADER_KEY, _options.get(SERVICE_ID_HEADER_KEY));
			 return _factory.sendRequest(methodName, args, options);
		  }
		  
		  /** The id. */
  		static volatile long id = 0;
		  
		  protected Map getHeaders()
		  {
			  Map result = new HashMap();
			  result.put(CALL_ID_HEADER_KEY, id++);
			  return result;
			  
		  }
		  
		  
		  protected String mangleName(Method method)
		  {
		    Class []param = method.getParameterTypes();
		    
		    if (param == null || param.length == 0)
		      return method.getName();
		    else
		      return AbstractSkeleton.mangleName(method, false);
		  }
		  
		  private Future immediateFuture(Object result)
		  {
			  HessianProxyFuture future = new HessianProxyFuture();
			  future.set(new HessianRPCReplyMessage(result, null, null));
			  return future;
		  }
		  
		  protected void invalidate()
		  {
			  _valid = false;
		  }
		  
		  public String getHost()
		  {
			  Channel c = getChannel();
			  if (c == null)
				  return null;
			  InetSocketAddress addr =  (InetSocketAddress) c.getRemoteAddress();
			  return addr.getHostName();
		  }
		  




}
