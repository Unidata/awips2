package org.rzo.netty.ahessian.session;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelEvent;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelHandler;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelSink;

/**
 * A channel pipeline which can be added to an active pipeline.
 * NOTE: This class should not be used be used as a standard pipeline.
 * It cannot be attached to a channel or context. It just manages a list
 * of handlers which can be added to a standard pipeline.
 */
public class MixinPipeline implements ChannelPipeline
{
	
	/** List of handlers added to the pipeline */
	private LinkedList<ChannelHandler> _handlersList = new LinkedList<ChannelHandler>();
	
	/** Maps names to handlers */
	Map<String, ChannelHandler> _handlersMap = new HashMap<String, ChannelHandler>();
	
	/** Names list, must be synchronous to handlersList */
	LinkedList<String> _namesList = new LinkedList<String>();
	
	Channel _channel;
	
	

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#addAfter(java.lang.String, java.lang.String, org.jboss.netty.channel.ChannelHandler)
	 */
	public void addAfter(String arg0, String arg1, ChannelHandler arg2)
	{
		ChannelHandler handler1 = getHandlerOrDie(arg0);
		checkDuplicateName(arg1);
		_handlersMap.put(arg1, arg2);
		int i = _handlersList.indexOf(handler1);
		_handlersList.add(i+1, arg2);
		_namesList.add(i+1, arg1);
	}

	private ChannelHandler getHandlerOrDie(String arg0)
	{
		ChannelHandler result = _handlersMap.get(arg0);
		if (result == null)
			throw new NoSuchElementException(arg0);
		return result;
	}
	
    private void checkDuplicateName(String name) {
        if (_handlersMap.containsKey(name)) {
            throw new IllegalArgumentException("Duplicate handler name.");
        }
    }


	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#addBefore(java.lang.String, java.lang.String, org.jboss.netty.channel.ChannelHandler)
	 */
	
	public void addBefore(String arg0, String arg1, ChannelHandler arg2)
	{
		ChannelHandler handler1 = getHandlerOrDie(arg0);
		checkDuplicateName(arg1);
		_handlersMap.put(arg1, arg2);
		int i = _handlersList.indexOf(handler1);
		_handlersList.add(i, arg2);
		_namesList.add(i, arg1);
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#addFirst(java.lang.String, org.jboss.netty.channel.ChannelHandler)
	 */
	
	public void addFirst(String arg0, ChannelHandler arg1)
	{
		checkDuplicateName(arg0);
		_handlersMap.put(arg0, arg1);
		_handlersList.addFirst(arg1);
		_namesList.addFirst(arg0);
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#addLast(java.lang.String, org.jboss.netty.channel.ChannelHandler)
	 */
	
	public void addLast(String arg0, ChannelHandler arg1)
	{
		checkDuplicateName(arg0);
		_handlersMap.put(arg0, arg1);
		_handlersList.addLast(arg1);
		_namesList.addLast(arg0);
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#attach(org.jboss.netty.channel.Channel, org.jboss.netty.channel.ChannelSink)
	 */
	
	public void attach(Channel arg0, ChannelSink arg1)
	{
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#get(java.lang.String)
	 */
	
	public ChannelHandler get(String arg0)
	{
		return _handlersMap.get(arg0);
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#get(java.lang.Class)
	 */
	
	public <T extends ChannelHandler> T get(Class<T> arg0)
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#getChannel()
	 */
	
	public Channel getChannel()
	{
		return _channel;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#getContext(org.jboss.netty.channel.ChannelHandler)
	 */
	
	public ChannelHandlerContext getContext(ChannelHandler arg0)
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#getContext(java.lang.String)
	 */
	
	public ChannelHandlerContext getContext(String arg0)
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#getContext(java.lang.Class)
	 */
	
	public ChannelHandlerContext getContext(Class<? extends ChannelHandler> arg0)
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#getFirst()
	 */
	
	public ChannelHandler getFirst()
	{
		return _handlersList.getFirst();
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#getLast()
	 */
	
	public ChannelHandler getLast()
	{
		return _handlersList.getLast();
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#getSink()
	 */
	
	public ChannelSink getSink()
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#isAttached()
	 */
	
	public boolean isAttached()
	{
		return false;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#remove(org.jboss.netty.channel.ChannelHandler)
	 */
	
	public void remove(ChannelHandler arg0)
	{
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#remove(java.lang.String)
	 */
	
	public ChannelHandler remove(String arg0)
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#remove(java.lang.Class)
	 */
	
	public <T extends ChannelHandler> T remove(Class<T> arg0)
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#removeFirst()
	 */
	
	public ChannelHandler removeFirst()
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#removeLast()
	 */
	
	public ChannelHandler removeLast()
	{
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#replace(org.jboss.netty.channel.ChannelHandler, java.lang.String, org.jboss.netty.channel.ChannelHandler)
	 */
	
	public void replace(ChannelHandler arg0, String arg1, ChannelHandler arg2)
	{
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#replace(java.lang.String, java.lang.String, org.jboss.netty.channel.ChannelHandler)
	 */
	
	public ChannelHandler replace(String arg0, String arg1, ChannelHandler arg2)
	{
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#replace(java.lang.Class, java.lang.String, org.jboss.netty.channel.ChannelHandler)
	 */
	
	public <T extends ChannelHandler> T replace(Class<T> arg0, String arg1, ChannelHandler arg2)
	{
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#sendDownstream(org.jboss.netty.channel.ChannelEvent)
	 */
	
	public void sendDownstream(ChannelEvent arg0)
	{
	}

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#sendUpstream(org.jboss.netty.channel.ChannelEvent)
	 */
	
	public void sendUpstream(ChannelEvent arg0)
	{
	}
	
	 public List<String> getNames() {
	        return new ArrayList(_namesList);
	    }

	/* (non-Javadoc)
	 * @see org.jboss.netty.channel.ChannelPipeline#toMap()
	 */
	
	public Map<String, ChannelHandler> toMap()
	{
		return null;
	}
	
	/**
	 * Adds this pipeline to the end of the given pipeline.
	 * 
	 * @param pipeline a standard pipeline
	 */
	public void mixin(ChannelPipeline pipeline)
	{
		_channel = pipeline.getChannel();
		for (int i = 0; i<_namesList.size(); i++)
		{
			ChannelHandler handler = _handlersList.get(i);
			String  name = _namesList.get(i);
			pipeline.addLast(name, handler);
		}
	}
	
    public ChannelFuture execute(Runnable task) {
        return getSink().execute(this, task);
    }


}
