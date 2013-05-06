package org.rzo.netty.ahessian.utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.jboss.netty.util.Timeout;
import org.jboss.netty.util.Timer;
import org.jboss.netty.util.TimerTask;
import org.rzo.netty.ahessian.Constants;

public class TimedBlockingPriorityQueue<T> implements MyBlockingQueue<T>
{
	
	int _defaultGroup = 0;
	LinkedList<T>[] _queues;
	int[] _sizes;
	long[] _timeouts;
	Map <T, Timeout> _timers = new HashMap<T, Timeout>();
	Timer _timer;
	Lock _lock = new MyReentrantLock();
	Condition _hasData = _lock.newCondition();
	volatile boolean waiting = false;
	int _size = 0;
	String _name = "?";
	T _last = null;
	
	public TimedBlockingPriorityQueue(Map<String, Object> options, Timer timer, String name)
	{
		_name = name;
		List<Integer> groups = new ArrayList<Integer>();
		_timer = timer;
		for (String key : options.keySet())
		{
			if (key.startsWith("group."))
			
			try {
				String rest = key.substring(key.indexOf('.')+1);
				int x = Integer.parseInt(rest.substring(0, rest.indexOf('.')));				
				if (!groups.contains(x))
					groups.add(x);
				
			}
			catch (Exception ex)
			{
				Constants.ahessianLogger.warn("", ex);
			}
		}
		if (!groups.contains(0))
			groups.add(0);
		Collections.sort(groups);
		int size = groups.get(groups.size()-1)+1;
		_queues = new LinkedList[size];
		_timeouts = new long[size];
		_sizes = new int[size];
		for (int i=0; i<size; i++)
		{
			int queueSize = options.containsKey("group."+i+".size") ? (Integer)options.get("group."+i+".size") : Integer.MAX_VALUE;
			_queues[i] = new LinkedList<T>();
			_sizes[i] = queueSize;
			Object timeout = options.get("group."+i+".timeout");
			long longTimeout = (long)-1;
			if (timeout instanceof Number)
				longTimeout = ((Number)timeout).longValue();
			_timeouts[i] = longTimeout;
		}
	}

	public TimedBlockingPriorityQueue(String name)
	{
		_name = name;
		_queues = new LinkedList[1];
		_timeouts = new long[1];
		_sizes = new int[1];
		_queues[0] = new LinkedList();
		_timeouts[0] = -1;
		_sizes[0] = Integer.MAX_VALUE;
	}

	public boolean add(Object e)
	{
		throw new RuntimeException("Unimplemented");
	}

	public T element()
	{
		throw new RuntimeException("Unimplemented");
	}

	public boolean offer(T e)
	{
		return offer(e, _defaultGroup);
	}

	public boolean offer(final T e, int group)
	{
		boolean result = false;
		_lock.lock();
		try
		{
		//if (_size == 0)
			//System.out.println("LRUQueue not empty: "+ _name);
		_size++;
		if (group >= _queues.length)
		{
			Constants.ahessianLogger.warn("group "+group+" not defined -> using group 0");
			group = 0;
		}
		final LinkedList<T> q = _queues[group];
		result = q.offer((T) e);
		if (q.size() >= _sizes[group])
		{
			// if queue is full remove an element and undo its timer
			T o = q.remove();
			Timeout timer = _timers.remove(o);
			if (timer != null)
				timer.cancel();
			Constants.ahessianLogger.warn("queue overflow -> removed "+e);
		}
		if (result)
			_last = e;
		
		if (_timer != null && result && _timeouts[group] > 0)
		{
			Timeout timer = _timer.newTimeout(new TimerTask()
			{

				public void run(Timeout arg0) throws Exception
				{
					_lock.lock();
					try
					{
					q.remove(e);
					Constants.ahessianLogger.warn("message timed out -> removed from queue "+e);
					}
					finally
					{
					_lock.unlock();
					}
				}
				
			}, _timeouts[group], TimeUnit.MILLISECONDS);
		}
		
		if (result && waiting)
		try
		{
			_hasData.signal();
		}
		catch (Exception ex)
		{
			Constants.ahessianLogger.warn("", ex);
		}

		}
		finally
		{
	_lock.unlock();	
		}
		return result;
	}

	public T poll()
	{
		T result = null;
		for (int i=_queues.length-1; i>=0; i--)
		{
				result = poll(i);
				if (result != null)
				{
					_size--;
					return result;
				}
			}
		return null;
	}

	public T poll(int group)
	{
		LinkedList<T> q = _queues[group];
		T result = null;
		if (q != null)
		{
			result = q.poll();
			if (result != null)
			{
				Timeout timer = _timers.remove(result);
				if (timer != null)
					timer.cancel();
			}
		}
		return result;
	}

	public T peek()
	{
		throw new RuntimeException("Unimplemented");
	}

	public T remove()
	{
		throw new RuntimeException("Unimplemented");
	}

	public boolean addAll(Collection c)
	{
		throw new RuntimeException("Unimplemented");
	}

	public void clear()
	{
		_lock.lock();
		try
		{
		for (int i=0; i<_queues.length; i++)
		{
			clear(i);
		}
		}
		finally
		{
		_lock.unlock();
		}
	}
	
	public void clear(int group)
	{
		_lock.lock();
		try
		{
		if (_queues[group] != null)
			_queues[group].clear();
		}
		finally
		{
		_lock.unlock();
		}
	}

	public boolean contains(Object o)
	{
		throw new RuntimeException("Unimplemented");
	}

	public boolean containsAll(Collection c)
	{
		throw new RuntimeException("Unimplemented");
	}

	public boolean isEmpty()
	{
		throw new RuntimeException("Unimplemented");
	}

	public Iterator iterator()
	{
		throw new RuntimeException("Unimplemented");
	}

	public boolean remove(Object o)
	{
		throw new RuntimeException("Unimplemented");
	}

	public boolean remove(T o, Integer group)
	{
		if (_queues[group] != null)
			return _queues[group].remove(o);
		
		return false;
	}

	public boolean removeAll(Collection c)
	{
		throw new RuntimeException("Unimplemented");
	}

	public boolean retainAll(Collection c)
	{
		throw new RuntimeException("Unimplemented");
	}

	public int size()
	{
		return _size;
	}

	public T[] toArray()
	{
		throw new RuntimeException("Unimplemented");
	}

	public T[] toArray(Object[] a)
	{
		throw new RuntimeException("Unimplemented");
	}

	public int drainTo(Collection c)
	{
		throw new RuntimeException("Unimplemented");
	}

	public int drainTo(Collection c, int maxElements)
	{
		throw new RuntimeException("Unimplemented");
	}

	public boolean offer(Object e, long timeout, TimeUnit unit) throws InterruptedException
	{
		throw new RuntimeException("Unimplemented");
	}

	public T poll(long timeout, TimeUnit unit) throws InterruptedException
	{
		throw new RuntimeException("Unimplemented");
	}

	public void put(T e) throws InterruptedException
	{
		put(e, _defaultGroup);
	}
	
	public void put(T e, Integer group)
	{
		offer(e, group);
	}

	public int remainingCapacity()
	{
		throw new RuntimeException("Unimplemented");
	}

	public T take() throws InterruptedException
	{
		T result = null;
		_lock.lock();
		try
		{
		do
			{
			result = poll();
			if (result == null)
			try
			{
				waiting = true;
				_hasData.await();
			}
			finally
			{
				waiting = false;
			}

			}
		while (result == null);
		if (result == _last)
			_last = null;
		}
		finally
		{
		_lock.unlock();
		}
		//if (_size == 0)
		//	System.out.println("LRUQueue empty: "+_name);

		return result;
		
	}
	
	public long getTimeout(Integer group)
	{
		if (group < _timeouts.length)
			return _timeouts[group];
		return -1;
	}
	
	public T getLast()
	{
		return _last;
	}
	

}
