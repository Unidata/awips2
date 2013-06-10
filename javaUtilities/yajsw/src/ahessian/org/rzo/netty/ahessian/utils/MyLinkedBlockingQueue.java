package org.rzo.netty.ahessian.utils;

import java.util.concurrent.LinkedBlockingQueue;

public class MyLinkedBlockingQueue<T> extends LinkedBlockingQueue<T> implements MyBlockingQueue<T>
{

	public long getTimeout(Integer group)
	{
		return -1;
	}

	public void put(T message, Integer group) throws InterruptedException
	{
		put(message);
	}

	public boolean remove(T message, Integer group)
	{
		return remove(message);
	}

}
