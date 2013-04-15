package org.rzo.netty.ahessian.utils;

import java.util.concurrent.BlockingQueue;

public interface MyBlockingQueue<T> extends BlockingQueue<T>
{

	long getTimeout(Integer group);

	boolean remove(T message, Integer group);

	void put(T message, Integer group)  throws InterruptedException;

}
