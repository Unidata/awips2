package org.rzo.yajsw.util;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

public class MyReentrantLock extends ReentrantLock
{
	
	   final  static long timeout = 100; // millis
	@Override
    synchronized public void lock() {
	    // To avoid a hang that seems to be caused by a lost-wakeup 
	    // we repeatedly use tryAcquire in a loop so that we can
	    // poll the lock state

		    boolean locked = false;
		    boolean interrupted = false;

	    while(!locked) {
		try {
		    locked = tryLock(timeout, TimeUnit.MILLISECONDS);
		}
		catch (InterruptedException ex) {
		    interrupted = true;
		}
	    }
	    
	    if (interrupted) {
		// re-assert interrupt state that occurred while we
		// were acquiring the lock
		Thread.currentThread().interrupt();
	    }
	}
	
	
}
