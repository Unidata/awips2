/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 */
package com.raytheon.uf.common.spatial.reprojection;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Class for managing a pool of concurrency locks organized by string keys.
 * 
 * @author bclement
 * @version 1.0
 */
public class KeyLocker {

	public class KeyLock {
		private final String key;

		private final Lock lock;

		private boolean released = false;

		public KeyLock(String key, Lock lock) {
			this.key = key;
			this.lock = lock;
		}

		@Override
		protected void finalize() throws Throwable {
			release();
		}

		public void release() {
			if (!this.released) {
				releaseLock(this.key);
				this.released = true;
			}
		}

		public void lock() {
			this.lock.lock();
		}

		public void unlock() {
			this.lock.unlock();
		}

	}

	private class Entry {
		int count = 0;
		final Lock lock = new ReentrantLock();
	}

	private final Map<String, Entry> locks = new HashMap<String, Entry>();

	public KeyLock getLock(String key) {
		synchronized (locks) {
			Entry e = locks.get(key);
			if (e == null) {
				e = new Entry();
				locks.put(key, e);
			}
			e.count++;
			return new KeyLock(key, e.lock);
		}
	}

	void releaseLock(String key) {
		synchronized (locks) {
			Entry e = locks.get(key);
			if (e != null) {
				e.count--;
				if (e.count <= 0) {
					locks.remove(key);
				}
			}
		}
	}

}
