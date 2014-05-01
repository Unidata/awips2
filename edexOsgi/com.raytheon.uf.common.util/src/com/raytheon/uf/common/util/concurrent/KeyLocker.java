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
package com.raytheon.uf.common.util.concurrent;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * Manages a pool of locks assigned to keys. Allows for synchronizing expensive
 * tasks on a per-key basis.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2011            bclement     Initial creation
 * Nov 8, 2013  1314       bclement     moved to common.util
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class KeyLocker<K> {

    private final boolean fair;

    public KeyLocker() {
        this(false);
    }

    /**
     * @param fair
     *            true if created locks should use a fair ordering policy
     */
    public KeyLocker(boolean fair) {
        this.fair = fair;
    }

    /**
     * Weak reference to lock that keeps track of the key used to clean the lock
     * map
     * 
     * @author bclement
     * @version 1.0
     */
    private class WeakKeyLock extends WeakReference<ReentrantReadWriteLock> {

        public final K key;

        public WeakKeyLock(K key, ReentrantReadWriteLock referent,
                ReferenceQueue<? super ReentrantReadWriteLock> q) {
            super(referent, q);
            this.key = key;
        }

    }

    private final Map<K, WeakKeyLock> locks = new HashMap<K, WeakKeyLock>();

    private final ReferenceQueue<ReentrantReadWriteLock> refQueue = new ReferenceQueue<ReentrantReadWriteLock>();

    /**
     * Get lock associated with key.
     * 
     * @param key
     * @return
     */
    public KeyLock<K> getLock(K key) {
		synchronized (locks) {
            cleanUp();
            WeakReference<ReentrantReadWriteLock> weakLock = locks.get(key);
            ReentrantReadWriteLock lock = null;
            if (weakLock != null) {
                lock = weakLock.get();
            }
            if (lock == null) {
                lock = new ReentrantReadWriteLock(fair);
                locks.put(key, new WeakKeyLock(key, lock, refQueue));
			}

            return new KeyLock<K>(key, lock);
		}
	}

    /**
     * Poll weak reference and remove from locks map. Must be externally
     * synchronized.
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    private void cleanUp() {
        Reference<? extends ReentrantReadWriteLock> unused = refQueue.poll();

        while (unused != null) {
            if (unused instanceof KeyLocker.WeakKeyLock) {
                WeakKeyLock wkl = (KeyLocker.WeakKeyLock) unused;
                locks.remove(wkl.key);
            }
            unused = refQueue.poll();
        }
    }

}
