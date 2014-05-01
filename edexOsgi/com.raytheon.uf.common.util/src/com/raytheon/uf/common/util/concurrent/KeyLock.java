/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.util.concurrent;

import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * Lock assigned to a key
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 8, 2013            bclement     moved from KeyLocker internal class
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class KeyLock<K> {

    private final K key;

    private final ReentrantReadWriteLock lock;

    public KeyLock(K key, ReentrantReadWriteLock lock) {
        this.key = key;
        this.lock = lock;
    }

    /**
     * Acquire write lock
     */
    public void lock() {
        this.lock.writeLock().lock();
    }

    /**
     * Let go of write lock
     */
    public void unlock() {
        this.lock.writeLock().unlock();
    }

    /**
     * Acquire read lock
     */
    public void readLock() {
        this.lock.readLock().lock();
    }

    /**
     * Let go of read lock
     */
    public void readUnlock() {
        this.lock.readLock().unlock();
    }

    /**
     * 
     * @return the key
     */
    public K getKey() {
        return key;
    }

}
