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
package com.raytheon.uf.common.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * RWLArrayList implements a read/write locked ArrayList which allows multiple
 * simultaneous readers but only single writers. This class will throw
 * {@link IllegalAccessException} if you attempt to use any of it's methods
 * without first acquiring the necessary lock.
 * <p>
 * Typical usage scenarios:
 * 
 * <pre>
 * RWLArrayList rwlList = new RWLArrayList();
 * 
 * // read access
 * rwlList.acquireReadLock();
 * try {
 *     // access rwlList
 * } finally {
 *     rwlList.releaseReadLock();
 * }
 * 
 * // write access
 * rwlList.acquireWriteLock();
 * try {
 *     // modify rwlList
 * } finally {
 *     rwlList.releaseWriteLock();
 * }
 * </pre>
 * 
 * For additional information see {@link ReentrantReadWriteLock}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 23, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class RWLArrayList<E> extends ArrayList<E> {
    public static class IllegalAccessException extends RuntimeException {

        private static final long serialVersionUID = 5885711016958102497L;

        private IllegalAccessException() {
            super();
        }

        private IllegalAccessException(String message, Throwable cause) {
            super(message, cause);
        }

        private IllegalAccessException(String message) {
            super(message);
        }

        private IllegalAccessException(Throwable cause) {
            super(cause);
        }

    }

    private class Itr<T> implements Iterator<T> {
        private Iterator<T> iter;

        private Itr(Iterator<T> iter) {
            this.iter = iter;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.util.Iterator#hasNext()
         */
        @Override
        public boolean hasNext() {
            checkRead();
            return iter.hasNext();
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.util.Iterator#next()
         */
        @Override
        public T next() {
            checkRead();
            return iter.next();
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.util.Iterator#remove()
         */
        @Override
        public void remove() {
            checkWrite();
            iter.remove();
        }

    }

    private class ListItr implements ListIterator<E> {
        private ListIterator<E> iter;

        private ListItr(ListIterator<E> iter) {
            this.iter = iter;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.util.ListIterator#hasNext()
         */
        @Override
        public boolean hasNext() {
            checkRead();
            return iter.hasNext();
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.util.ListIterator#next()
         */
        @Override
        public E next() {
            checkRead();
            return iter.next();
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.util.ListIterator#hasPrevious()
         */
        @Override
        public boolean hasPrevious() {
            checkRead();
            return iter.hasPrevious();
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.util.ListIterator#previous()
         */
        @Override
        public E previous() {
            checkRead();
            return iter.previous();
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.util.ListIterator#nextIndex()
         */
        @Override
        public int nextIndex() {
            checkRead();
            return iter.nextIndex();
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.util.ListIterator#previousIndex()
         */
        @Override
        public int previousIndex() {
            checkRead();
            return iter.previousIndex();
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.util.ListIterator#remove()
         */
        @Override
        public void remove() {
            checkWrite();
            iter.remove();
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.util.ListIterator#set(java.lang.Object)
         */
        @Override
        public void set(E e) {
            checkWrite();
            iter.set(e);
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.util.ListIterator#add(java.lang.Object)
         */
        @Override
        public void add(E e) {
            checkWrite();
            iter.add(e);
        }

    }

    private static final long serialVersionUID = 8683452581122892189L;

    private ReentrantReadWriteLock rwl = new ReentrantReadWriteLock();

    /**
     * 
     */
    public RWLArrayList() {
    }

    /**
     * @param initialCapacity
     */
    public RWLArrayList(int initialCapacity) {
        super(initialCapacity);
    }

    /**
     * @param c
     */
    public RWLArrayList(Collection<? extends E> c) {
        super(c);
    }

    private void checkRead() {
        if (rwl.getReadHoldCount() < 1) {
            throw new IllegalAccessException(
                    "You must acquire a read lock before calling this method.");
        }
    }

    private void checkWrite() {
        if (rwl.getWriteHoldCount() < 1) {
            throw new IllegalAccessException(
                    "You must acquire a write lock before calling this method.");
        }
    }

    /**
     * Acquires the read lock.
     * 
     * <p>
     * Acquires the read lock if the write lock is not held by another thread
     * and returns immediately.
     * 
     * <p>
     * If the write lock is held by another thread then the current thread
     * becomes disabled for thread scheduling purposes and lies dormant until
     * the read lock has been acquired.
     */
    public void acquireReadLock() {
        rwl.readLock().lock();
    }

    /**
     * Attempts to release the read lock.
     * 
     * <p>
     * If the number of readers is now zero then the lock is made available for
     * write lock attempts.
     */
    public void releaseReadLock() {
        rwl.readLock().unlock();
    }

    /**
     * Acquires the write lock.
     * 
     * <p>
     * Acquires the write lock if neither the read nor write lock are held by
     * another thread and returns immediately, setting the write lock hold count
     * to one.
     * 
     * <p>
     * If the current thread already holds the write lock then the hold count is
     * incremented by one and the method returns immediately.
     * 
     * <p>
     * If the lock is held by another thread then the current thread becomes
     * disabled for thread scheduling purposes and lies dormant until the write
     * lock has been acquired, at which time the write lock hold count is set to
     * one.
     */
    public void acquireWriteLock() {
        if (rwl.getReadHoldCount() > 0) {
            throw new IllegalAccessException(
                    "Attempt to aquire write lock while holding read lock would result in deadlock");
        }
        rwl.writeLock().lock();
        rwl.readLock().lock();
    }

    /**
     * Attempts to release this lock.
     * 
     * <p>
     * If the current thread is the holder of this lock then the hold count is
     * decremented. If the hold count is now zero then the lock is released. If
     * the current thread is not the holder of this lock then
     * {@link IllegalMonitorStateException} is thrown.
     * 
     * @throws IllegalMonitorStateException
     *             if the current thread does not hold this lock.
     */
    public void releaseWriteLock() {
        rwl.readLock().unlock();
        rwl.writeLock().unlock();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#trimToSize()
     */
    @Override
    public void trimToSize() {
        checkWrite();
        super.trimToSize();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#ensureCapacity(int)
     */
    @Override
    public void ensureCapacity(int minCapacity) {
        checkWrite();
        super.ensureCapacity(minCapacity);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#size()
     */
    @Override
    public int size() {
        checkRead();
        return super.size();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#isEmpty()
     */
    @Override
    public boolean isEmpty() {
        checkRead();
        return super.isEmpty();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#contains(java.lang.Object)
     */
    @Override
    public boolean contains(Object o) {
        checkRead();
        return super.contains(o);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#indexOf(java.lang.Object)
     */
    @Override
    public int indexOf(Object o) {
        checkRead();
        return super.indexOf(o);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#lastIndexOf(java.lang.Object)
     */
    @Override
    public int lastIndexOf(Object o) {
        checkRead();
        return super.lastIndexOf(o);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#clone()
     */
    @Override
    public Object clone() {
        checkRead();
        return super.clone();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#toArray()
     */
    @Override
    public Object[] toArray() {
        checkRead();
        return super.toArray();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#toArray(T[])
     */
    @Override
    public <T> T[] toArray(T[] a) {
        checkRead();
        return super.toArray(a);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#get(int)
     */
    @Override
    public E get(int index) {
        checkRead();
        return super.get(index);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#set(int, java.lang.Object)
     */
    @Override
    public E set(int index, E element) {
        checkWrite();
        return super.set(index, element);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#add(java.lang.Object)
     */
    @Override
    public boolean add(E e) {
        checkWrite();
        return super.add(e);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#add(int, java.lang.Object)
     */
    @Override
    public void add(int index, E element) {
        checkWrite();
        super.add(index, element);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#remove(int)
     */
    @Override
    public E remove(int index) {
        checkWrite();
        return super.remove(index);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#remove(java.lang.Object)
     */
    @Override
    public boolean remove(Object o) {
        checkWrite();
        return super.remove(o);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#clear()
     */
    @Override
    public void clear() {
        checkWrite();
        super.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#addAll(java.util.Collection)
     */
    @Override
    public boolean addAll(Collection<? extends E> c) {
        checkWrite();
        return super.addAll(c);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#addAll(int, java.util.Collection)
     */
    @Override
    public boolean addAll(int index, Collection<? extends E> c) {
        checkWrite();
        return super.addAll(index, c);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.ArrayList#removeRange(int, int)
     */
    @Override
    protected void removeRange(int fromIndex, int toIndex) {
        checkWrite();
        super.removeRange(fromIndex, toIndex);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.AbstractList#iterator()
     */
    @Override
    public Iterator<E> iterator() {
        return new Itr<E>(super.iterator());
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.AbstractList#listIterator()
     */
    @Override
    public ListIterator<E> listIterator() {
        return listIterator(0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.AbstractList#listIterator(int)
     */
    @Override
    public ListIterator<E> listIterator(int index) {
        return new ListItr(super.listIterator(index));
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.AbstractList#subList(int, int)
     */
    @Override
    public List<E> subList(int fromIndex, int toIndex) {
        checkRead();
        return super.subList(fromIndex, toIndex);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.AbstractList#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object o) {
        acquireReadLock();
        try {
            return super.equals(o);
        } finally {
            releaseReadLock();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.AbstractList#hashCode()
     */
    @Override
    public int hashCode() {
        acquireReadLock();
        try {
            return super.hashCode();
        } finally {
            releaseReadLock();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.AbstractCollection#containsAll(java.util.Collection)
     */
    @Override
    public boolean containsAll(Collection<?> c) {
        checkRead();
        return super.containsAll(c);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.AbstractCollection#removeAll(java.util.Collection)
     */
    @Override
    public boolean removeAll(Collection<?> c) {
        checkWrite();
        return super.removeAll(c);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.AbstractCollection#retainAll(java.util.Collection)
     */
    @Override
    public boolean retainAll(Collection<?> c) {
        checkWrite();
        return super.retainAll(c);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.AbstractCollection#toString()
     */
    @Override
    public String toString() {
        acquireReadLock();
        try {
            return super.toString();
        } finally {
            releaseReadLock();
        }
    }

}
