/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common;

import java.util.Iterator;

/**
 * Iterator that allows peeking at the next item without progressing
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 29, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class PeekingIterator<T> implements Iterator<T> {

    private final Iterator<T> iter;

    private T next;

    private boolean dirty = false;

    /**
     * @param iter
     */
    public PeekingIterator(Iterator<T> iter) {
        this.iter = iter;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.Iterator#hasNext()
     */
    @Override
    public boolean hasNext() {
        if (dirty) {
            return true;
        }
        return iter.hasNext();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.Iterator#next()
     */
    @Override
    public T next() {
        if (dirty) {
            dirty = false;
            return next;
        }
        return iter.next();
    }

    /**
     * Look at next item without progressing. Will return the same item until
     * {@link PeekingIterator#next()} is called.
     * 
     * {@link PeekingIterator#hasNext()} should be called beforehand.
     * 
     * @return
     */
    public T peek(){
        if (dirty){
            return next;
        }
        next = iter.next();
        dirty = true;
        return next;
    }

    /**
     * Unsupported. Throws {@link UnsupportedOperationException}
     */
    @Override
    public void remove() {
        throw new UnsupportedOperationException(
                "remove not supported on peeking iterator");
    }

}
