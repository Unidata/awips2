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
package com.raytheon.uf.common.util.collections;

import java.util.LinkedHashMap;
import java.util.Map.Entry;

/**
 * Map implementation that limits the map to a specified number of entries.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2014 2726       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 * @param <K>
 * @param <V>
 */
public class BoundedMap<K, V> extends LinkedHashMap<K, V> {
    private static final long serialVersionUID = 1L;

    private static final int DEFAULT_INITIAL_SIZE = 16;

    private static final float DEFAULT_LOAD_FACTOR = 0.75f;

    private final int maxSize;

    /**
     * BoundedMap with specified max size. Defaults to accessOrder elimination.
     * 
     * @param maxSize
     */
    public BoundedMap(int maxSize) {
        this(maxSize, DEFAULT_INITIAL_SIZE, DEFAULT_LOAD_FACTOR, true);
    }

    /**
     * BoundedMap with specified initial and max size. Defaults to accessOrder
     * elimination.
     * 
     * @param maxSize
     * @param initialSize
     */
    public BoundedMap(int maxSize, int initialSize) {
        this(maxSize, initialSize, DEFAULT_LOAD_FACTOR, true);
    }

    /**
     * BoundedMap with specified initial size, max size, and loadFactor.
     * Defaults to accessOrder elimination.
     * 
     * @param maxSize
     * @param initialSize
     * @param loadFactor
     */
    public BoundedMap(int maxSize, int initialSize, float loadFactor) {
        this(maxSize, initialSize, loadFactor, true);
    }

    /**
     * BoundedMap with specified initial size, max size, loadFactor, and
     * accessOrder elimination. If accessOrder is true, map is order by
     * accessOrder, false is insertion order.
     * 
     * @param maxSize
     * @param initialSize
     * @param loadFactor
     * @param accessOrder
     */
    public BoundedMap(int maxSize, int initialSize, float loadFactor,
            boolean accessOrder) {
        super(initialSize, loadFactor, accessOrder);
        this.maxSize = maxSize;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.LinkedHashMap#removeEldestEntry(java.util.Map.Entry)
     */
    @Override
    protected boolean removeEldestEntry(Entry<K, V> eldest) {
        return size() > maxSize;
    }
}
