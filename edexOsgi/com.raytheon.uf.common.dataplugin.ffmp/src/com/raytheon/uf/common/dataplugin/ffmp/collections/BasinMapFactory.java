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
package com.raytheon.uf.common.dataplugin.ffmp.collections;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeMap;

/**
 * Factory for constructing maps to use in FFMPBasins for organizing values.
 * 
 * Each basin in FFMP requires a NavigableMap<Date,Float> in order to achieve
 * fast dynamic calculations. Unfortunately due to the large number of basins
 * TreeMaps require too much memory. This factory produces maps that have the
 * same performance as TreeMaps but are much more memory efficient.
 * 
 * This factory achieves efficiency by taking advantage of the fact that all
 * basins will have data for the same times, so it can use a single TreeMap as
 * the backing map for all basins. Each NavigableMap returned from this factory
 * is simply a view into the backing map.
 * 
 * Because all maps returned from this factory use the same backing map, they
 * cannot be synchronized individually. Internally all map modifications are
 * synchronized on the factory object to prevent concurrent modification. If any
 * iterators are being used from any maps then the iteration should also be
 * synchronized on the factory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2013 2242       bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class BasinMapFactory<K> {

    /*
     * How much to grow the backing arrays to make room for new maps, this is
     * much more conservative than the algorithms used by ArrayList or HashMap
     * because typically growth will occur very briefly during initialization
     * and the extra overhead of a slower growth rate is minimal. Once all
     * basins are created, growth will stop and a conservative growth algorithm
     * limits wasted memory after the initial growth phase.
     */
    private static final int GROWTH_RATE = 32;

    /* The amount of space allocated in each value. */
    protected volatile int allocated_size;

    /* The number of indices used in maps so far. */
    private volatile int used_size;

    /* This is the real navigable map, responsible for all real work. */
    private final NavigableMap<K, MultiValue> backingMap;

    /**
     * Create a new factory
     * 
     * @param comparator
     *            the comparator to use for map keys
     * @param initialSize
     *            the amount of space to preallocate for basin maps.
     */
    public BasinMapFactory(Comparator<? super K> comparator, int initialSize) {
        allocated_size = initialSize;
        used_size = 0;
        this.backingMap = new TreeMap<K, MultiValue>(comparator);
    }

    /**
     * Get a new map from this factory.
     * 
     * @return
     */
    public NavigableMap<K, Float> getMap() {
        return new MapView<K>(this, getNextIndex(),
                backingMap);
    }
    
    /**
     * Get a new map from this factory, populated with all the values from m. If
     * m has the same keyset as other maps in this factory than this will
     * populate the new map faster than putAll.
     * 
     * @param m
     *            NavigableMap, must have the same comparator as this factory.
     * @return
     */
    public NavigableMap<K, Float> getMap(NavigableMap<K, Float> m) {
        Comparator<? super K> bc = backingMap.comparator();
        Comparator<? super K> mc = m.comparator();
        if (mc != bc && (mc == null || !mc.equals(bc))) {
            throw new IllegalArgumentException(
                    "Maps can only be constructed if the compators are the same.");
        }
        int index = getNextIndex();
        Iterator<Entry<K, MultiValue>> bit = backingMap.entrySet().iterator();
        Iterator<Entry<K, Float>> mit = m.entrySet().iterator();
        NavigableMap<K, Float> r = new MapView<K>(this, index,
                backingMap);
        /*
         * If both maps have the same keys, then iterating both simultaneously
         * is faster than multiple puts because it avoids doing multiple lookups
         */
        while (bit.hasNext() && mit.hasNext()) {
            Entry<K, MultiValue> bent = bit.next();
            Entry<K, Float> ment = mit.next();
            if (ment.getKey().equals(bent.getKey())) {
                bent.getValue().put(index, ment.getValue());
            } else {
                /* It turns out keys are not equals */
                r.put(ment.getKey(), ment.getValue());
                break;
            }
        }
        /*
         * This loop is only used if the backingMap was empty or if for some
         * reason the backingMap and the new map do not have the same keys.
         */
        while (mit.hasNext()) {
            Entry<K, Float> ment = mit.next();
            r.put(ment.getKey(), ment.getValue());
        }
        return r;
    }

    /* get the next free index for use in a MapView */
    private int getNextIndex() {
        synchronized (this) {
            int index = used_size;
            used_size += 1;
            if (used_size >= allocated_size) {
                allocated_size += GROWTH_RATE;
                for (MultiValue v : backingMap.values()) {
                    v.grow(allocated_size);

                }
            }
            return index;
        }
    }

    /**
     * Value within the backing map. This contains the raw data values for each
     * basin. In addition it contains a boolean for each basin indicating if the
     * value has been set. While most of the time all basins will be set, the
     * extra boolean is needed to allow different views to function as
     * independent maps, which is needed while new data is getting added.
     */
    private static class MultiValue {

        /* actual values for all basins. */
        private float[] data;

        /* booleans indicating whether a value has been set yet for a basin */
        private BitSet occupied;

        /*
         * The number of occupied basins for this time, when its zero the time
         * can be removed from the backing map.
         */
        private int size = 0;

        public MultiValue(int allocated_size) {
            this.data = new float[allocated_size];
            this.occupied = new BitSet(allocated_size);
        }

        public void grow(int allocated_size) {
            data = Arrays.copyOf(data, allocated_size);
            BitSet occupied = new BitSet(allocated_size);
            occupied.or(this.occupied);
            this.occupied = occupied;
        }

        public Float put(int index, float value) {
            Float oldValue = null;
            if (occupied.get(index)) {
                oldValue = data[index];
            } else {
                occupied.set(index);
                size += 1;
            }
            data[index] = value;
            return oldValue;
        }

        public Float get(int index) {
            if (occupied.get(index)) {
                return data[index];
            } else {
                return null;
            }
        }

        public boolean contains(int index) {
            return occupied.get(index);
        }

        public Float remove(int index) {
            if (occupied.get(index)) {
                occupied.clear(index);
                size -= 1;
                return data[index];
            }
            return null;
        }

        public boolean isEmpty() {
            return size == 0;
        }

    }

    /**
     * NavigableMap implementation which provides a view into the backingMap at
     * a specific index. This class can map any NavigableMap<K, MultiValue>
     * which simplifies creating sub maps or descending map because all that is
     * required is getting new maps from the backingMap and wrapping them in a
     * new MapView.
     */
    private static class MapView<K> extends AbstractMap<K, Float> implements
            NavigableMap<K, Float> {

        /* Factory, for syncronization */
        private final BasinMapFactory<K> factory;

        /* index into the MultiValue */
        private final int index;

        /* backingMap where all the data really lives. */
        private final NavigableMap<K, MultiValue> backingMap;

        public MapView(BasinMapFactory<K> factory, int index,
                NavigableMap<K, MultiValue> backingMap) {
            this.factory = factory;
            this.index = index;
            this.backingMap = backingMap;
        }

        @Override
        public Float put(K key, Float value) {
            synchronized (factory) {
                MultiValue v = backingMap.get(key);
                if (v == null) {
                    v = new MultiValue(factory.allocated_size);
                    backingMap.put(key, v);
                }
                return v.put(index, value);
            }
        }

        @Override
        public boolean containsKey(Object key) {
            MultiValue v = backingMap.get(key);
            if (v != null) {
                return v.contains(index);
            } else {
                return false;
            }
        }

        @Override
        public Float get(Object key) {
            MultiValue v = backingMap.get(key);
            if (v != null) {
                return v.get(index);
            } else {
                return null;
            }
        }

        @Override
        public Float remove(Object key) {
            Float oldValue = null;
            MultiValue v = backingMap.get(key);
            if (v != null) {
                synchronized (factory) {
                    oldValue = v.remove(index);
                    if (v.isEmpty()) {
                        backingMap.remove(key);
                    }
                }
            }
            return oldValue;
        }

        @Override
        public boolean isEmpty() {
            /*
             * The default implementation uses size() which will iterate over
             * all elements, this is much faster.
             */
            return !entrySet().iterator().hasNext();
        }

        @Override
        public Comparator<? super K> comparator() {
            return backingMap.comparator();
        }

        @Override
        public K firstKey() {
            Entry<K, Float> e = firstEntry();
            if (e != null) {
                return e.getKey();
            }
            return null;
        }

        @Override
        public K lastKey() {
            Entry<K, Float> e = lastEntry();
            if (e != null) {
                return e.getKey();
            }
            return null;
        }

        @Override
        public Entry<K, Float> lowerEntry(K key) {
            return headMap(key).lastEntry();
        }

        @Override
        public K lowerKey(K key) {
            return headMap(key).lastKey();

        }

        @Override
        public Entry<K, Float> floorEntry(K key) {
            return headMap(key, true).lastEntry();

        }

        @Override
        public K floorKey(K key) {
            return headMap(key, true).lastKey();

        }

        @Override
        public Entry<K, Float> ceilingEntry(K key) {
            return tailMap(key).firstEntry();
        }

        @Override
        public K ceilingKey(K key) {
            return tailMap(key).firstKey();

        }

        @Override
        public Entry<K, Float> higherEntry(K key) {
            return tailMap(key, false).firstEntry();
        }

        @Override
        public K higherKey(K key) {
            return tailMap(key, false).firstKey();

        }

        @Override
        public Entry<K, Float> firstEntry() {
            for (Entry<K, Float> e : entrySet()) {
                return e;
            }
            return null;
        }

        @Override
        public Entry<K, Float> lastEntry() {
            return descendingMap().firstEntry();
        }

        @Override
        public Entry<K, Float> pollFirstEntry() {
            Iterator<Entry<K, Float>> it = entrySet().iterator();
            if (it.hasNext()) {
                Entry<K, Float> e = it.next();
                it.remove();
                return e;
            }
            return null;
        }

        @Override
        public Entry<K, Float> pollLastEntry() {
            return descendingMap().pollFirstEntry();
        }

        @Override
        public NavigableMap<K, Float> descendingMap() {
            return new MapView<K>(factory, index, backingMap.descendingMap());
        }

        @Override
        public NavigableSet<K> navigableKeySet() {
            return new NavigableKeySet<K>(this);
        }

        @Override
        public NavigableSet<K> descendingKeySet() {
            return descendingMap().navigableKeySet();
        }

        @Override
        public NavigableMap<K, Float> subMap(K fromKey, boolean fromInclusive,
                K toKey, boolean toInclusive) {
            return new MapView<K>(factory, index, backingMap.subMap(fromKey,
                    fromInclusive, toKey, toInclusive));
        }

        @Override
        public NavigableMap<K, Float> headMap(K toKey, boolean inclusive) {
            return new MapView<K>(factory, index, backingMap.headMap(toKey,
                    inclusive));
        }

        @Override
        public NavigableMap<K, Float> tailMap(K fromKey, boolean inclusive) {
            return new MapView<K>(factory, index, backingMap.tailMap(fromKey,
                    inclusive));
        }

        @Override
        public NavigableMap<K, Float> subMap(K fromKey, K toKey) {
            return new MapView<K>(factory, index, backingMap.subMap(fromKey,
                    true, toKey, false));
        }

        @Override
        public NavigableMap<K, Float> headMap(K toKey) {
            return new MapView<K>(factory, index, backingMap.headMap(toKey,
                    false));

        }

        @Override
        public NavigableMap<K, Float> tailMap(K fromKey) {
            return new MapView<K>(factory, index, backingMap.headMap(fromKey,
                    true));

        }

        @Override
        public Set<Entry<K, Float>> entrySet() {
            return new EntrySet<K>(index, backingMap.entrySet());
        }

    }

    /**
     * Entry set for a MapView, just a wrapper over a Set<Entry<K, MultiValue>>
     */
    private static class EntrySet<K> extends AbstractSet<Entry<K, Float>> {

        private final int index;

        private final Set<Entry<K, MultiValue>> backingSet;

        public EntrySet(int index, Set<Entry<K, MultiValue>> backingSet) {
            this.index = index;
            this.backingSet = backingSet;
        }

        @Override
        public Iterator<Entry<K, Float>> iterator() {
            return new EntryIterator<K>(index, backingSet.iterator());
        }

        @Override
        public int size() {
            Iterator<Entry<K, Float>> it = iterator();
            int i = 0;
            while (it.hasNext()) {
                it.next();
                i += 1;
            }
            return i;
        }

    }

    /**
     * Iterator implementation for an EntrySet, This wraps an Iterator<Entry<K,
     * MultiValue>> but has extra logic for skipping over values that exist in
     * the backingSet but are not occupied within the MultiValue.
     */
    private static class EntryIterator<K> implements Iterator<Entry<K, Float>> {

        private final int index;

        private final Iterator<Entry<K, MultiValue>> backingIterator;

        /* The next valid entry in the backingMap */
        private transient Entry<K, MultiValue> next;

        /* Previous Value returned by next() */
        private transient Entry<K, MultiValue> previous;

        public EntryIterator(int index,
                Iterator<Entry<K, MultiValue>> backingIterator) {
            this.index = index;
            this.backingIterator = backingIterator;
        }

        private Entry<K, MultiValue> checkNext() {
            if (next == null) {
                while (backingIterator.hasNext()) {
                    Entry<K, MultiValue> e = backingIterator.next();
                    previous = null;
                    MultiValue v = e.getValue();
                    if (v.contains(index)) {
                        next = e;
                        return next;
                    }
                }
            }
            return next;
        }

        @Override
        public boolean hasNext() {
            return checkNext() != null;
        }

        @Override
        public Entry<K, Float> next() {
            Entry<K, MultiValue> next = checkNext();
            previous = next;
            if (next == null) {
                return null;
            } else {
                this.next = null;
                return new EntryImpl<K>(index, next);
            }
        }

        @Override
        public void remove() {
            if (previous == null) {
                throw new IllegalStateException(
                        "Cannot remove from iterator because next() was not called.");
            }
            MultiValue v = previous.getValue();
            previous = null;
            v.remove(index);
            if (v.isEmpty()) {
                backingIterator.remove();
            }
        }

    }

    /**
     * Entry for a MapView, just a wrapper over a Entry<K, MultiValue>.
     */
    private static class EntryImpl<K> implements Entry<K, Float> {

        private final int index;

        private final Entry<K, MultiValue> backingEntry;

        public EntryImpl(int index, Entry<K, MultiValue> timeEntry) {
            this.index = index;
            this.backingEntry = timeEntry;
        }

        @Override
        public K getKey() {
            return backingEntry.getKey();
        }

        @Override
        public Float getValue() {
            return backingEntry.getValue().get(index);
        }

        @Override
        public Float setValue(Float value) {
            return backingEntry.getValue().put(index, value);
        }

    }

}
