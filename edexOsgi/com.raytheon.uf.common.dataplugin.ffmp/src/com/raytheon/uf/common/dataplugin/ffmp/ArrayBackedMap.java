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
package com.raytheon.uf.common.dataplugin.ffmp;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;

/**
 * The ArrayBacked map is a navigable map implementation that is designed
 * specifically for fast serialization/deserialization because the underlining
 * data structure are simple sorted primitive arrays. It also has very efficient
 * memory usage because there are no intermediary Objects associated with
 * entries.
 * 
 * Most operations are implemented as a binary search on the key array so they
 * are guaranteed to complete in log(n) time. The exception to this rule is
 * structural modifications (A structural modification is any operation that
 * adds or deletes one or more mappings; merely changing the value associated
 * with an existing key is not a structural modification.) Structural
 * modification require copying both underlining arrays which will take up to n
 * time and should not be considered a high performance operation. For any use
 * of a NavigableMap which requires multiple structural modifications TreeMap
 * would be a much better choice. For combining this map with other SortedMaps
 * of similar size, putAll has been optimized and will perform much better then
 * a naive loop which puts all elements.
 * 
 * This map is not thread safe for modification and does not even throw
 * ConcurrentModificationExceptions. Any concurrent modification will result in
 * strange behavior and not explicit exceptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 18, 2013            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ArrayBackedMap extends AbstractMap<Date, Float> implements
        NavigableMap<Date, Float> {

    private long[] keys;

    private float[] values;

    public ArrayBackedMap(long[] keys, float[] values) {
        this.keys = keys;
        this.values = values;
    }

    public ArrayBackedMap(SortedMap<Date, Float> map) {
        this.keys = new long[map.size()];
        this.values = new float[keys.length];
        int i = 0;
        for (Entry<Date, Float> e : map.entrySet()) {
            keys[i] = e.getKey().getTime();
            values[i] = e.getValue();
            i += 1;
        }
    }

    public long[] getKeys() {
        return keys;
    }

    public float[] getValues() {
        return values;
    }

    /**
     * Replacing existing values is fast (log(n)) but inserting new values is
     * slow(n).
     */
    @Override
    public Float put(Date key, Float value) {
        int index = Arrays.binarySearch(keys, key.getTime());
        if (index >= 0) {
            float old = values[index];
            values[index] = value;
            return old;
        } else {
            long[] keys = new long[values.length + 1];
            float[] values = new float[keys.length];
            System.arraycopy(this.keys, 0, keys, 0, -index - 1);
            System.arraycopy(this.values, 0, values, 0, -index - 1);
            keys[-index - 1] = key.getTime();
            values[-index - 1] = value;
            System.arraycopy(this.keys, -index - 1, keys, -index,
                    this.values.length + index + 1);
            System.arraycopy(this.values, -index - 1, values, -index,
                    this.values.length + index + 1);
            this.keys = keys;
            this.values = values;
            return null;
        }
    }

    /**
     * Sorted maps which use the default sort order will be put in m+n time.
     * Otherwise it will take m*log(m_n).
     */
    @Override
    @SuppressWarnings("unchecked")
    public void putAll(Map<? extends Date, ? extends Float> map) {
        if (map instanceof SortedMap && !map.isEmpty()) {
            Comparator<?> c = ((SortedMap<?, ?>) map).comparator();
            if (c == null) {
                // much faster putAll.
                long[] keys = new long[values.length + map.size()];
                float[] values = new float[keys.length];
                int oldIndex = 0;
                int newIndex = 0;
                Iterator<?> it = map.entrySet().iterator();
                Entry<Date, Float> next = (Entry<Date, Float>) it.next();
                long nextLong = next.getKey().getTime();
                long myNextLong = this.keys[oldIndex];
                while (nextLong != Long.MAX_VALUE
                        || myNextLong != Long.MAX_VALUE) {
                    boolean incMe = false;
                    if (nextLong <= myNextLong) {
                        incMe = nextLong == myNextLong;
                        keys[newIndex] = nextLong;
                        values[newIndex] = next.getValue();
                        if (it.hasNext()) {
                            next = (Entry<Date, Float>) it.next();
                            nextLong = next.getKey().getTime();
                        } else {
                            nextLong = Long.MAX_VALUE;
                        }
                    } else {
                        keys[newIndex] = myNextLong;
                        values[newIndex] = this.values[oldIndex];
                        incMe = true;
                    }
                    if (incMe) {
                        oldIndex += 1;
                        if (oldIndex < this.keys.length) {
                            myNextLong = this.keys[oldIndex];
                        } else {
                            myNextLong = Long.MAX_VALUE;
                        }
                    }
                    newIndex += 1;
                }
                if (newIndex < keys.length) {
                    // there were some duplicates;
                    keys = Arrays.copyOf(keys, newIndex);
                    values = Arrays.copyOf(values, newIndex);
                }
                this.keys = keys;
                this.values = values;
            } else {
                super.putAll(map);
            }
        } else {
            super.putAll(map);
        }
    }

    @Override
    public Float get(Object key) {
        if (key instanceof Date) {
            int index = Arrays.binarySearch(keys, ((Date) key).getTime());
            if (index >= 0) {
                return values[index];
            }
        }
        return null;
    }

    @Override
    public boolean containsKey(Object key) {
        return get(key) != null;
    }

    @Override
    public Comparator<? super Date> comparator() {
        return null;
    }

    @Override
    public Date firstKey() {
        if (keys.length == 0) {
            return null;
        } else {
            return new Date(keys[0]);
        }
    }

    @Override
    public Date lastKey() {
        if (keys.length == 0) {
            return null;
        } else {
            return new Date(keys[keys.length - 1]);
        }

    }

    @Override
    public Entry<Date, Float> firstEntry() {
        if (keys.length == 0) {
            return null;
        } else {
            return new EntryImpl(0);
        }
    }

    @Override
    public Entry<Date, Float> lastEntry() {
        if (keys.length == 0) {
            return null;
        } else {
            return new EntryImpl(keys.length - 1);
        }
    }

    @Override
    public Set<Entry<Date, Float>> entrySet() {
        return new EntrySet(null, null);
    }

    public NavigableSet<Date> descendingKeySet() {
        return descendingMap().navigableKeySet();
    }

    @Override
    public NavigableSet<Date> navigableKeySet() {
        return new NavigableKeySet(this);
    }

    private int lowerIndex(Date key, boolean inclusive) {
        int index = Arrays.binarySearch(keys, key.getTime());
        if (index < 0) {
            return -index - 2;
        } else if (inclusive) {
            return index;
        } else {
            return index - 1;
        }
    }

    private int higherIndex(Date key, boolean inclusive) {
        int index = Arrays.binarySearch(keys, key.getTime());
        if (index < 0) {
            return -index - 1;
        } else if (inclusive) {
            return index;
        } else {
            return index + 1;
        }
    }

    @Override
    public Entry<Date, Float> lowerEntry(Date key) {
        int index = lowerIndex(key, false);
        if (index >= 0) {
            return new EntryImpl(index);
        }
        return null;
    }

    @Override
    public Date lowerKey(Date key) {
        int index = lowerIndex(key, false);
        if (index >= 0) {
            return new Date(keys[index]);
        }
        return null;
    }

    @Override
    public Entry<Date, Float> floorEntry(Date key) {
        int index = lowerIndex(key, true);
        if (index >= 0) {
            return new EntryImpl(index);
        }
        return null;
    }

    @Override
    public Date floorKey(Date key) {
        int index = lowerIndex(key, true);
        if (index >= 0) {
            return new Date(keys[index]);
        }
        return null;
    }

    @Override
    public Entry<Date, Float> higherEntry(Date key) {
        int index = higherIndex(key, false);
        if (index < keys.length) {
            return new EntryImpl(index);
        }
        return null;
    }

    @Override
    public Date higherKey(Date key) {
        int index = higherIndex(key, false);
        if (index < keys.length) {
            return new Date(keys[index]);
        }
        return null;
    }

    @Override
    public Entry<Date, Float> ceilingEntry(Date key) {
        int index = higherIndex(key, true);
        if (index < keys.length) {
            return new EntryImpl(index);
        }
        return null;
    }

    public Date ceilingKey(Date key) {
        int index = higherIndex(key, true);
        if (index < keys.length) {
            return new Date(keys[index]);
        }
        return null;
    }

    @Override
    public NavigableMap<Date, Float> descendingMap() {
        return new DescendingMap(new SubMap(null, null));
    }

    @Override
    public NavigableMap<Date, Float> subMap(Date fromKey, Date toKey) {
        return subMap(fromKey, true, toKey, false);
    }

    @Override
    public NavigableMap<Date, Float> headMap(Date toKey) {
        return headMap(toKey, false);
    }

    @Override
    public NavigableMap<Date, Float> tailMap(Date fromKey) {
        return tailMap(fromKey, true);
    }

    @Override
    public NavigableMap<Date, Float> subMap(Date fromKey,
            boolean fromInclusive, Date toKey, boolean toInclusive) {
        if (!fromInclusive) {
            fromKey = new Date(fromKey.getTime() + 1);
        }
        if (!toInclusive) {
            toKey = new Date(toKey.getTime() - 1);
        }
        return new SubMap(fromKey, toKey);
    }

    @Override
    public NavigableMap<Date, Float> headMap(Date toKey, boolean inclusive) {
        if (!inclusive) {
            toKey = new Date(toKey.getTime() - 1);
        }
        return new SubMap(null, toKey);
    }

    @Override
    public NavigableMap<Date, Float> tailMap(Date fromKey, boolean inclusive) {
        if (!inclusive) {
            fromKey = new Date(fromKey.getTime() + 1);
        }
        return new SubMap(fromKey, null);
    }

    @Override
    public Entry<Date, Float> pollFirstEntry() {
        // This could be made faster.
        Entry<Date, Float> e = firstEntry();
        remove(e.getKey());
        return e;
    }

    @Override
    public Entry<Date, Float> pollLastEntry() {
        // This could be made faster.
        Entry<Date, Float> e = lastEntry();
        remove(e.getKey());
        return e;
    }

    private void remove(int index) {
        long[] keys = new long[values.length - 1];
        float[] values = new float[keys.length];
        System.arraycopy(this.keys, 0, keys, 0, index);
        System.arraycopy(this.values, 0, values, 0, index);
        System.arraycopy(this.keys, index + 1, keys, index, this.values.length
                - index - 1);
        System.arraycopy(this.values, index + 1, values, index,
                this.values.length - index - 1);
        this.keys = keys;
        this.values = values;
    }

    private class EntrySet extends AbstractSet<Entry<Date, Float>> {

        private final boolean descending;

        private final Date startTime;

        private final Date endTime;

        public EntrySet(Date startTime, Date endTime) {
            this(startTime, endTime, false);
        }

        public EntrySet(Date startTime, Date endTime, boolean descending) {
            this.startTime = startTime;
            this.endTime = endTime;
            this.descending = descending;
        }

        @Override
        public Iterator<Entry<Date, Float>> iterator() {
            if (descending) {
                return new DecsendingEntryIterator(startTime, endTime);
            } else {
                return new EntryIterator(startTime, endTime);
            }

        }

        @Override
        public int size() {
            int startIndex = 0;
            int endIndex = keys.length;
            if (startTime != null) {
                startIndex = higherIndex(startTime, true);
            }
            if (endTime != null) {
                endIndex = lowerIndex(endTime, true) + 1;
            }
            return endIndex - startIndex;
        }

    }

    private class EntryIterator implements Iterator<Entry<Date, Float>> {

        private final long endTime;

        private int index;

        public EntryIterator(Date startTime, Date endTime) {
            if (startTime != null) {
                index = higherIndex(startTime, true);
            } else {
                index = 0;
            }
            if (endTime != null) {
                this.endTime = endTime.getTime();
            } else {
                this.endTime = Long.MAX_VALUE;
            }
        }

        @Override
        public boolean hasNext() {
            return index < keys.length && keys[index] <= endTime;
        }

        @Override
        public Entry<Date, Float> next() {
            if (hasNext()) {
                EntryImpl e = new EntryImpl(index);
                index += 1;
                return e;
            } else {
                throw new NoSuchElementException();
            }
        }

        @Override
        public void remove() {
            ArrayBackedMap.this.remove(index - 1);
        }

    }

    private class DecsendingEntryIterator implements
            Iterator<Entry<Date, Float>> {

        private final long endTime;

        private int index;

        public DecsendingEntryIterator(Date startTime, Date endTime) {
            if (endTime != null) {
                index = lowerIndex(endTime, true);
            } else {
                index = keys.length - 1;
            }
            if (startTime != null) {
                this.endTime = startTime.getTime();
            } else {
                this.endTime = Long.MIN_VALUE;
            }
        }

        @Override
        public boolean hasNext() {
            return index >= 0 && keys[index] >= endTime;
        }

        @Override
        public Entry<Date, Float> next() {
            if (hasNext()) {
                EntryImpl e = new EntryImpl(index);
                index -= 1;
                return e;
            } else {
                throw new NoSuchElementException();
            }
        }

        @Override
        public void remove() {
            ArrayBackedMap.this.remove(index + 1);
        }

    }

    private class EntryImpl implements Entry<Date, Float> {

        private final int index;

        public EntryImpl(int index) {
            this.index = index;
        }

        @Override
        public Date getKey() {
            return new Date(keys[index]);
        }

        @Override
        public Float getValue() {
            return values[index];
        }

        @Override
        public Float setValue(Float value) {
            Float f = values[index];
            values[index] = value;
            return f;
        }

    }

    private static class NavigableKeySet extends AbstractSet<Date> implements
            NavigableSet<Date> {

        private final NavigableMap<Date, Float> map;

        public NavigableKeySet(NavigableMap<Date, Float> map) {
            this.map = map;
        }

        @Override
        public Iterator<Date> iterator() {
            return map.keySet().iterator();
        }

        @Override
        public int size() {
            return map.size();
        }

        @Override
        public Comparator<? super Date> comparator() {
            return map.comparator();
        }

        @Override
        public Date first() {
            return map.firstKey();
        }

        @Override
        public Date last() {
            return map.lastKey();
        }

        @Override
        public Date lower(Date e) {
            return map.lowerKey(e);
        }

        @Override
        public Date floor(Date e) {
            return map.floorKey(e);
        }

        @Override
        public Date ceiling(Date e) {
            return map.ceilingKey(e);
        }

        @Override
        public Date higher(Date e) {
            return map.higherKey(e);
        }

        @Override
        public Date pollFirst() {
            return map.pollFirstEntry().getKey();
        }

        @Override
        public Date pollLast() {
            return map.pollLastEntry().getKey();
        }

        @Override
        public NavigableSet<Date> descendingSet() {
            return map.descendingKeySet();
        }

        @Override
        public Iterator<Date> descendingIterator() {
            return descendingSet().iterator();
        }

        @Override
        public NavigableSet<Date> subSet(Date fromElement,
                boolean fromInclusive, Date toElement, boolean toInclusive) {
            return map.subMap(fromElement, fromInclusive, toElement,
                    toInclusive).navigableKeySet();
        }

        @Override
        public NavigableSet<Date> headSet(Date toElement, boolean inclusive) {
            return map.headMap(toElement, inclusive).navigableKeySet();
        }

        @Override
        public NavigableSet<Date> tailSet(Date fromElement, boolean inclusive) {
            return map.tailMap(fromElement, inclusive).navigableKeySet();
        }

        @Override
        public SortedSet<Date> subSet(Date fromElement, Date toElement) {
            return subSet(fromElement, true, toElement, false);
        }

        @Override
        public SortedSet<Date> headSet(Date toElement) {
            return headSet(toElement, false);
        }

        @Override
        public SortedSet<Date> tailSet(Date fromElement) {
            return tailSet(fromElement, true);
        }

    }

    private class SubMap extends AbstractMap<Date, Float> implements
            NavigableMap<Date, Float> {

        private final Date min;

        private final Date max;

        public SubMap(Date min, Date max) {
            this.min = min;
            this.max = max;
        }

        @Override
        public Comparator<? super Date> comparator() {
            return ArrayBackedMap.this.comparator();
        }

        @Override
        public Date firstKey() {
            if (min == null) {
                return ArrayBackedMap.this.firstKey();
            } else {
                return ArrayBackedMap.this.ceilingKey(min);
            }
        }

        @Override
        public Date lastKey() {
            if (max == null) {
                return ArrayBackedMap.this.lastKey();
            } else {
                return ArrayBackedMap.this.floorKey(max);
            }
        }

        @Override
        public Entry<Date, Float> lowerEntry(Date key) {
            if (max != null && key.after(max)) {
                return ArrayBackedMap.this.floorEntry(max);
            }
            Entry<Date, Float> e = ArrayBackedMap.this.lowerEntry(key);
            if (e != null && min != null && e.getKey().before(min)) {
                return null;
            }
            return e;
        }

        @Override
        public Date lowerKey(Date key) {
            Entry<Date, Float> e = lowerEntry(key);
            return e == null ? null : e.getKey();
        }

        @Override
        public Entry<Date, Float> floorEntry(Date key) {
            if (max != null && key.after(max)) {
                return ArrayBackedMap.this.floorEntry(max);
            }
            Entry<Date, Float> e = ArrayBackedMap.this.floorEntry(key);
            if (e != null && min != null && e.getKey().before(min)) {
                return null;
            }
            return e;
        }

        @Override
        public Date floorKey(Date key) {
            Entry<Date, Float> e = floorEntry(key);
            return e == null ? null : e.getKey();
        }

        @Override
        public Entry<Date, Float> ceilingEntry(Date key) {
            if (min != null && key.before(min)) {
                return ArrayBackedMap.this.floorEntry(min);
            }
            Entry<Date, Float> e = ArrayBackedMap.this.ceilingEntry(key);
            if (e != null && max != null && e.getKey().after(max)) {
                return null;
            }
            return e;
        }

        @Override
        public Date ceilingKey(Date key) {
            Entry<Date, Float> e = ceilingEntry(key);
            return e == null ? null : e.getKey();
        }

        @Override
        public Entry<Date, Float> higherEntry(Date key) {
            if (min != null && key.before(min)) {
                return ArrayBackedMap.this.floorEntry(min);
            }
            Entry<Date, Float> e = ArrayBackedMap.this.higherEntry(key);
            if (e != null && max != null && e.getKey().after(max)) {
                return null;
            }
            return e;
        }

        @Override
        public Date higherKey(Date key) {
            Entry<Date, Float> e = higherEntry(key);
            return e == null ? null : e.getKey();
        }

        @Override
        public Entry<Date, Float> firstEntry() {
            if (min == null) {
                return ArrayBackedMap.this.firstEntry();
            } else {
                return ArrayBackedMap.this.ceilingEntry(min);
            }
        }

        @Override
        public Entry<Date, Float> lastEntry() {
            if (max == null) {
                return ArrayBackedMap.this.lastEntry();
            } else {
                return ArrayBackedMap.this.floorEntry(max);
            }
        }

        @Override
        public Entry<Date, Float> pollFirstEntry() {
            if (min == null) {
                return ArrayBackedMap.this.pollFirstEntry();
            } else {
                Entry<Date, Float> e = firstEntry();
                ArrayBackedMap.this.remove(e.getKey());
                return e;
            }
        }

        @Override
        public Entry<Date, Float> pollLastEntry() {
            if (min == null) {
                return ArrayBackedMap.this.pollLastEntry();
            } else {
                Entry<Date, Float> e = lastEntry();
                ArrayBackedMap.this.remove(e.getKey());
                return e;
            }
        }

        @Override
        public NavigableMap<Date, Float> descendingMap() {
            return new DescendingMap(this);
        }

        @Override
        public NavigableSet<Date> navigableKeySet() {
            return new NavigableKeySet(this);
        }

        @Override
        public NavigableSet<Date> descendingKeySet() {
            return descendingMap().navigableKeySet();
        }

        @Override
        public SubMap subMap(Date fromKey,
                boolean fromInclusive, Date toKey, boolean toInclusive) {
            if (!fromInclusive) {
                fromKey = new Date(fromKey.getTime() + 1);
            }
            if (!toInclusive) {
                toKey = new Date(toKey.getTime() - 1);
            }
            if (checkKey(fromKey) && checkKey(toKey)) {
                return new SubMap(fromKey, toKey);
            }
            throw new IllegalArgumentException();
        }

        @Override
        public SubMap headMap(Date toKey, boolean inclusive) {
            if (!inclusive) {
                toKey = new Date(toKey.getTime() - 1);
            }
            if (checkKey(toKey)) {
                return new SubMap(min, toKey);
            }
            throw new IllegalArgumentException();
        }

        @Override
        public SubMap tailMap(Date fromKey, boolean inclusive) {
            if (!inclusive) {
                fromKey = new Date(fromKey.getTime() + 1);
            }
            if (checkKey(fromKey)) {
                return new SubMap(fromKey, max);
            }
            throw new IllegalArgumentException();
        }

        public SubMap subMap(Date fromKey, Date toKey) {
            return subMap(fromKey, true, toKey, false);
        }

        @Override
        public SubMap headMap(Date toKey) {
            return headMap(toKey, false);
        }

        @Override
        public SubMap tailMap(Date fromKey) {
            return tailMap(fromKey, true);
        }

        @Override
        public Set<Entry<Date, Float>> entrySet() {
            return new EntrySet(min, max);
        }

        @Override
        public boolean containsKey(Object key) {
            return get(key) != null;
        }

        @Override
        public Float get(Object key) {
            if (checkKey(key)) {
                return ArrayBackedMap.this.get(key);
            }
            return null;
        }

        @Override
        public Float put(Date key, Float value) {
            if (checkKey(key)) {
                return ArrayBackedMap.this.put(key, value);
            }
            throw new IllegalArgumentException();
        }

        @Override
        public void putAll(Map<? extends Date, ? extends Float> m) {
            if (min != null || max != null) {
                for (Date key : m.keySet()) {
                    if (!checkKey(key)) {
                        throw new IllegalArgumentException();
                    }
                }
            }
            ArrayBackedMap.this.putAll(m);
        }

        @Override
        public Float remove(Object key) {
            if (checkKey(key)) {
                return ArrayBackedMap.this.remove(key);
            }
            throw new IllegalArgumentException();
        }

        private boolean checkKey(Object key) {
            if (key instanceof Date) {
                Date k = (Date) key;
                if (min != null && min.after(k)) {
                    return false;
                }
                if (max != null && max.before(k)) {
                    return false;
                }
                return true;
            }
            return false;
        }

    }

    private class DescendingMap extends AbstractMap<Date, Float> implements
            NavigableMap<Date, Float> {

        private final SubMap subMap;

        public DescendingMap(SubMap subMap) {
            this.subMap = subMap;
        }

        @Override
        public Comparator<? super Date> comparator() {
            return Collections.reverseOrder(subMap.comparator());
        }

        @Override
        public Date firstKey() {
            return subMap.lastKey();
        }

        @Override
        public Date lastKey() {
            return subMap.firstKey();
        }

        @Override
        public Entry<Date, Float> lowerEntry(Date key) {
            return subMap.higherEntry(key);
        }

        @Override
        public Date lowerKey(Date key) {
            return subMap.higherKey(key);
        }

        @Override
        public Entry<Date, Float> floorEntry(Date key) {
            return subMap.ceilingEntry(key);
        }

        @Override
        public Date floorKey(Date key) {
            return subMap.ceilingKey(key);
        }

        @Override
        public Entry<Date, Float> ceilingEntry(Date key) {
            return subMap.floorEntry(key);
        }

        @Override
        public Date ceilingKey(Date key) {
            return subMap.floorKey(key);
        }

        @Override
        public Entry<Date, Float> higherEntry(Date key) {
            return subMap.lowerEntry(key);
        }

        @Override
        public Date higherKey(Date key) {
            return subMap.lowerKey(key);
        }

        @Override
        public Entry<Date, Float> firstEntry() {
            return subMap.lastEntry();
        }

        @Override
        public Entry<Date, Float> lastEntry() {
            return subMap.firstEntry();
        }

        @Override
        public Entry<Date, Float> pollFirstEntry() {
            return subMap.pollLastEntry();
        }

        @Override
        public Entry<Date, Float> pollLastEntry() {
            return subMap.pollFirstEntry();
        }

        @Override
        public NavigableMap<Date, Float> descendingMap() {
            return subMap;
        }

        @Override
        public NavigableSet<Date> navigableKeySet() {
            return new NavigableKeySet(this);
        }

        @Override
        public NavigableSet<Date> descendingKeySet() {
            return subMap.navigableKeySet();
        }

        @Override
        public NavigableMap<Date, Float> subMap(Date fromKey,
                boolean fromInclusive, Date toKey, boolean toInclusive) {
            return new DescendingMap(subMap.subMap(toKey, toInclusive, fromKey,
                    fromInclusive));
        }

        @Override
        public NavigableMap<Date, Float> headMap(Date toKey, boolean inclusive) {
            return new DescendingMap(subMap.tailMap(toKey, inclusive));
        }

        @Override
        public NavigableMap<Date, Float> tailMap(Date fromKey, boolean inclusive) {
            return new DescendingMap(subMap.headMap(fromKey, inclusive));
        }

        @Override
        public NavigableMap<Date, Float> subMap(Date fromKey, Date toKey) {
            return new DescendingMap(subMap.subMap(toKey, false, fromKey, true));
        }

        @Override
        public NavigableMap<Date, Float> headMap(Date toKey) {
            return new DescendingMap(subMap.tailMap(toKey, false));
        }

        @Override
        public NavigableMap<Date, Float> tailMap(Date fromKey) {
            return new DescendingMap(subMap.headMap(fromKey, true));
        }

        @Override
        public Set<Entry<Date, Float>> entrySet() {
            return new EntrySet(subMap.min, subMap.max, true);
        }

        @Override
        public boolean containsValue(Object value) {
            return subMap.containsValue(value);
        }

        @Override
        public boolean containsKey(Object key) {
            return subMap.containsKey(key);
        }

        @Override
        public Float get(Object key) {
            return subMap.get(key);
        }

        @Override
        public Float put(Date key, Float value) {
            return subMap.put(key, value);
        }

        @Override
        public Float remove(Object key) {
            return subMap.remove(key);
        }

        @Override
        public void putAll(Map<? extends Date, ? extends Float> m) {
            subMap.putAll(m);
        }

        @Override
        public void clear() {
            subMap.clear();
        }

    }

    /**
     * compares the behavior of the provided map to a treeMap and verify all
     * operations produce equivalent results.
     * 
     * @param map
     */
    protected static void test(NavigableMap<Date, Float> map) {
        TreeMap<Date, Float> treeMap = new TreeMap<Date, Float>(map);
        if (!map.descendingKeySet().equals(treeMap.descendingKeySet())) {
            System.err.println("DescendingKeySet test failed");
        }
        if (!map.navigableKeySet().equals(treeMap.navigableKeySet())) {
            System.err.println("NavigableKeySet test failed");
        }
        if (!map.firstKey().equals(treeMap.firstKey())) {
            System.err.println("FirstKey test failed");
        }
        if (!map.lastKey().equals(treeMap.lastKey())) {
            System.err.println("LastKey test failed");
        }

        List<Date> testDates = new ArrayList<Date>();

        Iterator<Date> it = map.keySet().iterator();
        Date prev = it.next();
        Date next = it.next();
        for (int i = 0; i < map.size() / 3; i += 1) {
            prev = next;
            next = it.next();
        }
        testDates.add(next);
        testDates.add(prev);
        // a date about 1/3 of the way in and not equal to any keys
        testDates.add(new Date(prev.getTime() / 2 + next.getTime() / 2));
        for (int i = 0; i < map.size() / 3; i += 1) {
            prev = next;
            next = it.next();
        }
        testDates.add(next);
        testDates.add(prev);
        // a date about 2/3 of the way in and not equal to any keys
        testDates.add(new Date(prev.getTime() / 2 + next.getTime() / 2));

        testDates.add(map.firstKey());
        testDates.add(map.lastKey());
        // some dates just before and after the beginning and end.
        testDates.add(new Date(map.firstKey().getTime() - 1000));
        testDates.add(new Date(map.firstKey().getTime() + 1000));
        testDates.add(new Date(map.lastKey().getTime() - 1000));
        testDates.add(new Date(map.lastKey().getTime() + 1000));
        for (Date test : testDates) {
            Date d1 = map.ceilingKey(test);
            Date d2 = treeMap.ceilingKey(test);
            if (d1 == null) {
                if (d2 != null) {
                    System.err.println("CeilingKey test failed");
                }
            } else if (!d1.equals(d2)) {
                System.err.println("CeilingKey test failed");
            }
        }
        for (Date test : testDates) {
            Date d1 = map.higherKey(test);
            Date d2 = treeMap.higherKey(test);
            if (d1 == null) {
                if (d2 != null) {
                    System.err.println("HigherKey test failed");
                }
            } else if (!d1.equals(d2)) {
                System.err.println("HigherKey test failed");
            }
        }
        for (Date test : testDates) {
            Date d1 = map.floorKey(test);
            Date d2 = treeMap.floorKey(test);
            if (d1 == null) {
                if (d2 != null) {
                    System.err.println("FloorKey test failed");
                }
            } else if (!d1.equals(d2)) {
                System.err.println("FloorKey test failed");
            }
        }
        for (Date test : testDates) {
            Date d1 = map.lowerKey(test);
            Date d2 = treeMap.lowerKey(test);
            if (d1 == null) {
                if (d2 != null) {
                    System.err.println("LowerKey test failed");
                }
            } else if (!d1.equals(d2)) {
                System.err.println("LowerKey test failed");
            }
        }
        for (Date test : testDates) {
            NavigableMap<Date, Float> map1 = map.headMap(test, true);
            NavigableMap<Date, Float> map2 = treeMap.headMap(test, true);
            if (!map1.equals(map2)) {
                System.err.println("HeadMap test failed");
            }
        }
        for (Date test : testDates) {
            NavigableMap<Date, Float> map1 = map.tailMap(test, false);
            NavigableMap<Date, Float> map2 = treeMap.tailMap(test, false);
            if (!map1.equals(map2)) {
                System.err.println("TailMap test failed");
            }
        }
        for (Date test1 : testDates) {
            for (Date test2 : testDates) {
                try {
                    NavigableMap<Date, Float> map1 = map.subMap(test1, false,
                            test2, true);
                    NavigableMap<Date, Float> map2 = treeMap.subMap(test1,
                            false, test2, true);
                    if (!map1.equals(map2)) {
                        System.err.println("SubMap test failed");
                    }
                } catch (IllegalArgumentException e) {
                    ;// dates aren't in order and tree map freaks.
                }
            }
        }
        for (Date test : testDates) {
            map.remove(test);
            treeMap.remove(test);
        }
        if (!map.equals(treeMap)) {
            System.err.println("Remove test failed");
        }
        for (Date test : testDates) {
            map.put(test, 11.0f);
            treeMap.put(test, 11.0f);
        }
        if (!map.equals(treeMap)) {
            System.err.println("Put test failed");
        }
        for (Date test : testDates) {
            map.remove(test);
        }
        map.putAll(treeMap);
        if (!map.equals(treeMap)) {
            System.err.println("PutAll test failed");
        }
        if (map instanceof ArrayBackedMap) {
            test(map.descendingMap());
        }
    }

}
