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

import java.util.AbstractSet;
import java.util.Comparator;
import java.util.Iterator;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.SortedSet;

/**
 * 
 * Generic NavigableSet which is implemented by wrapping a fully implemented
 * NavigableMap. Very useful for custom NavigableMap implementations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2013 2242       bsteffen    Extracted from ArrayBackedMap
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @param <K>
 */
class NavigableKeySet<K> extends AbstractSet<K> implements NavigableSet<K> {

    private final NavigableMap<K, ?> map;

    public NavigableKeySet(NavigableMap<K, ?> map) {
        this.map = map;
    }

    @Override
    public Iterator<K> iterator() {
        return map.keySet().iterator();
    }

    @Override
    public int size() {
        return map.size();
    }

    @Override
    public Comparator<? super K> comparator() {
        return map.comparator();
    }

    @Override
    public K first() {
        return map.firstKey();
    }

    @Override
    public K last() {
        return map.lastKey();
    }

    @Override
    public K lower(K e) {
        return map.lowerKey(e);
    }

    @Override
    public K floor(K e) {
        return map.floorKey(e);
    }

    @Override
    public K ceiling(K e) {
        return map.ceilingKey(e);
    }

    @Override
    public K higher(K e) {
        return map.higherKey(e);
    }

    @Override
    public K pollFirst() {
        return map.pollFirstEntry().getKey();
    }

    @Override
    public K pollLast() {
        return map.pollLastEntry().getKey();
    }

    @Override
    public NavigableSet<K> descendingSet() {
        return map.descendingKeySet();
    }

    @Override
    public Iterator<K> descendingIterator() {
        return descendingSet().iterator();
    }

    @Override
    public NavigableSet<K> subSet(K fromElement, boolean fromInclusive,
            K toElement, boolean toInclusive) {
        return map.subMap(fromElement, fromInclusive, toElement,
                toInclusive).navigableKeySet();
    }

    @Override
    public NavigableSet<K> headSet(K toElement, boolean inclusive) {
        return map.headMap(toElement, inclusive).navigableKeySet();
    }

    @Override
    public NavigableSet<K> tailSet(K fromElement, boolean inclusive) {
        return map.tailMap(fromElement, inclusive).navigableKeySet();
    }

    @Override
    public SortedSet<K> subSet(K fromElement, K toElement) {
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public SortedSet<K> headSet(K toElement) {
        return headSet(toElement, false);
    }

    @Override
    public SortedSet<K> tailSet(K fromElement) {
        return tailSet(fromElement, true);
    }

}
