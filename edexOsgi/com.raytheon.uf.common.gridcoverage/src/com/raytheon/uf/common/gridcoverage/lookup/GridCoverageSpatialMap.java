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
package com.raytheon.uf.common.gridcoverage.lookup;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.gridcoverage.GridCoverage;

/**
 * A map for GridCoverages that does not require them to be completely equal but
 * instead uses the spatialEquals, also uses softreferences to coverages, so
 * entries may dissappear if memory is needed.
 * 
 * This map is not thread safe and must be externally synchronized.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridCoverageSpatialMap implements Map<GridCoverage, Integer> {

    private static class GridCoverageSpatialKey extends
            SoftReference<GridCoverage> {

        private final int hash;

        public GridCoverageSpatialKey(GridCoverage coverage,
                ReferenceQueue<GridCoverage> q) {
            super(coverage, q);
            HashCodeBuilder hashBuilder = new HashCodeBuilder();
            hashBuilder.append(coverage.getProjectionType());
            hashBuilder.append(coverage.getNx());
            hashBuilder.append(coverage.getNy());
            // this hash is not very unique and will lead to a fairly large
            // number of collisions under some circumstances, the hashmap should
            // be able to handle these collisions so there won't be errors and
            // hopefully the performance loss due to bad hashing is negligible,
            // but this may require further testing.
            hash = hashBuilder.toHashCode();
        }

        @Override
        public int hashCode() {
            return hash;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == this) {
                return true;
            } else if (obj instanceof GridCoverageSpatialKey) {
                GridCoverage thisCoverage = this.get();
                GridCoverage otherCoverage = ((GridCoverageSpatialKey) obj)
                        .get();
                return thisCoverage != null && otherCoverage != null
                        && thisCoverage.spatialEquals(otherCoverage);
            }
            return false;
        }

    }

    private Map<GridCoverageSpatialKey, Integer> internalMap = new HashMap<GridCoverageSpatialKey, Integer>();

    private ReferenceQueue<GridCoverage> refQueue = new ReferenceQueue<GridCoverage>();

    @Override
    public int size() {
        clearStaleReferences();
        return internalMap.size();
    }

    @Override
    public boolean isEmpty() {
        clearStaleReferences();
        return internalMap.isEmpty();
    }

    @Override
    public boolean containsKey(Object key) {
        return get(key) != null;
    }

    @Override
    public boolean containsValue(Object value) {
        clearStaleReferences();
        return internalMap.containsValue(value);
    }

    @Override
    public Integer get(Object key) {
        if (!(key instanceof GridCoverage)) {
            return null;
        }
        clearStaleReferences();
        GridCoverageSpatialKey coverageKey = new GridCoverageSpatialKey(
                (GridCoverage) key, null);
        return internalMap.get(coverageKey);
    }

    @Override
    public Integer put(GridCoverage key, Integer value) {
        clearStaleReferences();
        GridCoverageSpatialKey coverageKey = new GridCoverageSpatialKey(key,
                refQueue);
        return internalMap.put(coverageKey, value);
    }

    @Override
    public Integer remove(Object key) {
        if (!(key instanceof GridCoverage)) {
            return null;
        }
        clearStaleReferences();
        GridCoverageSpatialKey coverageKey = new GridCoverageSpatialKey(
                (GridCoverage) key, null);
        return internalMap.remove(coverageKey);
    }

    @Override
    public void putAll(Map<? extends GridCoverage, ? extends Integer> m) {
        for (Entry<? extends GridCoverage, ? extends Integer> entry : m
                .entrySet()) {
            this.put(entry.getKey(), entry.getValue());
        }
    }

    @Override
    public void clear() {
        internalMap.clear();
    }

    /**
     * this method is not implemented
     */
    @Override
    public Set<GridCoverage> keySet() {
        // TODO in order to fully comply with the map API this method should be
        // implemented
        return null;
    }

    @Override
    public Collection<Integer> values() {
        clearStaleReferences();
        return internalMap.values();
    }

    /**
     * this method is not implemented
     */
    @Override
    public Set<Entry<GridCoverage, Integer>> entrySet() {
        // TODO in order to fully comply with the map API this method should be
        // implemented
        return null;
    }

    private void clearStaleReferences() {
        GridCoverageSpatialKey key = (GridCoverageSpatialKey) refQueue.poll();
        while (key != null) {
            internalMap.remove(key);
            key = (GridCoverageSpatialKey) refQueue.poll();
        }
    }

}
