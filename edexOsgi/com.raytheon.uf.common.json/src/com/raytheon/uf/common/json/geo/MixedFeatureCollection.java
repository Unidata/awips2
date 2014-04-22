package com.raytheon.uf.common.json.geo;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.geotools.data.memory.MemoryFeatureCollection;
import org.geotools.data.simple.SimpleFeatureIterator;
import org.geotools.data.store.DataFeatureCollection;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.geometry.BoundingBox;

/**
 * Mixed Feature Collection
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug  9, 2011            bclement    Initial creation
 * Mar 11, 2014      #2718 randerso    Changes for GeoTools 10.5
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class MixedFeatureCollection extends DataFeatureCollection {

    protected List<MemoryFeatureCollection> colls;

    public MixedFeatureCollection(List<MemoryFeatureCollection> colls) {
        this.colls = colls;
    }

    @Override
    protected Iterator<SimpleFeature> openIterator() throws IOException {
        return new MixedIterator();
    }

    @SuppressWarnings({ "rawtypes" })
    @Override
    public boolean removeAll(Collection arg0) {
        Iterator<MemoryFeatureCollection> i = colls.iterator();
        boolean rval = false;
        while (i.hasNext()) {
            rval = rval || i.next().removeAll(arg0);
        }
        return rval;
    }

    @SuppressWarnings("rawtypes")
    @Override
    public boolean retainAll(Collection arg0) {
        Iterator<MemoryFeatureCollection> i = colls.iterator();
        boolean rval = false;
        while (i.hasNext()) {
            rval = rval || i.next().retainAll(arg0);
        }
        return rval;
    }

    @Override
    public Object[] toArray(Object[] array) {
        List<Object> rval = new ArrayList<Object>();
        Iterator<SimpleFeature> i = this.iterator();
        while (i.hasNext()) {
            rval.add(i.next());
        }
        return rval.toArray(array);
    }

    @Override
    public SimpleFeatureType getSchema() {
        return null;
    }

    class MixedIterator implements Iterator<SimpleFeature> {

        protected int curr = 0;

        protected SimpleFeatureIterator iter;

        @Override
        public boolean hasNext() {
            if ((iter != null) && iter.hasNext()) {
                return true;
            }
            while (curr < colls.size()) {
                if (iter != null) {
                    iter.close();
                }
                iter = colls.get(curr++).features();
                if (iter.hasNext()) {
                    return true;
                }
            }

            if (iter != null) {
                iter.close();
            }
            return false;
        }

        @Override
        public SimpleFeature next() {
            return iter.next();
        }

        @Override
        public void remove() {
            // TODO Auto-generated method stub

        }

    }

    @Override
    public ReferencedEnvelope getBounds() {
        Iterator<SimpleFeature> iterator = iterator();
        ReferencedEnvelope rval = null;
        if (iterator.hasNext()) {
            SimpleFeature sf = iterator.next();
            BoundingBox bbox = sf.getBounds();
            rval = new ReferencedEnvelope(bbox);
        }
        while (iterator.hasNext()) {
            SimpleFeature sf = iterator.next();
            BoundingBox bbox = sf.getBounds();
            rval.expandToInclude(new ReferencedEnvelope(bbox));
        }
        if (rval == null) {
            rval = new ReferencedEnvelope();
        }
        return rval;
    }

    @Override
    public int getCount() throws IOException {
        int count = 0;
        Iterator<SimpleFeature> iterator = iterator();
        try {
            while (iterator.hasNext()) {
                iterator.next();
                count++;
            }
        } finally {
            close(iterator);
        }

        return count;
    }

}
