package com.raytheon.uf.common.json.geo;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.geotools.data.store.DataFeatureCollection;
import org.geotools.feature.FeatureCollection;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.geometry.BoundingBox;

public class MixedFeatureCollection extends DataFeatureCollection {

	protected List<FeatureCollection<SimpleFeatureType, SimpleFeature>> colls;

	public MixedFeatureCollection(
			List<FeatureCollection<SimpleFeatureType, SimpleFeature>> colls) {
		this.colls = colls;
	}

	@Override
	protected Iterator<SimpleFeature> openIterator() throws IOException {
		return new MixedIterator();
	}

	@SuppressWarnings({ "rawtypes" })
	@Override
	public boolean removeAll(Collection arg0) {
		Iterator<FeatureCollection<SimpleFeatureType, SimpleFeature>> i = colls
				.iterator();
		boolean rval = false;
		while (i.hasNext()) {
			rval = rval || i.next().removeAll(arg0);
		}
		return rval;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public boolean retainAll(Collection arg0) {
		Iterator<FeatureCollection<SimpleFeatureType, SimpleFeature>> i = colls
				.iterator();
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

	public SimpleFeatureType getSchema() {
		return null;
	}

	class MixedIterator implements Iterator<SimpleFeature> {

		protected int curr = 0;
		protected Iterator<SimpleFeature> iter;

		@Override
		public boolean hasNext() {
			if ((iter != null) && iter.hasNext()) {
				return true;
			}
			while (curr < colls.size()) {
				if (iter != null) {
					colls.get(curr - 1).close(iter);
				}
				iter = colls.get(curr++).iterator();
				if (iter.hasNext()) {
					return true;
				}
			}

			if (iter != null) {
				colls.get(colls.size() - 1).close(iter);
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
