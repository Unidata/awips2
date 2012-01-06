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
package com.raytheon.viz.core.spatial;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.geotools.data.DefaultQuery;
import org.geotools.data.shapefile.indexed.IndexType;
import org.geotools.data.shapefile.indexed.IndexedShapefileDataStore;
import org.geotools.factory.CommonFactoryFinder;
import org.geotools.factory.GeoTools;
import org.geotools.feature.FeatureIterator;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.type.AttributeDescriptor;
import org.opengis.filter.Filter;
import org.opengis.filter.FilterFactory2;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.AbstractSpatialQuery;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.vividsolutions.jts.geom.Geometry;

/**
 * ShapefileQuery
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Nov 15, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ShapefileQuery extends AbstractSpatialQuery {

    protected ShapefileQuery() {

    }

    private SpatialQueryResult[] wrap(List<SimpleFeature> features) {
        SpatialQueryResult[] results = new SpatialQueryResult[features.size()];

        for (int i = 0; i < results.length; i++) {
            SimpleFeature f = features.get(i);
            results[i] = new SpatialQueryResult();
            results[i].geometry = (Geometry) f.getDefaultGeometry();
            results[i].attributes = new HashMap<String, Object>();

            List<AttributeDescriptor> types = f.getFeatureType()
                    .getAttributeDescriptors();

            for (int k = 0; k < types.size(); k++) {
                String name = types.get(k).getLocalName();
                Object value = f.getAttribute(k);
                results[i].attributes.put(name, value);
            }

        }

        return results;

    }

    @Override
    public Object[] dbRequest(String sql, String dbname)
            throws SpatialException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public SpatialQueryResult[] query(String dataSet, String theGeomField,
            String[] attributes, Geometry areaGeometry,
            Map<String, RequestConstraint> filter, SearchMode mode)
            throws SpatialException {
        File shapeFile = null;
        LocalizationFile[] files = PathManagerFactory.getPathManager()
                .listStaticFiles(FileUtil.join(VizApp.getMapsDir(), dataSet),
                        null, false, true);
        for (LocalizationFile file : files) {
            File f = file.getFile();
            if (f.getName().endsWith(".shp")) {
                shapeFile = f;
                // we don't break so we make sure all needed shapefiles are
                // downloaded
            }
        }

        if (shapeFile == null) {
            throw new SpatialException("Shapefile Not Found: " + dataSet);
        }

        FeatureIterator<SimpleFeature> featureIterator = null;
        try {
            IndexedShapefileDataStore ds = new IndexedShapefileDataStore(
                    shapeFile.toURI().toURL(), null, true, true, IndexType.QIX);

            String[] types = ds.getTypeNames();

            DefaultQuery query = new DefaultQuery();
            query.setTypeName(types[0]);

            FilterFactory2 ff = CommonFactoryFinder.getFilterFactory2(GeoTools
                    .getDefaultHints());

            Filter shpFilter = null;
            if (areaGeometry != null) {
                ReferencedEnvelope bbox = new ReferencedEnvelope(
                        areaGeometry.getEnvelopeInternal(),
                        MapUtil.LATLON_PROJECTION);

                shpFilter = ff.bbox(ff.property(theGeomField), bbox);
            }
            if (filter != null) {
                Set<String> keySet = filter.keySet();
                for (String key : keySet) {
                    RequestConstraint constraint = filter.get(key);
                    Filter compare = null;
                    switch (constraint.getConstraintType()) {
                    case EQUALS: {
                        compare = ff.equal(ff.property(key),
                                ff.literal(constraint.getConstraintValue()),
                                false);
                        break;
                    }
                    case NOT_EQUALS: {
                        compare = ff.notEqual(ff.property(key),
                                ff.literal(constraint.getConstraintValue()),
                                false);
                        break;
                    }
                    }
                    if (shpFilter == null) {
                        shpFilter = compare;
                    } else {
                        shpFilter = ff.and(shpFilter, compare);
                    }
                }
            }

            query.setFilter(shpFilter);

            featureIterator = ds.getFeatureSource().getFeatures(query)
                    .features();

            List<SimpleFeature> featureList = new ArrayList<SimpleFeature>();
            while (featureIterator.hasNext()) {
                SimpleFeature feature = featureIterator.next();
                Geometry geometry = (Geometry) feature.getDefaultGeometry();
                if (areaGeometry == null
                        || (mode == SearchMode.CONTAINS && areaGeometry
                                .contains(geometry))) {
                    featureList.add(feature);
                } else if (mode == SearchMode.INTERSECTS
                        && areaGeometry.intersects(geometry)) {
                    featureList.add(feature);
                }
            }

            return wrap(featureList);
        } catch (Exception e) {
            throw new SpatialException("Error Querying Shapefile", e);
        } finally {
            if (featureIterator != null) {
                featureIterator.close();
            }
        }
    }

    @Override
    public SpatialQueryResult[] query(String dataSet, String theGeomField,
            String[] attributes, Geometry areaGeometry,
            Map<String, RequestConstraint> filter, SearchMode mode,
            Integer limit) throws SpatialException {
        // not implemented
        return null;
    }
}
