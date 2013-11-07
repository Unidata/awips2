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
package com.raytheon.uf.edex.ogc.common.spatial;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.spatial.reprojection.DataReprojector;
import com.raytheon.uf.common.spatial.reprojection.ReferencedDataRecord;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Utility methods for reprojecting data records. Removing code only used by ogc
 * services from {@link PluginDao} to here
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 26, 2013 1638       mschenke    Code moved from PluginDao to clean up dependencies
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */

public class RecordUtil {

    /**
     * @param record
     * @param crs
     *            target crs for projected data
     * @param envelope
     *            bounding box in target crs
     * @return null if envelope is disjoint with data bounds
     * @throws Exception
     */
    public static ReferencedDataRecord getProjected(PluginDao dao,
            PluginDataObject record, CoordinateReferenceSystem crs,
            Envelope envelope) throws Exception {
        ReferencedEnvelope targetEnv = new ReferencedEnvelope(
                envelope.getMinX(), envelope.getMaxX(), envelope.getMinY(),
                envelope.getMaxY(), crs);
        return getProjected(dao, record, targetEnv);
    }

    public static double getHDF5Value(PluginDao dao, PluginDataObject pdo,
            CoordinateReferenceSystem crs, Coordinate coord,
            double defaultReturn) throws Exception {
        // TODO a cache would probably be good here
        double rval = defaultReturn;
        if (pdo instanceof ISpatialEnabled) {
            IDataStore store = dao.getDataStore((IPersistable) pdo);
            ISpatialObject spat = getSpatialObject(pdo);
            DataReprojector reprojector = getDataReprojector(store);
            ReferencedEnvelope nativeEnv = getNativeEnvelope(spat);
            IDataRecord data = reprojector.getProjectedPoints(pdo.getDataURI(),
                    spat, nativeEnv, crs, new Coordinate[] { coord });
            Double res = extractSingle(data);
            if (res != null) {
                rval = res;
            }
        }
        return rval;
    }

    /**
     * @param record
     * @param crs
     *            target crs for projected data
     * @param envelope
     *            bounding box in target crs
     * @return null if envelope is disjoint with data bounds
     * @throws Exception
     */
    public static GridCoverage2D getProjectedCoverage(PluginDao dao,
            PluginDataObject record, CoordinateReferenceSystem crs,
            Envelope envelope) throws Exception {
        ReferencedEnvelope targetEnv = new ReferencedEnvelope(
                envelope.getMinX(), envelope.getMaxX(), envelope.getMinY(),
                envelope.getMaxY(), crs);
        return getProjectedCoverage(dao, record, targetEnv);
    }

    /**
     * @param record
     * @param targetEnvelope
     *            bounding box in target crs
     * @return null if envelope is disjoint with data bounds
     * @throws Exception
     */
    public static ReferencedDataRecord getProjected(PluginDao dao,
            PluginDataObject record, ReferencedEnvelope targetEnvelope)
            throws Exception {
        IDataStore store = dao.getDataStore((IPersistable) record);
        ISpatialObject spatial = getSpatialObject(record);
        DataReprojector reprojector = getDataReprojector(store);
        ReferencedEnvelope nativeEnvelope = getNativeEnvelope(spatial);
        return reprojector.getReprojected(record.getDataURI(), spatial,
                nativeEnvelope, targetEnvelope);
    }

    /**
     * @param record
     * @param targetEnvelope
     *            bounding box in target crs
     * @return null if envelope is disjoint with data bounds
     * @throws Exception
     */
    public static GridCoverage2D getProjectedCoverage(PluginDao dao,
            PluginDataObject record, ReferencedEnvelope envelope)
            throws Exception {
        IDataStore store = dao.getDataStore((IPersistable) record);
        ISpatialObject spatial = getSpatialObject(record);
        DataReprojector reprojector = getDataReprojector(store);
        ReferencedEnvelope nativeEnvelope = getNativeEnvelope(spatial);
        return reprojector.getReprojectedCoverage(record.getDataURI(), spatial,
                nativeEnvelope, envelope);
    }

    public static DataReprojector getDataReprojector(IDataStore dataStore) {
        return new DataReprojector(dataStore);
    }

    public static ReferencedEnvelope getNativeEnvelope(ISpatialObject spatial)
            throws FactoryException {
        CoordinateReferenceSystem crs = spatial.getCrs();
        Geometry geom = spatial.getGeometry();
        return MapUtil.getBoundingEnvelope(crs, (Polygon) geom);
    }

    public static Double extractSingle(IDataRecord record) {
        Double rval = null;
        if (record == null) {
            return rval;
        }
        if (record instanceof ByteDataRecord) {
            byte[] data = ((ByteDataRecord) record).getByteData();
            rval = (double) data[0];
        } else if (record instanceof FloatDataRecord) {
            float[] data = ((FloatDataRecord) record).getFloatData();
            rval = (double) data[0];
        } else if (record instanceof IntegerDataRecord) {
            int[] data = ((IntegerDataRecord) record).getIntData();
            rval = (double) data[0];
        }
        return rval;
    }

    public static ISpatialObject getSpatialObject(PluginDataObject record)
            throws Exception {
        if (record instanceof ISpatialEnabled) {
            return ((ISpatialEnabled) record).getSpatialObject();
        } else {
            throw new Exception(record.getClass() + " is not spatially enabled");
        }
    }

}
