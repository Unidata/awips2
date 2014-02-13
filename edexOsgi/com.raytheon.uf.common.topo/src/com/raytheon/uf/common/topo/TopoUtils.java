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
package com.raytheon.uf.common.topo;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.CRSCache;

/**
 * Common utility methods that can be shared among different areas of the system
 * using topo.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug  2, 2013            bsteffen    Initial creation
 * Feb 10, 2014     #2788  randerso    Changed default topo file name
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class TopoUtils {

    // NOTE: this file is actually a symbolic link to the desired topo data file
    // allowing the topo data set to be changed without updating Java code
    private static final String DEFAULT_TOPO_PATH = "/topo/defaultTopo.h5";

    private static final String FULL_TOPO_DATASET = "/full";

    private static final String INTERPOLATED_DATASET_PREFIX = "/interpolated/";

    public static GridGeometry2D getTopoGeometry(IDataStore topoDataStore,
            String dataset) throws FileNotFoundException, StorageException,
            FactoryException, TransformException {

        Request request = Request.buildSlab(new int[] { 0, 0 }, new int[] { 1,
                1 });

        IDataRecord record = topoDataStore.retrieve("/", dataset, request);
        Map<String, Object> attributes = record.getDataAttributes();
        int width = (Integer) attributes.get("Width");
        int height = (Integer) attributes.get("Height");
        double ulLat = (Double) attributes.get("ulLat");
        double ulLon = (Double) attributes.get("ulLon");
        double lrLat = (Double) attributes.get("lrLat");
        double lrLon = (Double) attributes.get("lrLon");
        String crsString = (String) attributes.get("CRS");

        // Construct CRS for topo data
        CoordinateReferenceSystem crs = CRSCache.getInstance()
                .getCoordinateReferenceSystem(crsString);
        // Grid range
        GridEnvelope gridRange = new GeneralGridEnvelope(new int[] { 0, 0 },
                new int[] { width, height });

        // Convert ulLat/ulLon to crs space
        MathTransform mt = CRS.findMathTransform(DefaultGeographicCRS.WGS84,
                crs);
        double[] in = new double[] { ulLon, ulLat, lrLon, lrLat };
        double[] out = new double[in.length];

        mt.transform(in, 0, out, 0, 2);

        GeneralEnvelope gridEnvelope = new GeneralEnvelope(2);
        gridEnvelope.setCoordinateReferenceSystem(crs);
        gridEnvelope.setRange(0, Math.min(out[0], out[2]),
                Math.max(out[0], out[2]));
        gridEnvelope.setRange(1, Math.min(out[1], out[3]),
                Math.max(out[1], out[3]));

        return new GridGeometry2D(gridRange, gridEnvelope);
    }

    public static String getDatasetForLevel(int level) {
        if (level == 0) {
            return FULL_TOPO_DATASET;
        } else {
            return INTERPOLATED_DATASET_PREFIX + level;
        }
    }

    public static File getDefaultTopoFile() {
        return new File(DEFAULT_TOPO_PATH);
    }
}
