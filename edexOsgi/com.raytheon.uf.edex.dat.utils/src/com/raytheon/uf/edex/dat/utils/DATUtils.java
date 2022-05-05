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
package com.raytheon.uf.edex.dat.utils;

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.locationtech.jts.geom.Geometry;
import org.opengis.referencing.operation.TransformException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.util.SubGridGeometryCalculator;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.gridcoverage.subgrid.SubGrid;
import com.raytheon.uf.common.monitor.processing.IMonitorProcessing;
import com.raytheon.uf.common.monitor.xml.SCANModelParameterXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * DATUtils
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 06/22/09      2152       D. Hladky   Initial release
 * Apr 24, 2014  2060       njensen     Updates for removal of grid dataURI column
 * 09 Feb 2015   3077       dhladky     Updated cache logic
 * 11 Dec 2015   5166       kbisanz     Update logging to use SLF4J
 * 12 Jul 2019   7627       mroos       Update to add GridRecord population trimming
 *
 * </pre>
 *
 * @author dhladky
 * @version 1
 */

public class DATUtils {

    private static final Logger logger = LoggerFactory.getLogger("DATUtils");

    /**
     * Populate the PDO record
     *
     * @param uri
     * @return
     */
    public static PluginDataObject getPDORecord(String uri, SourceXML xml) {
        PluginDataObject rec = null;
        try {
            Class<?> clazz = Class.forName(xml.getPluginClass());
            java.lang.reflect.Constructor<?> constructor = clazz
                    .getConstructor(new Class[] { String.class });
            PluginDataObject pdo = (PluginDataObject) constructor
                    .newInstance(uri);
            PluginDao pd = PluginFactory.getInstance()
                    .getPluginDao(pdo.getPluginName());
            rec = pd.getMetadata(uri);
            IDataStore dataStore = pd.getDataStore((IPersistable) rec);
            ((IMonitorProcessing) rec).retrieveFromDataStore(dataStore);
        } catch (Exception e) {
            logger.error("No PDO record found.....", e);
        }

        return rec;
    }

    /**
     * Get the populated GridRecord.
     *
     * @param uri
     *            The URI of the GridRecord to grab (and populate)
     * @return
     * @throws PluginException
     * @throws TransformException
     */
    public static GridRecord getGridRecord(String uri)
            throws PluginException, TransformException {
        GridRecord gr = new GridRecord(uri);

        // Populate the grid record
        if (gr != null) {
            populateGridRecord(gr);
        }
        return gr;
    }

    /**
     * Get the populated GridRecord. This is the overloaded method for use by
     * FFMP, which may look at multiple CWA areas, so this method takes a string
     * of all the requested CWAs. Only one CWA will still be an array of
     * strings, but it will only be 1 long.
     *
     * @param uri
     *            The URI of the GridRecord to grab (and populate)
     * @param buffer
     *            The buffer size (in metres) for the area outside of the CWAs
     *            to add to the envelope size
     * @param cwas
     *            The CWAs to be requested (prevents memory overusage by
     *            limiting what gets populated into the GridRecord)
     * @return
     * @throws PluginException
     * @throws TransformException
     */
    public static GridRecord getGridRecord(String uri, double buffer,
            String... cwas) throws PluginException, TransformException {
        GridRecord gr = new GridRecord(uri);

        /*
         * Find the geometry of the area actually needed, rather than spending
         * memory populating areas you haven't requested; This is primarily
         * implemented in the case of unclipped data
         */

        // Populate the grid record, but only with the data needed
        if (gr != null) {
            populateGridRecord(gr, setNewGeo(gr, cwas, buffer));
        }
        return gr;
    }

    /**
     * Fills a GridRecord with the raw data retrieved from IDataStore. This base
     * version of the method is called when the caller lacks the functionality
     * to create a slab request, so a request of ALL is made instead
     *
     * @param gr
     *            The GridRecord to populate
     * @throws PluginException
     */
    public static void populateGridRecord(GridRecord gr)
            throws PluginException {
        // Call the overload using a request size of "ALL"
        populateGridRecord(gr, Request.ALL);
    }

    /**
     * Fills a GridRecord with the raw data retrieved from IDataStore. This is
     * the overloaded method for setting a request size to limit the amount of
     * data processed in.
     *
     * @param gr
     *            The GridRecord to populate
     * @param req
     *            The request used to limit what gets populated (memory-saving
     *            feature)
     * @throws PluginException
     */
    public static void populateGridRecord(GridRecord gr, Request req)
            throws PluginException {
        if (gr != null) {
            PluginDao gd = PluginFactory.getInstance()
                    .getPluginDao(gr.getPluginName());
            IDataStore dataStore = gd.getDataStore(gr);
            try {
                IDataRecord[] dataRec = dataStore
                        .retrieveGroups(new String[] { gr.getDataURI() }, req);
                for (int i = 0; i < dataRec.length; i++) {
                    if (dataRec[i] instanceof FloatDataRecord) {
                        gr.setMessageData(dataRec[i]);
                    }
                }
            } catch (Exception e) {
                logger.error("Error retrieving grid data for " + gr, e);
            }
        }
    }

    /**
     * Check status of cached model data
     *
     * @param interval
     * @param sql
     * @param param
     * @return
     */
    public static GridRecord getMostRecentGridRecord(int interval,
            GridRecord newRec, SCANModelParameterXML param) {
        GridRecord rec = null;

        try {
            ScanDataCache cache = ScanDataCache.getInstance();
            // if this is true, existing grid is in the cache.
            if (cache.getModelData().isType(param.getModelName(),
                    param.getParameterName())) {
                // get the old record for comparison
                GridRecord oldRec = cache.getModelData().getGridRecord(
                        param.getModelName(), param.getParameterName());

                if (newRec != null) {
                    if (newRec.getDataTime().getRefTime()
                            .after(oldRec.getDataTime().getRefTime())) {
                        // fully populate the new record
                        populateGridRecord(newRec, Request.ALL);
                        // insert new record
                        cache.getModelData().setGridRecord(param.getModelName(),
                                param.getParameterName(), newRec);
                        // new record replaces old record.
                        rec = newRec;
                    } else {
                        // old record is the same time as new
                        rec = oldRec;
                    }
                } else {
                    // new record is null, do not overwrite
                    rec = oldRec;
                }
                // if you get here this means that no grid exists in cache
                // currently
            } else {
                // new record is good, insert it
                if (newRec != null) {
                    // fully populate new record
                    populateGridRecord(newRec, Request.ALL);
                    // insert new record
                    cache.getModelData().setGridRecord(param.getModelName(),
                            param.getParameterName(), newRec);
                    rec = newRec;
                    // no records for this grid exist at all
                } else {
                    logger.warn(
                            "No record(s) found matching these criteria. Model:"
                                    + param.getModelName() + " Param:"
                                    + param.getParameterName() + " Interval:"
                                    + interval);
                }
            }

        } catch (Exception e) {
            logger.error(
                    "Error in grid retrieval: " + param.getModelName() + ": "
                            + param.getParameterName() + " record: " + newRec,
                    e);
        }

        return rec;
    }

    /**
     * Calculates the necessary area of data to look at (based on CWA(s)) and
     * generates a slab request from that area.
     *
     * @param gr
     *            The full GridRecord to clip the geometry of
     * @param cwa
     *            The list of CWAs that make up the area to look at
     * @param buff
     *            The buffer size (in metres) of which to expand the envelope
     *            size by
     * @throws CloneNotSupportedException
     * @throws TransformException
     * @throws GridCoverageException
     */
    private static Request setNewGeo(GridRecord gr, String[] cwas, double buff)
            throws TransformException, GridCoverageException {

        // Make an empty Envelope to start combining all the CWA envelopes into
        ReferencedEnvelope fullEnv = null;

        // Loop through all the CWAs to be observed
        for (String cwa : cwas) {
            // Find the geometry of each CWA area
            Geometry cwaGeo = FFMPUtils.getCwaGeometry(cwa);
            // Make an envelope based on that area
            ReferencedEnvelope env = new ReferencedEnvelope(
                    cwaGeo.getEnvelopeInternal(), DefaultGeographicCRS.WGS84);
            // Put the first cwa envelope into the envelope to combine...
            if (fullEnv == null) {
                fullEnv = env;
            } else {
                // ... and start adding the following envelopes onto it
                fullEnv.expandToInclude(env);
            }
        }

        /*
         * At this point, there is one large envelope that is the sum of the
         * envelopes of all the CWAs to be requested. This gives us the total
         * area to look at for the slab request. Next is adding the buffer to
         * that envelope. The buffer is a number of metres, found in
         * FFMPTemplates. Because geometry envelopes are in degrees, this must
         * be converted from kilometres. Some classes do not have FFMPTemplates,
         * and thus have no buffer functionality. SCAN passes a buffer of 0,
         * which will not expand the envelope at all.
         */

        if (buff != 0) {
            // Convert from metres to kilometres, then to degrees.
            double buffer = (buff / 1000) / FFMPUtils.KmToDegrees;
            // Add the buffer area to the envelope
            fullEnv.expandBy(buffer);
        }

        // Create a coverage area copy to make modifications to
        GridCoverage coverage = gr.getLocation();
        // coverage = coverage.clone();

        // Calculate the "subgrid", which in this case is everything in the
        // large, combined envelope area with the buffer.
        SubGridGeometryCalculator calc = new SubGridGeometryCalculator(fullEnv,
                coverage.getGridGeometry());

        // Create a new subgrid to set the trim area for
        SubGrid forTrim = new SubGrid();

        // First set the min and max indices for the coverage area (upper left
        // and lower right corners)
        int[] minIndex = calc.getGridRangeLow(true);
        int[] maxIndex = calc.getGridRangeHigh(false);

        // Set trim dimensions
        // It is worth noting that for trim, the min index is upper left and the
        // max index is the lower right. The y-axis is inverted.
        forTrim.setNX(calc.getSubGridGeometry2D().getGridRange2D().getSpan(0));
        forTrim.setNY(calc.getSubGridGeometry2D().getGridRange2D().getSpan(1));
        forTrim.setUpperLeftX(minIndex[0]);
        forTrim.setUpperLeftY(minIndex[1]);

        // Trim the grid coverage and throw it in a new coverage to fill
        GridCoverage newCov = coverage.trim(forTrim);

        // Fills in the rest of the coverage details after size is set
        newCov.initialize();

        // Set the GridCoverage of the GridRecord to the new coverage area
        gr.setLocation(newCov);

        // Build the Slab request to be used for grid population
        return Request.buildSlab(minIndex, maxIndex);
    }
}
