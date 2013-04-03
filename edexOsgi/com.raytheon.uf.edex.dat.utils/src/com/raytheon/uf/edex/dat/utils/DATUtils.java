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

import java.text.SimpleDateFormat;
import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPVirtualGageBasin;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.monitor.processing.IMonitorProcessing;
import com.raytheon.uf.common.monitor.xml.SCANModelParameterXML;
import com.raytheon.uf.common.monitor.xml.SourceXML;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
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
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class DATUtils {

    private transient final static Log logger = LogFactory.getLog("DATUtils");

    /**
     * Populate the PDO record
     * 
     * @param uri
     * @return
     */
    public static PluginDataObject getPDORecord(String uri, SourceXML xml)
            throws PluginException {
        PluginDataObject rec = null;
        try {
            Class<?> clazz = Class.forName(xml.getPluginClass());
            java.lang.reflect.Constructor<?> constructor = clazz
                    .getConstructor(new Class[] { String.class });
            PluginDataObject pdo = (PluginDataObject) constructor
                    .newInstance(uri);
            PluginDao pd = PluginFactory.getInstance().getPluginDao(
                    pdo.getPluginName());
            rec = pd.getMetadata(uri);
            IDataStore dataStore = pd.getDataStore((IPersistable) rec);
            ((IMonitorProcessing) rec).retrieveFromDataStore(dataStore);
        } catch (Exception e) {
            logger.error("No PDO record found.....");
        }

        return rec;
    }

    /**
     * get Populated grib record
     * 
     * @param uri
     * @return
     */
    public static GridRecord getGridRecord(String uri) throws PluginException {

        GridRecord gr = new GridRecord(uri);
        PluginDao gd = PluginFactory.getInstance().getPluginDao(
                gr.getPluginName());
        gr = (GridRecord) gd.getMetadata(uri);

        if (gr != null) {

            IDataStore dataStore = gd.getDataStore(gr);

            try {
                IDataRecord[] dataRec = dataStore.retrieve(uri);
                for (int i = 0; i < dataRec.length; i++) {
                    if (dataRec[i] instanceof FloatDataRecord) {
                        gr.setMessageData(dataRec[i]);
                    }
                }
            } catch (Exception se) {
                logger.error("No Grib record found.....");
            }
        } else {
            logger.error("URI: " + uri + " Not in Database...");
        }

        return gr;
    }

    /**
     * Make a DB request
     * 
     * @param sql
     * @return
     */
    public static Object[] dbRequest(String sql) {

        logger.debug("SQL to run: " + sql);
        CoreDao cdao = null;
        Object[] results = null;
        try {
            if (cdao == null) {
                try {
                    cdao = new CoreDao(DaoConfig.DEFAULT);
                } catch (Exception ed1) {
                    logger.error("Core DAO access failed. " + ed1);
                }
            }
            results = cdao.executeSQLQuery(sql);

        } catch (Exception ed2) {
            logger.error("SQL Query Failed to process. SQL=" + sql + " : "
                    + ed2);
        }
        return results;
    }

    /**
     * Gets the value for an accumulating Virtual Gage Basin
     * 
     * @param lid
     * @param startTime
     * @param endTime
     * @return
     */
    public static FFMPVirtualGageBasin getVirtualBasinData(String lid,
            FFMPVirtualGageBasin vgb, String endTime, String startTime) {

        // According to the AWIPS I code

        SimpleDateFormat dateFmt = new SimpleDateFormat("MMM dd yy HH:mm:ss");
        CoreDao dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

        String sql1 = "SELECT dur, value, ts FROM curpc WHERE lid = '" + lid
                + "' AND obstime >= '" + startTime + "' AND obstime < '"
                + endTime + "' AND pe = 'PC' ORDER BY obstime DESC, ts ASC";
        String sql2 = "SELECT dur, value, ts FROM curpp WHERE lid = '"
                + lid
                + "' AND obstime >= '"
                + startTime
                + "' AND obstime < '"
                + endTime
                + "' AND pe = 'PP' AND dur <= 1001 AND value >=0 ORDER BY dur ASC, obstime DESC, ts ASC";

        dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));

        try {
            // 15 min accumulation
            Object[] results = dao.executeSQLQuery(sql1.toString());
            Object[] tsresults = null;
            String ts = null;

            if (results.length > 0) {
                if (results.length > 1) {
                    // Gets the highest ranked
                    String sql3 = "SELECT ts, ts_rank FROM ingestfilter WHERE lid = '"
                            + lid
                            + "' AND pe = 'PC' ORDER BY ts_rank ASC limit 1";
                    tsresults = dao.executeSQLQuery(sql3.toString());
                }
            } else {
                results = dao.executeSQLQuery(sql2.toString());

                if (results.length > 0) {
                    if (results.length > 1) {
                        String sql3 = "SELECT ts, ts_rank FROM ingestfilter WHERE lid = '"
                                + lid
                                + "' AND pe = 'PP' ORDER BY ts_rank ASC limit 1";
                        tsresults = dao.executeSQLQuery(sql3.toString());
                    }
                }
            }

            // parse through getting rid of undesireable types
            if (tsresults != null && tsresults.length > 0) {
                ArrayList<Integer> durations = new ArrayList<Integer>();
                ArrayList<Float> values = new ArrayList<Float>();

                for (int i = 0; i < results.length; i++) {
                    Object[] obs = (Object[]) tsresults[i];

                    if (obs[2] != null) {
                        if (((String) obs[2]).equals(ts)) {

                            if (obs[0] != null) {
                                durations.add((Integer) obs[0]);
                            }
                            if (obs[1] != null) {
                                values.add(((Number) obs[1]).floatValue());
                            }
                        }
                    }
                }

                int totalDurations = 0;
                for (int dur : durations) {
                    totalDurations += dur;
                }

                int avDuration = totalDurations / durations.size();

                float totalVals = 0.0f;
                for (float val : values) {
                    totalVals += val;
                }

                float avVal = totalVals / values.size();

                vgb.setValue(dateFmt.parse(endTime), avVal);
            } else {
                vgb.setValue(dateFmt.parse(endTime), 0.0f);
            }
        } catch (Exception e) {
            logger.error("No Virual Gage Basin found.....");
        }

        return vgb;

    }

    /**
     * Check status of cached model data
     * 
     * @param interval
     * @param sql
     * @param param
     * @return
     */
    public static GridRecord getMostRecentGridRecord(int interval, String sql,
            SCANModelParameterXML param) {

        GridRecord rec = null;

        try {
            ScanDataCache cache = ScanDataCache.getInstance();
            Object[] obs = dbRequest(sql);
            GridRecord newRec = null;

            if (obs != null && obs.length > 0) {
                String uri = (String) obs[0];
                newRec = getGridRecord(uri);
            }

            if (cache.getModelData().isType(param.getModelName(),
                    param.getParameterName())) {
                GridRecord oldRec = cache.getModelData().getGridRecord(
                        param.getModelName(), param.getParameterName());

                if (newRec != null) {
                    if (newRec.getDataTime().getRefTime()
                            .after(oldRec.getDataTime().getRefTime())) {
                        cache.getModelData().setGridRecord(
                                param.getModelName(), param.getParameterName(),
                                newRec);
                        rec = newRec;
                    } else {
                        rec = oldRec;
                    }
                } else {
                    rec = oldRec;
                }
            } else {
                if (newRec != null) {
                    cache.getModelData().setGridRecord(param.getModelName(),
                            param.getParameterName(), newRec);
                    rec = newRec;
                }
            }
        } catch (Exception e) {
            logger.error("DatUtils: " + param.getModelName() + ": "
                    + param.getParameterName() + " SQL: " + sql
                    + "  error in retrieval..");
        }

        return rec;
    }
}
