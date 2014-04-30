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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
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
    public static PluginDataObject getPDORecord(String uri, SourceXML xml) {
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
        if (gr != null) {
            populateGridRecord(gr);
        }

        return gr;
    }

    /**
     * Fills a GridRecord with the raw data retrieved from IDataStore
     * 
     * @param gr
     * @throws PluginException
     */
    public static void populateGridRecord(GridRecord gr) throws PluginException {
        if (gr != null) {
            PluginDao gd = PluginFactory.getInstance().getPluginDao(
                    gr.getPluginName());
            IDataStore dataStore = gd.getDataStore(gr);
            try {
                IDataRecord[] dataRec = dataStore.retrieve(gr.getDataURI());
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
            /*
             * TODO njensen: we should only spend time populating if the new rec
             * replaces the old rec. Delaying that change as at present as I'm
             * just trying to make it work the same as before.
             */
            populateGridRecord(newRec);

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
            logger.error("Error in retrieval: " + param.getModelName() + ": "
                    + param.getParameterName() + " record: " + newRec);
        }

        return rec;
    }
}
