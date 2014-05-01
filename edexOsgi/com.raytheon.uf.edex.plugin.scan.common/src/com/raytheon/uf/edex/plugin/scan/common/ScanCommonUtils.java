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
package com.raytheon.uf.edex.plugin.scan.common;

import com.raytheon.edex.plugin.radar.dao.RadarDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.dataplugin.bufrua.UAObsAdapter;
import com.raytheon.uf.common.dataplugin.bufrua.dao.BufrUAPointDataTransform;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.pointdata.PointDataQuery;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class ScanCommonUtils {

    private static final IUFStatusHandler theHandler = UFStatus
            .getHandler(ScanCommonUtils.class);

    /**
     * Populate the radar record
     * 
     * @param uri
     * @return
     */
    public static RadarRecord getRadarRecord(String uri) throws PluginException {

        RadarRecord radRec = null;
        try {
            radRec = new RadarRecord(uri);
            RadarDao rd = (RadarDao) PluginFactory.getInstance().getPluginDao(
                    radRec.getPluginName());
            radRec = (RadarRecord) rd.getMetadata(uri);

            if (radRec != null) {
                IDataStore dataStore = rd.getDataStore(radRec);
                RadarDataRetriever.populateRadarRecord(dataStore, radRec);
            } else {
                theHandler.error("URI: " + uri + " not in Database...");
            }
        } catch (Exception e) {
            theHandler.error("RADAR Record: " + uri + " failed to retrieve...");
        }

        return radRec;
    }

    /**
     * Populate the sounding record for the pull strategy
     * 
     * @return
     */
    public static VerticalSounding[] getSoundingRecord(String wmo) {
        PointDataQuery request = null;
        PointDataContainer result = null;
        VerticalSounding[] soundingRec = null;
        UAObs upperAirObs[] = null;
        UAObsAdapter obsAdapt = null;
        try {
            request = new PointDataQuery("bufrua");
            request.requestAllLevels();
            request.addParameter("location.stationId", wmo, "=");
            request.setParameters(BufrUAPointDataTransform.MAN_PARAMS_LIST);
            result = request.execute();
            if (result != null) {
                upperAirObs = BufrUAPointDataTransform.toUAObsRecords(result);
                obsAdapt = new UAObsAdapter();
                obsAdapt.setObjects(upperAirObs);
                // May need to pass a boolean flag that signals keeping
                // soundings
                // below surface layers into createSoundings. Otherwise, these
                // sounding are discarded.
                obsAdapt.setBelowSurface(true);
                soundingRec = obsAdapt.createSoundings();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return soundingRec;
    }

}
