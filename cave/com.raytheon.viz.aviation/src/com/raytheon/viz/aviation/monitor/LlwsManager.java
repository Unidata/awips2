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
package com.raytheon.viz.aviation.monitor;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.MapValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.viz.core.HDF5Util;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2009            njensen     Initial creation
 * Jul 15, 2010 5078       rferrel     getVerticalWindProfile returns empty
 *                                     list when unable of locate the HD5 file.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class LlwsManager {

    public static List<VwpObs> getVerticalWindProfile(String radar, long time) {
        RadarRecord[] recs = MonitorDataUtil
                .getVerticalWindProfile(radar, time);
        List<VwpObs> vwpList = new ArrayList<VwpObs>();
        Map<String, Map<MapValues, String>> map = null;

        if (recs == null) {
            return vwpList;
        }

        for (RadarRecord radRec : recs) {
            File loc = HDF5Util.findHDF5Location(radRec);
            IDataStore dataStore = DataStoreFactory.getDataStore(loc);
            try {
                RadarDataRetriever.populateRadarRecord(dataStore, radRec);
                map = radRec.getMapProductVals().get(
                        RadarConstants.MapValues.VAD_TYPE);
            } catch (Exception e) {
                // Return empty list. Log messages already done.
                return vwpList;
            }

            List<String> heightList = new ArrayList<String>();
            heightList.addAll(map.keySet());
            Collections.sort(heightList);

            long timestamp = radRec.getDataTime().getRefTime().getTime();
            for (String h : heightList) {
                Map<MapValues, String> heightMap = map.get(h);
                VwpObs obs = new VwpObs();
                obs.setTime(timestamp);
                obs.setHeight(h);
                obs.setWindU(heightMap.get(MapValues.VAD_U));
                obs.setWindV(heightMap.get(MapValues.VAD_V));
                vwpList.add(obs);
            }
        }

        return vwpList;
    }

    public static ACARSSoundingRecord getAcarsRecord(String stationId, long time) {
        ACARSSoundingRecord[] records = MonitorDataUtil
                .getAcarsSoundingRecords(stationId, time);

        if (records == null || records.length == 0) {
            return null;
        } else {
            return records[0];
        }
    }
}
