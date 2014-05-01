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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.viz.avnconfig.ITafSiteConfig;
import com.raytheon.viz.avnconfig.TafSiteConfigFactory;
import com.raytheon.viz.avnconfig.TafSiteData;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * Manages the lightning data for the monitoring system. Contains a LtgData
 * object for each site which tracks the number of strikes within a radius
 * (miles) within the past N minutes.
 * 
 * This management is necessary since one record of lightning data that arrives
 * could apply to any number of configured sites. For efficiency, the lightning
 * data is checked and updated by Java before the python rules are ever applied.
 * For lightning monitoring, the python should only ever retrieve data from this
 * cache, not try to pull the latest on its own.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class LtgDataMgr {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LtgDataMgr.class);

    private static Map<String, LtgData> siteLtgMap = new HashMap<String, LtgData>();

    private static GeometryFactory geomFactory = new GeometryFactory();

    protected static int distance = 20; // TODO pull this from config file

    protected static int age = 15; // / TODO pull this from config file

    public static synchronized LtgData getLtgData(String siteID) {
        LtgData data = siteLtgMap.get(siteID);
        TafSiteData site = null;
        if (data == null) {
            ITafSiteConfig config = null;
            try {
                config = TafSiteConfigFactory.getInstance();
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            try {
                site = config.getSite(siteID);
            } catch (IOException e) {
                // Do nothing problem already logged.
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            // TODO fix
            if (site != null) {
                double longitude = 0;
                double latitude = 0;
                if (site.longitude != null) {
                    longitude = Double.valueOf(site.longitude);
                }
                if (site.latitude != null) {
                    latitude = Double.valueOf(site.latitude);
                }
                Coordinate c = new Coordinate(longitude, latitude);
                data = new LtgData(c, distance, age);
                siteLtgMap.put(siteID, data);
            }
        }
        return data;
    }

    private static synchronized void updateSites(long time, float lat, float lon) {
        Point point = geomFactory.createPoint(new Coordinate(lon, lat));

        Set<String> keySet = siteLtgMap.keySet();
        for (String siteID : keySet) {
            LtgData data = siteLtgMap.get(siteID);
            data.addStrikeIfInRange(time, point);
        }
    }

    public static void clearAllData() {
        Set<String> keySet = siteLtgMap.keySet();
        for (String s : keySet) {
            siteLtgMap.get(s).clearStrikes();
        }
    }

    public static void updateSiteData(BinLightningRecord[] recs) {
        HashMap<File, List<BinLightningRecord>> fileMap = new HashMap<File, List<BinLightningRecord>>();
        for (BinLightningRecord r : recs) {
            if (r == null) {
                continue;
            }
            File f = HDF5Util.findHDF5Location(r);
            List<BinLightningRecord> recList = fileMap.get(f);
            if (recList == null) {
                recList = new ArrayList<BinLightningRecord>();
                fileMap.put(f, recList);
            }
            recList.add(r);
        }

        for (File f : fileMap.keySet()) {
            List<BinLightningRecord> recList = fileMap.get(f);
            String[] groups = new String[recList.size()];
            for (int i = 0; i < recList.size(); i++) {
                groups[i] = recList.get(i).getDataURI();
            }

            // Go fetch data
            try {
                IDataStore ds = DataStoreFactory.getDataStore(f);
                IDataRecord[] records = ds.retrieveGroups(groups, Request.ALL);

                HashMap<String, List<IDataRecord>> recordMap = new HashMap<String, List<IDataRecord>>();
                // Throw in a map for easy accessibility
                for (IDataRecord rec : records) {
                    List<IDataRecord> recordList = recordMap.get(rec.getName());
                    if (recordList == null) {
                        recordList = new ArrayList<IDataRecord>();
                        recordMap.put(rec.getName(), recordList);
                    }
                    recordList.add(rec);
                }

                List<IDataRecord> times = recordMap.get("obsTime");

                int k = 0;
                for (IDataRecord timeRec : times) {
                    LongDataRecord time = (LongDataRecord) timeRec;

                    long[] timeData = time.getLongData();
                    float[] latitudeData = ((FloatDataRecord) recordMap.get(
                            "latitude").get(k)).getFloatData();
                    float[] longitudeData = ((FloatDataRecord) recordMap.get(
                            "longitude").get(k)).getFloatData();
                    for (int n = 0; n < timeData.length; n++) {
                        updateSites(timeData[n], latitudeData[n],
                                longitudeData[n]);
                    }
                    k++;
                }
            } catch (FileNotFoundException e) {
				statusHandler.handle(Priority.PROBLEM, e.getMessage());
            } catch (Exception e) {
                // TODO
                e.printStackTrace();
            }
        }
    }

}
