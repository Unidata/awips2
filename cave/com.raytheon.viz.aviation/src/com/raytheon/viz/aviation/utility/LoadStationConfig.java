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
package com.raytheon.viz.aviation.utility;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.aviation.xml.AviationStationConfig;
import com.raytheon.viz.aviation.xml.StationConfig;

/**
 * The LoadStationConfig class loads station configuration from the XML station
 * configuration file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 1, 2008	934			grichard	Initial creation.
 * 8/11/2008    1314        grichard    Used PathManager for pathnames.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class LoadStationConfig {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LoadStationConfig.class);

    /**
     * Configured Station Map or Taf Product Map.
     */
    private final Map<String, ArrayList<String>> stationMap = new HashMap<String, ArrayList<String>>();

    /**
     * Configured Station List or Taf Station List.
     */
    private ArrayList<StationConfig> stationConfig = new ArrayList<StationConfig>();

    /**
     * Get the Aviation Configuration information
     */
    public Map<String, ArrayList<String>> getStationConfig(String name) {
        IPathManager pm = PathManagerFactory.getPathManager();

        File path = pm.getFile(pm.getContext(LocalizationType.CAVE_STATIC,
                LocalizationLevel.BASE), "aviation" + File.separatorChar
                + "avnwatch" + File.separatorChar + name + ".xml");

        // System.out.println(path);

        AviationStationConfig ac;
        try {
            ac = JAXB.unmarshal(path, AviationStationConfig.class);
        } catch (Exception e) {
            // VizApp.logAndAlert(Status.ERROR, e,
            // "Error loading station config",
            // "Error loading the station config file", Activator
            // .getDefault(), Activator.PLUGIN_ID);
            statusHandler.handle(Priority.PROBLEM,
                    "Error loading station config", e);
            return null;
        }

        // System.out.println(ac);

        stationConfig = ac.getStationConfig();
        String[] icao;
        try {
            for (int i = 0; i < stationConfig.size(); i++) {
                ArrayList<String> stationList = new ArrayList<String>();
                if ((icao = stationConfig.get(i).getIcaosOfInterest()) != null) {
                    for (int j = 0; j < icao.length; ++j) {
                        stationList.add(icao[j]);
                    }
                }
                stationMap.put(stationConfig.get(i).getWfoSite().trim(),
                        stationList);
            }
        } catch (Exception e1) {
            // Less than the full complement of ICAO IDs present.
        }

        return stationMap;
    }

}
