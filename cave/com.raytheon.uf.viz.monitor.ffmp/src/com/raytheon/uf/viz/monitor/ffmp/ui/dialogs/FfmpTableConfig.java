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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import java.util.HashMap;
import java.util.Map;

/**
 * FFMP Table Configutation Class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2011            mpduff      Initial creation
 * Aug 07, 2018 6720       njensen     Added toString()
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class FfmpTableConfig {
    /**
     * Instance of this class.
     */
    private static FfmpTableConfig instance = null;

    private Map<String, FfmpTableConfigData> tblCfgDataMap;

    /**
     * Private constructor.
     */
    private FfmpTableConfig() {
        init();
    }

    /**
     * Get an instance of this class.
     * 
     * @return instance of the class
     */
    public static synchronized FfmpTableConfig getInstance() {
        if (instance == null) {
            instance = new FfmpTableConfig();
        }

        return instance;
    }

    /**
     * Read config data.
     */
    private void init() {
        if (tblCfgDataMap == null) {
            tblCfgDataMap = new HashMap<>();
        }
    }

    public void rereadConfigData(String siteKey) {
        FfmpTableConfigData tblCfgData = new FfmpTableConfigData(siteKey);

        tblCfgDataMap.put(siteKey, tblCfgData);
    }

    public FfmpTableConfigData getTableConfigData(String siteKey) {
        if (tblCfgDataMap.containsKey(siteKey)) {
            return tblCfgDataMap.get(siteKey);
        }

        FfmpTableConfigData tblCfgData = new FfmpTableConfigData(siteKey);

        tblCfgDataMap.put(siteKey, tblCfgData);

        return tblCfgData;
    }

    @Override
    public String toString() {
        return "FfmpTableConfig [tblCfgDataMap=" + tblCfgDataMap + "]";
    }

}
