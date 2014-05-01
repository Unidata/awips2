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

/**
 * FFMP Table Configutation Class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2011            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FfmpTableConfig {
    /**
     * Instance of this class.
     */
    private static FfmpTableConfig instance = null;

    private HashMap<String, FfmpTableConfigData> tblCfgDataMap;

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
            tblCfgDataMap = new HashMap<String, FfmpTableConfigData>();
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

    // /**
    // * Get the table column keys.
    // *
    // * @return Array of table column keys
    // */
    // public String[] getTableColumnKeys() {
    // return ffmpTableCols;
    // }
    //
    // /**
    // * Get the column index.
    // *
    // * @param columnName
    // * Column name/key.
    // * @return The column index.
    // */
    // public int getTableColumnIndex(String columnName) {
    //
    // }
    //
    // /**
    // * Get the column attribute data for the table.
    // *
    // * @param columnNameKey
    // * Column name/key.
    // * @return Column attribute data.
    // */
    // public ColumnAttribData getTableColumnAttr(String columnNameKey) {
    //
    // }
    //
    // /**
    // * @return the qpfTypeIdx
    // */
    // public int getQpfTypeIdx() {
    //
    // }
    //
    // /**
    // * Set the QPF Type index. At the same time set the column header text.
    // *
    // * @param qpfTypeIdx
    // * the qpfType to set
    // */
    // public void setQpfTypeIdx(int qpfTypeIdx, String siteKey) {
    //
    // }
}
