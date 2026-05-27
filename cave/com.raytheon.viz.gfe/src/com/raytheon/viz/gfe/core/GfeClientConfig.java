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
package com.raytheon.viz.gfe.core;

/**
 * Class to hold GFE client configuration settings. Actual settings are in
 * com.raytheon.viz.gfe/res/spring/gfe.xml
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2013     #1597  randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GfeClientConfig {

    private int maxSaveThreads = 3;

    private long gridSaveThreshold = 32 * 1024 * 1024; // 32 MB

    private static final GfeClientConfig instance = new GfeClientConfig();

    public static GfeClientConfig getInstance() {
        return instance;
    }

    private GfeClientConfig() {
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("GfeClientConfig:");
        sb.append("\n   maxSaveThreads: ").append(maxSaveThreads);
        sb.append("\n   gridSaveThreshold: ").append(gridSaveThreshold);
        return sb.toString();
    }

    public int getMaxSaveThreads() {
        return maxSaveThreads;
    }

    public void setMaxSaveThreads(int maxSaveThreads) {
        this.maxSaveThreads = maxSaveThreads;
    }

    public long getGridSaveThreshold() {
        return gridSaveThreshold;
    }

    public void setGridSaveThreshold(long gridSaveThreshold) {
        this.gridSaveThreshold = gridSaveThreshold;
    }

}
