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
package com.raytheon.edex.plugin.gfe.config;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;

/**
 * Simple version of the server configuration for use in python.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/13/08     #1030      randerso    Initial port
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SimpleServerConfig {
    public String serverHost, mhsid;

    public long rpcPort;

    public List<String> allowedNodes;

    public List<SimpleModelConfig> models;

    public List<String> weatherVisibilities;

    public List<SimpleWeatherTypeConfig> weatherTypes;

    public List<ProjectionData> projectionData;

    public List<String> siteID;

    public List<String> timeZone;

    public Map<String, String> d2dModels;

    public Map<String, String> satDirs;

    public boolean allowTopoBelowZero;

    public Map<String, List<String>> initMethods;

    public Map<String, List<String>> accumulativeD2DElements;

    public Map<String, List<String>> discreteDefinitions;

    public Map<String, List<Integer>> initSkips;

    public Map<String, Integer> d2dVersions;

    public int logFilePurgeAfter;

    public boolean autoConfigureNotifyTextProd;

    public String prdDir;

    public String baseDir;

    public List<String> allSites;

    public Map<String, Integer> extraWEPrecision;

    public SimpleGridLocation domain;

    public Map<String, String> iscRoutingTableAddress;

    public List<String> requestedISCsites;

    public boolean requestISC;

    public boolean sendiscOnSave;

    public boolean sendiscOnPublish;

    public List<String> requestedISCparms;

    public String transmitScript;

    public int tableFetchTime;

    public List<String> officeTypes;

    public SimpleServerConfig() {
        allowedNodes = new ArrayList<String>();
        rpcPort = 0;
        allowTopoBelowZero = false;
        logFilePurgeAfter = 6;
        tableFetchTime = 0;
    }
}
