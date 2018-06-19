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
package com.raytheon.uf.common.monitor.scan.config;

import java.io.File;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.scan.xml.SCANMonitorConfigXML;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;

/**
 * 
 * Configuration manager for SCAN monitor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * --/--/----                          Initial creation
 * Oct 02, 2013 2361       njensen     Use JAXBManager for XML
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SCANMonitorConfig {

    private static final SingleTypeJAXBManager<SCANMonitorConfigXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(SCANMonitorConfigXML.class);

    private SCANMonitorConfigXML scanMonitorCfg;

    public SCANMonitorConfig() {
        readScanMonitorConfig();
    }

    public double getCellTilt() {
        return scanMonitorCfg.getCellTilt();
    }

    public double getDmdTilt() {
        return scanMonitorCfg.getDmdTilt();
    }

    public String[] getPlugins() {
        return scanMonitorCfg.getPlugins().split(" ");
    }

    public int getInterval() {
        return scanMonitorCfg.getInterval();
    }

    private void readScanMonitorConfig() {
        scanMonitorCfg = null;

        try {
            String fs = String.valueOf(File.separatorChar);

            IPathManager pm = PathManagerFactory.getPathManager();
            String path = pm.getStaticFile(
                    "scan" + fs + "config" + fs + "SCANMonitorConfig.xml")
                    .getAbsolutePath();

            scanMonitorCfg = jaxb.unmarshalFromXmlFile(path.toString());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
