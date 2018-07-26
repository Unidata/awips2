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
package com.raytheon.uf.viz.monitor.scan.config;

import java.io.File;
import java.util.ArrayList;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.monitor.scan.xml.ScanRunConfigXML;
import com.raytheon.uf.viz.monitor.scan.xml.ScanVcpXML;

/**
 * SCAN Run Config Data manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2011            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ScanRunConfigMgr {
    /** Class singleton instance */
    private static ScanRunConfigMgr instance = null;

    private static final String XMLFILE = "scan/config/ScanRunConfig.xml";

    private ScanRunConfigXML xml = null;

    private IUFStatusHandler handler = UFStatus
            .getHandler(ScanRunConfigMgr.class);

    private ScanRunConfigMgr() {
        readFile();
    }

    public static synchronized ScanRunConfigMgr getInstance() {
        if (instance == null) {
            instance = new ScanRunConfigMgr();
        }

        return instance;
    }

    private void readFile() {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile(XMLFILE);

            xml = JAXB.unmarshal(path, ScanRunConfigXML.class);
        } catch (Exception e) {
            handler.handle(Priority.ERROR,
                    "Error loading Scan Run Config file: " + XMLFILE);
        }
    }

    public ArrayList<ScanVcpXML> getScanVcp() {
        return xml.getVcpList().getVcpData();
    }

    public ScanRunConfigXML getScanRunConfig() {
        return xml;
    }

    public void nullify() {
        instance = null;
    }
}
