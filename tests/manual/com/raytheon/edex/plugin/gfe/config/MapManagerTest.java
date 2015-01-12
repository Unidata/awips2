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

import java.io.File;

import jep.JepException;

import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.edex.plugin.gfe.reference.MapManager;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 10, 2008				randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MapManagerTest {
    public static void main(String[] args) {
        try {
            String siteID = "OAX";
            String baseDir = "../build.edex/esb/data/utility/edex_static/base/config/gfe";
            String siteDir = "./utility/edex_static/site/OAX/config/gfe/";
            String vtecDir = "../com.raytheon.uf.edex.activetable/utility/common_static/base/vtec";

            System.out.println(new File(baseDir).getAbsolutePath());
            System.out.println(new File(siteDir).getAbsolutePath());
            System.out.println(new File(vtecDir).getAbsolutePath());

            IFPServerConfig siteConfig = null;
            PythonScript py = null;
            try {
                py = new PythonScript(baseDir + File.separator + "wrapper.py",
                        PyUtil.buildJepIncludePath(siteDir, baseDir, vtecDir),
                        IFPServerConfig.class.getClassLoader());
                SimpleServerConfig simpleConfig = (SimpleServerConfig) py
                        .execute("getSimpleConfig", null);
                siteConfig = new IFPServerConfig(simpleConfig);
            } catch (JepException e) {
                throw new GfeConfigurationException(
                        "Exception occurred while processing serverConfig.py for site "
                                + siteID, e);
            } finally {
                if (py != null) {
                    py.dispose();
                }
            }
            new MapManager(siteConfig, "/tmp");
        } catch (Throwable e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
