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

import jep.Jep;
import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;

/**
 * Simple test driver to cause IFPServerConfig to initialize from
 * serverConfig.py
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/12/08     #1030      randerso    Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

// TODO: make this a JUnit test
public class ServerConfigTest {
    public static void main(String[] args) {
        try {
            long t0 = System.currentTimeMillis();

            // Need to do the Jep setup and run of serverConfig.py here rather
            // than using what's in IFPServerConfig.getInstance()
            // so it works when run from Eclipse
            Jep jep = new Jep(false, "opt/utility/edex_static/base/config/gfe",
                    DiscreteKey.class.getClassLoader());
            jep.eval("__builtins__.__import__ = jep.jimport");
            jep.runScript("opt/utility/edex_static/base/config/gfe/serverConfig.py");

            // TODO fix test
            IFPServerConfig ifpServerConfig = IFPServerConfigManager
                    .getServerConfig("OAX");
            long t = System.currentTimeMillis() - t0;

            System.out.println("took " + t + "ms");
            System.out.println(ifpServerConfig);

            DatabaseID id = ifpServerConfig.getOfficialDatabases().get(0);
            GridDbConfig config = ifpServerConfig.gridDbConfig(id);
            System.out.println(config.toString());

        } catch (JepException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
