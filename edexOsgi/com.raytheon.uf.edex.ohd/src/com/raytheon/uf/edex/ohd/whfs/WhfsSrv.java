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
package com.raytheon.uf.edex.ohd.whfs;

import java.io.File;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.ohd.ScriptService;
import com.raytheon.uf.edex.ohd.ServiceInterface;

/**
 * Service interface for the scripts in the whfs_bin_dir
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 18, 2008            jelkins     Initial creation
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class WhfsSrv implements ServiceInterface {

    private final String scriptDir = AppsDefaults.getInstance().getToken(
            "whfs_bin_dir");

    private String[] serviceScripts;

    public WhfsSrv(String... serviceScripts) {
        this.serviceScripts = serviceScripts;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.ohdlib.services.ServiceInterface#execute()
     */
    @Override
    public void execute() throws EdexException {
        for (String script : serviceScripts) {
            ScriptService s = new ScriptService(scriptDir + File.separatorChar
                    + script);

            if (AppsDefaults.getInstance().setAppContext(this)) {
                s.execute();
            }
        }
    }
}
