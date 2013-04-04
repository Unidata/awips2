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
package com.raytheon.uf.edex.ohd.pproc;

import java.io.File;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.ohd.ScriptService;
import com.raytheon.uf.edex.ohd.ServiceInterface;

/**
 * Service interface for the scripts in the pproc_bin_dir
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

public class PprocSrv implements ServiceInterface {

    private final String scriptDir = AppsDefaults.getInstance().getToken(
            "pproc_bin");

    private String[] serviceScripts;

    public PprocSrv(String... serviceScripts) {
        this.serviceScripts = serviceScripts;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.ohdlib.services.ServiceInterface#execute()
     */
    @Override
    public void execute() throws EdexException {

        if (AppsDefaults.getInstance().setAppContext(this)) {
            for (String script : serviceScripts) {
                ScriptService s = new ScriptService(scriptDir
                        + File.separatorChar + script);
                s.execute();
            }
        }
    }
}
