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
package com.raytheon.uf.edex.metartohmdb;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.metartohmdb.dao.HMDBRptDao;

/**
 * Created to extract purge method from MetarToHMDBSrv.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class HMDBPurger {

    private Log logger = LogFactory.getLog(getClass());

    private HMDBRptDao dao = null;

    // If a required resource is not available,
    // Set failSave to true. This will "short circuit" processing.
    private boolean failSafe = false;

    private int purgeHours = 8;
    
    /**
     * Construct an instance of this transformer.
     */
    public HMDBPurger() {

        try {
            dao = new HMDBRptDao();
        } catch (Exception e) {
            logger.error("HMDBRptDao creation failed", e);
            logger.error("Plugin set to failSafe mode");
            failSafe = true;
        }
    }

    /**
     * 
     */
    public void purgeHMDBRpt() {

        logger.info("Purge routine started [" + purgeHours + "]");
        if (failSafe) {
            logger.info("In failsafe mode. No purge performed.");
            return;
        }
        if(dao != null) {
            dao.purgeTable(purgeHours);
        }
    }

    /**
     * @return the purgeHours
     */
    public int getPurgeHours() {
        return purgeHours;
    }

    /**
     * @param purgeHours the purgeHours to set
     */
    public void setPurgeHours(int purgeHours) {
        this.purgeHours = purgeHours;
    }

    
}
