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
package com.raytheon.edex.monitors;

import com.raytheon.edex.services.MonitorSrv;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Performs a database monitoring task on the database utilized by the Mule
 * instance. The number of connections is reported to the system log. The format
 * of the log message is
 * <P>
 * Database: {connection URL}, connections = {count}
 * <P>
 * This class is intended to be injected into an {@link MonitorSrv} instance by
 * Mule. Because of that, all constructor arguments are of type String.
 * <P>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  06May2008    1113       MW Fegan    Initial creation.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class EdexDBLocalMonitor extends AEdexDBMonitor {

    /**
     * Constructor. Creates a Local Database Monitor using the default query
     * string.
     */
    public EdexDBLocalMonitor() {
        // Intentionally Empty
    }

    /**
     * Constructor. Creates a Local Database Monitor using the specified query
     * string.
     * 
     * @param query
     *            the monitoring query string
     */
    public EdexDBLocalMonitor(String query) {
        super(query);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.monitors.AEdexDBMonitor#execute()
     */
    @Override
    public void execute() {
        CoreDao dao = new CoreDao(DaoConfig.DEFAULT);

        Object[] results = dao.executeSQLQuery(query);
        if (results == null || results.length == 0) {
            logger.info("No results returned from query [" + query + "]");
            return;
        }

        logger.info(String.format(REPORT_FORMAT, "localhost/"
                + DaoConfig.DEFAULT_DB_NAME, results[0].toString()));

    }
}
