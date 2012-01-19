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

/**
 * An abstract base class for EDEX Database Monitors.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12May2008    1113       MW Fegan    Initial creation.
 * 	
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public abstract class AEdexDBMonitor extends AEdexMonitor {
    protected String query 
        = "select count(*) from pg_stat_activity where usename='awips' and datname='metadata'";

    protected static final String REPORT_FORMAT = "Database: %s, connections = %s";
    /**
     * Constructor. Creates a default object with the default query.
     */
    public AEdexDBMonitor() {
        // Intentionally empty.
    }
    /**
     * Constructor. Creates an object for the specified query.
     * @param query
     */
    public AEdexDBMonitor(String query) {
        this.query = query;
    }
    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.AEdexMonitor#execute()
     */
    @Override
    abstract public void execute();
    
    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.AEdexMonitor#setData(java.lang.String)
     */
    @Override
    public void setData(String data) {
        this.query = data;
    }

}
