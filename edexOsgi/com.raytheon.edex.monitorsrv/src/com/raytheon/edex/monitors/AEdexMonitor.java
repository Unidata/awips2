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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * TODO Add Description
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  06May2008    1113       MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public abstract class AEdexMonitor implements IEdexMonitor {
    protected transient Log logger = LogFactory.getLog(getClass());
    /**
     * Constructor. Creates an empty object.
     */
    public AEdexMonitor() {
        /* intentionally empty */
    }

    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.IEdexMonitor#execute()
     */
    @Override
    abstract public void execute();
    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.IEdexMonitor#setData(java.lang.String)
     */
    @Override
    abstract public void setData(String data);}
