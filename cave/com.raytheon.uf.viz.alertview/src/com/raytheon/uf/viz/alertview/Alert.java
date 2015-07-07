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
package com.raytheon.uf.viz.alertview;

import java.util.Date;

import com.raytheon.uf.viz.alertview.ui.view.AlertView;

/**
 * The primary interface for items that should be displayed in {@link AlertView}
 * .
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 17, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public interface Alert {

    public enum Priority {
        DEBUG, INFO, WARN, ERROR;
    }

    /**
     * @return The time the event occured that generated this alert.
     */
    public Date getTime();

    /**
     * @return The importance of the alert.
     */
    public Priority getPriority();

    /**
     * @return An arbitrary string describing the origin of the Alert. This
     *         should be a short word or phrase that can be used by the user to
     *         take special action based on origins that are more or less
     *         important for a specific user.
     */
    public String getOrigin();

    /**
     * @return A short summary of the alert.
     */
    public String getMessage();

    /**
     * @return A detailed description of the alert.
     */
    public String getDetails();

}
