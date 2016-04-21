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
package com.raytheon.uf.viz.alertviz;

/**
 * Service for alertviz
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 6, 2015             mschenke    Initial creation
 * Jun 2, 2015  4473       njensen     Added methods
 *
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface AlertService {

    /**
     * Returns if the alert service is running embedded in a CAVE process or
     * running standalone as its own process
     * 
     * 
     * @return true if embedded, false if standalone
     */
    public boolean isEmbedded();

    /**
     * Sets the exit status of the service when the service is shut down. This
     * typically corresponds to a status defined in IApplication.
     * 
     * @param exitStatus
     *            the exit status
     */
    public void setExitStatus(int exitStatus);

}
