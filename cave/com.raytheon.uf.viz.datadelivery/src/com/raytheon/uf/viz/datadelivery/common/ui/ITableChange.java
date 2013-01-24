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
package com.raytheon.uf.viz.datadelivery.common.ui;

/**
 * Update changed values in the table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2012            lvenable     Initial creation.
 * Sep 06, 2012  687       mpduff       Adding a tableSelection method.
 * Dec 03, 2012 1285       bgonzale     Added a tableLock method.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public interface ITableChange {
    /**
     * Values in the table have changed.
     */
    void tableChanged();

    /**
     * A Table selection event has occurred.
     */
    void tableSelection();

    /**
     * A Table lock event has occurred.
     * 
     * @param isLocked
     */
    void tableLock(boolean isLocked);

}
