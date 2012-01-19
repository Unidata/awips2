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
package com.raytheon.viz.gfe.core.msgs;

import com.raytheon.viz.gfe.core.ISampleSetManager;

/**
 * Listener for when the sample set changes
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/09/2009              rpeter      Initial Creation.
 * 
 * </pre>
 * 
 * @author rpeter
 * @version 1.0
 */

public interface ISampleSetChangedListener {

    /**
     * Notification that the sample set has changed.
     * 
     * @param sampleSetMgr
     *            the sample set mgr that has changed
     */
    public void sampleSetChanged(ISampleSetManager sampleSetMgr);

}
