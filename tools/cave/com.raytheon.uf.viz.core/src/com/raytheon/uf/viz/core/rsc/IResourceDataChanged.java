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
package com.raytheon.uf.viz.core.rsc;

/**
 * Implements a listener to be notified that a change has occurred in the
 * ResourceData
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 9, 2009             chammack    Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public interface IResourceDataChanged {

    /**
     * The type of a change
     * 
     * <UL>
     * <LI><B>CAPABILITY</B> - a capability has had its property change, the
     * payload is that capability object</LI>
     * <LI><B>DATA_UPDATE</B> - the resource data has been notified of an update
     * that has occurred. The payload is the object as transformed by
     * ResourceData</LI>
     * </UL>
     */
    public static enum ChangeType {
        CAPABILITY, DATA_UPDATE, DATA_REMOVE
    };

    /**
     * Indicates that a change has taken place in resource data
     * 
     * @param type
     *            the type of change
     * @param object
     *            a payload associated with a change
     */
    public void resourceChanged(ChangeType type, Object object);

}
