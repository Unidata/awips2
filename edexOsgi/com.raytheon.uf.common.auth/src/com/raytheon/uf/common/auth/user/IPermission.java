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
package com.raytheon.uf.common.auth.user;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * A permission.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 05, 2012  1302      djohnson     Initial creation
 * Jul 26, 2031  2232      mpduff       Refactored Data Delivery permissions.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IPermission extends ISerializableObject {

    /**
     * Get the description.
     * 
     * @return the description
     */
    String getDescription();

    /**
     * Get the name.
     * 
     * @return the name
     */
    String getName();
}
