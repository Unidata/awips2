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
package com.raytheon.viz.avnconfig;

import org.eclipse.swt.graphics.RGB;

/**
 * The IStatusSettable Interface allows the MetWatch Monitor the capability to
 * set the contents of the message status text and the associated label in the
 * message field at the bottom of the TAF Monitor Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 7, 2008	934			grichard	Initial creation
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public interface IStatusSettable {

    /**
     * Set the message text in the dialog.
     * 
     * @param msg --
     *            the message
     */
    void setMessageText(String msg, RGB rgbColor);
}
