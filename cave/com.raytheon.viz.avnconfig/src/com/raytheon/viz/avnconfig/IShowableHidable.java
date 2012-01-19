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

/**
 * The IShowableHidable interface specifies the methods that allow a dialog to
 * show the dialog, hide the dialog, and clear the dialog's contents.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 4/10/2008	934			grichard	Initial creation
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public interface IShowableHidable {

    /**
     * Show the dialog.
     */
    void showDialog();

    /**
     * Hide the dialog.
     */
    void hideDialog();

    /**
     * Clear the dialog's contents.
     */
    void clearAll();
}
