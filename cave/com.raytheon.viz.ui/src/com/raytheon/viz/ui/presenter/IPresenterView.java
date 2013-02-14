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
package com.raytheon.viz.ui.presenter;

/**
 * A view interface that defines methods all views should provide an
 * implementation for.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012 0743       djohnson     Initial creation
 * Nov 20, 2012 1322       djohnson     Extend IDisplay for displayYesNoPopup.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface IPresenterView extends IDisplay {

    /**
     * Performs initialization of the view.
     */
    void init();

    /**
     * Display a popup message.
     * 
     * @param title
     *            the title
     * @param message
     *            the message
     */
    void displayPopup(String title, String message);

    /**
     * Display an error popup message.
     * 
     * @param title
     *            the title
     * @param message
     *            the message
     */
    void displayErrorPopup(String title, String message);

    /**
     * Display a cancel/ok popup.
     * 
     * @param title
     *            the title
     * @param message
     *            the message
     * 
     * @return boolean true if OK clicked, false if Cancel
     */
    boolean displayOkCancelPopup(String title, String message);
}
