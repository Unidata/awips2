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
package com.raytheon.viz.hydrobase.dialogs;

import org.eclipse.swt.widgets.Text;

/**
 * The interface for hydrology dialogs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 19, 2008 1782       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public interface IHydroDialog {

    /**
     * Get the data from the database that matched the location ID.
     */
    void getDialogData();

    /**
     * Update the dialog's control.
     */
    void updateDialogDisplay();

    /**
     * Update the dialog's state.
     */
    void updateDialogState();

    /**
     * Update the information control.
     */
    void updateInformation();

    /**
     * Obtain the currently selected data.
     * 
     * @return the data
     */
    Object getSelectedDatum();

    /**
     * Validate the user entry. Checks for an empty entry in the text control.
     * 
     * @return True if there is data in the references text control.
     */
    boolean validateEntryData(Text tf);

    /**
     * Save the record to the database.
     */
    boolean saveRecord();

    /**
     * Delete the record from the database.
     */
    void deleteRecord();

    /**
     * Clear the text fields in the form.
     */
    void clearForm();
}
