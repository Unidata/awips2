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
package com.raytheon.uf.viz.python.swt;

import org.eclipse.jface.dialogs.IDialogConstants;

/**
 * Buttons used by the runtime interface.
 * 
 * Simple enum to handle Button IDs and Labels.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jun 3, 2008  1164        jelkins Initial creation
 * Jun 16,2008  1164        jelkins Added OK Button
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public enum ButtonConstant {

    // Predefined Buttons from IDialogConstants
    CANCEL(IDialogConstants.CANCEL_LABEL, IDialogConstants.CANCEL_ID), OK(
            IDialogConstants.OK_LABEL, IDialogConstants.OK_ID),

    // Custom Client Buttons
    RUN("Run"), RUN_DISMISS("Run/Dismiss");

    public final int id;

    public final String label;

    /**
     * Constructor for explicitly assigning both an id and label.
     * 
     * @param label
     * @param id
     */
    ButtonConstant(String label, int id) {
        this.label = label;
        this.id = id;
    }

    /**
     * Assigns the given label and generates an suitable id for the button.
     * 
     * @param label
     */
    ButtonConstant(String label) {
        this.id = IDialogConstants.CLIENT_ID + this.ordinal();
        this.label = label;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString() {
        return label;
    }

    /**
     * Returns a Button with the given id.
     * 
     * @param id
     * @return Button with the id.
     * @throws AssertionError
     *             when no button has the given id
     */
    public static final ButtonConstant getButton(int id) {
        for (ButtonConstant button : ButtonConstant.values()) {
            if (button.id == id)
                return button;
        }
        throw new AssertionError("unknown Button id: " + id);
    }

}