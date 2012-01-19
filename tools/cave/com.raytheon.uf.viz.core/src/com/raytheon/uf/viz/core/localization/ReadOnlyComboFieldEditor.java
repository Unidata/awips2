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
package com.raytheon.uf.viz.core.localization;

import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.swt.widgets.Composite;

/**
 * A modification of the ComboFieldEditor class which is read-only and does not
 * store its value to the underlying preference store.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 7, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class ReadOnlyComboFieldEditor extends ComboFieldEditor {

    /**
     * Create the read-only combo box field editor.
     * 
     * @param name
     *            the name of the preference this field editor works on
     * @param labelText
     *            the label text of the field editor
     * @param entryNamesAndValues
     *            the names (labels) and underlying values to populate the combo
     *            widget. These should be arranged as: { {name1, value1},
     *            {name2, value2}, ...}
     * @param parent
     *            the parent composite
     */
    public ReadOnlyComboFieldEditor(String name, String labelText,
            String[][] entryNamesAndValues, Composite parent) {
        super(name, labelText, entryNamesAndValues, parent);
        setEnabled(false, parent);
    }

    @Override
    protected void doStore() {
        return; // no-op as we don't want this control writing to the preference
                // store
    }

}
