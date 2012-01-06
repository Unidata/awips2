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

import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;

/**
 * A modification of the StringFieldEditor class which is read-only and does not
 * save its value to the underlying preference store.
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

public class ReadOnlyStringFieldEditor extends StringFieldEditor {

    private String value = "";

    /**
     * 
     */
    public ReadOnlyStringFieldEditor() {
        super();
    }

    /**
     * @param name
     * @param labelText
     * @param parent
     */
    public ReadOnlyStringFieldEditor(String name, String labelText,
            Composite parent) {
        super(name, labelText, parent);
        setEnabled(false, parent);
    }

    public ReadOnlyStringFieldEditor(String name, String labelText,
            String value, Composite parent) {
        this(name, labelText, parent);
        this.value = value;

    }

    /**
     * @param name
     * @param labelText
     * @param width
     * @param parent
     */
    public ReadOnlyStringFieldEditor(String name, String labelText, int width,
            Composite parent) {
        super(name, labelText, width, parent);
        setEnabled(false, parent);
    }

    /**
     * @param name
     * @param labelText
     * @param width
     * @param strategy
     * @param parent
     */
    public ReadOnlyStringFieldEditor(String name, String labelText, int width,
            int strategy, Composite parent) {
        super(name, labelText, width, strategy, parent);
        setEnabled(false, parent);
    }

    @Override
    protected void doStore() {
        return; // no-op as we don't want this control writing to the preference
                // store
    }

    @Override
    protected void doLoad() {
        if (getTextControl() != null) {
            getTextControl().setText(this.value);
        }
    }

    public void setValue(String value) {
        this.value = value;
    }

}
