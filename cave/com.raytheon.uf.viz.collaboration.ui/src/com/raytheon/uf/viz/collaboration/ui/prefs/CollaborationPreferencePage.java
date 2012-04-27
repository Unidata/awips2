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
package com.raytheon.uf.viz.collaboration.ui.prefs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.uf.viz.collaboration.ui.Activator;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 27, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationPreferencePage extends FieldEditorPreferencePage
        implements IWorkbenchPreferencePage {

    private List<FieldEditor> editors = null;

    public CollaborationPreferencePage() {
        super(GRID);
        editors = new ArrayList<FieldEditor>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(IWorkbench workbench) {
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setDescription("Collaboration User Interface Preferences");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditorPreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        for (FieldEditor editor : editors) {
            editor.store();
        }
        return super.performOk();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors
     * ()
     */
    @Override
    protected void createFieldEditors() {

        FieldEditor notifications = new BooleanFieldEditor("notifications",
                "Show Notifications", getFieldEditorParent());
        this.addField(notifications);

        // ListEditor significant = new ListEditor("significantwords",
        // "Significant Words", getFieldEditorParent()) {
        //
        // @Override
        // protected String[] parseString(String stringList) {
        // return new String[0];
        // }
        //
        // @Override
        // protected String getNewInputObject() {
        // return "STRING";
        // }
        //
        // @Override
        // protected String createList(String[] items) {
        // return "TEST";
        // }
        // };
        // significant.getButtonBoxControl(getFieldEditorParent()).getChildren()[2]
        // .setVisible(false);
        // significant.getButtonBoxControl(getFieldEditorParent()).getChildren()[3]
        // .setVisible(false);
        // this.addField(significant);
    }
}
