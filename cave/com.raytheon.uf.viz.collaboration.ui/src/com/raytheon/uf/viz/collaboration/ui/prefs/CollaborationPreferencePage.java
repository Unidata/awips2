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

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;

/**
 * Collaboration specific preferences editor
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 27, 2012            mnash     Initial creation
 * Jan 14, 2014 2630       bclement     added away on idle
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class CollaborationPreferencePage extends FieldEditorPreferencePage
        implements IWorkbenchPreferencePage {

    public CollaborationPreferencePage() {
        super(GRID);
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
     * @see
     * org.eclipse.jface.preference.FieldEditorPreferencePage#createFieldEditors
     * ()
     */
    @Override
    protected void createFieldEditors() {
        FieldEditor notifications = new BooleanFieldEditor("notifications",
                "Show Notifications", getFieldEditorParent());
        this.addField(notifications);
        FieldEditor autojoinColl = new BooleanFieldEditor(
                CollabPrefConstants.AUTO_JOIN, "Join Discussion On Login",
                getFieldEditorParent());
        this.addField(autojoinColl);
        
        FieldEditor toggleIdle = new BooleanFieldEditor(
                CollabPrefConstants.AWAY_ON_IDLE, "Change Status On Idle",
                getFieldEditorParent());
        this.addField(toggleIdle);
        FieldEditor awayTimeOut = new IntegerFieldEditor(
                CollabPrefConstants.AWAY_TIMEOUT,
                "Minutes Before Becoming Idle:", getFieldEditorParent());
        this.addField(awayTimeOut);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditorPreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection != null && connection.isConnected()) {
            CollaborationConnection.getConnection().postEvent(
                    Activator.getDefault().getPreferenceStore());
        }
        return super.performOk();
    }
}
