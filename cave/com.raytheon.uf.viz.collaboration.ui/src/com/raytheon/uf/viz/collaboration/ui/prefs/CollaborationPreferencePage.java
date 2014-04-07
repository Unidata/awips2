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
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants.HandleOption;

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
 * Jan 27, 2014 2700       bclement     added auto accept subscribe
 * Feb  3, 2014 2699       bclement     added handle preferences
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class CollaborationPreferencePage extends FieldEditorPreferencePage
        implements IWorkbenchPreferencePage {

    private FieldEditor awayTimeOut;

    private FieldEditor customHandle;

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
                getFieldEditorParent()) {
            @Override
            protected void fireValueChanged(String property, Object oldValue,
                    Object newValue) {
                setEnabledForFieldEditor(awayTimeOut, (Boolean) newValue);
            }
        };
        this.addField(toggleIdle);

        awayTimeOut = new IntegerFieldEditor(
                CollabPrefConstants.AWAY_TIMEOUT,
                "Minutes Before Becoming Idle:", getFieldEditorParent());
        boolean awayChecked = this.getPreferenceStore().getBoolean(
                CollabPrefConstants.AWAY_ON_IDLE);
        setEnabledForFieldEditor(awayTimeOut, awayChecked);
        this.addField(awayTimeOut);

        FieldEditor autoSubscribe = new BooleanFieldEditor(
                CollabPrefConstants.AUTO_ACCEPT_SUBSCRIBE,
                "Automatically Accept Contact Requests",
                getFieldEditorParent());

        this.addField(autoSubscribe);

        FieldEditor defaultHandle = new ComboFieldEditor(
                CollabPrefConstants.DEFAULT_HANDLE, "Default Session Handle",
                CollabPrefConstants.HandleOption.displayValues(),
                getFieldEditorParent()) {
            @Override
            protected void fireValueChanged(String property, Object oldValue,
                    Object newValue) {
                super.fireValueChanged(property, oldValue, newValue);
                setEnableForCustomHandle(newValue.toString());
            }
        };
        this.addField(defaultHandle);
        
        customHandle = new StringFieldEditor(CollabPrefConstants.CUSTOM_HANDLE,
                "Custom Handle Text (see above)", getFieldEditorParent());
        String string = this.getPreferenceStore().getString(
                CollabPrefConstants.DEFAULT_HANDLE);
        setEnableForCustomHandle(string);
        this.addField(customHandle);
        
    }

    /**
     * Enable or disable customHandle field based on preference value
     * 
     * @param preference
     */
    private void setEnableForCustomHandle(String preference) {
        setEnabledForFieldEditor(
                customHandle,
                preference != null
                        && preference.equals(HandleOption.CUSTOM.name()));
    }

    /**
     * Enable or disable field editor
     * 
     * @param editor
     * @param enabled
     */
    private void setEnabledForFieldEditor(FieldEditor editor,
            boolean enabled){
        editor.setEnabled(enabled, getFieldEditorParent());
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
