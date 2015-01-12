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

import java.io.File;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.collaboration.ui.Activator;

/**
 * Collaboration's Room Events preferences page.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2014   2631     mpduff      Initial creation.
 * Mar 24, 2014   2936     mpduff      Remove join alerts from feed view.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class CollaborationRoomChangePreferencePage extends
        FieldEditorPreferencePage implements IWorkbenchPreferencePage {

    /** Join event controls */
    private FileFieldEditor joinFileEditor;

    /** Enable join alerts controls */
    private BooleanFieldEditor enableJoinAlerts;

    /**
     * Constructor.
     */
    public CollaborationRoomChangePreferencePage() {
        super(GRID);
    }

    @Override
    public void init(IWorkbench workbench) {
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
    }

    @Override
    protected void createFieldEditors() {
        enableJoinAlerts = new BooleanFieldEditor(
                CollabPrefConstants.ENABLE_JOIN_EVENTS_FIELD_EDITOR_ID,
                "Enable Join Room Alerts", BooleanFieldEditor.DEFAULT,
                getFieldEditorParent());
        this.addField(enableJoinAlerts);

        joinFileEditor = new FileFieldEditor(
                CollabPrefConstants.JOIN_FILE_FIELD_EDITOR_ID, "Sound File",
                getFieldEditorParent());

        PathManager manager = (PathManager) PathManagerFactory.getPathManager();
        LocalizationContext context = manager.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = manager.getLocalizationFile(context,
                "collaboration" + File.separator + "sounds" + File.separator);
        if (!file.exists()) {
            file.getFile().mkdirs();
        }
        joinFileEditor.setFilterPath(file.getFile());
        this.addField(joinFileEditor);

        boolean enabled = this.getPreferenceStore().getBoolean(
                CollabPrefConstants.ENABLE_JOIN_EVENTS_FIELD_EDITOR_ID);
        joinFileEditor.setEnabled(enabled, getFieldEditorParent());

        if (!joinFileEditor.isValid()) {
            joinFileEditor.setStringValue("");
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.FieldEditorPreferencePage#propertyChange
     * (org.eclipse.jface.util.PropertyChangeEvent)
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {
        if (event.getSource().equals(this.enableJoinAlerts)) {
            boolean enabled = (Boolean) event.getNewValue();
            joinFileEditor.setEnabled(enabled, getFieldEditorParent());
        } else if (event.getSource().equals(this.joinFileEditor)) {
            if (enableJoinAlerts.getBooleanValue()) {
                super.propertyChange(event);
            }
        } else {
            super.propertyChange(event);
        }
    }
}
