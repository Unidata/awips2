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

package com.raytheon.monitor.preferences;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.monitor.activator.Activator;

/**
 * OHD monitor preferences page
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 25, 2008            ebabin     Initial creation
 * Oct, 1, 2008            randerso   Fixed a babinism
 * 16Jan2009    1860       MW Fegan   get defaults from plug-in.
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class MonitorPreferences extends FieldEditorPreferencePage implements
        IWorkbenchPreferencePage {

    public MonitorPreferences() {
        super(GRID);
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
    }

    @Override
    public void createFieldEditors() {

        addField(new StringFieldEditor(
                com.raytheon.monitor.preferences.PreferenceConstants.JDBC_URL,
                "&Database Connection String", getFieldEditorParent()));
        addField(new StringFieldEditor(
                com.raytheon.monitor.preferences.PreferenceConstants.RIVERMON_CONFIG_DIR,
                "&Config Directory", getFieldEditorParent()));
        addField(new StringFieldEditor(
                com.raytheon.monitor.preferences.PreferenceConstants.RIVERMON_LOG_DIR,
                "&Log Directory", getFieldEditorParent()));

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    public void init(IWorkbench workbench) {
        /*
         * intentionally empty -- causes page to be populated using the
         * plug-in's config.xml 
         */
    }
}
