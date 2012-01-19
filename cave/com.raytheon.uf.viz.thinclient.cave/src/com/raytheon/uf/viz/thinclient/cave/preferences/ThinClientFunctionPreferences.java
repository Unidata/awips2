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

package com.raytheon.uf.viz.thinclient.cave.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;

import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * 
 * Functional preferences for thin client
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 8, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class ThinClientFunctionPreferences extends FieldEditorPreferencePage {

    /**
     * Constructor
     */
    public ThinClientFunctionPreferences() {
        super(GRID);
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setDescription("Specify Thin Client function settings");
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
        // Add cache directory preference
        addField(new DirectoryFieldEditor(
                ThinClientPreferenceConstants.P_CACHE_DIR, "&Cache Directory",
                getFieldEditorParent()));

        // Add caching options
        // pypies
        addField(new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_CACHE_WEATHER,
                "&Cache Weather Data", getFieldEditorParent()));
        // localization files

        LocalizationCacheFieldEditor localizationCache = new LocalizationCacheFieldEditor(
                ThinClientPreferenceConstants.P_CACHE_LOCALIZATION,
                "&Cache Localization Files", getFieldEditorParent());

        addField(localizationCache);

        SyncLocalizationEditor disableRemoteLocalization = new SyncLocalizationEditor(
                ThinClientPreferenceConstants.P_DISABLE_REMOTE_LOCALIZATION,
                "&Disable Remote Localization Files", getFieldEditorParent());

        addField(disableRemoteLocalization);

        localizationCache.setSyncEditor(disableRemoteLocalization);

        // map data
        addField(new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_CACHE_MAPS, "&Cache Map Data",
                getFieldEditorParent()));

        // Add menu time options
        // TODO: Hook in complete disabling of menu time querying (URICatalog?)
        addField(new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_DISABLE_MENU_TIMES,
                "&Disable Menu Times", getFieldEditorParent()));

        HierarchicalPreferenceStore uiStore = Activator.getDefault()
                .getUiPreferenceStore();
        float[] intervals = uiStore
                .getFloatArray(ThinClientPreferenceConstants.P_MENU_TIME_UPDATE_INTERVALS);
        String[][] values = new String[intervals.length + 1][2];
        values[0] = new String[] { "Off", "0" };
        for (int i = 0; i < intervals.length; ++i) {
            String val = Integer.toString((int) intervals[i]);
            values[i + 1] = new String[] { val, val };
        }

        // TODO: Hook in periodic menu time updating (URICatalog?)
        addField(new ComboFieldEditor(
                ThinClientPreferenceConstants.P_MENU_TIME_REFRESH_INTERVAL,
                "&Menu Time Update Interval (min)", values,
                getFieldEditorParent()));

        // Add data update options
        intervals = uiStore
                .getFloatArray(ThinClientPreferenceConstants.P_DATA_UPDATE_INTERVALS);
        values = new String[intervals.length + 1][2];
        values[0] = new String[] { "Off", "0" };
        for (int i = 0; i < intervals.length; ++i) {
            String val = Integer.toString((int) intervals[i]);
            values[i + 1] = new String[] { val, val };
        }
        addField(new ComboFieldEditor(
                ThinClientPreferenceConstants.P_DATA_REFRESH_INTERVAL,
                "&Data Update Interval (min)", values, getFieldEditorParent()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    public void init(IWorkbench workbench) {

    }

}