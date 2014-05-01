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

import java.io.File;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.util.PropertyChangeEvent;

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
public class ThinClientCachePreferences extends FieldEditorPreferencePage {

    private BooleanFieldEditor cacheLocalization;

    private SyncLocalizationEditor disableRemoteLocalization;

    /**
     * Constructor
     */
    public ThinClientCachePreferences() {
        super(GRID);
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setTitle("Thin Client Caches");
        setDescription("Specify Thin Client Cache settings");
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
        if (getPreferenceStore().isDefault(
                ThinClientPreferenceConstants.P_CACHE_DIR)) {
            // If they are using the default directory we will create it before
            // making a field editor because the field editor is considered
            // invalid if it is refering to a directory which does not exist and
            // the default should be considered valid.
            new File(getPreferenceStore().getString(
                    ThinClientPreferenceConstants.P_CACHE_DIR)).mkdirs();
        }
        addField(new DirectoryFieldEditor(
                ThinClientPreferenceConstants.P_CACHE_DIR, "&Cache Directory",
                getFieldEditorParent()));

        // Add caching options
        // pypies
        addField(new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_CACHE_WEATHER,
                "Cache &Weather Data", getFieldEditorParent()));
        // localization files

        cacheLocalization = new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_CACHE_LOCALIZATION,
                "Cache &Localization Files", getFieldEditorParent());

        addField(cacheLocalization);

        disableRemoteLocalization = new SyncLocalizationEditor(
                ThinClientPreferenceConstants.P_DISABLE_REMOTE_LOCALIZATION,
                "Use &Only Cached Localization Files", getFieldEditorParent());

        addField(disableRemoteLocalization);

        // map data
        addField(new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_CACHE_MAPS, "Cache &Map Data",
                getFieldEditorParent()));

    }

    @Override
    public void propertyChange(PropertyChangeEvent event) {
        super.propertyChange(event);
        this.updateEnabledFields();
    }

    @Override
    protected void checkState() {
        super.checkState();
        this.updateEnabledFields();
    }

    private void updateEnabledFields() {
        super.checkState();
        boolean cacheLocalization = this.cacheLocalization.getBooleanValue();
        disableRemoteLocalization.setEnabled(cacheLocalization);
    }

}