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
 * Nov 8, 2011             mschenke    Initial creation
 * Oct 08, 2015  4891      njensen     Added tooltips
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
        setDescription("Settings to reduce bandwidth usage by saving data locally");
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

        // data retrieved directly from pypies
        BooleanFieldEditor cacheWx = new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_CACHE_WEATHER,
                "Cache &Weather Data", getFieldEditorParent());
        cacheWx.getDescriptionControl(getFieldEditorParent()).setToolTipText(
                "Save data for swapping and multiple panes");
        addField(cacheWx);

        // localization files
        cacheLocalization = new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_CACHE_LOCALIZATION,
                "Cache &Localization Files", getFieldEditorParent());
        cacheLocalization
                .getDescriptionControl(getFieldEditorParent())
                .setToolTipText(
                        "Only download missing localization files (not updated files)");
        addField(cacheLocalization);

        // avoid the server for localization
        disableRemoteLocalization = new SyncLocalizationEditor(
                ThinClientPreferenceConstants.P_DISABLE_REMOTE_LOCALIZATION,
                "Use &Only Cached Localization Files", getFieldEditorParent());
        disableRemoteLocalization.getDescriptionControl(getFieldEditorParent())
                .setToolTipText(
                        "Only use previously downloaded localization files");
        addField(disableRemoteLocalization);

        // map data
        BooleanFieldEditor cacheMaps = new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_CACHE_MAPS, "Cache &Map Data",
                getFieldEditorParent());
        cacheMaps.getDescriptionControl(getFieldEditorParent()).setToolTipText(
                "Cache map data such as state/county boundaries");
        addField(cacheMaps);

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