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
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.util.PropertyChangeEvent;

import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * Thin client server preferences
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
public class ThinClientConnectionPreferences extends FieldEditorPreferencePage {

    private BooleanFieldEditor disableJMS;

    private BooleanFieldEditor disableMenuTimes;

    private ComboFieldEditor menuTimeInterval;

    private ComboFieldEditor dataUpdateInterval;

    /**
     * Constructor
     */
    public ThinClientConnectionPreferences() {
        super(GRID);
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setTitle("Thin Client Connections");
        setDescription("Thin Client Connections");
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
        disableJMS = new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_DISABLE_JMS, "Disable &JMS",
                getFieldEditorParent());
        addField(disableJMS);

        disableMenuTimes = new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_DISABLE_MENU_TIMES,
                "Disable Menu &Times", getFieldEditorParent());

        addField(disableMenuTimes);

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

        menuTimeInterval = new ComboFieldEditor(
                ThinClientPreferenceConstants.P_MENU_TIME_REFRESH_INTERVAL,
                "&Menu Time Update Interval (min)", values,
                getFieldEditorParent());

        // TODO: Hook in periodic menu time updating (URICatalog?)
        addField(menuTimeInterval);

        // Add data update options
        intervals = uiStore
                .getFloatArray(ThinClientPreferenceConstants.P_DATA_UPDATE_INTERVALS);
        values = new String[intervals.length + 1][2];
        values[0] = new String[] { "Off", "0" };
        for (int i = 0; i < intervals.length; ++i) {
            String val = Integer.toString((int) intervals[i]);
            values[i + 1] = new String[] { val, val };
        }

        dataUpdateInterval = new ComboFieldEditor(
                ThinClientPreferenceConstants.P_DATA_REFRESH_INTERVAL,
                "&Data Update Interval (min)", values, getFieldEditorParent());

        addField(dataUpdateInterval);
        boolean disableJMS = getPreferenceStore().getBoolean(
                ThinClientPreferenceConstants.P_DISABLE_JMS);
        menuTimeInterval.setEnabled(disableJMS, getFieldEditorParent());
        dataUpdateInterval.setEnabled(disableJMS, getFieldEditorParent());
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
        boolean disableMenuTimes = this.disableMenuTimes.getBooleanValue();
        boolean disableJMS = this.disableJMS.getBooleanValue();
        dataUpdateInterval.setEnabled(disableJMS, getFieldEditorParent());
        menuTimeInterval.setEnabled(!disableMenuTimes && disableJMS,
                getFieldEditorParent());
    }

}
