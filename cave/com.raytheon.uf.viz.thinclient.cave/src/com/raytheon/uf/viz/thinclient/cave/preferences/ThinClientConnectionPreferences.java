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
import org.eclipse.jface.preference.RadioGroupFieldEditor;
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
 * Feb 08, 2016, 5281     tjensen      Replaced disableJms checkbox with radio buttons 
 *                                      and combined Data and Menu Refresh Intervals
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public class ThinClientConnectionPreferences extends FieldEditorPreferencePage {

    private RadioGroupFieldEditor dataRefreshMethod;

    private BooleanFieldEditor disableMenuTimes;

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

    @Override
    protected void createFieldEditors() {
        dataRefreshMethod = new RadioGroupFieldEditor(
                ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD,
                "Data Refresh: ",
                2,
                new String[][] {
                        {
                                "&Automatic Push",
                                ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_PUSH },
                        {
                                "&Timed Poll",
                                ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_POLL } },
                getFieldEditorParent());
        addField(dataRefreshMethod);

        disableMenuTimes = new BooleanFieldEditor(
                ThinClientPreferenceConstants.P_DISABLE_MENU_TIMES,
                "Disable &Menu Times", getFieldEditorParent());

        addField(disableMenuTimes);

        HierarchicalPreferenceStore uiStore = Activator.getDefault()
                .getUiPreferenceStore();

        // Add data update options
        float[] intervals = uiStore
                .getFloatArray(ThinClientPreferenceConstants.P_DATA_UPDATE_INTERVALS);
        String[][] values = new String[intervals.length + 1][2];
        values[0] = new String[] { "Off", "0" };
        for (int i = 0; i < intervals.length; ++i) {
            String val = Integer.toString((int) intervals[i]);
            values[i + 1] = new String[] { val, val };
        }

        dataUpdateInterval = new ComboFieldEditor(
                ThinClientPreferenceConstants.P_DATA_REFRESH_INTERVAL,
                "&Data Update Interval (min)", values, getFieldEditorParent());

        addField(dataUpdateInterval);
        boolean disableJMS = ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_POLL
                .equals(getPreferenceStore().getString(
                        ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD));
        dataUpdateInterval.setEnabled(disableJMS, getFieldEditorParent());
    }

    @Override
    public void propertyChange(PropertyChangeEvent event) {
        super.propertyChange(event);
        if (event.getSource() == dataRefreshMethod) {
            this.updateEnabledFields((String) event.getNewValue());
        }
        ;
    }

    private void updateEnabledFields(String refreshMethod) {
        boolean disableJMS = ThinClientPreferenceConstants.P_DATA_REFRESH_METHOD_POLL
                .equals(refreshMethod);
        dataUpdateInterval.setEnabled(disableJMS, getFieldEditorParent());
    }

}
