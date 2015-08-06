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
package com.raytheon.uf.viz.alertview.ui.prefs;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.alertview.prefs.PopUpPreferences;
import com.raytheon.uf.viz.alertview.prefs.PopUpPreferences.PopUpCorner;
import com.raytheon.uf.viz.alertview.prefs.PreferenceFile;

/**
 * Preference page for configuring {@link PopUpPreferences}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 25, 2015  4474     bsteffen  Initial creation
 * Aug 06, 2015  4693     bsteffen  Update text and add tooltips.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PopupPreferencePage extends PreferencePage implements
        PreferenceFile.Listener<PopUpPreferences> {

    protected PreferenceFile<PopUpPreferences> preferenceFile;

    protected PriorityFilterCombo filterCombo;

    protected Text durationText;

    protected Combo positionCombo;

    @Override
    protected Control createContents(Composite parent) {
        preferenceFile = PopUpPreferences.load(this);
        Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayout(new GridLayout(2, false));
        new Label(composite, SWT.NONE).setText("Popup alerts with priority: ");
        filterCombo = new PriorityFilterCombo(composite);
        filterCombo
                .setToolTipText("A popup window will appear when alerts occur with this priority.");

        new Label(composite, SWT.NONE).setText("Duration (seconds): ");
        durationText = new Text(composite, SWT.SINGLE | SWT.BORDER);
        durationText.setToolTipText("Time the popup is displayed.");
        GridData gridData = new GridData();
        gridData.widthHint = 30;
        durationText.setLayoutData(gridData);
        new Label(composite, SWT.NONE).setText("Position: ");
        positionCombo = new Combo(composite, SWT.READ_ONLY);
        for (PopUpCorner position : PopUpCorner.values()) {
            positionCombo.add(position.getPrettyName());
        }

        populate();
        return composite;
    }

    protected void populate() {
        PopUpPreferences preferences = preferenceFile.get();
        filterCombo.setSelection(preferences.getFilter());
        durationText
                .setText(Double.toString(preferences.getDuration() / 1000.0));
        positionCombo.select(positionCombo.indexOf(preferences.getCorner()
                .getPrettyName()));
    }

    @Override
    protected void performDefaults() {
        populate();
        super.performDefaults();
    }

    @Override
    public boolean performOk() {
        PopUpPreferences newPrefs = new PopUpPreferences(preferenceFile.get());
        String filter = filterCombo.getSelection();
        if (filter != null) {
            newPrefs.setFilter(filter);
        }
        newPrefs.setDuration((int) (Double.parseDouble(durationText.getText()) * 1000));
        newPrefs.setCorner(PopUpCorner.fromPrettyName(positionCombo.getText()));
        preferenceFile.write(newPrefs);
        return super.performOk();
    }

    @Override
    public void dispose() {
        preferenceFile.close();
        super.dispose();
    }

    @Override
    public void update(PopUpPreferences preferences) {
        Display.getDefault().asyncExec(new Runnable() {

            @Override
            public void run() {
                if (!getControl().isDisposed()) {
                    populate();
                }
            }
        });
    }

}
