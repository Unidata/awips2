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

import java.util.Arrays;
import java.util.List;

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
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PopupPreferencePage extends PreferencePage implements
        PreferenceFile.Listener<PopUpPreferences> {

    private static final List<String> SIZES = Arrays.asList("Small", "Medium",
            "Large");

    private static final int BASE_WIDTH = 500;

    private static final int BASE_HEIGHT = 50;

    protected PreferenceFile<PopUpPreferences> preferenceFile;

    protected PriorityFilterCombo filterCombo;

    protected Text durationText;

    protected Combo positionCombo;

    protected Combo sizeCombo;

    @Override
    protected Control createContents(Composite parent) {
        preferenceFile = PopUpPreferences.load(this);
        Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayout(new GridLayout(2, false));
        new Label(composite, SWT.NONE).setText("Popup Priority: ");
        filterCombo = new PriorityFilterCombo(composite);
        new Label(composite, SWT.NONE).setText("Duration(seconds): ");
        durationText = new Text(composite, SWT.SINGLE | SWT.BORDER);
        GridData gridData = new GridData();
        gridData.widthHint = 30;
        durationText.setLayoutData(gridData);
        new Label(composite, SWT.NONE).setText("Position: ");
        positionCombo = new Combo(composite, SWT.READ_ONLY);
        for (PopUpCorner position : PopUpCorner.values()) {
            positionCombo.add(position.getPrettyName());
        }

        new Label(composite, SWT.NONE).setText("Size: ");
        sizeCombo = new Combo(composite, SWT.READ_ONLY);
        for (String size : SIZES) {
            sizeCombo.add(size);
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
        boolean sizeFound = false;
        for (int i = 0; i < SIZES.size(); i += 1) {
            if (preferences.getHeight() == BASE_HEIGHT + 25 * i
                    && preferences.getWidth() == BASE_WIDTH + 50 * i) {
                sizeCombo.select(i);
                sizeFound = true;
                break;
            }
        }
        if (!sizeFound) {
            if (sizeCombo.getItemCount() > SIZES.size()) {
                sizeCombo.remove(SIZES.size());
            }
            sizeCombo.add(preferences.getWidth() + " x "
                    + preferences.getHeight());
            sizeCombo.select(SIZES.size());
        }
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
        int index = SIZES.indexOf(sizeCombo.getText());
        if (index >= 0) {
            newPrefs.setWidth(BASE_WIDTH + 50 * index);
            newPrefs.setHeight(BASE_HEIGHT + 25 * index);
        }
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
