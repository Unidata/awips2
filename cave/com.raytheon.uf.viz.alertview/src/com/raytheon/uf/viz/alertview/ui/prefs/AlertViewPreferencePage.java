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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.alertview.prefs.AlertViewPreferences;
import com.raytheon.uf.viz.alertview.prefs.PreferenceFile;
import com.raytheon.uf.viz.alertview.ui.view.AlertTable;

/**
 * Preference page for configuring {@link AlertViewPreferences}
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
public class AlertViewPreferencePage extends PreferencePage implements
        PreferenceFile.Listener<AlertViewPreferences> {

    protected PreferenceFile<AlertViewPreferences> preferenceFile;

    protected PriorityFilterCombo openFilterCombo;

    protected Text intervalText;

    protected Table columnTable;

    @Override
    protected Control createContents(Composite parent) {
        preferenceFile = AlertViewPreferences.load(this);
        Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayout(new GridLayout(2, false));

        new Label(composite, SWT.NONE).setText("Auto Open Priority: ");
        openFilterCombo = new PriorityFilterCombo(composite);

        new Label(composite, SWT.NONE)
                .setText("Hide Duplicate Interval(seconds): ");
        intervalText = new Text(composite, SWT.SINGLE | SWT.BORDER);
        GridData gridData = new GridData();
        gridData.widthHint = 30;
        intervalText.setLayoutData(gridData);
        Group columnGroup = new Group(composite, SWT.NONE);
        columnGroup.setLayout(new GridLayout(2, false));
        columnGroup.setText("Column Configuration");
        columnTable = new Table(columnGroup, SWT.CHECK);
        Composite columnButtonComp = new Composite(columnGroup, SWT.NONE);
        RowLayout rowLayout = new RowLayout(SWT.VERTICAL);
        rowLayout.fill = true;
        rowLayout.justify = true;
        columnButtonComp.setLayout(rowLayout);
        Button up = new Button(columnButtonComp, SWT.PUSH);
        up.setText("Up");
        up.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                moveColumn(true);
            }

        });
        Button down = new Button(columnButtonComp, SWT.PUSH);
        down.setText("Down");
        down.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                moveColumn(false);
            }

        });
        populate();
        return composite;
    }

    public void moveColumn(boolean up) {
        int selection = columnTable.getSelectionIndex();
        if (selection < 0) {
            return;
        }
        int newIndex;
        if (up) {
            if (selection == 0) {
                return;
            }
            newIndex = selection - 1;
        } else {
            if (selection == columnTable.getItemCount() - 1) {
                return;
            }
            newIndex = selection + 2;
        }
        TableItem oldItem = columnTable.getItem(selection);
        TableItem newItem = new TableItem(columnTable, SWT.NONE, newIndex);
        newItem.setText(oldItem.getText());
        newItem.setChecked(oldItem.getChecked());
        oldItem.dispose();
        columnTable.setSelection(newItem);
    }

    protected void populate() {
        AlertViewPreferences preferences = preferenceFile.get();
        openFilterCombo.setSelection(preferences.getOpenFilter());
        intervalText.setText(Double.toString(preferences
                .getMergeRepeatInterval() / 1000.0));
        columnTable.removeAll();
        List<String> columns = preferences.getColumns();
        for (String column : columns) {
            TableItem item = new TableItem(columnTable, SWT.NONE);
            item.setText(column);
            item.setChecked(true);
        }
        for (String column : AlertTable.ALL_COLUMNS) {
            if (!columns.contains(column)) {
                TableItem item = new TableItem(columnTable, SWT.NONE);
                item.setText(column);
                item.setChecked(false);
            }
        }
    }

    @Override
    protected void performDefaults() {
        populate();
        super.performDefaults();
    }

    @Override
    public boolean performOk() {
        AlertViewPreferences newPrefs = new AlertViewPreferences(
                preferenceFile.get());
        String openFilter = openFilterCombo.getSelection();
        if (openFilter != null) {
            newPrefs.setOpenFilter(openFilter);
        }
        newPrefs.setMergeRepeatInterval((int) (Double.parseDouble(intervalText
                .getText()) * 1000));
        List<String> columns = new ArrayList<>();
        for (TableItem item : columnTable.getItems()) {
            if (item.getChecked()) {
                columns.add(item.getText());
            }
        }
        newPrefs.setColumns(columns);
        preferenceFile.write(newPrefs);
        return super.performOk();
    }

    @Override
    public void dispose() {
        preferenceFile.close();
        super.dispose();
    }

    @Override
    public void update(AlertViewPreferences preferences) {
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
