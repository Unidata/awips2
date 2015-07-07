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

import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FontDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.viz.alertview.Alert.Priority;
import com.raytheon.uf.viz.alertview.prefs.PreferenceFile;
import com.raytheon.uf.viz.alertview.style.AlertStyle;
import com.raytheon.uf.viz.alertview.style.StyleManager;
import com.raytheon.uf.viz.alertview.style.StylePreferences;

/**
 * Preference page for configuring {@link StylePreferences}
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
public class StylePreferencePage extends PreferencePage implements
        PreferenceFile.Listener<StylePreferences> {

    protected PreferenceFile<StylePreferences> preferenceFile;

    protected Table table;

    protected Combo filterCombo;

    protected ColorSelector foregroundSelector;

    protected ColorSelector backgroundSelector;

    protected Button fontButton;

    @Override
    protected Control createContents(Composite parent) {
        preferenceFile = StylePreferences.load(this);
        Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayout(new GridLayout(1, false));
        Composite tableComp = new Composite(composite, SWT.NONE);
        tableComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        tableComp.setLayout(new GridLayout(2, false));
        table = new Table(tableComp, SWT.NONE);
        table.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        table.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                styleSelected();
            }

        });
        Composite columnButtonComp = new Composite(tableComp, SWT.NONE);
        RowLayout rowLayout = new RowLayout(SWT.VERTICAL);
        rowLayout.fill = true;
        rowLayout.justify = true;
        columnButtonComp.setLayout(rowLayout);
        columnButtonComp.setLayoutData(new GridData(SWT.RIGHT, SWT.FILL, false,
                true));
        Button newButton = new Button(columnButtonComp, SWT.PUSH);
        newButton.setText("New");
        newButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                newStyle();
            }

        });
        Button deleteButton = new Button(columnButtonComp, SWT.PUSH);
        deleteButton.setText("Delete");
        deleteButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                deleteStyle();
            }

        });

        Composite editComp = new Composite(composite, SWT.NONE);
        editComp.setLayoutData(new GridData(SWT.BEGINNING, SWT.CENTER, false,
                false));
        editComp.setLayout(new GridLayout(2, false));
        new Label(editComp, SWT.NONE).setText("Priority: ");
        filterCombo = new Combo(editComp, SWT.READ_ONLY);
        for (Priority priority : Priority.values()) {
            filterCombo.add(priority.name());
        }
        filterCombo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                filterSelected();
            }

        });
        new Label(editComp, SWT.NONE).setText("Foreground Color: ");
        foregroundSelector = new ColorSelector(editComp);
        foregroundSelector.getButton().setLayoutData(
                new GridData(GridData.FILL_HORIZONTAL));
        foregroundSelector.addListener(new IPropertyChangeListener() {

            @Override
            public void propertyChange(PropertyChangeEvent event) {
                foregroundSelected();
            }
        });
        new Label(editComp, SWT.NONE).setText("Background Color: ");
        backgroundSelector = new ColorSelector(editComp);
        backgroundSelector.getButton().setLayoutData(
                new GridData(GridData.FILL_HORIZONTAL));
        backgroundSelector.addListener(new IPropertyChangeListener() {

            @Override
            public void propertyChange(PropertyChangeEvent event) {
                backgroundSelected();
            }
        });
        new Label(editComp, SWT.NONE).setText("Font: ");
        fontButton = new Button(editComp, SWT.PUSH);
        fontButton.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        fontButton.setText("Configure Font");
        fontButton.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                fontSelected();
            }

        });
        performDefaults();
        styleSelected();
        return composite;
    }

    protected void fontSelected() {
        FontData data = new FontDialog(getShell()).open();
        if (data == null) {
            return;
        }
        TableItem selection = table.getSelection()[0];
        AlertStyle style = (AlertStyle) selection.getData();
        StyleManager.setFont(style, data);
        selection.setFont(StyleManager.getFont(getShell().getDisplay(), style));
    }

    protected void backgroundSelected() {
        TableItem selection = table.getSelection()[0];
        selection.setBackground(new Color(getShell().getDisplay(),
                backgroundSelector.getColorValue()));
        AlertStyle style = (AlertStyle) selection.getData();
        style.setBackgroundColor(StyleManager.formatColor(backgroundSelector
                .getColorValue()));
    }

    protected void foregroundSelected() {
        TableItem selection = table.getSelection()[0];
        selection.setForeground(new Color(getShell().getDisplay(),
                foregroundSelector.getColorValue()));
        AlertStyle style = (AlertStyle) selection.getData();
        style.setForegroundColor(StyleManager.formatColor(foregroundSelector
                .getColorValue()));
    }

    protected void filterSelected() {
        TableItem selection = table.getSelection()[0];
        selection.setText(filterCombo.getItem(filterCombo.getSelectionIndex()));
        AlertStyle style = (AlertStyle) selection.getData();
        style.setFilter(selection.getText().toLowerCase());
    }

    protected void newStyle() {
        TableItem item = new TableItem(table, SWT.NONE);
        item.setText("INFO");
        AlertStyle style = new AlertStyle();
        style.setFilter(item.getText().toLowerCase());
        item.setData(style);
        table.select(table.indexOf(item));
        styleSelected();
    }

    @Override
    public void dispose() {
        preferenceFile.close();
        super.dispose();
    }

    protected void deleteStyle() {
        table.getSelection()[0].dispose();
        styleSelected();
    }

    protected void styleSelected() {
        int index = table.getSelectionIndex();
        if (index < 0) {
            filterCombo.deselectAll();
            filterCombo.setEnabled(false);
            foregroundSelector.setEnabled(false);
            backgroundSelector.setEnabled(false);
            fontButton.setEnabled(false);
            return;
        }
        TableItem selection = table.getItem(index);
        filterCombo.setEnabled(true);
        foregroundSelector.setEnabled(true);
        backgroundSelector.setEnabled(true);
        fontButton.setEnabled(true);

        index = filterCombo.indexOf(selection.getText());
        filterCombo.select(index);
        backgroundSelector.setColorValue(selection.getBackground().getRGB());
        foregroundSelector.setColorValue(selection.getForeground().getRGB());
    }

    @Override
    protected void performDefaults() {
        table.removeAll();
        for (AlertStyle style : preferenceFile.get().getStyles()) {
            TableItem item = new TableItem(table, SWT.NONE);
            item.setText(style.getFilter().toUpperCase());
            item.setBackground(StyleManager.parseColor(getShell().getDisplay(),
                    style.getBackgroundColor()));
            item.setForeground(StyleManager.parseColor(getShell().getDisplay(),
                    style.getForegroundColor()));
            item.setFont(StyleManager.getFont(getShell().getDisplay(), style));
            item.setData(new AlertStyle(style));
        }
        super.performDefaults();
    }

    @Override
    public boolean performOk() {
        List<AlertStyle> styles = new ArrayList<AlertStyle>();
        for (TableItem item : table.getItems()) {
            styles.add((AlertStyle) item.getData());
        }
        StylePreferences newPrefs = new StylePreferences();
        newPrefs.setStyles(styles);
        preferenceFile.write(newPrefs);
        return super.performOk();
    }

    @Override
    public void update(StylePreferences preferences) {
        Display.getDefault().asyncExec(new Runnable() {

            @Override
            public void run() {
                if (!getControl().isDisposed()) {
                    performDefaults();
                }
            }
        });
    }

}
