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
package com.raytheon.viz.ui.input.preferences;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.viz.ui.UiPlugin;

/**
 * 
 * The MousePreference Page allows the user to select which mouse events should
 * trigger actions. Actions are registered using the
 * com.raytheon.uf.viz.ui.mouse.action extension point to specify an id to use
 * internally as well as some information for the user. All preferences are made
 * available from the MousePreferenceManager using the the action id.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2009            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class MousePreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {
    
    private static final String NEW_EVENT_STRING = "New Event";

    private static final String DISABLED_EVENT_STRING = MouseEvent.DISABLED
            .toString();

    private Table table;
    
    private Combo perspectiveCombo;

    private Combo profileCombo;

    private Label actionLabel;

    private Label descriptionLabel;

    private List<Combo> eventCombos = new ArrayList<Combo>();

    private Composite editorComposite;

    private TableItem activeItem;

    private MousePreferenceManager prefManager = MousePreferenceManager
            .getInstance();

    
    private SelectionListener selectEventListener = new SelectionAdapter() {
        public final void widgetSelected(final SelectionEvent e) {
            StringBuilder value = new StringBuilder();
            // Rebuild the text for the table
            for (Combo combo : eventCombos) {
                if (!combo.getText().equals(NEW_EVENT_STRING)
                        && !combo.getText().equals(DISABLED_EVENT_STRING)) {
                    value.append(combo.getText());
                    value.append(',');
                }
            }
            // If nothing else then it must be disabled
            if (value.length() == 0) {
                value.append(DISABLED_EVENT_STRING);
            }
            activeItem.setText(2, value.toString().replaceAll(",$", ""));
            // Reset all the editor fields, mostly so all combos remain in sync
            editItem();
        }
    };

    public MousePreferencePage() {
        setDescription("Specify which mouse events should trigger specific actions");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    public void init(IWorkbench workbench) {

    }

    @Override
    protected Control createContents(Composite parent) {
        final Composite composite = new Composite(parent, SWT.NULL);
        composite.setLayout(new GridLayout());
        composite.setLayoutData(new GridData(GridData.FILL_BOTH));
        createPerspectiveSelector(composite);
        createProfileSelector(composite);
        createTable(composite);
        createEditorComposite(composite);
        loadTable();
        return composite;
    }

    private void createPerspectiveSelector(Composite parent) {
        Composite composite = new Composite(parent, SWT.NULL);
        composite.setLayout(new GridLayout(2, false));
        composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        // Just a combo and a label
        new Label(composite, SWT.LEFT).setText("Perspective:");
        perspectiveCombo = new Combo(composite, SWT.READ_ONLY);
        GridData gridData = new GridData();
        gridData.widthHint = 150;
        perspectiveCombo.setLayoutData(gridData);
        perspectiveCombo.setVisibleItemCount(9);
        perspectiveCombo.addSelectionListener(new SelectionAdapter() {
            public final void widgetSelected(final SelectionEvent e) {
                loadPerspective();// Do something
            }
        });
        // Here is where we actually add the perspectives
        IWorkbench workbench = PlatformUI.getWorkbench();
        for (IPerspectiveDescriptor perspective : workbench
                .getPerspectiveRegistry().getPerspectives()) {
            perspectiveCombo.add(perspective.getLabel());
        }
        int index = perspectiveCombo.indexOf(workbench
                .getActiveWorkbenchWindow().getActivePage().getPerspective()
                .getLabel());
        perspectiveCombo.select(index);
    }

    private void createProfileSelector(Composite parent) {
        Composite composite = new Composite(parent, SWT.NULL);
        composite.setLayout(new GridLayout(5, false));
        composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        // First a combo and a label
        new Label(composite, SWT.LEFT).setText("Profile:");
        profileCombo = new Combo(composite, SWT.READ_ONLY);
        GridData gridData = new GridData();
        gridData.widthHint = 150;
        profileCombo.setLayoutData(gridData);
        profileCombo.setVisibleItemCount(9);
        profileCombo.setItems(prefManager.getProfileNames());
        profileCombo.select(profileCombo.indexOf(prefManager
                .getProfileName(perspectiveCombo.getText())));
        profileCombo.addSelectionListener(new SelectionAdapter() {
            public final void widgetSelected(final SelectionEvent e) {
                loadProfile();
            }
        });
        // Next our buttons, its getting horizontally crowded in here
        Button duplicateButton = new Button(composite, SWT.PUSH);
        setButtonWidth(duplicateButton);
        duplicateButton.setText("Duplicate");
        duplicateButton.addSelectionListener(new SelectionAdapter() {
            public final void widgetSelected(final SelectionEvent e) {
                duplicateProfile();
            }
        });
        Button exportButton = new Button(composite, SWT.PUSH);
        setButtonWidth(exportButton);
        exportButton.setText("Export");
        exportButton.addSelectionListener(new SelectionAdapter() {
            public final void widgetSelected(final SelectionEvent e) {
                exportProfile();
            }
        });
        Button importButton = new Button(composite, SWT.PUSH);
        setButtonWidth(importButton);
        importButton.setText("Import");
        importButton.addSelectionListener(new SelectionAdapter() {
            public final void widgetSelected(final SelectionEvent e) {
                importProfile();
            }
        });
    }

    private void setButtonWidth(Button button) {
        int widthHint = convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_WIDTH);
        GridData gridData = new GridData();
        gridData.widthHint = Math.max(widthHint, button.computeSize(
                SWT.DEFAULT, SWT.DEFAULT, true).x) + 5;
        button.setLayoutData(gridData);
    }

    private void createTable(Composite parent) {
        table = new Table(parent, SWT.BORDER | SWT.FULL_SELECTION
                | SWT.H_SCROLL | SWT.V_SCROLL);
        table.setHeaderVisible(true);
        GridData gridData = new GridData(GridData.FILL_BOTH);
        gridData.heightHint = 80;
        gridData.widthHint = 450;
        table.setLayoutData(gridData);
        TableColumn categoryColumn = new TableColumn(table, SWT.NULL, 0);
        categoryColumn.setResizable(true);
        categoryColumn.setText("Category");
        categoryColumn.setWidth(150);
        TableColumn actionColumn = new TableColumn(table, SWT.NULL, 1);
        actionColumn.setResizable(true);
        actionColumn.setText("Action");
        actionColumn.setWidth(150);
        TableColumn eventColumn = new TableColumn(table, SWT.NULL, 2);
        eventColumn.setResizable(true);
        eventColumn.setText("Events");
        eventColumn.setWidth(150);
        table.addSelectionListener(new SelectionAdapter() {
            public final void widgetSelected(final SelectionEvent e) {
                editItem();// Do something
            }
        });
    }

    private void createEditorComposite(Composite parent) {
        editorComposite = new Composite(parent, SWT.NULL);
        editorComposite.setLayout(new GridLayout(4, false));
        editorComposite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        new Label(editorComposite, SWT.LEFT).setText("Action:");
        actionLabel = new Label(editorComposite, SWT.NONE);
        GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
        gridData.horizontalSpan = 3;
        actionLabel.setLayoutData(gridData);
        new Label(editorComposite, SWT.LEFT).setText("Description:");
        descriptionLabel = new Label(editorComposite, SWT.NONE);
        descriptionLabel.setLayoutData(gridData);
        new Label(editorComposite, SWT.LEFT).setText("Event:");
        // One combo just to avoid it looking all empty
        createCombo().setEnabled(false);
    }

    private Combo createCombo() {
        Combo eventCombo = new Combo(editorComposite, SWT.READ_ONLY);
        GridData gridData = new GridData();
        if ((eventCombos.size() % 3 == 0) && eventCombos.size() > 0) {
            gridData.horizontalSpan = 2;
            gridData.horizontalAlignment = GridData.HORIZONTAL_ALIGN_END;
        }
        gridData.widthHint = 125;
        eventCombo.setLayoutData(gridData);
        eventCombos.add(eventCombo);
        eventCombo.addSelectionListener(selectEventListener);
        return eventCombo;
    }

    private void editItem() {
        // Get the selected Item
        activeItem = table.getItem(table.getSelectionIndex());
        // Fill in the Action
        actionLabel.setText(activeItem.getText(1));
        // Fill in the Description
        descriptionLabel.setText(((IConfigurationElement) activeItem.getData()).getAttribute("description"));
        // Should I put category here, how much space do I really have?
        // Get all possible options
        String[] options = ((IConfigurationElement) activeItem.getData())
                .getAttribute("eventOptions").split(",");
        // A list of all the options selected(Except Disabled)
        String[] selection = activeItem.getText(2).replaceAll(
                DISABLED_EVENT_STRING, "").split(",");
        // Sometimes a single empty string slips in there
        if (selection.length == 1 && selection[0].isEmpty()) {
            selection = new String[0];
        }
        // A couple loops to even out the number of combo boxes
        while (eventCombos.size() <= selection.length) {
            createCombo();
        }
        while (eventCombos.size() > selection.length + 1) {
            eventCombos.get(0).dispose();
            eventCombos.remove(0);
        }
        // Now fill them all up
        for (int i = 0; i < selection.length; i++) {
            Combo combo = eventCombos.get(i);
            combo.setEnabled(true);
            combo.setItems(options);
            combo.add(DISABLED_EVENT_STRING, 0);
            combo.select(combo.indexOf(selection[i]));
        }
        // Create one extra one, in case they want to add in a bonus event
        Combo combo = eventCombos.get(selection.length);
        combo.setEnabled(true);
        combo.setItems(options);
        combo.add(NEW_EVENT_STRING, 0);
        combo.select(combo.indexOf(NEW_EVENT_STRING));
        // Remove any duplicate entries
        for (int i = 0; i <= selection.length; i++) {
            String remove = eventCombos.get(i).getText();
            if (remove.equals(DISABLED_EVENT_STRING) || remove.isEmpty()) {
                continue;
            }
            for (int j = i + 1; j <= selection.length; j++) {
                eventCombos.get(j).remove(remove);
            }
        }
        // Repack, seems necessary to make the world refresh ok
        getShell().pack();
        getShell().layout(true, true);

    }

    private void loadPerspective() {
        // Loads the correct profile for the currently displayed perspective
        String profileName = prefManager.getProfileName(perspectiveCombo
                .getText());
        profileCombo.select(profileCombo.indexOf(profileName));
        loadProfile();
    }

    private void loadProfile() {
        // clear the editor area
        activeItem = null;
        actionLabel.setText("");
        descriptionLabel.setText("");
        table.deselectAll();
        for (Combo eventCombo : eventCombos) {
            eventCombo.clearSelection();
            eventCombo.setEnabled(false);
        }
        // Loop through all the table items and get the values for this profile
        for (TableItem item : table.getItems()) {
            String id = ((IConfigurationElement) item.getData())
                    .getAttribute("id");
            String events = MouseEvent.toListString(prefManager
                    .getEvents(profileCombo.getText(), id));
            item.setText(2, events);
        }
    }

    public boolean performOk() {
        // First set the profile
        prefManager.setProfile(perspectiveCombo.getText(), profileCombo
                .getText());
        // Now save each table item.
        for (TableItem item : table.getItems()) {
            String id = ((IConfigurationElement) item.getData())
                    .getAttribute("id");
            prefManager.setEvents(profileCombo.getText(), id, MouseEvent
                    .fromListString(item.getText(2)));
        }
        // Attempt to save
        try {
            prefManager.save();
        } catch (IOException e) {
            e.printStackTrace();
            final String str = "Error saving preferences";
            final Status s = new Status(Status.ERROR, UiPlugin.PLUGIN_ID, str,
                    e);
            org.eclipse.jface.dialogs.ErrorDialog.openError(Display
                    .getCurrent().getActiveShell(), str, str, s);
        }
        return true;
    }

    protected void performDefaults() {
        // Ask nicely
        final String title = "Restore Mouse Defaults;";
        final String message = "This will restore all mouse settings to the default settings and remove any user changes.\n Are you sure you want to do that?";
        final boolean confirmed = MessageDialog.openConfirm(getShell(), title,
                message);

        if (confirmed) {
            // Blow away everything
            prefManager.restoreDefaults();
            // Remove any custom profiles
            profileCombo.setItems(prefManager.getProfileNames());
            // reload the current perspective
            loadPerspective();
            // performOk handles all the errors right, its better then
            // prefManager.save()
            performOk();
        }
    }

    private void loadTable() {
        IConfigurationElement[] config = Platform.getExtensionRegistry()
                .getConfigurationElementsFor(
                        MousePreferenceManager.EXTENSION_POINT);
         Arrays.sort(config, new Comparator<IConfigurationElement>() {

            @Override
            public int compare(IConfigurationElement e1,
                    IConfigurationElement e2) {
                String c1 = e1.getAttribute("category");
                String c2 = e2.getAttribute("category");
                int catCompare = c1.compareTo(c2);
                if (catCompare == 0) {
                    String n1 = e1.getAttribute("name");
                    String n2 = e2.getAttribute("name");
                    return n1.compareTo(n2);
                }
                return catCompare;
            }
        });
        // Loop through all the extension points and make table entries for them
        for (IConfigurationElement e : config) {
            String id = e.getAttribute("id");
            String action = e.getAttribute("name");
            String editor = e.getAttribute("category");
            String events = MouseEvent.toListString(prefManager
                    .getEvents(profileCombo.getText(), id));
            TableItem item = new TableItem(table, SWT.NONE, 0);
            item.setText(new String[] { editor, action, events });
            item.setData(e);
        }
    }

    private void importProfile() {
        // Ask for a file
        FileDialog fd = new FileDialog(getShell(), SWT.OPEN);
        fd.setFilterExtensions(new String[] { "*.pref" });
        if (fd.open() != null) {
            String path = fd.getFilterPath();
            String fileName = fd.getFileName();
            try {
                // Hand it off to the prefManager
                String newProfile = prefManager.importProfile(FileUtil.join(
                        path, fileName));
                // Select the new(or old) profile
                selectProfile(newProfile);
            } catch (ConfigurationException e) {
                e.printStackTrace();
                final String str = "Error importing preferences";
                final Status s = new Status(Status.ERROR, UiPlugin.PLUGIN_ID,
                        str, e);
                org.eclipse.jface.dialogs.ErrorDialog.openError(Display
                        .getCurrent().getActiveShell(), str, str, s);
            }
        }
    }

    private void exportProfile() {
        // Ask for a file
        FileDialog fd = new FileDialog(getShell(), SWT.SAVE);
        fd.setFileName("mousePreferences.pref");
        fd.setFilterExtensions(new String[] { "*.pref" });
        if (fd.open() != null) {
            String path = fd.getFilterPath();
            String fileName = fd.getFileName();
            try {
                // Hand it off to the prefManager
                prefManager.exportProfile(FileUtil.join(path, fileName),
                        profileCombo.getText());
            } catch (ConfigurationException e) {
                e.printStackTrace();
                final String str = "Error exporting preferences";
                final Status s = new Status(Status.ERROR, UiPlugin.PLUGIN_ID,
                        str, e);
                org.eclipse.jface.dialogs.ErrorDialog.openError(Display
                        .getCurrent().getActiveShell(), str, str, s);
            }

        }
    }
    
    private void duplicateProfile() {
        String oldProfileName = profileCombo.getText();
        String newProfileName = "Copy of " + oldProfileName;
        // Ask for a new name
        InputDialog dlg = new InputDialog(getShell(), "Duplicate Profile",
                "Enter a unique name for the new profile:", newProfileName,
                null);
        if (dlg.open() == Window.OK) {
            newProfileName = dlg.getValue();
        } else {
            return;
        }
        // Hand it off to the prefManager
        prefManager.duplicateProfile(oldProfileName, newProfileName);
        // Select the new profile
        selectProfile(newProfileName);
    }

    /**
     * Selects the given profile, adding it if it does not exist
     * 
     * @param profileName
     */
    private void selectProfile(String profileName) {
        int index = profileCombo.indexOf(profileName);
        if (index == -1) {
            profileCombo.add(profileName);
            index = profileCombo.indexOf(profileName);
        }
        profileCombo.select(index);
        loadProfile();
    }
}