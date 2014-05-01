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

package com.raytheon.uf.viz.core.localization;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.comm.ConnectivityManager;
import com.raytheon.uf.viz.core.comm.ConnectivityManager.ConnectivityResult;
import com.raytheon.uf.viz.core.comm.IConnectivityCallback;

/**
 * 
 * Localization preferences
 * 
 * NOTE: These preferences are stored using the regular Eclipse preference
 * mechanism to ensure that there is no collision with the localized
 * preferences.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 18, 2007            chammack    Initial Creation.
 * Aug 02, 2013 2202       bsteffen    Add edex specific connectivity checking.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class LocalizationPreferences extends FieldEditorPreferencePage
        implements IWorkbenchPreferencePage {

    private StringFieldEditor localizationEditor;

    private StringFieldEditor alertEditor;

    private boolean prefsModified;

    /**
     * Constructor
     */
    public LocalizationPreferences() {
        super(GRID);
        setPreferenceStore(LocalizationManager.getInstance()
                .getLocalizationStore());
        setDescription("Specify localization settings (requires a CAVE restart)");
        prefsModified = false;
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
        createUserAndSiteEditor();
        createServerEditors();
    }

    /**
     * Create the user and site field editors
     */
    private void createUserAndSiteEditor() {
        StringFieldEditor userNameField = new StringFieldEditor(
                LocalizationConstants.P_LOCALIZATION_USER_NAME, "&Username: ",
                getFieldEditorParent());
        userNameField.setEnabled(false, getFieldEditorParent());
        this.addField(userNameField);

        String[] siteList;
        if (!LocalizationManager.getInstance().isOverrideSite()) {
            String[] config = PathManagerFactory.getPathManager()
                    .getContextList(LocalizationLevel.CONFIGURED);
            String[] site = PathManagerFactory.getPathManager().getContextList(
                    LocalizationLevel.SITE);
            Set<String> sites = new HashSet<String>();
            sites.addAll(Arrays.asList(config));
            sites.addAll(Arrays.asList(site));
            siteList = sites.toArray(new String[sites.size()]);
            Arrays.sort(siteList);
        } else {
            siteList = new String[] { LocalizationManager.getInstance()
                    .getCurrentSite() };
        }
        String[][] entryNamesAndValues = new String[siteList.length][2];
        for (int i = 0; i < siteList.length; i++) {
            entryNamesAndValues[i][0] = siteList[i];
            entryNamesAndValues[i][1] = siteList[i];
        }

        FieldEditor site;
        if (!LocalizationManager.getInstance().isOverrideSite()) {
            site = new ComboFieldEditor(
                    LocalizationConstants.P_LOCALIZATION_SITE_NAME, "&Site: ",
                    entryNamesAndValues, getFieldEditorParent());
        } else {
            Composite note = createNoteComposite(
                    getFont(),
                    getFieldEditorParent(),
                    "NOTE:",
                    "The site preference has been disabled because the \"-site\"\n"
                            + "command-line parameter was specified at runtime.");
            GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                    false);
            layoutData.horizontalSpan = 2;
            note.setLayoutData(layoutData);
            site = new ReadOnlyComboFieldEditor(
                    LocalizationConstants.P_LOCALIZATION_SITE_NAME, "&Site: ",
                    entryNamesAndValues, getFieldEditorParent());
        }
        this.addField(site);
    }

    /**
     * Create the localizations and alert server field editors
     */
    private void createServerEditors() {
        if (!LocalizationManager.getInstance().isOverrideServer()) {
            localizationEditor = new StringFieldEditor(
                    LocalizationConstants.P_LOCALIZATION_HTTP_SERVER,
                    "&Localization Server: ", getFieldEditorParent());
        } else {
            Composite note = createNoteComposite(
                    getFont(),
                    getFieldEditorParent(),
                    "NOTE:",
                    "The server preference has been disabled because the \"-server\"\n"
                            + "command-line parameter was specified at runtime.");
            GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                    false);
            layoutData.horizontalSpan = 2;
            note.setLayoutData(layoutData);
            localizationEditor = new ReadOnlyStringFieldEditor(
                    LocalizationConstants.P_LOCALIZATION_HTTP_SERVER,
                    "&Localization Server: ", LocalizationManager.getInstance()
                            .getLocalizationServer(), getFieldEditorParent());
        }
        localizationEditor
                .setErrorMessage("Unable to connect to localization server");
        this.addField(localizationEditor);

        if (LocalizationManager.internalAlertServer == false) {
            alertEditor = new StringFieldEditor(
                    LocalizationConstants.P_ALERT_SERVER, "&Alert Server: ",
                    getFieldEditorParent());
            alertEditor.setErrorMessage("Unable to connect to alert server");
            this.addField(alertEditor);
        }
        addConnectivityButton();
    }

    /**
     * Add button to check server connectivity
     */
    private void addConnectivityButton() {
        Button b = new Button(getFieldEditorParent(), SWT.PUSH);
        GridData gd = new GridData(SWT.RIGHT, SWT.TOP, false, true);
        gd.horizontalSpan = 2;
        b.setLayoutData(gd);
        b.setText("Check Connectivity");
        b.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                checkConnectivity();
            }

        });
    }

    /**
     * Check the connectivity of the server field editors
     */
    private void checkConnectivity() {
        final ConnectivityResult result = new ConnectivityResult(false, "");
        Text text = localizationEditor.getTextControl(getFieldEditorParent());
        ConnectivityManager.checkLocalizationServer(text.getText().trim(),
                new IConnectivityCallback() {
                    @Override
                    public void connectionChecked(ConnectivityResult results) {
                        result.hasConnectivity = results.hasConnectivity;
                    }
                });
        if (result.hasConnectivity) {
            text.setBackground(Display.getDefault().getSystemColor(
                    SWT.COLOR_WHITE));
        } else {
            text.setBackground(Display.getDefault().getSystemColor(
                    SWT.COLOR_RED));
            localizationEditor.showErrorMessage();
        }

        if (alertEditor != null) {
            text = alertEditor.getTextControl(getFieldEditorParent());
            ConnectivityManager.checkAlertService(text.getText().trim(),
                    new IConnectivityCallback() {
                        @Override
                        public void connectionChecked(ConnectivityResult results) {
                            result.hasConnectivity = results.hasConnectivity;
                        }
                    });
            if (result.hasConnectivity) {
                text.setBackground(Display.getDefault().getSystemColor(
                        SWT.COLOR_WHITE));
            } else {
                text.setBackground(Display.getDefault().getSystemColor(
                        SWT.COLOR_RED));
                alertEditor.showErrorMessage();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    @Override
    public void init(IWorkbench workbench) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditorPreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        if (prefsModified) {
            MessageBox warning = new MessageBox(getShell(), SWT.ICON_WARNING
                    | SWT.OK | SWT.CANCEL);
            warning.setText("CAVE Localization preferences changed");
            warning.setMessage("Localization preferences have changed "
                    + "and CAVE needs to be restarted to use the new "
                    + "settings. \n"
                    + "Click OK to save your changes. You MUST restart CAVE "
                    + "after closing this window or errors will occur in your "
                    + "current CAVE session. \n"
                    + "Click Cancel if you do not wish to save these new preferences.");

            int retVal = warning.open();
            if (retVal == SWT.CANCEL) {
                return false;
            }
        }

        return super.performOk();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.preference.FieldEditorPreferencePage#propertyChange
     * (org.eclipse.jface.util.PropertyChangeEvent)
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {
        if (!event.getNewValue().equals(event.getOldValue())) {
            prefsModified = true;
        }
        super.propertyChange(event);
    }
}
