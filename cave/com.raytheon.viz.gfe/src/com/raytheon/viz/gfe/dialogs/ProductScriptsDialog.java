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
package com.raytheon.viz.gfe.dialogs;

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.tasks.TaskManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * The product generation scripts dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Mar 7, 2008			   Eric Babin   Initial Creation
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ProductScriptsDialog extends CaveJFACEDialog {
    private static final String TIME_FORMAT_STRING = "yyyyMMdd_HHmm";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProductScriptsDialog.class);

    public final static int RUN_ID = IDialogConstants.CLIENT_ID + 1;

    private String[] scripts;

    private Map<String, String> scriptDict;

    private String prddir;

    private ToggleSelectList scriptsList;

    private Button publishToOfficialButton;

    private DataManager dataManager;

    private DatabaseID productDB;

    private DatabaseID mutableDB;

    public ProductScriptsDialog(Shell parent, DataManager dataManager) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);
        this.dataManager = dataManager;

        prddir = Activator.getDefault().getPreferenceStore()
                .getString("GFESUITE_PRDDIR");
        scripts = Activator.getDefault().getPreferenceStore()
                .getStringArray("Scripts");
        productDB = this.dataManager.getParmManager().getProductDB();
        mutableDB = this.dataManager.getParmManager().getMutableDatabase();
        scriptDict = new HashMap<String, String>();
        for (int i = 0; i < scripts.length; i++) {
            int splitIdx = scripts[i].indexOf(':');
            String name = scripts[i].substring(0, splitIdx).trim();
            String script = scripts[i].substring(splitIdx + 1).trim();
            scriptDict.put(name, script);
        }
    }

    private void launchPublishToOfficial() {
        PublishDialog dialog = new PublishDialog(getParentShell(), dataManager);
        dialog.setBlockOnOpen(true);
        dialog.open();
    }

    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId != IDialogConstants.CANCEL_ID) {
            runScripts();
        }
        super.buttonPressed(buttonId);
    }

    /**
     * 
     */
    private void runScripts() {
        // TODO implement this
        int[] idxs = scriptsList.getSelectionIndices();

        for (int idx : idxs) {
            try {
                String name = scriptsList.getItem(idx);
                String cmd = scriptDict.get(name);

                String requestEndpoint = VizApp.getHttpServer().replace(
                        "http://", "");
                String[] uriParts = requestEndpoint.split("/", 2);
                String[] hostParts = uriParts[0].split(":", 2);

                Date curTime = SimulatedTime.getSystemTime().getTime();
                SimpleDateFormat gmtTime = new SimpleDateFormat(
                        TIME_FORMAT_STRING);
                gmtTime.setTimeZone(TimeZone.getTimeZone("GMT"));
                SimpleDateFormat localTime = new SimpleDateFormat(
                        TIME_FORMAT_STRING);
                localTime.setTimeZone(TimeZone.getTimeZone(dataManager
                        .getClient().getSiteTimeZone()));
                String curLocalTime = localTime.format(curTime);
                String curGMTTime = gmtTime.format(curTime);

                Date seStart = dataManager.getSpatialDisplayManager()
                        .getSpatialEditorTime();
                Date seEnd = new Date(seStart.getTime() + 60 * 1000);

                // The following variables are replaced by known values:
                cmd = cmd.replace("{host}", hostParts[0]);
                cmd = cmd.replace("{port}", hostParts[1]);
                cmd = cmd.replace("{site}", dataManager.getSiteID());
                cmd = cmd.replace("{productDB}", productDB.toString());
                cmd = cmd.replace("{SEstart}", gmtTime.format(seStart));
                cmd = cmd.replace("{SEend}", gmtTime.format(seEnd));
                // cmd = cmd.replace("{SelectedStart}",
                // gmtTime.format(selStart));
                // cmd = cmd.replace("{SelectedEnd}", gmtTime.format(selEnd));
                cmd = cmd.replace("{time}", curLocalTime);
                cmd = cmd.replace("{ztime}", curGMTTime);
                // cmd = cmd.replace("{module:<module name>}", replaceWith);
                // cmd = cmd.replace("{home}", siteConfig.GFESUITE_HOME);
                cmd = cmd.replace("{prddir}", prddir);

                // The user is prompted to enter the value with which to replace
                // the
                // following variables:
                // {parmsMutable} (Those listed in Forecast database)
                // {refsets}
                // {maps}
                // {databases}
                // {output file}
                // {output directory}
                // {startTime}
                // {endTime}

                // The user is prompted for a list of radiobutton values.
                // {entryButtons: <name of variable>: <list of values separated
                // by
                // commas>}
                // int entryIdx = cmd.indexOf("{entryButtons:");
                // if (entryIdx >= 0) {
                // int endEntryIdx = cmd.indexOf("}", entryIdx);
                // }

                // The user is prompted for a list of check box values.
                // {entryChecks: <name of variable>: <list of values separated
                // by
                // commas>}
                // int entryIdx = cmd.indexOf("{entryChecks:");
                // if (entryIdx >= 0) {
                // int endEntryIdx = cmd.indexOf("}", entryIdx);
                // }

                // The user is prompted for a named variable, same as the
                // user-supplied variables above, but for non-standard
                // variables.
                int entryIdx = cmd.indexOf("{entry:");
                if (entryIdx >= 0) {
                    int endEntryIdx = cmd.indexOf("}", entryIdx);
                    String[] entry = cmd.substring(entryIdx + 1, endEntryIdx)
                            .split(":");
                    String configFile = entry[2];
                    UserEntryDialog entryDlg = new UserEntryDialog(
                            this.getShell(), entry[1] + " Entry", entry[1]
                                    + ":", entry[2]);
                    String returnMsg = entryDlg.open();
                    if (returnMsg == null) {
                        // cancel pressed
                        continue;
                    }
                    configFile = returnMsg;

                    cmd = cmd.substring(0, entryIdx) + configFile
                            + cmd.substring(endEntryIdx + 1);
                }

                TaskManager.getInstance().createScriptTask(name, cmd);

            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout();
        top.setLayout(layout);

        GridData data;
        if (!productDB.equals(mutableDB)) {
            data = new GridData(GridData.FILL_HORIZONTAL);
            data.horizontalAlignment = SWT.CENTER;
            Label lab = new Label(top, SWT.NONE);
            lab.setText("Remember to publish before generating products.");
            lab.setLayoutData(data);
        }

        publishToOfficialButton = new Button(top, SWT.PUSH);
        publishToOfficialButton.setText("Publish to Official...");
        publishToOfficialButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                launchPublishToOfficial();
            }
        });

        data = new GridData(GridData.FILL_HORIZONTAL);
        data.horizontalAlignment = SWT.CENTER;
        publishToOfficialButton.setLayoutData(data);
        Label scriptsLab = new Label(top, SWT.NONE);
        scriptsLab.setText("Scripts");
        scriptsLab.setLayoutData(data);

        data = new GridData(GridData.FILL_HORIZONTAL);
        data.horizontalAlignment = SWT.CENTER;
        scriptsLab.setLayoutData(data);
        scriptsList = new ToggleSelectList(top, SWT.BORDER | SWT.MULTI);
        String[] scriptLabels = scriptDict.keySet().toArray(
                new String[scriptDict.size()]);
        Arrays.sort(scriptLabels);
        scriptsList.setItems(scriptLabels);

        data = new GridData(400, 160);
        data.horizontalAlignment = SWT.CENTER;
        scriptsList.setLayoutData(data);

        Label space = new Label(top, SWT.NONE);
        space.setText("");

        return top;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);

        shell.setText("Product Generation Scripts");
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        super.createButton(parent, RUN_ID, "Run", false);
        super.createButton(parent, IDialogConstants.OK_ID, "Run/Dismiss", false);
        super.createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

}
