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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
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
import com.raytheon.viz.gfe.smartscript.FieldDefinition;
import com.raytheon.viz.gfe.smartscript.FieldDefinition.FieldType;
import com.raytheon.viz.gfe.tasks.TaskManager;
import com.raytheon.viz.gfe.ui.runtimeui.ValuesDialog;
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
 * Oct 27, 2012 1287       rferrel     Code cleanup for non-blocking dialog.
 * Oct 25, 2012 1287       rferrel     Code changes for non-blocking PublishDialog.
 * Nov 13, 2012 1298       rferrel     Code changes for non-blocking UserEntryDialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ProductScriptsDialog extends CaveJFACEDialog {
    private final String TIME_FORMAT_STRING = "yyyyMMdd_HHmm";

    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProductScriptsDialog.class);

    private final int RUN_ID = IDialogConstants.CLIENT_ID + 1;

    private String[] scripts;

    private Map<String, String> scriptDict;

    private String prddir;

    private ToggleSelectList scriptsList;

    private Button publishToOfficialButton;

    private DataManager dataManager;

    private DatabaseID productDB;

    private DatabaseID mutableDB;

    private PublishDialog publishDlg;

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
        if (publishDlg == null || publishDlg.getShell() == null
                || publishDlg.isDisposed()) {
            publishDlg = new PublishDialog(getParentShell(), dataManager);
            publishDlg.setBlockOnOpen(false);
            publishDlg.open();
        } else {
            publishDlg.bringToTop();
        }
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
        int[] idxs = scriptsList.getSelectionIndices();

        for (int idx : idxs) {
            try {
                Boolean run = true;

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
                cmd = cmd.replace("{time}", curLocalTime);
                cmd = cmd.replace("{ztime}", curGMTTime);
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

                // The user is prompted for a list of radio button values.
                // {entryButtons: <name of variable>: <list of values separated
                // by
                // commas>}
                int count = cmd.split("entryButtons").length - 1;
                if (count > 0) {
                    int entryIdx = 0, i = 0, start = 0;
                    List<FieldDefinition> fieldDefs = new ArrayList<FieldDefinition>();

                    while (entryIdx != -1) {
                        entryIdx = cmd.indexOf("{entryButtons:", entryIdx);
                        if (entryIdx >= 0) {
                            int endEntryIdx = cmd.indexOf("}", entryIdx);
                            String[] entry = cmd.substring(entryIdx + 1,
                                    endEntryIdx).split(":");
                            String[] fields = entry[2].split(",");

                            fieldDefs
                                    .add(new FieldDefinition(
                                            (Object) entry[1],
                                            entry[1],
                                            FieldType.RADIO,
                                            (Object) fields[0],
                                            Arrays.asList(Arrays
                                                    .asList(fields)
                                                    .toArray(
                                                            new Object[fields.length])),
                                            (float) 1.0, (int) 3));
                            if (i == 0) {
                                start = entryIdx;
                            }
                            entryIdx = endEntryIdx + 1;
                            i++;
                        }

                        if (entryIdx == -1) {
                            ValuesDialog buttonDlg = new ValuesDialog(name,
                                    fieldDefs, dataManager);
                            if (buttonDlg.open() > 0) {
                                run = false;
                                continue;
                            }

                            Map<Object, Object> map = buttonDlg.getValues();
                            String returnMsg = "";

                            for (Map.Entry<Object, Object> entry : map
                                    .entrySet()) {
                                returnMsg = returnMsg
                                        + entry.getValue().toString() + " ";
                            }

                            start = start - 3;
                            cmd = cmd.substring(0, start) + returnMsg;
                        }
                    }
                }

                // The user is prompted for a list of check box values.
                // {entryChecks: <name of variable>: <list of values separated
                // by
                // commas>}
                count = cmd.split("entryChecks").length - 1;
                if (count > 0) {
                    int entryIdx = 0, i = 0, start = 0;
                    List<FieldDefinition> fieldDefs = new ArrayList<FieldDefinition>();

                    while (entryIdx != -1) {
                        entryIdx = cmd.indexOf("{entryChecks:", entryIdx);
                        if (entryIdx >= 0) {
                            int endEntryIdx = cmd.indexOf("}", entryIdx);
                            String[] entry = cmd.substring(entryIdx + 1,
                                    endEntryIdx).split(":");
                            String[] fields = entry[2].split(",");

                            fieldDefs
                                    .add(new FieldDefinition(
                                            (Object) entry[1],
                                            entry[1],
                                            FieldType.CHECK,
                                            (Object) null,
                                            Arrays.asList(Arrays
                                                    .asList(fields)
                                                    .toArray(
                                                            new Object[fields.length])),
                                            (float) 1.0, (int) 3));
                            if (i == 0) {
                                start = entryIdx;
                            }
                            entryIdx = endEntryIdx + 1;
                            i++;
                        }

                        if (entryIdx == -1) {
                            ValuesDialog buttonDlg = new ValuesDialog(name,
                                    fieldDefs, dataManager);
                            if (buttonDlg.open() > 0) {
                                run = false;
                                continue;
                            }

                            Map<Object, Object> map = buttonDlg.getValues();
                            String returnMsg = "";

                            for (Map.Entry<Object, Object> entry : map
                                    .entrySet()) {
                                returnMsg = returnMsg
                                        + entry.getValue().toString() + " ";
                            }

                            start = start - 3;
                            cmd = cmd.substring(0, start) + returnMsg;
                        }
                    }
                }

                // The user is prompted for a named variable, same as the
                // user-supplied variables above, but for non-standard
                // variables.
                int entryIdx = cmd.indexOf("{entry:");
                if (entryIdx >= 0) {
                    int endEntryIdx = cmd.indexOf("}", entryIdx);
                    String[] entry = cmd.substring(entryIdx + 1, endEntryIdx)
                            .split(":");
                    String configFile = entry[2];
                    // The dialog being opened is modal to the parent dialog.
                    // This will prevent the launching of another dialog until
                    // the modal dialog is closed.

                    // Keep this a blocking dialog so the loop will only display
                    // one dialog at a time.
                    UserEntryDialog entryDlg = new UserEntryDialog(
                            this.getShell(), entry[1] + " Entry", entry[1]
                                    + ":", entry[2]);
                    Object returnMsg = entryDlg.open();
                    if (returnMsg == null) {
                        // cancel pressed
                        run = false;
                        continue;
                    }
                    configFile = returnMsg.toString();

                    cmd = cmd.substring(0, entryIdx) + configFile
                            + cmd.substring(endEntryIdx + 1);
                }

                if (run) {
                    TaskManager.getInstance().createScriptTask(name, cmd);
                }

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
        scriptsList = new ToggleSelectList(top, SWT.MULTI | SWT.V_SCROLL);
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
