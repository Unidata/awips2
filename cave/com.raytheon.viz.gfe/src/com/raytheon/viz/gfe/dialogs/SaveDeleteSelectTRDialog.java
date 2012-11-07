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
import java.util.List;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.gfe.time.SelectTimeRange;
import com.raytheon.uf.common.dataplugin.gfe.time.SelectTimeRange.Mode;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.SpinScale;

/**
 * Dialog for performing selected time range save or delete.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 7, 2009            randerso     Initial creation
 * Aug 1, 2012   #965     dgilling     Change location of SelectTimeRange.
 * Oct 25, 2012  #1287     rferrel     Code cleanup part of non-blocking dialogs.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SaveDeleteSelectTRDialog extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SaveDeleteSelectTRDialog.class);

    private DataManager dataManager;

    private String optionStr;

    private TimeZone timeZone;

    private List<String> ids = new ArrayList<String>();

    private List<String> protectedIds = new ArrayList<String>();

    private Composite top;

    private org.eclipse.swt.widgets.List timeRangeList;

    private Text identifierField;

    private Button localButton;

    private Button zuluButton;

    private SpinScale startScale;

    private SpinScale stopScale;

    private boolean first = true;

    public SaveDeleteSelectTRDialog(Shell parent, DataManager dataManager,
            String optionStr) {
        super(parent);
        this.dataManager = dataManager;
        this.optionStr = optionStr;

        // Need to get them listed in order by time range
        String[] names = dataManager.getSelectTimeRangeManager().inventory();

        try {
            timeZone = TimeZone.getTimeZone(dataManager.getClient()
                    .getDBGridLocation().getTimeZone());
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving GFE time zone", e);
            timeZone = TimeZone.getDefault();
        }

        if (optionStr.equals("Save")) {
            ids.addAll(Arrays.asList(names));
        } else {
            for (String name : names) {
                if (dataManager.getSelectTimeRangeManager().getRange(name)
                        .getLevel().equals(LocalizationLevel.USER)) {
                    ids.add(name);
                } else {
                    protectedIds.add(name);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);

        if (optionStr.equals("Save")) {
            newShell.setText("Save Select Time Range");
        } else {
            newShell.setText("Delete Selection Time Range");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);
        top.setLayout(new GridLayout(2, false));

        Composite leftComp = new Composite(top, SWT.NONE);
        GridLayout layout = new GridLayout(1, true);
        leftComp.setLayout(layout);
        leftComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        Label label = new Label(leftComp, SWT.NONE);
        label.setText(parent.getShell().getText());
        GridData layoutData = new GridData(SWT.CENTER, SWT.DEFAULT, true, true);
        label.setLayoutData(layoutData);

        timeRangeList = new org.eclipse.swt.widgets.List(leftComp, SWT.SINGLE
                | SWT.V_SCROLL);
        for (String id : ids) {
            timeRangeList.add(id);
        }
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        layoutData.minimumHeight = timeRangeList.getItemHeight();
        layoutData.heightHint = timeRangeList.getItemHeight() * 10;
        timeRangeList.setLayoutData(layoutData);
        timeRangeList.deselectAll();
        timeRangeList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {

                listBoxCB();
            }
        });

        if (optionStr.equals("Save")) {
            Label ident = new Label(leftComp, SWT.NONE);
            ident.setText("Identifier");

            identifierField = new Text(leftComp, SWT.BORDER);
            layoutData = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
            identifierField.setLayoutData(layoutData);
            identifierField.addModifyListener(new ModifyListener() {

                @Override
                public void modifyText(ModifyEvent e) {
                    getButton(IDialogConstants.OK_ID).setEnabled(
                            !identifierField.getText().trim().isEmpty());
                }

            });

            Composite rightComp = new Composite(top, SWT.NONE);
            GridLayout rightLayout = new GridLayout(1, false);
            rightComp.setLayout(rightLayout);
            rightComp
                    .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

            label = new Label(rightComp, SWT.NONE);
            label.setText("Start/Stop Times Relative to 0000 Today:");
            GridData data = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
            label.setLayoutData(data);

            Composite timeComp = new Composite(rightComp, SWT.NONE);
            data = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
            timeComp.setLayoutData(data);
            timeComp.setLayout(new GridLayout(2, false));

            localButton = new Button(timeComp, SWT.RADIO);
            SimpleDateFormat sdf = new SimpleDateFormat("zzz");
            sdf.setTimeZone(timeZone);
            String s = sdf.format(SimulatedTime.getSystemTime().getTime());
            s = String.format("Local Time (%s)", s);
            localButton.setText(s);
            localButton.setSelection(true);

            localButton.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER,
                    true, false));

            zuluButton = new Button(timeComp, SWT.RADIO);
            zuluButton.setText("Zulu");
            zuluButton.setSelection(false);

            zuluButton.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, true,
                    false));

            label = new Label(rightComp, SWT.NONE);
            label.setText("Start Hour");
            label.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

            startScale = new SpinScale(rightComp, SWT.HORIZONTAL);
            startScale.setValues(0, -24, 240, 0, 1, 1);
            data = new GridData(265, SWT.DEFAULT);
            startScale.setLayoutData(data);
            startScale.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    startCB();
                }
            });

            Label lab2 = new Label(rightComp, SWT.NONE);
            lab2.setText("Stop Hour");
            lab2.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

            stopScale = new SpinScale(rightComp, SWT.HORIZONTAL);
            stopScale.setValues(1, -24, 240, 0, 1, 1);
            data = new GridData(265, SWT.DEFAULT);
            stopScale.setLayoutData(data);
            stopScale.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    stopCB();
                }
            });
        }

        return top;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        String label = "Delete";
        if (optionStr.equals("Save")) {
            label = "Save";
        }
        createButton(parent, IDialogConstants.OK_ID, label, true).setEnabled(
                false);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void okPressed() {
        if (optionStr.equals("Save")) {
            save();
        } else {
            delete();
        }
    }

    // callback for list box (list box portion)
    private void listBoxCB() {
        // kludge to handle automatic selection of first item by SWT
        if (first) {
            first = false;
            timeRangeList.deselectAll();
            return;
        }

        getButton(IDialogConstants.OK_ID).setEnabled(true);
        String[] items = timeRangeList.getSelection();
        for (String id : items) {
            if (optionStr.equals("Save")) {
                SelectTimeRange range = dataManager.getSelectTimeRangeManager()
                        .getRange(id);
                identifierField.setText(id);
                stopScale.setSelection(range.getEnd());
                startScale.setSelection(range.getStart());
                localButton.setSelection(range.getMode().equals(Mode.LT));
                zuluButton.setSelection(!range.getMode().equals(Mode.LT));
            }
        }
    }

    // Callback for saving
    private void save() {
        if (optionStr.equals("Save")) {
            String id = identifierField.getText().trim();
            if (id.length() > 0 && !protectedIds.contains(id)) {
                Mode mode;
                if (localButton.getSelection()) {
                    mode = Mode.LT;
                } else {
                    mode = Mode.ZULU;
                }
                dataManager.getSelectTimeRangeManager().save(id,
                        startScale.getSelection(), stopScale.getSelection(),
                        mode);
                super.okPressed();
            } else {
                String message = "TimeRange " + id
                        + " is protected or an invalid " + "name";
                statusHandler.handle(Priority.SIGNIFICANT, message);
            }
        }
    }

    // Callback for deleting
    private void delete() {
        String[] id = timeRangeList.getSelection();
        if (id.length < 1) {
            // no selection
            return;
        }

        if (!protectedIds.contains(id[0]) && ids.contains(id[0])) {
            if (!MessageDialog.openConfirm(this.getShell(), "Confirm Delete",
                    "Delete Select Time Range: \"" + id[0] + "\"?")) {
                return;
            }

            dataManager.getSelectTimeRangeManager().remove(id[0]);
            super.okPressed();
        } else {
            String message = "TimeRange " + id[0]
                    + " is protected or an invalid name";
            statusHandler.handle(Priority.SIGNIFICANT, message);
        }
    }

    // Callback when changing the start slider
    private void startCB() {
        int startValue = startScale.getSelection();
        if (startValue >= stopScale.getSelection()) {
            stopScale.setSelection(startValue + 1);
        }
    }

    // Callback when changing the stop slider
    private void stopCB() {
        int stopValue = stopScale.getSelection();
        if (stopValue <= startScale.getSelection()) {
            startScale.setSelection(stopValue - 1);
        }
    }
}
