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
package com.raytheon.viz.aviation.utility;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.tafqueue.ServerResponse;
import com.raytheon.uf.common.tafqueue.TafQueueRecord.TafQueueState;
import com.raytheon.uf.common.tafqueue.TafQueueRequest;
import com.raytheon.uf.common.tafqueue.TafQueueRequest.Type;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.IStatusSettable;
import com.raytheon.viz.avnconfig.MessageStatusComp;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * TransmissionQueueDlg class displays the Transmission Queue dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation
 * 14 MAY 2012  14715      rferrel     Use EDEX to perform requests.
 * 10 OCT 2012  1229       rferrel     Make dialog non-blocking.
 * 10 OCT 2012  1229       rferrel     Changes for non-blocking HelpUsageDlg.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TransmissionQueueDlg extends CaveSWTDialog {

    /**
     * Pending radio button.
     */
    private Button pendingRdo;

    /**
     * Sent radio button.
     */
    private Button sentRdo;

    /**
     * Bad radio button.
     */
    private Button badRdo;

    /**
     * Button to retransmit selected sent or bad records.
     */
    private Button retransmitBtn;

    /**
     * Transmission list control.
     */
    private ToggleSelectList transList;

    private java.util.List<String> transListId;

    /**
     * Transmission day control.
     */
    private StyledText transStText;

    /**
     * Day label.
     */
    private Label dayLbl;

    /**
     * Day of the week enumeration.
     * 
     * @author lvenable
     */
    private String[] dayOfWeek = new String[] { "Sunday", "Monday", "Tuesday",
            "Wednesday", "Thursday", "Friday", "Saturday" };

    /**
     * The Selected day of the week Calendar day of the week value.
     */
    private int selectedDay;

    /**
     * Main composite.
     */
    private Composite mainComp;

    /**
     * Message status composite.
     */
    private IStatusSettable msgStatComp;

    private HelpUsageDlg usageDlg;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public TransmissionQueueDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT
                        | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Transmission Queue");
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout gl = new GridLayout(1, true);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        return gl;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        GridLayout gl = new GridLayout(1, true);
        mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainComp.setLayoutData(gd);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        selectedDay = Calendar.getInstance().get(Calendar.DAY_OF_WEEK);

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        configMgr.setDefaultColors(mainComp);

        createTopButtons(configMgr);

        createFileTransmissionControls(configMgr);

        createDayOfWeekControls(configMgr);

        createDayTransListControl(configMgr);

        createMessageControl(configMgr);

        populateData();
        updateDayTransList();
    }

    /**
     * Create the buttons at the top of the display.
     */
    private void createTopButtons(ResourceConfigMgr configMgr) {
        GridLayout gl = new GridLayout(6, true);
        Composite buttonComp = new Composite(mainComp, SWT.BORDER);
        buttonComp.setLayout(gl);
        configMgr.setDefaultColors(buttonComp);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setLayoutData(gd);
        closeBtn.setText("Close");
        configMgr.setDefaultFontAndColors(closeBtn);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(true);
                shell.dispose();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button refreshBtn = new Button(buttonComp, SWT.PUSH);
        refreshBtn.setLayoutData(gd);
        refreshBtn.setText("Refresh");
        configMgr.setDefaultFontAndColors(refreshBtn);
        refreshBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                populateData();
                updateDayTransList();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button viewBtn = new Button(buttonComp, SWT.PUSH);
        viewBtn.setLayoutData(gd);
        viewBtn.setText("View");
        configMgr.setDefaultFontAndColors(viewBtn);
        viewBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                viewTafs();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button removeBtn = new Button(buttonComp, SWT.PUSH);
        removeBtn.setLayoutData(gd);
        removeBtn.setText("Remove");
        removeBtn.setToolTipText("Delete selected forecast");
        configMgr.setDefaultFontAndColors(removeBtn);
        removeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                MessageBox questionMB = new MessageBox(shell, SWT.ICON_WARNING
                        | SWT.YES | SWT.NO);
                questionMB.setText("Remove Pending Transmission");
                questionMB
                        .setMessage("Are you sure you want to remove this pending transmission from the queue?");
                int result = questionMB.open();

                if (result == SWT.YES) {
                    removeSelected();
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        retransmitBtn = new Button(buttonComp, SWT.PUSH);
        retransmitBtn.setLayoutData(gd);
        retransmitBtn.setText("Retransmit");
        configMgr.setDefaultFontAndColors(retransmitBtn);
        retransmitBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                retransmit();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button helpBtn = new Button(buttonComp, SWT.PUSH);
        helpBtn.setLayoutData(gd);
        helpBtn.setText("Help");
        configMgr.setDefaultFontAndColors(helpBtn);
        helpBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (mustCreate(usageDlg)) {
                    String description = "Help";

                    String helpText = "This dialog is used to manage transmission and transmission log files.\n\n"
                            + "The top area manages the forecast files written by the forecast editor.\n"
                            + "The 'Files' scrolled list window lists files in one of 'pending', 'sent'\n"
                            + "and 'bad' directories. The time is when the file was written. The file\n"
                            + "name is \n"
                            + "    xxx-CCCCNNNXXX-yymmddHHMM-BBB\n"
                            + "where xxx is the forecaster number. The transmission program \n"
                            + "avnxmitserv uses NNN to determine the transmission window for regular\n"
                            + "forecasts.\n\n"
                            + "The bottom area is used to view transmission log files.  There is one\n"
                            + "file for each day of the week. By default, log files for the current day\n"
                            + "are shown.\n\nButtons:\n"
                            + "   Refresh:    refreshes both the directory list and log file windows.\n"
                            + "   View:       allows to view selected transmission file(s)\n"
                            + "   Remove:     deletes transmission files\n"
                            + "   Retransmit: forces the transmission program to send selected files.\n"
                            + "               If the file is in the 'bad' or 'sent' directory, it is \n"
                            + "               moved back to 'pending'. The transmission time (the last\n"
                            + "               part of the file name) is updated to the current time.\n"
                            + "   Help:       displays this window";
                    usageDlg = new HelpUsageDlg(shell, description,
                            helpText);
                    usageDlg.open();
                } else {
                    usageDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Action method to request the selected records be immediately transmitted.
     */
    @SuppressWarnings("unchecked")
    private void retransmit() {
        int[] indices = transList.getSelectionIndices();
        if (indices.length == 0) {
            return;
        }
        java.util.List<String> idList = new ArrayList<String>(indices.length);
        for (int index : indices) {
            idList.add(transListId.get(index));
        }
        TafQueueRequest request = new TafQueueRequest();
        request.setType(Type.RETRANSMIT);
        request.setState(getDisplayState());
        request.setArgument(idList);

        try {
            ServerResponse<java.util.List<String>> response = (ServerResponse<java.util.List<String>>) ThriftClient
                    .sendRequest(request);
            int color = SWT.COLOR_GREEN;
            if (response.isError()) {
                color = SWT.COLOR_RED;
            }
            msgStatComp.setMessageText(response.getMessages().get(0),
                    getParent().getDisplay().getSystemColor(color).getRGB());
            populateTransList(response.getPayload());
        } catch (VizException e) {
            msgStatComp.setMessageText(e.getMessage(), getParent().getDisplay()
                    .getSystemColor(SWT.COLOR_RED).getRGB());
        }

    }

    /**
     * Determine the state of the records being viewed.
     * 
     * @return state
     */
    private TafQueueState getDisplayState() {
        TafQueueState state = TafQueueState.PENDING;
        if (sentRdo.getSelection()) {
            state = TafQueueState.SENT;
        } else if (badRdo.getSelection()) {
            state = TafQueueState.BAD;
        }
        return state;
    }

    /**
     * This brings up the TAF viewer and populates it with the selected records.
     */
    @SuppressWarnings("unchecked")
    private void viewTafs() {
        int[] indices = transList.getSelectionIndices();
        if (indices.length == 0) {
            return;
        }

        java.util.List<String> idList = new ArrayList<String>(indices.length);
        for (int index : indices) {
            idList.add(transListId.get(index));
        }

        TafQueueRequest request = new TafQueueRequest();
        request.setType(Type.GET_TAFS);
        request.setArgument(idList);
        ServerResponse<String> response = null;
        try {
            response = (ServerResponse<String>) ThriftClient
                    .sendRequest(request);
            String tafText = response.getPayload();

            String tafInfo = null;
            if (indices.length == 1) {
                tafInfo = transList.getItem(indices[0]);
            } else {
                tafInfo = "Viewing multiple forecasts";
            }

            // Allow multiple instances of this dialog.
            TransmissionViewerDlg tvd = new TransmissionViewerDlg(shell,
                    tafText, tafInfo);
            tvd.open();
        } catch (VizException e) {
            msgStatComp.setMessageText(e.getMessage(), getParent().getDisplay()
                    .getSystemColor(SWT.COLOR_RED).getRGB());
        }

    }

    /**
     * Action to perform to remove the selected records from being displayed.
     * This updates the database and repopulates the list.
     */
    @SuppressWarnings("unchecked")
    private void removeSelected() {
        int[] indices = transList.getSelectionIndices();
        if (indices.length == 0) {
            return;
        }
        java.util.List<String> idList = new ArrayList<String>(indices.length);
        for (int index : indices) {
            idList.add(transListId.get(index));
        }
        TafQueueRequest request = new TafQueueRequest();
        request.setType(Type.REMOVE_SELECTED);
        request.setState(getDisplayState());
        request.setArgument(idList);

        try {
            ServerResponse<java.util.List<String>> response = (ServerResponse<java.util.List<String>>) ThriftClient
                    .sendRequest(request);
            populateTransList(response.getPayload());
            int color = SWT.COLOR_GREEN;
            if (response.isError()) {
                color = SWT.COLOR_RED;
            }
            msgStatComp.setMessageText(response.getMessages().get(0),
                    getParent().getDisplay().getSystemColor(color).getRGB());
        } catch (VizException e) {
            msgStatComp.setMessageText(e.getMessage(), getParent().getDisplay()
                    .getSystemColor(SWT.COLOR_RED).getRGB());
        }
    }

    /**
     * Create the file transmission controls.
     */
    private void createFileTransmissionControls(ResourceConfigMgr configMgr) {
        GridLayout gl = new GridLayout(2, false);
        Composite mainTransComp = new Composite(mainComp, SWT.NONE);
        mainTransComp.setLayout(gl);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        mainTransComp.setLayoutData(gd);

        configMgr.setDefaultColors(mainTransComp);

        // ---------------------------------
        // Create left side radio buttons
        // ---------------------------------

        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Group directoryGroup = new Group(mainTransComp, SWT.NONE);
        directoryGroup.setText("Directory");
        gl = new GridLayout(1, false);
        directoryGroup.setLayout(gl);
        directoryGroup.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(directoryGroup);

        SelectionAdapter adapter = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Button button = (Button) event.getSource();
                if (button.getSelection()) {
                    retransmitBtn.setEnabled((Boolean) button.getData());
                    populateData();
                }
            }
        };

        gd = new GridData(85, SWT.DEFAULT);
        pendingRdo = new Button(directoryGroup, SWT.RADIO);
        configMgr.setDefaultFontAndColors(pendingRdo, "pending", gd);
        pendingRdo.addSelectionListener(adapter);
        pendingRdo.setData(false);

        gd = new GridData(85, SWT.DEFAULT);
        sentRdo = new Button(directoryGroup, SWT.RADIO);
        sentRdo.setSelection(true);
        retransmitBtn.setEnabled(true);
        configMgr.setDefaultFontAndColors(sentRdo, "sent", gd);
        sentRdo.addSelectionListener(adapter);
        sentRdo.setData(true);

        gd = new GridData(85, SWT.DEFAULT);
        badRdo = new Button(directoryGroup, SWT.RADIO);
        configMgr.setDefaultFontAndColors(badRdo, "bad", gd);
        badRdo.addSelectionListener(adapter);
        badRdo.setData(true);

        // -------------------------------------------------
        // Create right side label and transmission list
        // -------------------------------------------------

        gl = new GridLayout(1, false);
        Composite transListComp = new Composite(mainTransComp, SWT.NONE);
        transListComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        transListComp.setLayoutData(gd);
        configMgr.setDefaultColors(transListComp);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Label filesLbl = new Label(transListComp, SWT.NONE);
        filesLbl.setText("Files");
        filesLbl.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(filesLbl);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 600;
        gd.heightHint = 150;
        transList = new ToggleSelectList(transListComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        transList.setLayoutData(gd);
        configMgr.setListBoxFont(transList);
        transListId = new ArrayList<String>();
    }

    /**
     * Create the Day of Week radio buttons.
     */
    private void createDayOfWeekControls(ResourceConfigMgr configMgr) {
        GridLayout gl = new GridLayout(7, false);
        Composite dayOfWeekComp = new Composite(mainComp, SWT.BORDER);
        dayOfWeekComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dayOfWeekComp.setLayoutData(gd);
        configMgr.setDefaultColors(dayOfWeekComp);

        SelectionAdapter adapter = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Button dayRdo = (Button) event.getSource();
                if (dayRdo.getSelection()) {
                    selectedDay = (Integer) dayRdo.getData();
                    updateDayTransList();
                }
            }
        };

        for (int index = 0; index < dayOfWeek.length; ++index) {
            String day = dayOfWeek[index];
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            Button dayRdo = new Button(dayOfWeekComp, SWT.RADIO);
            dayRdo.setText(day);
            // The Calendar.SUNDAY, MONDAY, etc.
            dayRdo.setData(index + 1);
            dayRdo.setLayoutData(gd);
            configMgr.setDefaultFontAndColors(dayRdo);
            dayRdo.addSelectionListener(adapter);

            if (day.compareTo(dayOfWeek[selectedDay - 1]) == 0) {
                dayRdo.setSelection(true);
            }
        }
    }

    /**
     * Create the transmission day list control.
     */
    private void createDayTransListControl(ResourceConfigMgr configMgr) {
        GridLayout gl = new GridLayout(1, false);
        Composite transAttemptComp = new Composite(mainComp, SWT.NONE);
        transAttemptComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        transAttemptComp.setLayoutData(gd);
        configMgr.setDefaultColors(transAttemptComp);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dayLbl = new Label(transAttemptComp, SWT.CENTER);
        dayLbl.setText(dayOfWeek[selectedDay - 1]);
        dayLbl.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(dayLbl);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 750;
        gd.heightHint = 150;

        transStText = new StyledText(transAttemptComp, SWT.BORDER | SWT.MULTI
                | SWT.V_SCROLL | SWT.H_SCROLL);
        transStText.setEditable(false);
        transStText.setLayoutData(gd);
        configMgr.setTextEditorFontAndColors(transStText);
    }

    /**
     * Create the message status composite.
     */
    private void createMessageControl(ResourceConfigMgr configMgr) {
        msgStatComp = new MessageStatusComp(mainComp,
                configMgr.getDefaultBackgroundRGB(),
                configMgr.getMsgBarBackground());
    }

    /**
     * Update the transmission day list.
     */
    @SuppressWarnings("unchecked")
    private void updateDayTransList() {
        TafQueueRequest request = new TafQueueRequest();
        request.setType(Type.GET_LOG);
        List<Date> dateList = new ArrayList<Date>(2);

        dayLbl.setText(dayOfWeek[selectedDay - 1]);
        // Adjust currentDay to start of the day
        Calendar currentDay = Calendar.getInstance();
        currentDay.set(Calendar.HOUR_OF_DAY, 0);
        currentDay.set(Calendar.MINUTE, 0);
        currentDay.set(Calendar.SECOND, 0);
        currentDay.set(Calendar.MILLISECOND, 0);

        // Adjust selected day to current or previous week.
        Calendar selectedDayStart = Calendar.getInstance();
        selectedDayStart.setTime(currentDay.getTime());
        selectedDayStart.set(Calendar.DAY_OF_WEEK, selectedDay);
        if (currentDay.compareTo(selectedDayStart) < 0) {
            selectedDayStart.add(Calendar.DAY_OF_MONTH, -7);
        }
        dateList.add(selectedDayStart.getTime());

        // Determine start of next day.
        Calendar selectedDayEnd = Calendar.getInstance();
        selectedDayEnd.setTime(selectedDayStart.getTime());
        selectedDayEnd.add(Calendar.DAY_OF_MONTH, 1);
        dateList.add(selectedDayEnd.getTime());
        request.setArgument(dateList);

        try {
            ServerResponse<String> response = (ServerResponse<String>) ThriftClient
                    .sendRequest(request);
            String text = response.getPayload();
            transStText.setText(text);
        } catch (VizException e) {
            msgStatComp.setMessageText(e.getMessage(), getParent().getDisplay()
                    .getSystemColor(SWT.COLOR_RED).getRGB());
        }
    }

    /**
     * Populate the top text area with a list of TAF messages that have been
     * successfully sent, are pending transmission, or have failed.
     */
    @SuppressWarnings("unchecked")
    private void populateData() {
        transList.removeAll();
        transListId.clear();

        try {
            TafQueueRequest request = new TafQueueRequest();
            request.setType(Type.GET_LIST);

            request.setState(getDisplayState());

            ServerResponse<java.util.List<String>> response = (ServerResponse<java.util.List<String>>) ThriftClient
                    .sendRequest(request);
            if (response.isError()) {
                msgStatComp.setMessageText(response.getMessages().get(0),
                        getParent().getDisplay().getSystemColor(SWT.COLOR_RED)
                                .getRGB());
            }
            populateTransList(response.getPayload());
        } catch (VizException e) {
            msgStatComp.setMessageText(e.getMessage(), getParent().getDisplay()
                    .getSystemColor(SWT.COLOR_RED).getRGB());
        }
    }

    /**
     * This takes the payload list of strings and splits them up into the
     * display list and a hidden list of record ids. Assumes each entry is of
     * the format: record_id,record_info.
     * 
     * @param payload
     */
    private void populateTransList(java.util.List<String> payload) {
        transList.removeAll();
        transListId.clear();
        for (String record : payload) {
            String[] bits = record.split(",");
            transListId.add(bits[0]);
            transList.add(bits[1]);
        }
    }
}
