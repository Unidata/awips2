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
import java.util.Collections;
import java.util.HashMap;

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
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.avnconfig.HelpUsageDlg;
import com.raytheon.viz.avnconfig.MessageStatusComp;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TransmissionQueueDlg class displays the Transmission Queue dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation
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
     * Transmission list control.
     */
    private List transList;

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
     * Selected day of the week.
     */
    private String selectedDay;

    /**
     * Main composite.
     */
    private Composite mainComp;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public TransmissionQueueDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT);
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

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        int dayInt = Calendar.getInstance().get(Calendar.DAY_OF_WEEK);
        selectedDay = dayOfWeek[dayInt - 1];

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        configMgr.setDefaultColors(mainComp);

        createTopButtons(configMgr);

        createFileTransmissionControls(configMgr);

        createDayOfWeekControls(configMgr);

        createDayTransListControl(configMgr);

        createMessageControl(configMgr);

        populateData();
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
                int idx = transList.getSelectionIndex();

                if (idx >= 0) {
                    String tafInfo = transList.getItem(idx);
                    TransmissionQueue queue = TransmissionQueue.getInstance();
                    String tafText = null;

                    if (pendingRdo.getSelection()) {
                        tafText = queue.getPendingText(tafInfo);
                    } else if (sentRdo.getSelection()) {
                        tafText = queue.getSentText(tafInfo);
                    } else if (badRdo.getSelection()) {
                        tafText = queue.getErrorText(tafInfo);
                    }

                    TransmissionViewerDlg tvd = new TransmissionViewerDlg(
                            shell, tafText, tafInfo);
                    tvd.open();
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button removeBtn = new Button(buttonComp, SWT.PUSH);
        removeBtn.setLayoutData(gd);
        removeBtn.setText("Remove");
        configMgr.setDefaultFontAndColors(removeBtn);
        removeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                TransmissionQueue queue = TransmissionQueue.getInstance();

                if (pendingRdo.getSelection()) {
                    MessageBox questionMB = new MessageBox(shell,
                            SWT.ICON_WARNING | SWT.YES | SWT.NO);
                    questionMB.setText("Remove Pending Transmission");
                    questionMB
                            .setMessage("Are you sure you want to remove this pending transmission from the queue?");
                    int result = questionMB.open();

                    if (result == SWT.YES) {
                        queue.remove(transList.getItem(transList
                                .getSelectionIndex()));
                        populateData();
                    }
                }
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Button retransmitBtn = new Button(buttonComp, SWT.PUSH);
        retransmitBtn.setLayoutData(gd);
        retransmitBtn.setText("Retransmit");
        configMgr.setDefaultFontAndColors(retransmitBtn);
        retransmitBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                TransmissionQueue queue = TransmissionQueue.getInstance();
                queue.retransmit(
                        transList.getItem(transList.getSelectionIndex()),
                        badRdo.getSelection());
                populateData();
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
                String text = "This dialog is used to manage transmission and to transmission log files.\n\nThe top area is to manage forecast files written by the forecast editor.\nThe 'Files' scrolled list window lists files in one of 'pending', 'sent'\nand 'bad' directories. The time is when the file was written. The file\nname is \n    xxx-CCCCNNNXXX-yymmddHHMM-BBB\nwhere xxx is the forecaster number. The transmission program \navnxmitserv uses NNN to determine the transmission window for regular \nforecasts.\n\nThe bottom area is used to view transmission log files.  There is one \nfile for each day of the week. By default, log files for the current day \nare shown.\n\nButtons:\n   Refresh:    refreshes both the directory list and log file windows.\n   View:       allows to view selected transmission file(s)\n   Remove:     deletes transmission files\n   Retransmit: forces the transmission program to send selected files.\n               If the file is in the 'bad' or 'sent' directory, it is \n               moved back to 'pending'. The transmission time (the last\n           part of the file name) is updated to the current time.\n   Help:       displays this window";
                String description = "Help";
                HelpUsageDlg usageDlg = new HelpUsageDlg(shell, description,
                        text);
                usageDlg.open();
            }
        });
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

        gd = new GridData(85, SWT.DEFAULT);
        pendingRdo = new Button(directoryGroup, SWT.RADIO);
        configMgr.setDefaultFontAndColors(pendingRdo, "pending", gd);
        pendingRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                populateData();
            }
        });

        gd = new GridData(85, SWT.DEFAULT);
        sentRdo = new Button(directoryGroup, SWT.RADIO);
        sentRdo.setSelection(true);
        configMgr.setDefaultFontAndColors(sentRdo, "sent", gd);
        sentRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                populateData();
            }
        });

        gd = new GridData(85, SWT.DEFAULT);
        badRdo = new Button(directoryGroup, SWT.RADIO);
        configMgr.setDefaultFontAndColors(badRdo, "bad", gd);
        badRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                populateData();
            }
        });

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
        transList = new List(transListComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL | SWT.H_SCROLL);
        transList.setLayoutData(gd);
        configMgr.setListBoxFont(transList);
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

        for (String day : dayOfWeek) {
            gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            final Button dayRdo = new Button(dayOfWeekComp, SWT.RADIO);
            dayRdo.setText(day);
            dayRdo.setData(day);
            dayRdo.setLayoutData(gd);
            configMgr.setDefaultFontAndColors(dayRdo);
            dayRdo.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    selectedDay = (String) dayRdo.getData();
                    updateDayTransList();
                }
            });

            if (day.compareTo(selectedDay) == 0) {
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
        dayLbl.setText(selectedDay);
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
        updateDayTransList();
    }

    /**
     * Create the message status composite.
     */
    private void createMessageControl(ResourceConfigMgr configMgr) {
        new MessageStatusComp(mainComp, configMgr.getDefaultBackgroundRGB(),
                configMgr.getMsgBarBackground());
    }

    /**
     * Update the transmission day list.
     */
    private void updateDayTransList() {
        TransmissionQueue queue = TransmissionQueue.getInstance();
        ArrayList<String> display = null;
        Calendar c = Calendar.getInstance();
        HashMap<String, String> calendar = new HashMap<String, String>();
        String year = Integer.toString(c.get(Calendar.YEAR));
        String month = Integer.toString(c.get(Calendar.MONTH) + 1);
        String day = Integer.toString(c.get(Calendar.DATE));
        String date = year + month + day;
        calendar.put(dayOfWeek[c.get(Calendar.DAY_OF_WEEK) - 1], date);

        for (int i = 1; i < 7; i++) {
            c.add(Calendar.DATE, -1);
            year = Integer.toString(c.get(Calendar.YEAR));
            month = Integer.toString(c.get(Calendar.MONTH) + 1);
            day = Integer.toString(c.get(Calendar.DATE));
            date = year + month + day;
            calendar.put(dayOfWeek[c.get(Calendar.DAY_OF_WEEK) - 1], date);
        }

        dayLbl.setText(selectedDay);
        System.out.println(selectedDay);

        display = queue.getLog();
        String txt = "";

        if (display.size() == 1 && display.get(0).startsWith("Error")) {
            txt = display.get(0);
        } else {
            for (String str : display) {
                String today = calendar.get(selectedDay);
                String[] bits = str.split(",");

                if (today.equals(bits[0])) {
                    txt += bits[1] + "\n";
                }
            }
        }

        transStText.setText(txt);
    }

    /**
     * Populate the top text area with a list of TAF messages that have been
     * successfuly sent, are pending transmission, or have failed.
     */
    private void populateData() {
        TransmissionQueue queue = TransmissionQueue.getInstance();
        transList.removeAll();
        ArrayList<String> display = null;

        if (pendingRdo.getSelection()) {
            display = queue.getPending();
        } else if (sentRdo.getSelection()) {
            display = queue.getSent();
        } else if (badRdo.getSelection()) {
            display = queue.getErrors();
        }

        Collections.sort(display);

        for (String str : display) {
            transList.add(str);
        }
    }
}
