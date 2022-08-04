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
package com.raytheon.viz.aviation.observer;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXB;

import org.apache.commons.collections.ListUtils;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.aviation.avnconfig.AvnConfigFileUtil;
import com.raytheon.uf.common.aviation.avnconfig.ITafSiteConfig;
import com.raytheon.uf.common.aviation.avnconfig.TafSiteConfigFactory;
import com.raytheon.uf.common.aviation.avnconfig.TafSiteData;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.tafqueue.ServerResponse;
import com.raytheon.uf.common.tafqueue.TafQueueRecord;
import com.raytheon.uf.common.tafqueue.TafQueueRecord.TafQueueState;
import com.raytheon.uf.common.tafqueue.TafQueueRequest;
import com.raytheon.uf.common.tafqueue.TafQueueRequest.Type;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.aviation.AviationDialog;
import com.raytheon.viz.aviation.editor.EditorTafTabComp;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.xml.AviationForecasterConfig;
import com.raytheon.viz.aviation.xml.ForecasterConfig;
import com.raytheon.viz.avnconfig.IStatusSettable;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * SendDialog class displays the Send dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 4/15/2009    1982       grichard    Provide feedback when saving a working TAF.
 * 12/08/2011   11745      rferrel     Updated header time to transmission time.
 * 08AUG2012    15613      zhao        Determine proper BBB for transmission
 * 09OCT2012    1229       rferrel     Make dialog non-blocking.
 * 0yJUN2013    1981       mpduff      Set user on the request.
 * 06May2014    3091       rferrel     Use OUP authorization to bring up send dialog.
 * 20May2015    4510       rferrel     Added {@link #getForecasterId()}.
 * Sep 25, 2015 4918       rferrel     Allow selection of forecaster to send a TAF.
 * Nov 06, 2015 5108       rferrel     Display warning when forecast xmittime will result in a
 *                                     header time the same as any existing pending forecast.
 * Feb 12, 2016 5335       tgurney     Add alert popup when TAF send fails
 * May 15, 2019 20693   mgamazaychikov AvnConfigFileUtil, ITafSiteConfig, TafSiteConfigFactory, 
 *                                     TafSiteData refactor
 * Jan 30, 2020 21710      zalberts    Modify getXmitBbb() to check for underscores
 * Feb 05, 2020 21825      zalberts    Assign timeControlsCalendar date values to xmitTime
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class SendDialog extends CaveSWTDialog {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SendDialog.class);

    /** Site file to use to find user's forecater id. */
    private final String FORECAST_CONFIG_FILE = "aviation"
            + IPathManager.SEPARATOR + "avnwatch" + IPathManager.SEPARATOR
            + "aviationForecasterConfig.xml";

    /**
     * Expression to find time stamp in a TAF.
     */
    private static final Pattern TIMESTAMP_PATTERN = Pattern
            .compile(" \\d{6}Z ");

    /**
     * Format to replace string found by TIME_STAMP.
     */
    private static final String TIMESTAMP_FORMAT = " %02d%02d%02dZ ";

    /**
     * Time the Send dialog time controls were initialized.
     */
    private Calendar timeControlsCalendar;

    /**
     * Hour spinner control.
     */
    private Spinner hourSpnr;

    /**
     * Minute spinner control.
     */
    private Spinner minuteSpnr;

    /**
     * Second spinner control.
     */
    private Spinner secondSpnr;

    /**
     * Forecaster.
     */
    private Label forecasterLabel;

    /**
     * Tab composite containing the TAF Viewer and the TAF Editor.
     */
    private final EditorTafTabComp tabComp;

    /**
     * Message status composite.
     */
    private final IStatusSettable msgStatComp;

    /**
     * Main composite.
     */
    private Composite mainComp;

    /*
     * Available forecasters.
     */
    private org.eclipse.swt.widgets.List forecasterList;

    /**
     * Send the TAFs individually or as a collective.
     */
    private final boolean sendCollective;

    /**
     * @return true when user authorized to send TAFs.
     */
    public static boolean isAuthorized() {
        TafQueueRequest request = new TafQueueRequest();
        request.setType(Type.CHECK_AUTHORIZED);
        request.setUser(UserController.getUserObject());
        request.setState(TafQueueRecord.TafQueueState.SENT);
        boolean response = false;
        try {
            ThriftClient.sendRequest(request);
            response = true;
        } catch (VizException e) {
            response = false;
        }
        return response;
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public SendDialog(Shell parent, EditorTafTabComp tabComp,
            IStatusSettable msgStatComp, boolean sendCollective) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Send");

        this.tabComp = tabComp;
        this.msgStatComp = msgStatComp;
        this.sendCollective = sendCollective;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.verticalSpacing = 0;
        return gl;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        GridLayout gl = new GridLayout(1, false);
        mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        mainComp.setLayoutData(gd);

        // Initialize all of the controls and layouts
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        configMgr.setDefaultColors(mainComp);

        createTimeControls(configMgr);
        createPersonListControl(configMgr);
        createBottomButtons(configMgr);
    }

    /**
     * Create the time controls.
     */
    private void createTimeControls(ResourceConfigMgr configMgr) {
        GridData gd = new GridData(SWT.CENTER, SWT.NONE, true, false);
        Group timeGroup = new Group(mainComp, SWT.NONE);
        timeGroup.setText(" Transmit at: ");
        GridLayout gl = new GridLayout(6, false);
        timeGroup.setLayout(gl);
        timeGroup.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(timeGroup);

        // Hour
        gd = new GridData(30, SWT.DEFAULT);
        Label hourLbl = new Label(timeGroup, SWT.RIGHT);
        configMgr.setDefaultFontAndColors(hourLbl, "HH:", gd);

        hourSpnr = new Spinner(timeGroup, SWT.BORDER);
        hourSpnr.setDigits(0);
        hourSpnr.setIncrement(1);
        hourSpnr.setPageIncrement(5);
        hourSpnr.setMinimum(0);
        hourSpnr.setMaximum(23);
        configMgr.setDefaultFont(hourSpnr);

        // Minute
        gd = new GridData(50, SWT.DEFAULT);
        Label minuteLbl = new Label(timeGroup, SWT.RIGHT);
        configMgr.setDefaultFontAndColors(minuteLbl, "MM:", gd);

        minuteSpnr = new Spinner(timeGroup, SWT.BORDER);
        minuteSpnr.setDigits(0);
        minuteSpnr.setIncrement(1);
        minuteSpnr.setPageIncrement(5);
        minuteSpnr.setMinimum(0);
        minuteSpnr.setMaximum(59);
        configMgr.setDefaultFont(minuteSpnr);

        // Second
        gd = new GridData(50, SWT.DEFAULT);
        Label secondLbl = new Label(timeGroup, SWT.RIGHT);
        configMgr.setDefaultFontAndColors(secondLbl, "SS:", gd);

        secondSpnr = new Spinner(timeGroup, SWT.BORDER);
        secondSpnr.setDigits(0);
        secondSpnr.setIncrement(1);
        secondSpnr.setPageIncrement(5);
        secondSpnr.setMinimum(0);
        secondSpnr.setMaximum(59);
        configMgr.setDefaultFont(secondSpnr);

        /*
         * Get current time
         */
        timeControlsCalendar = Calendar.getInstance();
        timeControlsCalendar.setTimeZone(TimeZone.getTimeZone("GMT"));
        int curHr = timeControlsCalendar.get(Calendar.HOUR_OF_DAY);
        int curMin = timeControlsCalendar.get(Calendar.MINUTE);
        int curSec = timeControlsCalendar.get(Calendar.SECOND);

        int sendHr = 0;
        int sendMin = 0;
        int sendSec = 0;

        String tafText = tabComp.getTextEditorControl().getText();
        Pattern regexPattern = Pattern.compile(tabComp.ISSUE_TIME);
        Matcher regexMatcher = regexPattern.matcher(tafText);

        /*
         * Get issue time
         */
        if (regexMatcher.find()) {
            String matched = regexMatcher.group();
            sendHr = Integer.parseInt(matched.substring(2, 4));
            sendMin = Integer.parseInt(matched.substring(4, 6));
        }

        /*
         * Use the newer time
         */
        if (curHr >= sendHr && curMin >= sendMin) {
            sendHr = curHr;
            sendMin = curMin;
            sendSec = curSec;
        }

        /*
         * Set the time values of the spinner controls.
         */
        hourSpnr.setSelection(sendHr);
        minuteSpnr.setSelection(sendMin);
        secondSpnr.setSelection(sendSec);
    }

    /**
     * Create the Person list control.
     */
    private void createPersonListControl(ResourceConfigMgr configMgr) {
        Composite listComp = new Composite(mainComp, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, true);
        listComp.setLayout(new GridLayout(1, false));
        listComp.setLayoutData(gd);
        configMgr.setDefaultColors(listComp);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Label repsonsibleLbl = new Label(listComp, SWT.CENTER);
        repsonsibleLbl.setText("Responsible for this forecast:");
        repsonsibleLbl.setLayoutData(gd);
        configMgr.setDefaultFontAndColors(repsonsibleLbl);

        List<ForecasterConfig> forecasters = getForecasters();
        if (forecasters == null) {
            forecasters = new ArrayList<ForecasterConfig>();
        }
        String forecaster = AviationDialog.getForecaster();
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.minimumWidth = 100;
        forecasterLabel = new Label(listComp, SWT.CENTER);
        forecasterLabel.setLayoutData(gd);
        if (forecasters.isEmpty()) {
            forecasterLabel.setText("NO FORECASTERS FOUND");
        } else {
            forecasterLabel.setText(forecaster);
        }
        configMgr.setListBoxFont(forecasterLabel);

        forecasterList = new org.eclipse.swt.widgets.List(listComp, SWT.SINGLE
                | SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
        gd = new GridData(SWT.CENTER, SWT.FILL, true, true);
        forecasterList.setLayoutData(gd);
        Font monoFont = new Font(shell.getDisplay(), "Monospace", 10,
                SWT.NORMAL);
        forecasterList.setFont(monoFont);
        forecasterList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                int i = forecasterList.getSelectionIndex();
                if (i >= 0) {
                    String sel = forecasterList.getItem(i);
                    String name = sel.substring(0, sel.indexOf(" "));
                    forecasterLabel.setText(name);
                }
            }

        });

        /*
         * Using the mono-space font and the format allows the ids to line up.
         */
        String[] fcEntries = new String[forecasters.size()];
        for (int i = 0; i < fcEntries.length; ++i) {
            ForecasterConfig fc = forecasters.get(i);
            fcEntries[i] = String.format("%-20s %s", fc.getName(),
                    fc.getFormattedId());
        }

        /*
         * Order by forecaster name then id.
         */
        Arrays.sort(fcEntries, String.CASE_INSENSITIVE_ORDER);
        int selIndex = -1;
        String entryHead = forecaster + " ";

        /*
         * Generate list get the index for the forecaster's entry.
         */
        for (String entry : fcEntries) {
            if (entry.startsWith(entryHead)) {
                selIndex = forecasterList.getItemCount();
            }
            forecasterList.add(entry);
        }

        monoFont.dispose();

        /*
         * Forecaster not in the send list. Just select the first entry.
         */
        if ((selIndex < 0) && (forecasterList.getItemCount() > 0)) {
            selIndex = 0;
            String sel = forecasterList.getItem(selIndex);
            String name = sel.substring(0, sel.indexOf(" "));
            forecasterLabel.setText(name);
        }

        forecasterList.select(selIndex);
        forecasterList.showSelection();
    }

    /**
     * Create the OK and Cancel buttons.
     */
    private void createBottomButtons(ResourceConfigMgr configMgr) {
        Composite buttonComp = new Composite(mainComp, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);
        configMgr.setDefaultColors(buttonComp);

        gd = new GridData(80, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(okBtn, "Cancel", gd);
        okBtn.setText("OK");
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                sendAction();
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(cancelBtn, "Cancel", gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    @SuppressWarnings("unchecked")
    private void sendAction() {
        String forecasterId = null;

        try {
            String selString = forecasterList.getItem(forecasterList
                    .getSelectionIndex());
            forecasterId = selString.split("\\s+")[1];
        } catch (IllegalArgumentException ex) {
            String msg = "Cannot send the TAF. No forecaster is selected.";
            statusHandler.handle(Priority.SIGNIFICANT, msg, ex);
            return;
        }

        TafQueueRequest request = new TafQueueRequest();
        request.setType(Type.CREATE);
        request.setUser(UserController.getUserObject());

        Calendar xmitTime = TimeUtil.newGmtCalendar();
        xmitTime.set(Calendar.YEAR, timeControlsCalendar.get(Calendar.YEAR));
        xmitTime.set(Calendar.MONTH, timeControlsCalendar.get(Calendar.MONTH));
        xmitTime.set(Calendar.DAY_OF_MONTH, timeControlsCalendar.get(Calendar.DAY_OF_MONTH));
        xmitTime.set(Calendar.HOUR_OF_DAY, hourSpnr.getSelection());
        xmitTime.set(Calendar.MINUTE, minuteSpnr.getSelection());
        xmitTime.set(Calendar.SECOND, secondSpnr.getSelection());
        xmitTime.set(Calendar.MILLISECOND, 0);
        String xmitTimestamp = String.format(TIMESTAMP_FORMAT,
                xmitTime.get(Calendar.DAY_OF_MONTH),
                xmitTime.get(Calendar.HOUR_OF_DAY),
                xmitTime.get(Calendar.MINUTE));

        /*
         * Check for pending entries with same xmitTimestamp. The header
         * timestamp is always adjusted to the xmitTimestamp.
         */
        TafQueueRequest pendingRequest = new TafQueueRequest();
        pendingRequest.setType(Type.GET_TAFS);
        pendingRequest.setUser(UserController.getUserObject());
        pendingRequest.setXmitTime(xmitTime.getTime());
        pendingRequest.setState(TafQueueState.PENDING);
        ServerResponse<List<String>> pendingResponse = null;
        try {
            pendingResponse = (ServerResponse<List<String>>) ThriftClient
                    .sendRequest(pendingRequest);
        } catch (VizException e1) {
            String msg = "Unable to get pending TAFs. ";
            statusHandler.handle(Priority.PROBLEM, msg, e1);
            msgStatComp.setMessageText(msg,
                    shell.getDisplay().getSystemColor(SWT.COLOR_RED).getRGB());
            return;
        }

        List<String> pendingList = pendingResponse.getPayload();

        if (!pendingList.isEmpty()) {
            StringBuilder message = new StringBuilder("The pending queue ");
            if (pendingList.size() == 1) {
                message.append("has a product with the same header time");
            } else {
                message.append("has products with the same header time");
            }

            msgStatComp.setMessageText(message.toString() + ".", shell
                    .getDisplay().getSystemColor(SWT.COLOR_RED).getRGB());
            message.append(":\n");
            for (String filename : pendingList) {
                message.append(filename.split(",", 2)[1]).append("\n");
            }
            message.append("\nSelect OK to transmit this product with the same header time.");

            MessageDialog dlg = new MessageDialog(shell,
                    "Duplicate Transmission Time", null, message.toString(),
                    MessageDialog.WARNING, new String[] { "OK", "Cancel" }, 1);
            if (0 != dlg.open()) {
                return;
            } else {
                statusHandler
                        .info("WARNING multiple products with same header time in the pending queue.");
            }
        }

        // BBB
        String in = tabComp.getTextEditorControl().getText();
        String bbb = tabComp.getBBB();
        // WMO ID
        String siteWmoId = tabComp.getWmoId();
        // WMO Site
        String siteNode = null;
        // java.util.List<String> stationIds = new ArrayList<String>();

        ArrayList<String> tafs = new ArrayList<String>();
        ArrayList<String> updatedTafs = new ArrayList<String>();

        // Split the text into individual TAFs if necessary
        if (sendCollective) {
            tafs.add(in.trim());
        } else {
            int idx = 0;
            int idx2 = 0;

            while (idx > -1 && idx2 > -1) {
                idx = in.indexOf("TAF", idx);
                idx2 = in.indexOf("TAF", (idx + 3));
                String tafStr;

                if (idx > -1 && idx2 > -1) {
                    tafStr = in.substring(idx, idx2);
                    idx += 3;
                } else {
                    tafStr = in.substring(idx);
                }

                tafs.add(tafStr.trim());
            }
        }

        boolean tafsQeueued = true;
        java.util.List<TafQueueRecord> records = new ArrayList<TafQueueRecord>();

        for (String tafText : tafs) {
            String fourLetterId = tafText.substring(tafText.indexOf('\n') + 1,
                    tafText.indexOf(' ', tafText.indexOf('\n')));

            // Site ID
            String siteId = fourLetterId.substring(1);

            /*
             * If "AAX" or "CCX" or "RRX", determine BBB for transmission
             */
            String xmitBbb = bbb;
            if (bbb.equals("AAX") || bbb.equals("CCX") || bbb.equals("RRX")) {
                String type = bbb.substring(0, 2);
                xmitBbb = getXmitBbb(type, siteId);
            }

            // Update Header Time to transmission time.
            tafText = TIMESTAMP_PATTERN.matcher(tafText).replaceFirst(
                    xmitTimestamp);
            updatedTafs.add(tafText);
            try {
                ITafSiteConfig config = TafSiteConfigFactory.getInstance();
                TafSiteData siteData = config.getSite(fourLetterId);
                siteWmoId = siteData.wmo.split(" ")[0];
                siteNode = siteData.wmo.split(" ")[1];
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error reading site configuration for " + siteId
                                + ", attempting to proceed anyway", e);
            }

            TafQueueRecord record = new TafQueueRecord(forecasterId,
                    xmitTime.getTime(), tafText, xmitBbb, siteId, siteWmoId,
                    siteNode, xmitTime.getTime());
            records.add(record);
        }

        try {
            // Enqueue TAFs for transmission
            request.setRecords(records);
            ServerResponse<String> response = (ServerResponse<String>) ThriftClient
                    .sendRequest(request);
            if (response.isError()) {
                statusHandler.handle(Priority.PROBLEM, response.toString());
                tafsQeueued = false;
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            msgStatComp.setMessageText(e.getMessage(), shell.getDisplay()
                    .getSystemColor(SWT.COLOR_RED).getRGB());
        }

        if (tafsQeueued) {
            // Update the TAF display with transmission time.
            StringBuilder sb = new StringBuilder();
            String prefix = "";
            for (String taf : updatedTafs) {
                sb.append(prefix).append(taf);
                prefix = "\n\n";
            }
            tabComp.getTextEditorControl().setText(sb.toString());
            msgStatComp.setMessageText(
                    "The TAF has been sent to the transmission queue.", shell
                            .getDisplay().getSystemColor(SWT.COLOR_GREEN)
                            .getRGB());
        }
        tabComp.setTafSent(tafsQeueued);
        shell.dispose();
    }

    @SuppressWarnings("unchecked")
    private String getXmitBbb(String type, String siteId) {

        try {
            TafQueueRequest request = new TafQueueRequest();
            request.setType(Type.GET_LIST);
            request.setUser(UserController.getUserObject());
            request.setState(TafQueueRecord.TafQueueState.SENT);
            ServerResponse<java.util.List<String>> response = (ServerResponse<java.util.List<String>>) ThriftClient
                    .sendRequest(request);
            java.util.List<String> payload = response.getPayload();
            String[] records = payload.toArray(new String[0]);
            int numRecords = records.length;
            for (int i = numRecords - 1; i >= 0; i--) {
                if (records[i].contains(siteId)) {
                    String[] texts = records[i].split("-");
                    String bbb = texts[texts.length - 2];
                    if (bbb.equals("___")) {
                        return type + "A";
                    }
                    if (bbb.subSequence(0, 2).equals(type)) {
                        char[] newX = new char[] { bbb.charAt(2) };
                        if (newX[0] == 'X') {
                            newX[0] = 'A';
                        } else {
                            newX[0]++;
                        }
                        return type + new String(newX);
                    }
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        return type + "A";
    }

    @SuppressWarnings("unchecked")
    private List<ForecasterConfig> getForecasters() {
        List<ForecasterConfig> forecasters = null;

        File f = AvnConfigFileUtil.getStaticFile(FORECAST_CONFIG_FILE);
        if (f == null) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Cannot send the TAF. Unable to load, "
                            + FORECAST_CONFIG_FILE
                            + ", unable to obtain forecaster's ID. ");
            return ListUtils.EMPTY_LIST;
        }
        try {
            forecasters = JAXB.unmarshal(f, AviationForecasterConfig.class)
                    .getForecasterConfig();
        } catch (RuntimeException ex) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Cannot send the TAF. Unable to parse, "
                            + FORECAST_CONFIG_FILE
                            + ", unable to obtain forecaster's ID. ", ex);
            return ListUtils.EMPTY_LIST;
        }
        return forecasters;
    }
}
