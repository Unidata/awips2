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
package com.raytheon.viz.aviation.monitor;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.edex.plugin.taf.common.TafRecord;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.aviation.editor.ITafSettable;
import com.raytheon.viz.aviation.editor.TafViewerEditorDlg.TafSettings;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;
import com.raytheon.viz.aviation.xml.MonitorCfg;
import com.raytheon.viz.aviation.xml.TafMonitorCfg;
import com.raytheon.viz.avnconfig.IStatusSettable;

/**
 * This class contains display information for a TAF site.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 28 FEB 2008  938         lvenable    Initial creation.
 * 3/7/2008     938         grichard    Build 03 update.
 * 3/25/2008    934         grichard    Build 04 update.
 * 4/24/2008    934         grichard    Retrieve metwatch entities at startup time.
 * 6/16/2008    937         grichard    Improved viewer/editor interaction.
 * 8/15/2008    1444        grichard    Added startup metwatch checks.
 * 8/28/2008    1444        grichard    Added force metwatch now checks.
 * 5/27/2009    1982        grichard    Added buffer mos data retrieval.
 * 8/18/2009    2830        grichard    Added comments on active checkbox control.
 * 9/01/2009    3027        njensen    Major refactor and cleanup
 * 9/02/2010    4022        rferrel     Alert highlights
 * 9/13/2010    5429        rferrel     Modified columnValueChanged to add
 *                                      its value into the monitor's args list.
 * 9/27/2010    6195        rferrel     Determine index for MetarMonitor and no
 *                                      block thread waiting for monitors.
 * 10/97/2010   7220        rferrel     Added grace period to the metar timeout.
 *                                      Time values now based on TAF/Metar
 *                                      string so they match the viewer/editor.
 * 10/27/2010   7383        rferrel     stop blinking when setBlikable's 
 *                                      is false.
 * 05/13/2011   8611        rferrel     Added type to Site Monitor requests and update
 *                                      Viewer when a METAR changes alert status.
 * 04/26/2012   14717       zhao        Indicator labels turn gray when Metar is outdated
 * 20JUL2012    14570       gzhang/zhao Add data structure for highlighting correct time groups in TAF viewer
 * 01/02/2013   15606		gzhang		Remove GridData widthHint so button/label size change with GUI
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TafSiteComp {

    /**
     * Number of seconds to blink the button.
     */
    private static final int BLINK_IN_SECONDS = 300;

    /**
     * A metar is good for up to one hour and can be issued up to 10 minutes
     * before they take affect. This is the time out in milliseconds for 1 hour
     * with a 10 minute grace period.
     * 
     * DR14717: changed grace period from 10 minutes to 5 minutes.
     */
    private final static long METAR_TIMEOUT = (1L * 60L + 5L) * 60L * 1000L;
    
    /**
     * DR14717: When Metar is 2 hours old, the persistence indicators turn gray. 
     * This is the timeout in milliseconds for 2 hours.
     */
    public final static long METAR_TIMEOUT_2HR = 2L * 60L * 60L * 1000L;
    
    /**
     * DR14717: When Metar is 4 hours plus 10 minutes old, Metar Time Label is replaced by "None",
     * and the current observation and persistence indicators turn gray.
     * This is the timeout in milliseconds for 4 hours plus 10 minutes.
     */
    public final static long METAR_TIMEOUT_4HR10MIN = (4L * 60L + 10L) * 60L * 1000L;
    
    /**
     * DR14717:
     */
    private long latestMtrTime = -1;
    private boolean persistMonitorProcessedFirst = false;
    
    /**
     * A TAF is good for up to 6 hours and can be issued up to 40 minutes before
     * they take affect. This time out is in milliseconds for 6 hours with a 40
     * minute grace period.
     */
    private final static long TAF_TIMEOUT = (6L * 60L + 40L) * 60L * 1000L;

    private static Color warningColor;

    private static Color errorColor;

    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Site ID button.
     */
    private Button siteIdBtn;

    /**
     * Active check box.
     */
    private Button activeChk;

    /**
     * TAF label.
     */
    private Label tafLbl;

    /**
     * TAF time label.
     */
    private Label tafTimeLbl;

    /**
     * METAR label.
     */
    private Label mtrLbl;

    /**
     * METAR time label.
     */
    private Label mtrTimeLbl;

    /**
     * Amended button.
     */
    private Button amdBtn;

    /**
     * Routine button.
     */
    private Button rtdBtn;

    /**
     * Corrected button.
     */
    private Button corBtn;

    /**
     * Station Name of the ICAO of interest.
     */
    private String stationName;

    /**
     * The callback to use to open the Taf Viewer/Editor.
     */
    private ITafSettable tveDlg;

    /**
     * Default background color - DO NOT DISPOSE...
     */
    private Color backgroundColor;

    /**
     * TAF monitor configuration.
     */
    private TafMonitorCfg tafMonCfg;

    /**
     * Message status composite.
     */
    private IStatusSettable msgStatComp;

    /**
     * Array of available monitors.
     */
    private ArrayList<SiteMonitor> monitorArray;

    private Map<String, String[]> alertMap;

    public Map<String, String[]> getAlertMap() {
        return alertMap;
    }

    private TafRecord lastTaf;

    private boolean updatable;

    private boolean blinkable;

    /**
     * Index in the monitorArray that where the MetarMonitor is located.
     */
    private int metarMontiorIndex = -1;

    /**
     * Used by <code>tafArrived</code> to determine if blinking should be turned
     * off when no alerts detected for the TAF.
     */
    private boolean noBlinkTriggered = true;

    /**
     * Set by <code>tarArrived</code> to determine when the last monitor request
     * is being processed.
     */
    private final AtomicInteger monitorRequestLeft = new AtomicInteger();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite
     * @param stationName
     *            Station name
     * @param cb
     *            Callback method
     * @param tveDlg
     *            TAF viewer/editor dialog
     * @param msgStatCb
     *            Message status callback
     */
    public TafSiteComp(Composite parent, String stationName,
            ITafSettable tveDlg, TafMonitorCfg tafMonCfg,
            IStatusSettable msgStatComp) {

        this.parent = parent;
        this.stationName = stationName;
        this.tveDlg = tveDlg;
        this.tafMonCfg = tafMonCfg;
        this.msgStatComp = msgStatComp;

        init();
        updatable = activeChk.getSelection();

        // this ensures the site is initialized for those types of data
        LtgDataMgr.getLtgData(stationName);
        CcfpData.getReports(stationName);
    }

    /**
     * Initialize the composite.
     */
    private void init() {
        initializeComponents();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        GridData gd = new GridData();
//        gd.widthHint = 70;	// DR 15606
        gd.minimumWidth = 70;
        siteIdBtn = new Button(parent, SWT.PUSH);
        configMgr.setDefaultFontAndColors(siteIdBtn, "WWWW", gd);
        siteIdBtn.setText(stationName);
        siteIdBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                tveDlg.populateViewerStation(siteIdBtn.getText());
                // Display the TAF and METAR of the first site selected in the
                // TAF Viewer.
                tveDlg.updateSettings(TafSettings.OPEN_VIEW, stationName);
                tveDlg.showDialog();
                stopBlinkingButton();
            }
        });

        siteIdBtn.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                if (e.button == 3) {
                    if (setTafNoError()) {
                        stopBlinkingButton();
                    }
                }
            }
        });

        gd = new GridData(20, SWT.DEFAULT);
        activeChk = new Button(parent, SWT.CHECK);
        activeChk.setSelection(true);
        activeChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updatable = activeChk.getSelection();
            }

        });
        configMgr.setDefaultFontAndColors(activeChk);

        createTafMetarTimeComp(configMgr);

        // Create the status monitors label that displays status
        createStatusMonitors();

        createEditorButtonsComp(configMgr);

    }

    /**
     * Create the METAR time composite.
     */
    private void createTafMetarTimeComp(ResourceConfigMgr configMgr) {

        GridLayout gl = new GridLayout(2, false);
        gl.verticalSpacing = 2;
        Composite tafMtrComp = new Composite(parent, SWT.NONE);
        tafMtrComp.setLayout(gl);
        configMgr.setDefaultColors(tafMtrComp);

        tafLbl = new Label(tafMtrComp, SWT.NONE);
        tafLbl.setText("TAF");
        configMgr.setDefaultFontAndColors(tafLbl);

        tafTimeLbl = new Label(tafMtrComp, SWT.NONE);
        tafTimeLbl.setText("HH:MM");
        configMgr.setDefaultFontAndColors(tafTimeLbl);

        mtrLbl = new Label(tafMtrComp, SWT.NONE);
        mtrLbl.setText("MTR");
        configMgr.setDefaultFontAndColors(mtrLbl);

        mtrTimeLbl = new Label(tafMtrComp, SWT.NONE);
        mtrTimeLbl.setText("HH:MM");
        configMgr.setDefaultFontAndColors(mtrTimeLbl);
    }

    /**
     * Create the status monitors from the TAF monitor configuration XML.
     */
    private void createStatusMonitors() {
        monitorArray = new ArrayList<SiteMonitor>();
        alertMap = new HashMap<String, String[]>();
        ArrayList<MonitorCfg> monitorCfgs = tafMonCfg.getMonitorCfgs();
        alertTimeMap = new HashMap<String,String>();/* DR 14570 */ 
        tempoMap = new HashMap<String,String[]>();//20120711
        for (MonitorCfg monCfg : monitorCfgs) {
            SiteMonitor monitor = null;
            if ("MetarMonitor".equals(monCfg.getClassName())) {
                monitor = new SiteMonitor(parent, this, monCfg, alertMap, /* DR 14570 */alertTimeMap,tempoMap);
                metarMontiorIndex = monitorArray.size(); 
            } else {
                monitor = new SiteMonitor(parent, this, monCfg, null, /* DR 14570 */null,null);
            }
            monitorArray.add(monitor);
        }
    }

    public void checkNow() {
        TafRecord taf = TafUtil.getLatestTaf(stationName);
        this.tafArrived(taf, false);
    }

    /**
     * Start a timer to blink the site button because a weather change has
     * occurred.
     */
    protected void startBlinkingButton() {
        if (isBlinkable()) {
            noBlinkTriggered = false;
            SiteBlinker.getInstance().add(siteIdBtn, BLINK_IN_SECONDS);
        }
    }

    protected void stopBlinkingButton() {
        SiteBlinker.getInstance().remove(siteIdBtn);
    }

    /**
     * Create the editor shortcut buttons.
     */
    private void createEditorButtonsComp(ResourceConfigMgr configMgr) {

        /*
         * Check if the AMD button need to be displayed.
         */
        boolean showAmdButtons = configMgr
                .getResourceAsBoolean(ResourceTag.AmdButtons);

        if (showAmdButtons == false) {
            // "Invisible" label to fill the grid column
            new Label(parent, SWT.NONE);

            return;
        }

        /*
         * Add the AMD buttons to the display
         */
        int btnMinWidth = 80;

        GridLayout gl = new GridLayout(3, false);
        gl.marginWidth = 2;
        Composite btnStatusComp = new Composite(parent, SWT.NONE);
        btnStatusComp.setLayout(gl);
        configMgr.setDefaultColors(btnStatusComp);

        GridData gd = new GridData();
        gd.minimumWidth = btnMinWidth;
//        gd.widthHint = btnMinWidth;	// DR 15606
        amdBtn = new Button(btnStatusComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(amdBtn, "Amd", gd);
        amdBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                tveDlg.populateViewerStation(stationName);

                tveDlg.updateSettings(TafSettings.OPEN_VIEW, stationName);
                tveDlg.updateSettings(TafSettings.OPEN_AMD, stationName);
                tveDlg.showDialog();
                stopBlinkingButton();
            }
        });

        gd = new GridData();
        gd.minimumWidth = btnMinWidth;
//        gd.widthHint = btnMinWidth;	// DR 15606
        rtdBtn = new Button(btnStatusComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(rtdBtn, "Rtd", gd);
        rtdBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                tveDlg.populateViewerStation(stationName);

                tveDlg.updateSettings(TafSettings.OPEN_VIEW, stationName);
                tveDlg.updateSettings(TafSettings.OPEN_RTD, stationName);
                tveDlg.showDialog();
                stopBlinkingButton();
            }
        });

        gd = new GridData();
        gd.minimumWidth = btnMinWidth;
//        gd.widthHint = btnMinWidth;	// DR 15606
        corBtn = new Button(btnStatusComp, SWT.PUSH);
        configMgr.setDefaultFontAndColors(corBtn, "Cor", gd);
        corBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                tveDlg.populateViewerStation(stationName);
                tveDlg.updateSettings(TafSettings.OPEN_VIEW, stationName);
                tveDlg.updateSettings(TafSettings.OPEN_COR, stationName);
                tveDlg.showDialog();
                stopBlinkingButton();
            }
        });
    }

    /**
     * Turns off a site's button when a new TAF no longer contains any alerts.
     * Used by the monitors queued by <code>tafArrived</code> to allow the last
     * one processed to stop blinking if no alerts were detected.
     */
    public void checkSiteButton() {
        if (monitorRequestLeft.decrementAndGet() <= 0 && noBlinkTriggered) {
            stopBlinkingButton();
        }
    }

    public void tafArrived(TafRecord taf) {
        tafArrived(taf, true);
    }

    public void tafArrived(TafRecord taf, boolean doBlinkCheck) {
        if (taf != null && updatable) {
            lastTaf = taf;
            this.noBlinkTriggered = doBlinkCheck;
            monitorRequestLeft.set(monitorArray.size());

            for (int i = 0; i < monitorArray.size(); i++) {
                SiteMonitor m = monitorArray.get(i);
                m.monitor(taf, "TAF");
            }

            if (stationName.equals(tveDlg.getCurrentViewerStation())
                    && metarMontiorIndex >= 0) {

                // Don't hold up thread waiting for monitors to finish.
                WaitForMonitors job = new WaitForMonitors("Update Viewer");
                job.setSystem(true);
                job.setPriority(Job.SHORT);
                job.schedule();
            }
        }

        if (taf == null) {
            VizApp.runAsync(new Runnable() {
                public void run() {
                    siteIdBtn.setBackground(getErrorColor());
                    tafTimeLbl.setText("HH:MM");
                }
            });
        }
    }

    public void fireMonitor(String monitor) {
        if (updatable) {
            for (int i = 0; i < monitorArray.size(); i++) {
                SiteMonitor m = monitorArray.get(i);
                if (m.getMonitorClassName().equals(monitor)) {
                    m.monitor(lastTaf, monitor);

                    // Up date the viewer after processing the METAR.
                    if (monitor.equals("MetarMonitor")
                            && stationName.equals(tveDlg
                                    .getCurrentViewerStation())
                            && metarMontiorIndex >= 0) {
                        monitorRequestLeft.incrementAndGet();
                        // Don't hold up thread waiting for monitors to finish.
                        WaitForMonitors job = new WaitForMonitors(
                                "Update Viewer");
                        job.setSystem(true);
                        job.setPriority(Job.SHORT);
                        job.schedule();
                    }
                }
            }
        }
    }

    public void columnValueChanged(String monitorClassName, String arg,
            Object value) {
        // TODO get taf better way
        TafRecord taf = TafUtil.getLatestTaf(stationName);

        for (SiteMonitor m : monitorArray) {
            if (m.getMonitorClassName().equals(monitorClassName)) {
                m.args.put(arg, value);
                m.monitor(taf, "TAF");
            }
        }
    }

    private synchronized Color getBackgroundColor() {
        if (backgroundColor == null || backgroundColor.isDisposed()) {
            backgroundColor = ResourceConfigMgr.getInstance()
                    .getDefaultBackgroundColor();
        }
        return backgroundColor;
    }

    private synchronized Color getWarningColor() {
        if (warningColor == null || warningColor.isDisposed()) {
            RGB rgb = RGBColors.getRGBColor("orange");
            warningColor = new Color(parent.getDisplay(), rgb);
        }
        return warningColor;
    }

    private synchronized Color getErrorColor() {
        if (errorColor == null || errorColor.isDisposed()) {
            RGB rgb = RGBColors.getRGBColor("red");
            errorColor = new Color(parent.getDisplay(), rgb);
        }
        return errorColor;
    }

    /**
     * Set Taf time label to HH:MM based on the value in timestamp.
     * 
     * @param tafTime
     *            Issue time
     * @param timestamp
     *            Taf time stamp assumed to be in the format DDHHMMZ
     */
    public void setTafTime(long tafTime, String timestamp) {
        if (!tafTimeLbl.isDisposed()) {
            // Note the minutes from the tafTime does not always match what is
            // in the timestamp. We use timestamp here so what is displayed in
            // the monitor will match what is displayed in the viewer/editor.
            tafTimeLbl.setText(timestamp.substring(2, 4) + ":"
                    + timestamp.substring(4, 6));
            long currentTime = SimulatedTime.getSystemTime().getTime()
                    .getTime();
            if (currentTime > (tafTime + TAF_TIMEOUT)) {
                tafTimeLbl.setBackground(getWarningColor());
            } else {
                tafTimeLbl.setBackground(getBackgroundColor());
            }

            if (!mtrTimeLbl.getText().equals("HH:MM")) {
                siteIdBtn.setBackground(getBackgroundColor());
            }
        }
    }

    /**
     * Set Metar time label to HH:MM based on the values in timestamp.
     * 
     * @param metarTime
     *            Issue time
     * @param timestamp
     *            Metar time stamp assumed to be in the format DDHHMMZ
     */
    public void setMetarTime(long metarTime, String timestamp) {
        if (!mtrTimeLbl.isDisposed()) {
            // Note the minutes from the metarTime does not always match what is
            // in the timestamp. We use timestamp here so what is displayed in
            // the monitor will match what is displayed in the viewer/editor.
            mtrTimeLbl.setText(timestamp.substring(2, 4) + ":"
                    + timestamp.substring(4, 6));
            long currentTime = SimulatedTime.getSystemTime().getTime()
                    .getTime();
            
            if ( currentTime > ( metarTime + METAR_TIMEOUT_4HR10MIN ) ) { 
            	mtrTimeLbl.setText("None");
                mtrTimeLbl.setBackground(getBackgroundColor());
            	if ( persistMonitorProcessedFirst ) {
                	SiteMonitor psstMonitor = monitorArray.get(1);
                	Color grayColor = psstMonitor.getGraySeverityColor();
                	Map<String, Label> psstLabelMap = psstMonitor.getLabelMap();
                	Set<String> psstKeys = psstLabelMap.keySet();
                	for ( String key : psstKeys ) {
                		psstLabelMap.get(key).setBackground(grayColor);
                	}
            	}
            } else if (currentTime > (metarTime + METAR_TIMEOUT)) {
                mtrTimeLbl.setBackground(getWarningColor());
                if ( currentTime > ( metarTime + METAR_TIMEOUT_2HR ) ) {
                	if ( persistMonitorProcessedFirst ) {
                    	SiteMonitor psstMonitor = monitorArray.get(1);
                    	Color grayColor = psstMonitor.getGraySeverityColor();
                    	Map<String, Label> psstLabelMap = psstMonitor.getLabelMap();
                    	Set<String> psstKeys = psstLabelMap.keySet();
                    	for ( String key : psstKeys ) {
                    		psstLabelMap.get(key).setBackground(grayColor);
                    	}
                	}
                }
            } else {
                mtrTimeLbl.setBackground(getBackgroundColor());
            }

            if (!tafTimeLbl.getText().equals("HH:MM")) {
                siteIdBtn.setBackground(getBackgroundColor());
            }
        }
    }

    public void setMetarMissing() {
        siteIdBtn.setBackground(getErrorColor());
        mtrTimeLbl.setText("HH:MM");
    }

    public String getStationName() {
        return stationName;
    }

    public boolean isBlinkable() {
        return blinkable;
    }

    public void setBlinkable(boolean blinkable) {
        if (this.blinkable != blinkable) {
            this.blinkable = blinkable;
            if (!blinkable) {
                stopBlinkingButton();
            }
        }
    }

    public void setTafError(String error) {
        siteIdBtn.setBackground(getWarningColor());
    }

    public boolean setTafNoError() {
        if (!mtrTimeLbl.getText().equals("HH:MM")) {
            siteIdBtn.setBackground(getBackgroundColor());
            return true;
        }
        return false;
    }

    public void updateSeverity(int severity) {
        // setActive();
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        List<?> deiconifyValues = Arrays.asList(configMgr
                .getComboValues(ResourceTag.NotifyDeiconify));
        int index1 = deiconifyValues.indexOf(configMgr
                .getResourceAsString(ResourceTag.NotifyDeiconify));

        List<?> raiseValues = Arrays.asList(configMgr
                .getComboValues(ResourceTag.NotifyRaise));
        int index2 = raiseValues.indexOf(configMgr
                .getResourceAsString(ResourceTag.NotifyRaise));

        List<?> playValues = Arrays.asList(configMgr
                .getComboValues(ResourceTag.NotifyPlay));
        int index3 = playValues.indexOf(configMgr
                .getResourceAsString(ResourceTag.NotifyPlay));
        // mmaron DR 2847
        // Date now = new Date();
        // SimpleDateFormat fmat = new SimpleDateFormat(" yyyy/MM/dd HH:mm:ss");
        // String now_ = fmat.format(now);
        /*
         * if (severity>1) System.out.println("updateSeverity: " + severity +
         * " focus: " + parent.getShell().isFocusControl() + " index1: " +
         * index1 + " index2: " + index2 + " index3: " + index3 + now_);
         */
        if (!parent.getShell().isFocusControl()
                && ((0 < index1 && index1 < severity) || (0 < index2 && index2 < severity))) {
            setActive();
        }

        if (0 < index3 && index3 < severity) {
            String soundFile = configMgr
                    .getResourceAsString(ResourceTag.PlayFile);
            if (soundFile != null && !soundFile.equals("None")) {
                try {
                    System.out.println("playFile: play");
                    NotifyAudioManager.getInstance().playFile(soundFile);
                } catch (IOException e) {
                    msgStatComp.setMessageText(e.getMessage(), parent
                            .getDisplay().getSystemColor(SWT.COLOR_RED)
                            .getRGB());
                }
            }
        }
    }

    private void setActive() {
        Shell avnMonitor = parent.getShell();
        Date date = new Date(System.currentTimeMillis());
        SimpleDateFormat fmat = new SimpleDateFormat(" yyyy/MM/dd HH:mm:ss");
        System.out.println("setActive() " + fmat.format(date) + " "
                + avnMonitor.getText());

        // TODO: find a better fix for this
        // The setVisible false/true causes flicker but forces
        // the dialog on top of everything but the active window.
        // Bug in forceActive keeps it from working.
        Point p = avnMonitor.getLocation();
        avnMonitor.setVisible(false);
        avnMonitor.setMinimized(false);
        avnMonitor.forceActive();
        avnMonitor.setLocation(p);
        avnMonitor.setVisible(true);
    }

    /**
     * 
     * Job to wait for all monitors to finish prior to updating the taf in the
     * viewer. This ensures the taf and most recent metar are in sync so alert
     * highlighting are done correctly.
     */
    class WaitForMonitors extends Job {
        public WaitForMonitors(String name) {
            super(name);
        }

        protected IStatus run(IProgressMonitor monitor) {
            // Wait for monitor to finish updating the alert map.
            monitorArray.get(metarMontiorIndex).waitForMonitor();

            // Update viewer with new TAF.
            VizApp.runAsync(new Runnable() {
                public void run() {
                    tveDlg.updateSettings(TafSettings.UPDATE_VIEW, stationName);
                }
            });
            return org.eclipse.core.runtime.Status.OK_STATUS;
        }
    }

	public void setPersistMonitorProcessedFirst(boolean b) {
		persistMonitorProcessedFirst = b;
	}
	
	public void setLatestMtrTime(long latestMtrTime) {
		this.latestMtrTime = latestMtrTime;
	}

	public long getLatestMtrTime() {
		return latestMtrTime;
	}
	
	//------------------------------- DR 14570:
	private Map<String, String[]> tempoMap = null;//20120711
	private Map<String, String> alertTimeMap = null;
	public Map<String,String> getAlertTimeMap(){ return alertTimeMap;}
	public Map<String,String[]> getTempoMap(){return tempoMap;}//20120711
}
