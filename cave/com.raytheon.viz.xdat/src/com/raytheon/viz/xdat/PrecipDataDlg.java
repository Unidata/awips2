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
package com.raytheon.viz.xdat;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Precipitation Data dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 27 Oct 2008             lvenable    Initial creation.
 * 10 Feb 2009             wkwock      Added functions.
 * 31 May 2015  4501       skorolev    Got rid of Vector.
 * 04 Aug 2016  5800       mduff       Cleanup
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class PrecipDataDlg extends CaveSWTDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    /**
     * Hour spinner.
     */
    private Spinner hourSpnr;

    /**
     * Duration spinner.
     */
    private Spinner durationSpnr;

    /**
     * this class is called by owner
     */
    ITextDisplay displayCB;

    /**
     * database
     */
    XdatDB databaseMgr;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public PrecipDataDlg(Shell parentShell, ITextDisplay displayCB,
            XdatDB database) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        setText("Precipitation Data");

        this.displayCB = displayCB;
        this.databaseMgr = database;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.verticalSpacing = 10;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        createControls();
        createCloseButton();
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(shell.getBounds().width, shell.getBounds().height);
    }

    /**
     * Create the controls on the dialog.
     */
    private void createControls() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group precipGrp = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 10;
        gl.marginHeight = 10;
        precipGrp.setLayout(gl);
        precipGrp.setLayoutData(gd);
        precipGrp.setText(" Choose Precipitation Option ");

        Button coopPrecBtn = new Button(precipGrp, SWT.PUSH);
        coopPrecBtn.setText(" 24 Hour COOP Precipitation ");
        coopPrecBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayCoopPrecip();
            }
        });

        Button pcPrecBtn = new Button(precipGrp, SWT.PUSH);
        pcPrecBtn.setText(" 24 Hour (12Z) PC Precipitation ");
        pcPrecBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayPrecipAccumulation(12, 24);
            }
        });

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group endingHrGrp = new Group(shell, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        gl.marginHeight = 10;
        endingHrGrp.setLayout(gl);
        endingHrGrp.setLayoutData(gd);
        endingHrGrp.setText(" Choose Precipitation Option ");

        Label hourLbl = new Label(endingHrGrp, SWT.NONE);
        hourLbl.setText("Choose Hour (GMT): ");

        gd = new GridData(45, SWT.DEFAULT);
        gd.horizontalIndent = 10;
        hourSpnr = new Spinner(endingHrGrp, SWT.BORDER);
        hourSpnr.setDigits(0);
        hourSpnr.setIncrement(1);
        hourSpnr.setPageIncrement(5);
        hourSpnr.setMinimum(0);
        hourSpnr.setMaximum(23);
        hourSpnr.setSelection(12);
        hourSpnr.setLayoutData(gd);

        Label durationLbl = new Label(endingHrGrp, SWT.NONE);
        durationLbl.setText("Choose Duration in Hours: ");

        gd = new GridData(45, SWT.DEFAULT);
        gd.horizontalIndent = 10;
        durationSpnr = new Spinner(endingHrGrp, SWT.BORDER);
        durationSpnr.setDigits(0);
        durationSpnr.setIncrement(1);
        durationSpnr.setPageIncrement(5);
        durationSpnr.setMinimum(0);
        durationSpnr.setMaximum(72);
        durationSpnr.setSelection(12);
        durationSpnr.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, true);
        gd.horizontalSpan = 2;
        Button precipAccumBtn = new Button(endingHrGrp, SWT.PUSH);
        precipAccumBtn.setText("Display Precipitation Accumulation");
        precipAccumBtn.setLayoutData(gd);
        precipAccumBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int hour = hourSpnr.getSelection();
                int duration = durationSpnr.getSelection();
                displayPrecipAccumulation(hour, duration);
            }
        });
    }

    /**
     * Create close button at the bottom of the dialog.
     */
    private void createCloseButton() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    private void displayCoopPrecip() {
        String startDate = displayCB.getEndDate();
        String header1 = "\t\t\t 24 Hour Precipitation Ending at "
                + displayCB.getEndDate() + " 12Z";
        String header2 = "  ID     PE  DUR  TS E       OBSTIME        PRODUCT       VALUE";
        String dashLine = "-------------------------------------------------------------------";
        String dataFmt = "%-8s %2s  %4s %2s %1s %19s %13s %6.2f";
        List<String[]> coopPrecipData = databaseMgr
                .getCoopPrecipData(startDate);

        if (coopPrecipData == null) {
            displayCB.setDisplayText("No data available.");
        } else {
            String[] displayText = new String[coopPrecipData.size() + 3];
            displayText[0] = header1;
            displayText[1] = header2;
            displayText[2] = dashLine;

            int i = 3;
            for (String[] dataText : coopPrecipData) {
                String productID = dataText[5];
                if (productID == null) {
                    productID = "";
                }

                double value = HydroConstants.MISSING_VALUE;
                try {
                    value = Double.parseDouble(dataText[6]);
                } catch (NumberFormatException nfe) {
                    value = HydroConstants.MISSING_VALUE;
                    statusHandler.handle(Priority.ERROR, "Fail to parse "
                            + dataText[6] + ".");
                }
                displayText[i] = String.format(dataFmt, dataText[0], "PP",
                        dataText[1], dataText[2], dataText[3], dataText[4],
                        productID, value);
                i++;
            }
            displayCB.setDisplayText(displayText);
        }
    }

    /**
     * Display precipitation Accumulation
     * 
     */
    private void displayPrecipAccumulation(int hour, int duration) {
        SimpleDateFormat obsDate = new SimpleDateFormat("yyyy-MM-dd", Locale.US);
        Calendar date = null;
        try {
            Date sDate = obsDate.parse(displayCB.getEndDate());
            date = TimeUtil.newGmtCalendar(sDate);
        } catch (ParseException e) {
            date = TimeUtil.newGmtCalendar();
            statusHandler.handle(Priority.ERROR,
                    "Fail to parse " + displayCB.getEndDate() + ".");
        }

        obsDate = new SimpleDateFormat("yyyy-MM-dd HH:00:00", Locale.US);
        obsDate.setTimeZone(TimeZone.getTimeZone("GMT"));

        String obsTimeStr = obsDate.format(date.getTime());
        String displayTime = obsTimeStr;
        Map<String, Double> precipLidAndValue = databaseMgr
                .getPrecipLidAndValue(obsTimeStr);

        date.add(Calendar.HOUR_OF_DAY, 0 - duration);
        obsTimeStr = obsDate.format(date.getTime());

        Map<String, Double> precipLidAndValue2 = databaseMgr
                .getPrecipLidAndValue(obsTimeStr);

        List<String> precipLidAndDiffBuf = new ArrayList<>(
                precipLidAndValue.size());

        if (precipLidAndValue.size() == 0) {
            precipLidAndDiffBuf.add("No " + duration
                    + " Hour Precipitation data found.");
        } else {
            precipLidAndDiffBuf.add("\t\t\t " + duration
                    + " Hour PC Accumulation Ending at " + displayTime);
            precipLidAndDiffBuf.add("  ID     VALUE");
            precipLidAndDiffBuf.add("--------------");

            Iterator<String> iter = precipLidAndValue.keySet().iterator();
            String lid = null;
            List<XdatPcData> dataList = new ArrayList<>();
            while (iter.hasNext()) {
                lid = iter.next();

                double value = precipLidAndValue.get(lid);
                double value2 = -999;
                if (precipLidAndValue2.containsKey(lid)) {
                    value2 = precipLidAndValue2.get(lid);
                }
                // TODO determine how A1 handles missing data
                double valDiff = value - value2;
                int valint = (int) (valDiff * 100);
                valDiff = Math.floor(valint) / 100.0;

                if (valDiff < 0) {
                    valDiff = 0;
                }

                XdatPcData data = new XdatPcData(lid, valDiff);
                dataList.add(data);
            }

            Collections.sort(dataList);
            for (int i = 0; i < dataList.size(); i++) {
                XdatPcData data = dataList.get(i);

                precipLidAndDiffBuf.add(String.format("%-8s %4.2f",
                        data.getLid(), data.getValue()));
            }

        }
        displayCB.setDisplayText(precipLidAndDiffBuf
                .toArray(new String[precipLidAndDiffBuf.size()]));
    }
}
