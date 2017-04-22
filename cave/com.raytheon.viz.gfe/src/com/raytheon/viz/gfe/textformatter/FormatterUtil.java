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
package com.raytheon.viz.gfe.textformatter;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData;
import com.raytheon.viz.gfe.tasks.TaskManager;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeProhibitedOpException;

/**
 * Utilities for text formatter
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 8, 2008             njensen     Initial creation 
 * Jan 15, 2010  3395      ryu         Fix &quot;issued by&quot; functionality
 * Sep 05, 2013  2329      randerso    Removed save of combinations file
 * Feb 12, 2014  2591      randerso    Passed dataMgr instance to FormatterUtil.runFormatterScript
 *                                     Removed call to TextProductManager.reloadModule
 * Apr 20, 2015  4027      randerso    Renamed ProductStateEnum with an initial capital
 *                                     Fixed hard coded active table mode in runFormatterScript
 * Jul 30, 2015  4263      dgilling    Pass DataManager instance to TextFormatter, stop passing
 *                                     varDict through.
 * Aug 26, 2015  4804      dgilling    Add methods so SmartScript can run formatters.
 * Sep 15, 2015  4858      dgilling    Disable store/transmit in DRT mode.
 * Oct 01, 2015  4888      dgilling    Fix javadoc for exceptions.
 * Apr 14, 2016  5578      dgilling    Ensure formatters launched interactively
 *                                     ask for varDict before execution.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class FormatterUtil {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FormatterUtil.class);

    public static final String[] VTEC_MODES = { "Normal: NoVTEC",
            "Normal: O-Vtec", "Normal: E-Vtec", "Normal: X-Vtec",
            "Test: NoVTEC", "Test: T-Vtec" };

    private FormatterUtil() {
        throw new AssertionError();
    }

    /**
     * Runs a text formatter script for a given product
     * 
     * @param dataMgr
     *            the DataManager instance to use
     * 
     * @param productMgr
     *            the formatter instance to use
     * @param productName
     *            the name of the text product
     * @param dbId
     *            source database
     * @param vtecMode
     *            VTEC mode
     * @param finish
     *            listener to fire when formatter finishes generating product
     * 
     */
    public static void runFormatterScript(final DataManager dataMgr,
            final TextProductManager productMgr, final String productName,
            final String dbId, final String vtecMode,
            final TextProductFinishListener finish) {
        /*
         * we wrap this inside an eclipse Job so that we aren't blocking the UI
         * thread by waiting for the varDict result. Waiting on the varDict on
         * the UI thread would cause a deadlock because ValuesDialog requires
         * use of VizApp.runAsync.
         */
        Job runFormatterJob = new Job("Running product formatter") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                String activeTable = getActiveTableName(dataMgr);
                int testMode = getTestMode(dataMgr, vtecMode);
                String shortVtec = getVTECModeCode(vtecMode);
                String name = productMgr.getModuleName(productName);
                String time = getDRTString();
                String varDict = dataMgr.getTextProductMgr().obtainVarDictSelections(name,
                        dataMgr, dbId);

                try {
                    runFormatterScript(name, shortVtec, dbId, varDict,
                            activeTable, time, testMode, finish, dataMgr);
                } catch (Exception e) {
                    statusHandler.error(String.format(
                            "Error running text formatter %s", productName), e);
                }

                return Status.OK_STATUS;
            }
        };
        runFormatterJob.setSystem(true);
        runFormatterJob.schedule();
    }

    /**
     * Convenience method for SmartScript based script objects to run text
     * formatters. Uses native formatter execution framework, so product may be
     * queued and not run until other formatters launched interactively have
     * completed execution.
     * 
     * @param productName
     *            The display name of the formatter to execute.
     * @param dbId
     *            String form of the {@code DatabaseID} of the source database.
     * @param varDict
     *            String form of the formatters variable dictionary.
     * @param vtecMode
     *            VTEC mode--operational, experimental, test, etc.
     * @param dataMgr
     *            the {@code DataManager} instance to use.
     * @param listener
     *            listener to fire when formatter finishes generating product
     * @throws SimulatedTimeProhibitedOpException
     */
    public static void callFromSmartScript(String productName, String dbId,
            String varDict, String vtecMode, DataManager dataMgr,
            TextProductFinishListener listener)
            throws SimulatedTimeProhibitedOpException {
        String activeTable = getActiveTableName(dataMgr);
        int testMode = getTestMode(dataMgr, vtecMode);
        String shortVtec = getVTECModeCode(vtecMode);
        String name = dataMgr.getTextProductMgr().getModuleName(productName);
        String time = getDRTString();
        runFormatterScript(name, shortVtec, dbId, varDict, activeTable, time,
                testMode, listener, dataMgr);
    }

    public static void runFormatterScript(String name, String vtecMode,
            String databaseID, String varDict, String vtecActiveTable,
            String drtTime, int testMode, TextProductFinishListener finish,
            DataManager dataMgr) throws SimulatedTimeProhibitedOpException {
        if (!SimulatedTimeOperations.isTransmitAllowed()) {
            throw SimulatedTimeOperations
                    .constructProhibitedOpException("GFE Text formatters");
        }

        TextFormatter formatter = new TextFormatter(name, vtecMode, databaseID,
                varDict, vtecActiveTable, drtTime, testMode, finish, dataMgr);
        finish.textProductQueued(ConfigData.ProductStateEnum.Queued);
        TaskManager.getInstance().queueFormatter(formatter);
    }

    public static long getTimeRangeIntersectionDuration(TimeRange tr1,
            TimeRange tr2) {
        return tr1.intersection(tr2).getDuration();
    }

    private static String getActiveTableName(DataManager dataMgr) {
        ActiveTableMode atMode = ActiveTableMode.OPERATIONAL;
        CAVEMode caveMode = dataMgr.getOpMode();
        if (caveMode.equals(CAVEMode.PRACTICE)) {
            atMode = ActiveTableMode.PRACTICE;
        }
        return atMode.name();
    }

    private static String getVTECModeCode(String displayString) {
        String shortVtec = null;
        if (displayString != null) {
            if (displayString.length() == 1) {
                shortVtec = displayString;
            } else if (displayString.equals(VTEC_MODES[1])) {
                shortVtec = "O";
            } else if (displayString.equals(VTEC_MODES[2])) {
                shortVtec = "E";
            } else if (displayString.equals(VTEC_MODES[3])) {
                shortVtec = "X";
            } else if (displayString.equals(VTEC_MODES[5])) {
                shortVtec = "T";
            }
        }
        return shortVtec;
    }

    private static int getTestMode(DataManager dataMgr, String vtecMode) {
        int testMode = 0;

        CAVEMode caveMode = dataMgr.getOpMode();
        if (caveMode.equals(CAVEMode.TEST)) {
            testMode = 1;
        }

        if (VTEC_MODES[4].equals(vtecMode) || VTEC_MODES[5].equals(vtecMode)
                || "T".equals(vtecMode)) {
            testMode = 1;
        }

        return testMode;
    }

    private static String getDRTString() {
        String time = null;
        if (!SimulatedTime.getSystemTime().isRealTime()) {
            DateFormat gmtFormatter = new SimpleDateFormat("yyyyMMdd_HHmm");
            gmtFormatter.setTimeZone(TimeZone.getTimeZone("GMT"));
            time = gmtFormatter.format(SimulatedTime.getSystemTime().getTime());
        }
        return time;
    }
}
