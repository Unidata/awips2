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

import java.text.SimpleDateFormat;
import java.util.TimeZone;

import com.raytheon.uf.common.activetable.ActiveTableMode;
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
 * Sep 15, 2015  4858      dgilling    Prevent formatters from being run in DRT mode.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class FormatterUtil {

    public static final String[] VTEC_MODES = { "Normal: NoVTEC",
            "Normal: O-Vtec", "Normal: E-Vtec", "Normal: X-Vtec",
            "Test: NoVTEC", "Test: T-Vtec" };

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
     * @throws SimulatedTimeProhibitedOperationException
     */
    public static void runFormatterScript(DataManager dataMgr,
            TextProductManager productMgr, String productName, String dbId,
            String vtecMode, TextProductFinishListener finish)
            throws SimulatedTimeProhibitedOpException {

        int testMode = 0;
        ActiveTableMode atMode = ActiveTableMode.OPERATIONAL;
        CAVEMode caveMode = dataMgr.getOpMode();
        if (caveMode.equals(CAVEMode.TEST)) {
            testMode = 1;
        } else if (caveMode.equals(CAVEMode.PRACTICE)) {
            atMode = ActiveTableMode.PRACTICE;
        }

        String shortVtec = null;
        if (vtecMode.length() == 1) {
            shortVtec = vtecMode;
        } else if (vtecMode.equals(VTEC_MODES[1])) {
            shortVtec = "O";
        } else if (vtecMode.equals(VTEC_MODES[2])) {
            shortVtec = "E";
        } else if (vtecMode.equals(VTEC_MODES[3])) {
            shortVtec = "X";
        } else if (vtecMode.equals(VTEC_MODES[4])) {
            testMode = 1;
        } else if (vtecMode.equals(VTEC_MODES[5])) {
            shortVtec = "T";
            testMode = 1;
        }

        String name = productMgr.getModuleName(productName);
        String varDict = productMgr.getVarDict(productName, dataMgr, dbId);

        if (varDict != null) {
            String time = null;
            if (!SimulatedTime.getSystemTime().isRealTime()) {
                SimpleDateFormat gmtFormatter = new SimpleDateFormat(
                        "yyyyMMdd_HHmm");
                gmtFormatter.setTimeZone(TimeZone.getTimeZone("GMT"));

                time = gmtFormatter.format(SimulatedTime.getSystemTime()
                        .getTime());
            }
            runFormatterScript(name, shortVtec, dbId, varDict, atMode.name(),
                    time, testMode, finish);
        } else {
            finish.textProductFinished("Formatter canceled",
                    ConfigData.ProductStateEnum.Finished);
        }
    }

    public static void runFormatterScript(String name, String vtecMode,
            String databaseID, String varDict, String vtecActiveTable,
            String drtTime, int testMode, TextProductFinishListener finish)
            throws SimulatedTimeProhibitedOpException {
        if (!SimulatedTimeOperations.isTransmitAllowed()) {
            throw SimulatedTimeOperations
                    .constructProhibitedOpException("GFE Text formatters");
        }

        TextFormatter formatter = new TextFormatter(name, vtecMode, databaseID,
                varDict, vtecActiveTable, drtTime, testMode, finish);
        finish.textProductQueued(ConfigData.ProductStateEnum.Queued);
        TaskManager.getInstance().queueFormatter(formatter);
    }

    public static long getTimeRangeIntersectionDuration(TimeRange tr1,
            TimeRange tr2) {
        return tr1.intersection(tr2).getDuration();
    }
}
