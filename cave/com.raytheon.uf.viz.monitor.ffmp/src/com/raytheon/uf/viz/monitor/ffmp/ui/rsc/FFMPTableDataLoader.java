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
package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

import java.util.Date;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPGap;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord.ZOOM;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.ffmp.FFMPMonitor;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.BasinTrendDlg;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FFMPTableData;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.FfmpBasinTableDlg;

/**
 * Loads the table data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 13, 2011            dhladky     Initial creation.
 * Jul 31, 2012 14517      mpduff      Fix for Rapid slider changes
 * 02/01/13     1569       D. Hladky   Added constants
 * Feb 28, 2013  1729      dhladky     Removed un-necessary logging.
 * Jun 28, 2018  6796      mduff       Get DataTime from ResourceData.
 * Aug 07, 2018  6720      njensen     Pass along updateTime if paintTime is null
 * Aug 24, 2018  7413      mduff       Removed table update logic.
 * 
 * </pre>
 * 
 * @author dhladky
 */

public class FFMPTableDataLoader extends Thread {

    private IMonitorEvent fme = null;

    private FFMPResource resource = null;

    private BasinTrendDlg basinTrendDlg = null;

    private boolean sourceUpdate = false;

    private Date date = null;

    private FfmpBasinTableDlg callback = null;

    private boolean isDone = false;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FFMPTableDataLoader.class);

    /**
     * Public only used constructor
     * 
     * @param fme
     * @param resource
     * @param basinTrendDlg
     * @param allowNewTableUpdate
     * @param sourceUpdate
     * @param date
     * @param callback
     */
    public FFMPTableDataLoader(IMonitorEvent fme, FFMPResource resource,
            BasinTrendDlg basinTrendDlg, boolean sourceUpdate, Date date,
            FfmpBasinTableDlg callback) {

        this.fme = fme;
        this.resource = resource;
        this.basinTrendDlg = basinTrendDlg;
        this.sourceUpdate = sourceUpdate;
        this.date = date;
        this.callback = callback;
    }

    @Override
    public void run() {

        if (fme.getSource() instanceof FFMPMonitor) {
            FFMPTableDataUpdate tableDataUpdate = new FFMPTableDataUpdate();
            FFMPMonitor ffmp = (FFMPMonitor) fme.getSource();

            // Get the time for the resource.
            DataTime updateTime = resource.getDescriptor()
                    .getTimeForResource(resource);
            if (resource.getTableTime() != null) {

                FFMPTableData tData = null;

                try {

                    if (updateTime == null) {
                        updateTime = resource.getPaintTime();
                    }
                    FFMPDrawable drawable = resource.getDrawable(updateTime);

                    String iHuc = null;
                    if (resource.lowestCenter == ZOOM.WFO) {
                        iHuc = resource.getHuc();
                    } else {
                        iHuc = FFMPRecord.ALL;
                    }
                    if ((drawable != null)
                            && (drawable.getDrawTime() == resource.getTime())) {
                        if (drawable.getTableData(iHuc) != null) {
                            // Getting data from the cache
                            tData = drawable.getTableData(iHuc);
                        }
                    }

                    if (tData == null) {
                        if (drawable == null) {
                            drawable = new FFMPDrawable(resource.getDomains());
                        }

                        // Data not in cache, get data.
                        double origDrawTime = resource.getTime();
                        if (resource.getPaintTime() != null) {
                            updateTime = resource.getPaintTime();
                        }
                        FFMPDataGenerator dg = new FFMPDataGenerator(ffmp,
                                resource, updateTime.getRefTime());
                        tData = dg.generateFFMPData();

                        drawable.setTableData(iHuc, tData);
                        drawable.setDrawTime(origDrawTime);
                    }
                } catch (Exception e) {

                    /*
                     * TODO need to have the table display no data Lee has code
                     * that will write a message across multiple table cells
                     */
                    statusHandler.handle(Priority.WARN, "No Data available...",
                            e);
                }

                if (tData != null) {

                    // Check if the date has changed
                    if ((resource.getTableTime() != null) && (date != null)) {
                        if ((date.getTime() != resource.getTableTime()
                                .getTime()) || sourceUpdate) {

                            if ((basinTrendDlg != null)
                                    && (!basinTrendDlg.isDisposed())) {

                                tableDataUpdate.setFireGraph(true);
                                tableDataUpdate
                                        .setGraphPfaf(basinTrendDlg.getPfaf());
                                tableDataUpdate.setGraphTime(
                                        resource.getMostRecentTime());
                            }

                            sourceUpdate = false;
                        }
                    }

                    // Set the date/time and update the label on
                    // the dialog with the most recent time.
                    // NOTE: this needs to be done after the
                    // above date check above
                    // setValidTime(resource.getMostRecentTime());

                    // Reset the data in the table.
                    tableDataUpdate.setTableData(tData);
                }
            }
            // valid time is always the most recent for this frame
            tableDataUpdate.setValidTime(resource.getMostRecentTime());
            double gapVal = 0.00;
            for (FFMPGap gap : resource.getGaps()) {
                gapVal += gap.getGap();
            }
            gapVal = gapVal / 60;
            // Update the gap label value
            tableDataUpdate.setGapValueLabel(gapVal);
            tableDataUpdate.setSourceUpdate(sourceUpdate);

            isDone = true;
            callback.tableDataUpdateComplete(tableDataUpdate);
        }
    }

    public boolean isDone() {
        return isDone;
    }
}
