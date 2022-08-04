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
package com.raytheon.viz.texteditor.notify;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.TimerTask;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.GetActiveTableRequest;
import com.raytheon.uf.common.activetable.GetActiveTableResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.vtec.VtecObject;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.alarmalert.util.AlarmBeepJob;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.dialogs.SWTMessageBox;

/**
 * Notify Expiration Task
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 25, 2010            jsanchez     Initial creation
 * Sep 21, 2010 2187       cjeanbap    Removed hard coded values of
 *                                     tables names.
 * Dec 20, 2010 7210       cjeanbap    Added non-blocking dialog.
 * Apr 25, 2014 DR 16668   D. Friedman Handle partial cancellations
 * Apr 28, 2015 4027       randerso    Expunged Calendar from ActiveTableRecord
 * Nov 03, 2015 5086       rferrel     Generate sound while displaying warning expire message.
 * Jan 26, 2016 5054       randerso    Changed to use display as parent,
 *                                     code cleanup
 * Nov 03, 2016  5934      randerso    Moved VtecObject and VtecUtil to a
 *                                     separate plugin.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class NotifyExpirationTask extends TimerTask {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NotifyExpirationTask.class);

    /**
     * Miliseconds in two days
     */
    private static final long TWO_DAYS = 2 * TimeUtil.MILLIS_PER_DAY;

    private String office;

    private String phenSig;

    private String etn;

    private String message;

    private Display display;

    /**
     * @param display
     * @param productId
     * @param vtecObject
     */
    public NotifyExpirationTask(Display display, String productId,
            VtecObject vtecObject) {
        this.display = display;
        office = vtecObject.getOffice();
        phenSig = vtecObject.getPhenomena() + "."
                + vtecObject.getSignificance();
        etn = pad(vtecObject.getSequence());
        DateFormat dateFormat = new SimpleDateFormat("HH:mm");
        this.message = "Your watch or warning will expire soon!" + "\n"
                + "Product ID: " + productId + "\n" + "Expiration time: "
                + dateFormat.format(vtecObject.getEndTime().getTime());
    }

    @Override
    public void run() {
        if (!isCanceled()) {
            final AlarmBeepJob abj = new AlarmBeepJob("expire", 0);
            abj.schedule();
            display.asyncExec(new Runnable() {
                @Override
                public void run() {
                    SWTMessageBox mb = new SWTMessageBox(
                            NotifyExpirationTask.this.display,
                            "Watch/Warning Expires Soon", message,
                            SWT.ICON_WARNING | SWT.OK);
                    mb.addCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            abj.cancel();
                        }
                    });
                    mb.open();
                }
            });
        }
        this.cancel();
        NotifyExpiration.remove(this);
    }

    private boolean isCanceled() {
        GetActiveTableRequest req = new GetActiveTableRequest();

        if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)) {
            req.setMode(ActiveTableMode.PRACTICE);
        } else {
            req.setMode(ActiveTableMode.OPERATIONAL);
        }

        req.setSiteID(office);
        req.setPhensigList(phenSig);
        req.setEtn(etn);

        java.util.List<ActiveTableRecord> activeTable = null;
        try {
            GetActiveTableResponse resp = (GetActiveTableResponse) ThriftClient
                    .sendRequest(req);
            activeTable = resp.getActiveTable();
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR,
                    "Error querying active table for " + office + "." + phenSig
                            + "." + etn, e);
        }

        if (activeTable != null) {
            boolean haveCAN = false;
            for (ActiveTableRecord record : activeTable) {
                if (record != null) {
                    /*
                     * textNotifyExpiration.tcl ln 106: If any of the found
                     * products are less than 48 hours old,return true.
                     */
                    if ("CAN".equals(record.getAct())
                            && ((System.currentTimeMillis() - record
                                    .getIssueTime().getTime()) < TWO_DAYS)) {
                        haveCAN = true;
                    } else if ("CON".equals(record.getAct())) {
                        /*
                         * If there CANs and the event is still active, there
                         * should be some CONs. Thus, it is not necessary to
                         * check for every other kind of non-terminal action.
                         */
                        return false;
                    }
                }
            }
            return haveCAN;
        }

        return false;
    }

    private String pad(int value) {
        String rval = "";
        String str = Integer.toString(value);
        for (int i = 0; i < (4 - str.length()); i++) {
            rval += "0";
        }

        return rval + str;
    }
}
