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

import java.util.ArrayList;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.texteditor.TextDisplayModel;
import com.raytheon.viz.texteditor.util.VtecObject;
import com.raytheon.viz.texteditor.util.VtecUtil;

/**
 * Schedules {@link NotifyExpirationTask}s to notify a user when there are ten
 * (10) minutes left before a recently created watch or warning expires. Also
 * notifies the user if there are any remaining pending NotifyExpirationTasks
 * before closing warngen.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2010            jsanchez    Initial creation
 * Apr 25, 2014 DR 16668   D. Friedman Only notify on NEW products.
 * Jan 26, 2016 5054       randerso    Changed to use display as parent
 * Mar 23, 2016 5343       bkowal      Updated site retrieval for compatibility with non-standard
 *                                     products.
 * Mar 30, 2016 5513       randerso    Fixed to return the status of the close prompt
 * May 12, 2016 5621       jschmid     Changed timer to use simulated time alert (or immediate)
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class NotifyExpiration {

    private static final String EXIT_MSG = "There are still warning expiration notices running. Do you want to stop them?";

    private static final String EXIT_TITLE = "Exiting Text Workstation";

    private static ArrayList<TimerTask> tasks = new ArrayList<TimerTask>();

    private static ArrayList<String> ALLOWED_TYPES = new ArrayList<String>(5);

    private Display display;

    static {
        ALLOWED_TYPES.add("SMW");
        ALLOWED_TYPES.add("SVR");
        ALLOWED_TYPES.add("TOR");
        ALLOWED_TYPES.add("FFW");
        ALLOWED_TYPES.add("FLW");
    }

    public NotifyExpiration(Display display) {
        this.display = display;
    }

    public void add(String warning) {
        String[] nnnxxx = TextDisplayModel.getNnnXxx(warning);
        String siteNode = TextDisplayModel.getSiteNode(warning, nnnxxx[1]);
        String productId = siteNode + nnnxxx[0] + nnnxxx[1];

        if (!ALLOWED_TYPES.contains(nnnxxx[0])) {
            return;
        }

        VtecObject vtecObject = VtecUtil.parseMessage(warning);
        if (vtecObject == null || !"NEW".equals(vtecObject.getAction())) {
            return;
        }

        // Note: EndTimes defined to occur on 15 minute boundaries by WarnGen
        long msToWarningEndTime = (vtecObject.getEndTime().getTimeInMillis() - SimulatedTime
                .getSystemTime().getMillis());
        long msToAlert = (msToWarningEndTime - (10 * TimeUtil.MILLIS_PER_MINUTE));

        Timer timer = new Timer();
        TimerTask notify = new NotifyExpirationTask(display, productId,
                vtecObject);

        if (msToAlert < 0) { // Alert time must be now or in the future
            msToAlert = 0;
        }
        timer.schedule(notify, msToAlert);
        tasks.add(notify);
    }

    public boolean checkExpirationNotices(Shell shell) {
        boolean shouldClose = true;
        if (shell != null && !tasks.isEmpty()) {
            shouldClose = MessageDialog.openQuestion(shell, EXIT_TITLE,
                    EXIT_MSG);
            if (shouldClose) {
                for (TimerTask t : tasks) {
                    t.cancel();
                }
                tasks.clear();
            }
        }
        return shouldClose;
    }

    public static void remove(NotifyExpirationTask task) {
        if (tasks.contains(task)) {
            tasks.remove(task);
        }
    }
}