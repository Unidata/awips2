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
import java.util.Calendar;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.texteditor.TextDisplayModel;
import com.raytheon.viz.texteditor.util.SiteAbbreviationUtil;
import com.raytheon.viz.texteditor.util.VtecObject;
import com.raytheon.viz.texteditor.util.VtecUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2010            jsanchez     Initial creation
 * Apr 25, 2014 DR 16668   D. Friedman Only notify on NEW products.
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

    private Shell parent;

    static {
        ALLOWED_TYPES.add("SMW");
        ALLOWED_TYPES.add("SVR");
        ALLOWED_TYPES.add("TOR");
        ALLOWED_TYPES.add("FFW");
        ALLOWED_TYPES.add("FLW");
    }

    public NotifyExpiration(Shell parent) {
        this.parent = parent;
    }

    public void add(String warning) {
        String[] nnnxxx = TextDisplayModel.getNnnXxx(warning);
        String siteNode = SiteAbbreviationUtil.getSiteNode(nnnxxx[1]);
        String productId = siteNode + nnnxxx[0] + nnnxxx[1];

        if (!ALLOWED_TYPES.contains(nnnxxx[0])) {
            return;
        }

        VtecObject vtecObject = VtecUtil.parseMessage(warning);
        if (vtecObject == null || !"NEW".equals(vtecObject.getAction())) {
            return;
        }
        Calendar expire = vtecObject.getEndTime();
        TimerTask notify = new NotifyExpirationTask(parent, productId,
                vtecObject);
        Timer timer = new Timer();
        expire.add(Calendar.MINUTE, -10);
        timer.schedule(notify, expire.getTime());
        tasks.add(notify);
    }

    public void checkExpirationNotices(Shell shell) {
        if (shell != null && !tasks.isEmpty()) {
            if (MessageDialog.openQuestion(shell, EXIT_TITLE, EXIT_MSG)) {
                for (TimerTask t : tasks) {
                    t.cancel();
                }
                tasks.clear();
            }
        }

    }

    public static void remove(NotifyExpirationTask task) {
        if (tasks.contains(task)) {
            tasks.remove(task);
        }
    }
}