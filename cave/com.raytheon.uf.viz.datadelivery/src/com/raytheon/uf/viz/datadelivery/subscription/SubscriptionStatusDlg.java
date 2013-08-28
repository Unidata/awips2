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
package com.raytheon.uf.viz.datadelivery.subscription;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.datadelivery.bandwidth.data.SubscriptionStatusSummary;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Subscription Status Summary dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2013    1653    mpduff      Initial creation
 * Aug 21, 2013    2248    bgonzale    Changed label to minutes.
 * Aug 28, 2013    2290    mpduff      Changed output to work with unscheduled subs.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionStatusDlg extends CaveSWTDialog {

    /** The subscription status object */
    private final SubscriptionStatusSummary summary;

    /** The status message */
    private final String statusMsg;

    /**
     * Constructor.
     * 
     * @param parent
     *            The parent shell
     * @param summary
     *            The summary object
     * @param statusMsg
     *            The status message
     */
    public SubscriptionStatusDlg(Shell parent,
            SubscriptionStatusSummary summary, String statusMsg) {
        super(parent, SWT.DIALOG_TRIM);

        this.summary = summary;
        this.statusMsg = statusMsg;
        this.setText("Subscription Summary");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayoutData()
     */
    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initializeComponents(Shell shell) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(gl);
        mainComp.setLayoutData(gd);

        Label msgLabel = new Label(mainComp, SWT.NONE);
        msgLabel.setText(statusMsg);
        msgLabel.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true, true));

        createSummary(mainComp);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 75;
        Button okBtn = new Button(mainComp, SWT.PUSH);
        okBtn.setLayoutData(gd);
        okBtn.setText(" OK ");
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Create the summary.
     */
    private void createSummary(Composite mainComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);
        Composite sumComp = new Composite(mainComp, SWT.NONE);
        sumComp.setLayout(gl);
        sumComp.setLayoutData(gd);

        Label l1 = new Label(sumComp, SWT.NONE);
        l1.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT, false, false));
        l1.setText("Dataset Size:  ");

        Label l11 = new Label(sumComp, SWT.NONE);
        l11.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT, true, false));
        l11.setText(summary.getDataSize() + " kB");

        Label l2 = new Label(sumComp, SWT.NONE);
        l2.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT, false, false));
        l2.setText("Latency:  ");

        Label l22 = new Label(sumComp, SWT.NONE);
        l22.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT, true, false));
        l22.setText(summary.getLatency() + " minutes");

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        Label l3 = new Label(sumComp, SWT.NONE);
        l3.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT, false, false));
        l3.setText("Start Time:  ");

        Calendar cal = TimeUtil.newGmtCalendar();
        Label l33 = new Label(sumComp, SWT.NONE);
        l33.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT, true, false));
        if (summary.getStartTime() != SubscriptionStatusSummary.MISSING_VALUE) {
            cal.setTimeInMillis(summary.getStartTime());
            l33.setText(sdf.format(cal.getTime()));
        } else {
            l33.setText("");

        }

        Label l4 = new Label(sumComp, SWT.NONE);
        l4.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT, false, false));
        l4.setText("End Time:  ");

        cal.setTimeInMillis(summary.getEndTime());
        Label l44 = new Label(sumComp, SWT.NONE);
        l44.setLayoutData(new GridData(SWT.LEFT, SWT.DEFAULT, true, false));
        if (summary.getEndTime() != SubscriptionStatusSummary.MISSING_VALUE) {
            l44.setText(sdf.format(cal.getTime()));
        } else {
            l44.setText("");
        }
    }
}
