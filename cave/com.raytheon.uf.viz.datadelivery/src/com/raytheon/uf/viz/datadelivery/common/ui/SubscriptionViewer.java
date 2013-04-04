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
package com.raytheon.uf.viz.datadelivery.common.ui;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionAction;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionTableComp;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionTableComp.SubscriptionType;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;

/**
 * Subscription Details Viewer
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2012            mpduff     Initial creation.
 * Mar 14, 2012   420      jpiatt     Update getCellText().
 * Jun 07, 2012   687      lvenable   Table data refactor.
 * Jun 21, 2012   736      djohnson   Change OPERATION_STATUS to OperationStatus.
 * Dec 03, 2012  1269      mpduff     Change to take a list of subscriptions for the view mode.
 * Dec 10, 2012  1300      bgonzale   Table filtering by dataset and provider.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionViewer extends AbstractViewDlg implements
        ISubscriptionAction {

    /**
     * Provider name associated with subscriptions. i.e. NOMADS, NCDC,...
     */
    private String providerName;

    /**
     * Dataset name that the subscriptions are associated with. i.e. gfs,
     * nam,...
     */
    private String datasetName;

    /** Table composite */
    private SubscriptionTableComp tableComp;

    private List<String> subscriptionNameList;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param providerName
     *            Provider name.
     * @param callback
     *            Dialog closed callback interface.
     * @param id
     *            Dialog ID. also, the name of the Dataset the dialog is
     *            associated with.
     */
    public SubscriptionViewer(Shell parent, String providerName,
            IDialogClosed callback, String id) {
        super(parent, callback, new Point(1100, 350), id);

        this.providerName = providerName;
        this.datasetName = id;

        if (this.datasetName != null) {
            setText("Data Delivery Subscription Viewer - " + this.datasetName);
        } else {
            setText("Data Delivery Subscription Viewer");
        }
    }

    /**
     * Constructor taking a list of subscriptions.
     *
     * @param parent
     *            Parent Shell
     * @param subscriptionList
     *            List of subscription names
     * @param callback
     *            IDialogClosed callback
     */
    public SubscriptionViewer(Shell parent, List<String> subscriptionList,
            IDialogClosed callback) {
        super(parent, callback, new Point(1100, 350), "bandwidth");
        setText("Subscriptions");
        this.subscriptionNameList = subscriptionList;
        this.providerName = null;
        this.datasetName = null;
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        return mainLayout;
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

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        // GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        // shell.setLayoutData(gd);
        shell.setMinimumSize(750, 320);

        // sortImages = new SortImages(this.getShell());
        // subManagerData = new
        // TableDataManager<SubscriptionManagerRowData>(TABLE_TYPE.SUBSCRIPTION);

        createTableControl();
        createCloseBtn();
    }

    /**
     * Create the table control.
     */
    private void createTableControl() {

        TableCompConfig tableConfig = new TableCompConfig(
                TABLE_TYPE.SUBSCRIPTION);
        tableConfig.setTableStyle(SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL
                | SWT.MULTI | SWT.FULL_SELECTION);
        tableConfig.setTableHeight(200);
        tableComp = new SubscriptionTableComp(shell, tableConfig, this,
                SubscriptionType.VIEWER);

        if (isTableFilteredByDatasetAndProvider()) {
            tableComp.populateActiveFilteredDataByDataSetAndProvider(
                    datasetName, providerName);
        } else {
            tableComp.setSubscriptionNameList(this.subscriptionNameList);
            tableComp.populateData();
        }
        tableComp.populateTable();
    }

    /**
     * @return true if the table is filtered by dataset and provider; false
     *         otherwise.
     */
    private boolean isTableFilteredByDatasetAndProvider() {
        return this.datasetName != null && !this.datasetName.isEmpty()
                && this.providerName != null && !this.providerName.isEmpty();
    }

    /**
     * Create the close button.
     */
    private void createCloseBtn() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 2;
        gl.marginWidth = 2;

        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);

        int buttonWidth = 75;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);

        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(btnData);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionAction#
     * activateButtonUpdate(java.lang.String)
     */
    @Override
    public void activateButtonUpdate(String text) {
        // Not used.
    }
}
