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
package com.raytheon.uf.viz.datadelivery.system;

import java.util.List;
import java.util.TimerTask;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.datadelivery.event.status.DataDeliverySystemStatus;
import com.raytheon.uf.common.datadelivery.event.status.DataDeliverySystemStatusDefinition;
import com.raytheon.uf.common.datadelivery.event.status.SystemStatusRequest;
import com.raytheon.uf.common.datadelivery.event.status.SystemStatusResponse;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryConstants;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.image.StatusImages;
import com.raytheon.uf.viz.core.image.StatusImages.StatusImage;

/**
 * Data Delivery system status composite.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 07, 2013    2180    mpduff      Initial creation
 * Nov 18, 2013    2387    skorolev    Add status refreshing
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
public class StatusComposite extends Composite implements ISystemStatusListener {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StatusComposite.class);

    /** Provider Group */
    private ScrolledComposite providerComp;

    /** Registry Group */
    private ScrolledComposite registryComp;

    /** Status images */
    private StatusImages images;

    /** Providers entry composite */
    private Composite providerEntryComp;

    /** Registers entry composite */
    private Composite regEntryComp;

    /** Label for time until refresh */
    private Label timeLbl;

    /** The refresh timer */
    public ScheduledExecutorService timer;

    /** Refresh period in minutes */
    public int refreshPeriod = 1;

    /** Counts the seconds until the next refresh */
    private int secCount = 60;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Composite
     * @param style
     *            Style Bits
     */
    public StatusComposite(Composite parent, int style) {
        super(parent, style);
        SystemRuleManager.getInstance().registerAsRefreshListener(this);
        init();
    }

    /**
     * Initialize class
     */
    private void init() {
        images = new StatusImages(this.getShell());

        GridLayout gl = new GridLayout(1, true);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        this.setLayout(gl);
        this.setLayoutData(gd);

        // Legend
        gl = new GridLayout(4, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        final Group legendGrp = new Group(this, SWT.NONE);
        legendGrp.setText(" Legend ");
        legendGrp.setLayout(gl);
        legendGrp.setLayoutData(gd);

        CLabel l1 = new CLabel(legendGrp, SWT.NONE);
        l1.setImage(images.getStatusImage(StatusImage.GREEN));
        l1.setText(DataDeliverySystemStatusDefinition.UP.getStatus() + "   ");

        CLabel l2 = new CLabel(legendGrp, SWT.NONE);
        l2.setImage(images.getStatusImage(StatusImage.YELLOW));
        l2.setText(DataDeliverySystemStatusDefinition.PROBLEM.getStatus()
                + "   ");

        CLabel l3 = new CLabel(legendGrp, SWT.NONE);
        l3.setImage(images.getStatusImage(StatusImage.RED));
        l3.setText(DataDeliverySystemStatusDefinition.DOWN.getStatus() + "   ");

        CLabel l4 = new CLabel(legendGrp, SWT.NONE);
        l4.setImage(images.getStatusImage(StatusImage.UNKNOWN));
        l4.setText(DataDeliverySystemStatusDefinition.UNKNOWN.getStatus() + "");

        gl = new GridLayout(2, true);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite sysComp = new Composite(this, SWT.NONE);
        sysComp.setLayout(gl);
        sysComp.setLayoutData(gd);

        // Registry group
        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group registryGrp = new Group(sysComp, SWT.NONE);
        registryGrp.setText(" Registry Status ");
        registryGrp.setLayout(gl);
        registryGrp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        registryComp = new ScrolledComposite(registryGrp, SWT.V_SCROLL
                | SWT.BORDER);
        registryComp.setLayout(gl);
        registryComp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        regEntryComp = new Composite(registryComp, SWT.NONE);
        regEntryComp.setLayout(gl);
        regEntryComp.setLayoutData(gd);

        registryComp.setContent(regEntryComp);
        registryComp.layout();

        // Provider group
        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group providerGroup = new Group(sysComp, SWT.NONE);
        providerGroup.setText(" Provider Status ");
        providerGroup.setLayout(gl);
        providerGroup.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        providerComp = new ScrolledComposite(providerGroup, SWT.V_SCROLL
                | SWT.BORDER);
        providerComp.setLayout(gl);
        providerComp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        providerEntryComp = new Composite(providerComp, SWT.NONE);
        providerEntryComp.setLayout(gl);
        providerEntryComp.setLayoutData(gd);

        providerComp.setContent(providerEntryComp);
        providerComp.layout();

        // Refresh group
        gl = new GridLayout(3, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite refreshGrp = new Composite(this, SWT.NONE);
        refreshGrp.setLayout(gl);
        refreshGrp.setLayoutData(gd);

        Label refPeriodLbl = new Label(refreshGrp, SWT.NONE);
        refPeriodLbl.setText("Refresh Period (Minutes):");

        String[] periodsText = { "1", "2", "3", "4", "5", "6", "7", "8", "9",
                "10" };
        final Combo periodSelectionCbo = new Combo(refreshGrp, SWT.BORDER
                | SWT.READ_ONLY);
        periodSelectionCbo.setItems(periodsText);
        // Set default period
        periodSelectionCbo.select(0);
        GridData typeComboData = new GridData(GridData.FILL);
        typeComboData.widthHint = 70;
        periodSelectionCbo.setLayoutData(typeComboData);

        Button nextRefreshBtn = new Button(refreshGrp, SWT.NONE);
        nextRefreshBtn.setText("Refresh");
        nextRefreshBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                statusRefresh();
                restartTimer();
            }
        });

        Label nextRefreshLbl = new Label(refreshGrp, SWT.NONE);
        nextRefreshLbl.setText("Next refresh:");
        final GridData ltData = new GridData(SWT.END, SWT.NONE, false, true);
        nextRefreshLbl.setLayoutData(ltData);

        // Time before next refresh.
        timeLbl = new Label(refreshGrp, SWT.NONE);
        final GridData lcData = new GridData(SWT.BEGINNING, SWT.NONE, false,
                true);
        timeLbl.setLayoutData(lcData);
        timeLbl.setText("");
        refreshGrp.layout();

        periodSelectionCbo.addSelectionListener(new SelectionAdapter() {

            public void widgetSelected(SelectionEvent e) {
                setRefreshPeriod(periodSelectionCbo.indexOf(periodSelectionCbo
                        .getText()) + 1);
                statusRefresh();
                restartTimer();
            };
        });

        populate();
    }

    /**
     * Populate this composite.
     */
    private void populate() {
        try {
            SystemStatusResponse response = (SystemStatusResponse) RequestRouter
                    .route(new SystemStatusRequest(),
                            DataDeliveryConstants.DATA_DELIVERY_SERVER);

            SystemStatusData statusData = new SystemStatusData(
                    response.getData());

            for (String systemType : statusData.getSystemTypes()) {
                if (systemType.equalsIgnoreCase("Provider")) {
                    List<DataDeliverySystemStatus> dataList = statusData
                            .getRecords(systemType);
                    addProviders(dataList);
                } else if (systemType.equalsIgnoreCase("Registry")) {
                    List<DataDeliverySystemStatus> dataList = statusData
                            .getRecords(systemType);
                    addRegistries(dataList);
                }
            }
        } catch (Exception e) {
            statusHandler.error("Error generating Status Data", e);
        }
    }

    /**
     * Add Provider status information.
     * 
     * @param dataList
     *            List of system statuses
     */
    private void addProviders(List<DataDeliverySystemStatus> dataList) {
        for (DataDeliverySystemStatus status : dataList) {
            CLabel l = new CLabel(providerEntryComp, SWT.NONE);
            l.setImage(getStatusImage(status));
            l.setText(status.getKey().getName());
        }
        providerEntryComp.setSize(providerEntryComp.computeSize(SWT.DEFAULT,
                SWT.DEFAULT));
        providerEntryComp.layout();
    }

    /**
     * Add Registry status information.
     * 
     * @param dataList
     *            List of registry statuses
     */
    private void addRegistries(List<DataDeliverySystemStatus> dataList) {
        for (DataDeliverySystemStatus status : dataList) {
            CLabel l = new CLabel(regEntryComp, SWT.NONE);
            l.setImage(getStatusImage(status));
            l.setText(status.getKey().getName());
        }
        regEntryComp
                .setSize(regEntryComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        regEntryComp.layout();
    }

    /**
     * Get status image.
     * 
     * @param status
     * @return
     */
    private Image getStatusImage(DataDeliverySystemStatus status) {
        Image retValue = null;
        if (DataDeliverySystemStatusDefinition.UP.getStatus().equals(
                status.getStatus())) {
            retValue = images.getStatusImage(StatusImage.GREEN);
        } else if (DataDeliverySystemStatusDefinition.PROBLEM.getStatus()
                .equals(status.getStatus())) {
            retValue = images.getStatusImage(StatusImage.YELLOW);
        } else if (DataDeliverySystemStatusDefinition.DOWN.getStatus().equals(
                status.getStatus())) {
            retValue = images.getStatusImage(StatusImage.RED);
        } else {
            retValue = images.getStatusImage(StatusImage.UNKNOWN);
        }
        return retValue;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.system.ISystemStatusListener#statusRefresh
     * ()
     */
    @Override
    public void statusRefresh() {
        for (Control control : providerEntryComp.getChildren()) {
            control.dispose();
        }
        for (Control control : regEntryComp.getChildren()) {
            control.dispose();
        }
        populate();
        registryComp.layout(true);
        providerComp.layout(true);
    }

    /**
     * Refresh inner class
     */
    private class RefreshTask extends TimerTask {
        @Override
        public void run() {
            secCount--;
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    SystemRuleManager.getInstance().fireTimeChangeListener();
                }
            });
            if (secCount == 0) {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        secCount = refreshPeriod * TimeUtil.SECONDS_PER_MINUTE;
                        SystemRuleManager.getInstance()
                                .fireStatusChangeListener();
                    }
                });
            }
        }
    }

    /**
     * Create the refresh timer.
     */
    public void createTimer() {
        long updateRate = TimeUtil.MILLIS_PER_SECOND;
        timer = Executors.newSingleThreadScheduledExecutor();
        timer.scheduleAtFixedRate(new RefreshTask(), 0, updateRate,
                TimeUnit.MILLISECONDS);
    }

    /**
     * Restart refresh timer.
     */
    protected void restartTimer() {
        timer.shutdownNow();
        secCount = refreshPeriod * TimeUtil.SECONDS_PER_MINUTE;
        createTimer();
    }

    /**
     * Get Refresh period
     * 
     * @return
     */
    public int getRefreshPeriod() {
        return refreshPeriod;
    }

    /**
     * Set Refresh period
     * 
     * @param refreshPeriod
     */
    public void setRefreshPeriod(int period) {
        this.refreshPeriod = period;
    }

    /*
     * Refresh time label.
     * 
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.system.ISystemStatusListener#
     * timeLabelRefresh()
     */
    @Override
    public void timeLabelRefresh() {
        if (timeLbl != null || !timeLbl.isDisposed()) {
            if (secCount < 120) {
                timeLbl.setText(secCount + " sec.");
            } else {
                timeLbl.setText((int) (secCount / 60) + 1 + " min.");
            }
            timeLbl.pack();
        }
    }
}
