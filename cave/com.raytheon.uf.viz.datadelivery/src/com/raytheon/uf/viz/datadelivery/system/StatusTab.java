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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
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
import com.raytheon.uf.viz.core.image.StatusImages;
import com.raytheon.uf.viz.core.image.StatusImages.StatusImage;

/**
 * Data Delivery Status Tab
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2013    1655    mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class StatusTab extends SystemTab {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StatusTab.class);

    /** Provider Group */
    private Composite providerComp;

    /** Registry Group */
    private Composite registryComp;

    private StatusImages images;

    /**
     * Constructor.
     * 
     * @param parentComp
     *            The Composite holding these
     *            controlsDataDeliverySystemStatusDefinition
     */
    public StatusTab(Composite parentComp) {
        super(parentComp);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void init() {
        images = new StatusImages(getParentComp().getShell());

        GridLayout gl = new GridLayout(2, true);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gl.marginWidth = 0;

        Composite sysComp = new Composite(parentComp, SWT.NONE);
        sysComp.setLayout(gl);
        sysComp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group registryGrp = new Group(sysComp, SWT.NONE);
        registryGrp.setText(" Registry Status ");
        registryGrp.setLayout(gl);
        registryGrp.setLayoutData(gd);

        gl = new GridLayout(1, true);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        registryComp = new Composite(registryGrp, SWT.NONE);
        registryComp.setLayout(gl);
        registryComp.setLayoutData(gd);

        gl = new GridLayout(1, false);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group providerGroup = new Group(sysComp, SWT.NONE);
        providerGroup.setText(" Provider Status ");
        providerGroup.setLayout(gl);
        providerGroup.setLayoutData(gd);

        gl = new GridLayout(1, true);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        providerComp = new Composite(providerGroup, SWT.NONE);
        providerComp.setLayout(gl);
        providerComp.setLayoutData(gd);

        // Legend
        gl = new GridLayout(8, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group legendGrp = new Group(parentComp, SWT.NONE);
        legendGrp.setText(" Legend ");
        legendGrp.setLayout(gl);
        legendGrp.setLayoutData(gd);

        Label l = new Label(legendGrp, SWT.NONE);
        l.setImage(images.getStatusImage(StatusImage.GREEN));

        Label l2 = new Label(legendGrp, SWT.NONE);
        l2.setText(DataDeliverySystemStatusDefinition.UP.getStatus() + "   ");

        Label l3 = new Label(legendGrp, SWT.NONE);
        l3.setImage(images.getStatusImage(StatusImage.YELLOW));

        Label l4 = new Label(legendGrp, SWT.NONE);
        l4.setText(DataDeliverySystemStatusDefinition.PROBLEM.getStatus()
                + "   ");

        Label l5 = new Label(legendGrp, SWT.NONE);
        l5.setImage(images.getStatusImage(StatusImage.RED));

        Label l6 = new Label(legendGrp, SWT.NONE);
        l6.setText(DataDeliverySystemStatusDefinition.DOWN.getStatus() + "   ");

        Label l7 = new Label(legendGrp, SWT.NONE);
        l7.setImage(images.getStatusImage(StatusImage.UNKNOWN));

        Label l8 = new Label(legendGrp, SWT.NONE);
        l8.setText(DataDeliverySystemStatusDefinition.UNKNOWN.getStatus() + "");

        populate();
    }

    /**
     * Populate this tab.
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
            GridLayout gl = new GridLayout(2, false);
            GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            Composite entryComp = new Composite(providerComp, SWT.NONE);
            entryComp.setLayout(gl);
            entryComp.setLayoutData(gd);

            Label l = new Label(entryComp, SWT.NONE);
            if (DataDeliverySystemStatusDefinition.UP.getStatus().equals(
                    status.getStatus())) {
                l.setImage(images.getStatusImage(StatusImage.GREEN));
            } else if (DataDeliverySystemStatusDefinition.PROBLEM.getStatus()
                    .equals(status.getStatus())) {
                l.setImage(images.getStatusImage(StatusImage.YELLOW));
            } else if (DataDeliverySystemStatusDefinition.DOWN.getStatus()
                    .equals(status.getStatus())) {
                l.setImage(images.getStatusImage(StatusImage.RED));
            } else {
                l.setImage(images.getStatusImage(StatusImage.UNKNOWN));
            }

            Label nameLbl = new Label(entryComp, SWT.NONE);
            nameLbl.setText(status.getKey().getName());
        }
    }

    /**
     * Add Registry status information.
     * 
     * @param dataList
     *            List of registry statuses
     */
    private void addRegistries(List<DataDeliverySystemStatus> dataList) {
        for (DataDeliverySystemStatus status : dataList) {
            GridLayout gl = new GridLayout(2, false);
            GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            Composite entryComp = new Composite(registryComp, SWT.NONE);
            entryComp.setLayout(gl);
            entryComp.setLayoutData(gd);

            Label l = new Label(entryComp, SWT.NONE);
            if (DataDeliverySystemStatusDefinition.UP.getStatus().equals(
                    status.getStatus())) {
                l.setImage(images.getStatusImage(StatusImage.GREEN));
            } else if (DataDeliverySystemStatusDefinition.PROBLEM.getStatus()
                    .equals(status.getStatus())) {
                l.setImage(images.getStatusImage(StatusImage.YELLOW));
            } else if (DataDeliverySystemStatusDefinition.DOWN.getStatus()
                    .equals(status.getStatus())) {
                l.setImage(images.getStatusImage(StatusImage.RED));
            } else {
                l.setImage(images.getStatusImage(StatusImage.UNKNOWN));
            }

            Label nameLbl = new Label(entryComp, SWT.NONE);
            nameLbl.setText(status.getKey().getName());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getTabText() {
        return "System Status";
    }
}
