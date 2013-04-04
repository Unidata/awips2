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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

import com.raytheon.viz.ui.presenter.components.ComboBoxConf;


/**
 * This is the subscription delivery options composite. This class is intended 
 * to be extended so common classes can be created and shared.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 27, 2012     702    jpiatt      Initial creation.
 * Aug 29, 2012     223    mpduff      Refactor change.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */
public class DeliveryOptionsComp extends Composite {

    /** Deliver combo box. */
    private Combo deliverCombo;
    
    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public DeliveryOptionsComp(Composite parent) {
        super(parent, SWT.NONE);
        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        /*
         * Setup the layout for the composite
         */
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, true);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createDeliveryOptionsGroup();

    }
    
    /**
     * Create the Delivery Options Group
     */
    private void createDeliveryOptionsGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);
        Group deliveryOptionsGrp = new Group(this, SWT.NONE);
        deliveryOptionsGrp.setLayout(gl);
        deliveryOptionsGrp.setLayoutData(gd);
        deliveryOptionsGrp.setText("  Subscription Delivery Options  ");

        Label groupName = new Label(deliveryOptionsGrp, SWT.NONE);
        groupName.setText(" Deliver: ");
        
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        deliverCombo = new Combo(deliveryOptionsGrp, SWT.READ_ONLY);
        deliverCombo.setLayoutData(gd);
    }

    
    /**
     * Get deliver setting.
     * 
     * @return index
     */
    public int getDeliverSetting() {
        return deliverCombo.getSelectionIndex();
    }

    /**
     * Set deliver setting.
     * 
     * @param index
     */
    public void setDeliverSetting(int index) {
        deliverCombo.select(index);
    }

    public void setDeliveryOptions(String[] deliveryOptions) {
        if (deliveryOptions != null && deliveryOptions.length > 0) {
            this.deliverCombo.setItems(deliveryOptions);
            deliverCombo.select(0);
        }
    }

    public void setDeliveryConfig(ComboBoxConf deliveryCombo) {
        deliverCombo.setToolTipText(deliveryCombo.getToolTipText());        
    }

}
