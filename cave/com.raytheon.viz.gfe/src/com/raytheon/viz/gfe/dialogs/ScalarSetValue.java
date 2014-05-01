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
package com.raytheon.viz.gfe.dialogs;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.viz.gfe.core.msgs.IColorTableModifiedListener;
import com.raytheon.viz.gfe.core.msgs.IPickupValueChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.visual.SetValueScalarVisual;

/**
 * Provides UI to set a scalar wx value for the set value dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2009 #1318      randerso     Initial creation
 * Feb 20, 2012 #346       dgilling     Create PickupChangedJob to fix 
 *                                      Invalid Thread Access exceptions 
 *                                      in pickupValueChanged.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ScalarSetValue extends AbstractSetValue implements
        IPickupValueChangedListener, IColorTableModifiedListener {

    private class PickupChangedJob extends UIJob {

        private WxValue pickupValue;

        public PickupChangedJob() {
            super("PickupValueChanged");
            setSystem(true);
        }

        public void setPickupValue(WxValue newValue) {
            this.pickupValue = newValue;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.ui.progress.UIJob#runInUIThread(org.eclipse.core.runtime
         * .IProgressMonitor)
         */
        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            if ((!monitor.isCanceled())
                    && (!ScalarSetValue.this.entryField.isDisposed())
                    && (!ScalarSetValue.this.mainDA.isDisposed())) {
                ScalarSetValue.this.entryField.setText(pickupValue.toString());
                ScalarSetValue.this.mainDA.redraw();
                return Status.OK_STATUS;
            }

            return Status.CANCEL_STATUS;
        }
    }

    private Canvas mainDA;

    private SetValueScalarVisual mainVisual;

    private Composite entryFrame;

    private Label entryLabel;

    private Text entryField;

    private PickupChangedJob pickupJob;

    /**
     * Constructor
     * 
     * @param parent
     *            composite to contain the controls
     * @param parm
     *            the parm to be acted on
     */
    public ScalarSetValue(Composite parent, Parm parm) {
        super(parent, parm);
        GridLayout layout = (GridLayout) getLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        layout.verticalSpacing = 0;

        this.mainDA = new Canvas(this, SWT.NONE);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.heightHint = 300;
        layoutData.widthHint = 300;
        mainDA.setLayoutData(layoutData);
        mainDA.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        mainDA.addPaintListener(new PaintListener() {

            @Override
            public void paintControl(PaintEvent e) {
                paint(e);
            }

        });

        pickupJob = new PickupChangedJob();

        setParm(parm);
    }

    protected void paint(PaintEvent e) {
        mainVisual.render(e);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.swt.widgets.Widget#dispose()
     */
    @Override
    public void dispose() {
        parm.getListeners().removePickupValueChangedListener(this);
        parm.getListeners().removeColorTableModifiedListener(this);
        pickupJob.cancel();
        mainVisual.dispose();

        super.dispose();
    }

    private void setParm(Parm parm) {
        if (parm == null || !parm.isMutable()) {
            return;
        }

        parm.getListeners().addPickupValueChangedListener(this);
        parm.getListeners().addColorTableModifiedListener(this);

        // create visuals and edit tools
        this.mainVisual = new SetValueScalarVisual(this.mainDA, parm);
        // this.mainET = Graphics.SetValueTool(this.dBSubsystem,
        // this.mainDA.drawingAreaInterface(), this.parm);

        // Add a text entry field for Scalar Value
        this.entryFrame = new Composite(this, SWT.BORDER);
        GridLayout layout = new GridLayout(2, false);
        layout.marginHeight = 0;
        entryFrame.setLayout(layout);
        GridData layoutData = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        entryFrame.setLayoutData(layoutData);

        this.entryLabel = new Label(this.entryFrame, SWT.NONE);
        entryLabel.setText("Enter Value: ");

        this.entryField = new Text(this.entryFrame, SWT.BORDER);
        layoutData = new GridData(40, SWT.DEFAULT);
        entryField.setLayoutData(layoutData);
        entryField.addKeyListener(new KeyAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt
             * .events.KeyEvent)
             */
            @Override
            public void keyReleased(KeyEvent e) {
                if (e.character == SWT.CR) {
                    entryChanged();
                }
            }

        });
        entryField.addFocusListener(new FocusAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt
             * .events.FocusEvent)
             */
            @Override
            public void focusLost(FocusEvent e) {
                entryChanged();
            }

        });

        pickupValueChanged(parm, parm.getParmState().getPickUpValue());

        this.mainDA.redraw();
    }

    protected void entryChanged() {
        ScalarWxValue pickupValue = (ScalarWxValue) parm.getParmState()
                .getPickUpValue();

        // Update the Scalar pickup Value
        try {
            float floatVal = Float.parseFloat(entryField.getText());
            if (floatVal >= parm.getGridInfo().getMinValue()
                    && floatVal <= parm.getGridInfo().getMaxValue()) {
                pickupValue = new ScalarWxValue(floatVal, parm);
                parm.getParmState().setPickUpValue(pickupValue);
            } else {
                pickupValueChanged(parm, pickupValue);
            }
        } catch (NumberFormatException e) {
            pickupValueChanged(parm, pickupValue);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.IPickupValueChangedListener#pickupValueChanged
     * (com.raytheon.viz.gfe.core.parm.Parm,
     * com.raytheon.viz.gfe.core.wxvalue.WxValue)
     */
    @Override
    public void pickupValueChanged(Parm parm, WxValue pickUpValue) {
        pickupJob.setPickupValue(pickUpValue);
        pickupJob.schedule();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.IColorTableModifiedListener#colorTableModified
     * (com.raytheon.viz.gfe.core.parm.Parm)
     */
    @Override
    public void colorTableModified(Parm parm) {
        mainDA.redraw();
    }

}
