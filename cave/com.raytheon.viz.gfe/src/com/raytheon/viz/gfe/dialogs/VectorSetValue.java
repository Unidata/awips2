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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.msgs.IPickupValueChangedListener;
import com.raytheon.viz.gfe.core.msgs.IVectorModeChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState.VectorMode;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.visual.SetValueVectorVisual;

/**
 * Provides UI to set a vector wx value for the set value dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2009 #1318      randerso     Initial creation
 * Feb 20, 2012 #346       dgilling     Create PickupChangedJob to fix 
 *                                      Invalid Thread Access exceptions 
 *                                      in pickupValueChanged.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class VectorSetValue extends AbstractSetValue implements
        IVectorModeChangedListener, IPickupValueChangedListener {

    private class PickupChangedJob extends UIJob {

        private WxValue pickupValue;

        private boolean redrawMainDA;

        public PickupChangedJob() {
            super("PickupValueChanged");
            setSystem(true);
        }

        public void setPickupValue(WxValue newValue, boolean redraw) {
            this.pickupValue = newValue;
            this.redrawMainDA = redraw;
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
                    && (!VectorSetValue.this.magField.isDisposed())
                    && (!VectorSetValue.this.dirField.isDisposed())
                    && (!VectorSetValue.this.mainDA.isDisposed())) {
                VectorWxValue value = (VectorWxValue) pickupValue;
                VectorSetValue.this.magField.setText(value.magToString());
                VectorSetValue.this.dirField.setText(value.dirToString());
                if (redrawMainDA) {
                    VectorSetValue.this.mainDA.redraw();
                }
                return Status.OK_STATUS;
            }

            return Status.CANCEL_STATUS;
        }
    }

    private Canvas mainDA;

    private SetValueVectorVisual mainVisual;

    private Text dirField;

    private Text magField;

    private Button[] modeButton;

    private PickupChangedJob pickupJob;

    /**
     * Constructor
     * 
     * @param parent
     *            composite to contain the controls
     * @param parm
     *            the parm to be acted on
     */
    public VectorSetValue(Composite parent, Parm parm) {
        super(parent, parm);
        GridLayout layout = (GridLayout) getLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        layout.verticalSpacing = 0;

        this.mainDA = new Canvas(this, SWT.NONE);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.heightHint = 150;
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

    private void setParm(Parm parm) {
        if (parm == null || !parm.isMutable()) {
            return;
        }
        parm.getListeners().addVectorModeChangedListener(this);
        parm.getListeners().addPickupValueChangedListener(this);

        resetDomain();

        // create visuals and edit tools
        // self._mainDA.setMaxWCDomain(
        // Graphics.SetValueVectorVisual_recommendedWorldDomain())
        // self._mainDA.setWCDomain(self._mainDA.maxWCDomain())

        mainVisual = new SetValueVectorVisual(mainDA, parm);
        // self._mainET = Graphics.SetValueVectorTool(self._parm,
        // self._mainDA.drawingAreaInterface(), self._dBSubsystem)

        // Add a text entry fields for Vector Magnitude and Direction
        // Add vector mode radio buttons
        Composite bottomFrame = new Composite(this, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        layout.verticalSpacing = 0;
        bottomFrame.setLayout(layout);
        GridData layoutData = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        bottomFrame.setLayoutData(layoutData);

        Composite dirFrame = new Composite(bottomFrame, SWT.BORDER);
        layout = new GridLayout(2, false);
        layout.marginHeight = 0;
        dirFrame.setLayout(layout);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dirFrame.setLayoutData(layoutData);

        Label dirLabel = new Label(dirFrame, SWT.NONE);
        dirLabel.setText("Enter Direction: ");

        this.dirField = new Text(dirFrame, SWT.BORDER);
        layoutData = new GridData(SWT.TRAIL, SWT.DEFAULT, true, false);
        layoutData.widthHint = 40;
        dirField.setLayoutData(layoutData);
        dirField.addKeyListener(new KeyAdapter() {

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
                    dirChanged();
                }
            }

        });
        dirField.addFocusListener(new FocusAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt
             * .events.FocusEvent)
             */
            @Override
            public void focusLost(FocusEvent e) {
                dirChanged();
            }

        });

        Composite magFrame = new Composite(bottomFrame, SWT.BORDER);
        layout = new GridLayout(2, false);
        layout.marginHeight = 0;
        magFrame.setLayout(layout);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        magFrame.setLayoutData(layoutData);

        Label magLabel = new Label(magFrame, SWT.NONE);
        magLabel.setText("Enter Magnitude: ");

        this.magField = new Text(magFrame, SWT.BORDER);
        layoutData = new GridData(SWT.TRAIL, SWT.DEFAULT, true, false);
        layoutData.widthHint = 40;
        magField.setLayoutData(layoutData);
        magField.addKeyListener(new KeyAdapter() {

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
                    magChanged();
                }
            }

        });
        magField.addFocusListener(new FocusAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt
             * .events.FocusEvent)
             */
            @Override
            public void focusLost(FocusEvent e) {
                magChanged();
            }

        });

        pickUpValueChanged(parm.getParmState().getPickUpValue());

        Composite modeFrame = new Composite(bottomFrame, SWT.BORDER);
        layout = new GridLayout(1, true);
        layout.marginHeight = 0;
        layout.verticalSpacing = 0;
        modeFrame.setLayout(layout);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        modeFrame.setLayoutData(layoutData);

        modeButton = new Button[VectorMode.values().length];
        for (VectorMode mode : VectorMode.values()) {
            Button b = new Button(modeFrame, SWT.RADIO);
            modeButton[mode.ordinal()] = b;
            b.setData(mode);
            b.setText(mode.toString());
            b.addSelectionListener(new SelectionAdapter() {

                /*
                 * (non-Javadoc)
                 * 
                 * @see
                 * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org
                 * .eclipse.swt.events.SelectionEvent)
                 */
                @Override
                public void widgetSelected(SelectionEvent e) {
                    modeChanged((VectorMode) e.widget.getData());
                }

            });
        }
        vectorModeChanged(parm, parm.getParmState().getVectorMode());
    }

    protected void magChanged() {
        // Update the Vector magnitude
        VectorWxValue pickupValue = (VectorWxValue) parm.getParmState()
                .getPickUpValue();
        try {
            float floatVal = Float.parseFloat(magField.getText());
            // LogStream.logUse("Magnitude Changed: ", floatVal)
            if (floatVal >= parm.getGridInfo().getMinValue()
                    && floatVal <= parm.getGridInfo().getMaxValue()) {
                float dirVal = pickupValue.getDir();
                pickupValue = new VectorWxValue(floatVal, dirVal, parm);
                parm.getParmState().setPickUpValue(pickupValue);
            } else {
                pickupValueChanged(parm, pickupValue);
            }
        } catch (NumberFormatException e) {
            pickupValueChanged(parm, pickupValue);
        }
    }

    protected void dirChanged() {
        // Update the Vector direction
        VectorWxValue pickupValue = (VectorWxValue) parm.getParmState()
                .getPickUpValue();
        try {
            float floatVal = Float.parseFloat(dirField.getText());
            // LogStream.logUse("Direction Changed: ", floatVal)
            if (floatVal >= 0 && floatVal <= 36) {
                float magVal = pickupValue.getMag();
                pickupValue = new VectorWxValue(magVal, floatVal * 10, parm);
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
     * @see org.eclipse.swt.widgets.Widget#dispose()
     */
    @Override
    public void dispose() {
        parm.getListeners().removeVectorModeChangedListener(this);
        parm.getListeners().removePickupValueChangedListener(this);
        pickupJob.cancel();
        mainVisual.dispose();
        super.dispose();
    }

    private void resetDomain() {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.IVectorModeChangedListener#vectorModeChanged
     * (com.raytheon.viz.gfe.core.parm.Parm,
     * com.raytheon.viz.gfe.core.parm.ParmState.VectorMode)
     */
    @Override
    public void vectorModeChanged(Parm parm, final VectorMode newMode) {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                for (VectorMode mode : VectorMode.values()) {
                    modeButton[mode.ordinal()].setSelection(mode
                            .equals(newMode));
                }

                dirField.setEnabled(!newMode.equals(VectorMode.MAGNITUDE));
                magField.setEnabled(!newMode.equals(VectorMode.DIRECTION));
            }
        });
    }

    protected void modeChanged(VectorMode mode) {
        // callback when mode radio buttons change

        // LogStream.logUse("Change Vector Mode: ", self._modeVar.get())
        dataManager.getParmOp().setVectorMode(mode);
    }

    private void pickUpValueChanged(WxValue pickUpValue) {
        pickupJob.setPickupValue(pickUpValue, false);
        pickupJob.schedule();
    }

    @Override
    public void pickupValueChanged(Parm parm, WxValue pickupValue) {
        pickupJob.setPickupValue(pickupValue, true);
        pickupJob.schedule();
    }
}
