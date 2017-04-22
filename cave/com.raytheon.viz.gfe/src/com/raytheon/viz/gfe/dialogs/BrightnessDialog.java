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

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * A dialog to set the brightness of a weather element.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2010            wldougher     Initial creation
 * Oct 30, 2012 1298       rferrel     Code cleanup for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

public class BrightnessDialog extends CaveJFACEDialog {

    // A width hint that keeps part of the title from being cut off
    private final int WIDTH_HINT = 235;

    /**
     * A label for displaying the scale's value, that moves with the scale thumb
     */
    private Label driftingLabel;

    /**
     * A scale for adjusting brightness.
     */
    private Scale scale;

    /**
     * A text field; an alternate way of changing the brightness.
     */
    private Text field;

    /**
     * The brightness for the weather element (0-1).
     */
    private float brightness;

    /**
     * The brightness value when the dialog is first opened. Used by field
     * validation code.
     */
    private float originalBrightness;

    /**
     * The composite for the main dialog area.
     */
    private Composite comp;

    private Parm parm;

    private ResourcePair pair;

    /**
     * Multiplier to keep driftingLabel from very ends of the dialog.
     */
    private final float FUDGE_FACTOR = 0.8f;

    /**
     * Constructor.
     * 
     * @param parent
     */
    public BrightnessDialog(Shell parent, Parm parm) {
        super(parent);
        this.parm = parm;
        this.pair = getResourcePair(parm);
    }

    /**
     * Sets the title for the dialog.
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     *      .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Set Color Table Brightness");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        comp = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout(1, false);
        comp.setLayout(layout);

        Label title2 = new Label(comp, SWT.HORIZONTAL | SWT.CENTER);
        title2.setText("Color Table Brightness for "
                + parm.getParmID().compositeNameUI());
        GridData t2LayoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        t2LayoutData.widthHint = WIDTH_HINT;
        title2.setLayoutData(t2LayoutData);

        // Label that follows the thumb of scale
        driftingLabel = new Label(comp, SWT.HORIZONTAL);
        GridData dlLayoutData = new GridData(SWT.LEFT, SWT.DEFAULT, false,
                false);
        driftingLabel.setLayoutData(dlLayoutData);

        // Scale for user to adjust brightness
        scale = new Scale(comp, SWT.HORIZONTAL);
        GridData scaleLayoutData = new GridData(GridData.FILL, SWT.DEFAULT,
                true, false);
        scale.setLayoutData(scaleLayoutData);
        scale.setMinimum(0);
        scale.setMaximum(100);
        scale.setIncrement(10);
        scale.setPageIncrement(10);
        scale.addSelectionListener(new SelectionAdapter() {
            /**
             * When the user manipulates the scale, update the other controls
             * and the brightness value.
             * 
             * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent event) {
                int val = scale.getSelection();
                setPct(val);
                updateDlgControls();
            }
        });

        // A group to give a raised appearance to the entry field and its label
        Group bevelComp = new Group(comp, SWT.SHADOW_OUT);
        GridLayout bevLayout = new GridLayout(2, true);
        bevelComp.setLayout(bevLayout);
        GridData bevLayoutData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                false);
        bevelComp.setLayoutData(bevLayoutData);

        // Label for the text entry field
        Label fieldLabel = new Label(bevelComp, SWT.HORIZONTAL);
        fieldLabel.setText("Enter Value:");
        GridData fieldLabelLayoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT,
                false, false);
        fieldLabel.setLayoutData(fieldLabelLayoutData);

        // Set up the text entry field
        field = new Text(bevelComp, SWT.LEFT | SWT.SINGLE);
        GridData fieldLayoutData = new GridData(SWT.FILL, SWT.DEFAULT, false,
                false);
        field.setLayoutData(fieldLayoutData);
        field.addTraverseListener(new TraverseListener() {

            /**
             * When the user presses the <Enter> key in the field, parse the
             * field and set the brightness and other controls.
             * 
             * @see org.eclipse.swt.events.TraverseListener#keyTraversed(org.eclipse.swt.events.TraverseEvent)
             */
            @Override
            public void keyTraversed(TraverseEvent event) {
                if (event.detail == SWT.TRAVERSE_RETURN) {
                    int val;
                    try {
                        val = Integer.parseInt(field.getText());
                        setPct(val);
                    } catch (NumberFormatException exc) {
                        setPct((int) (originalBrightness * 100));
                    }
                    updateDlgControls();
                }
            }

        });

        field.addControlListener(new ControlAdapter() {
            /**
             * Until scale has been assigned a size, updateDlgControls() can't
             * set the location of driftingLabel, so it appears at the left edge
             * of the dialog.
             * <p>
             * This method is triggered when the dialog is first given a size.
             * It invokes updateDlgControls() to position driftingLabel, then
             * removes this listener because it is no longer needed.
             * 
             * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
             */
            @Override
            public void controlResized(ControlEvent e) {
                updateDlgControls();
                field.removeControlListener(this);
            }
        });

        brightness = pair.getResource().getCapability(ImagingCapability.class)
                .getBrightness();
        originalBrightness = brightness;

        return comp;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        super.buttonPressed(buttonId);
    }

    /**
     * We only want a "Dismiss" button, and we don't want it to be the default
     * because the field is right before it and users have to hit <Enter> there
     * to have their changes applied.
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.CANCEL_ID, "Dismiss", false);
    }

    /**
     * Store the brightness percentage pct as an brightness value, and apply the
     * change immediately to the grid.
     * 
     * @param pct
     *            The percentage (0-100) to assign.
     */
    protected void setPct(int pct) {
        pct = Math.round(pct / 10.0f) * 10;
        pct = Math.max(0, pct);
        pct = Math.min(pct, 100);
        brightness = pct / 100.0f;

        ImagingCapability imagingCapability = pair.getResource().getCapability(
                ImagingCapability.class);
        if (imagingCapability.getBrightness() != brightness) {
            imagingCapability.setBrightness(brightness);
            pair.getResource().issueRefresh();
        }
    }

    /**
     * Get the resource pair for a parm
     * 
     * @return
     */
    protected ResourcePair getResourcePair(Parm parm) {
        DataManager dataManager = parm.getDataManager();
        ISpatialDisplayManager spatialDisplayManager = dataManager
                .getSpatialDisplayManager();
        ResourcePair pair = spatialDisplayManager.getResourcePair(parm);
        return pair;
    }

    /**
     * Get the current brightness value and use it to update the values of
     * slider, driftingLabel, and field, and the position of driftingLabel.
     */
    protected void updateDlgControls() {
        int value = (int) (brightness * 100.0f);
        String valString = Integer.toString(value).trim();

        scale.setSelection(value);
        driftingLabel.setText(valString);
        field.setText(valString);

        // Position the drifting label over the thumb of the scale
        Point dlSize = driftingLabel.getSize();
        Point dlPref = driftingLabel.computeSize(SWT.DEFAULT, dlSize.y, true);
        Point sSize = scale.getSize();
        Point dlLocation = driftingLabel.getLocation();
        dlLocation.x = (sSize.x) / 2;
        dlLocation.x += FUDGE_FACTOR * (brightness - 0.5) * sSize.x;

        driftingLabel.setSize(dlPref);
        driftingLabel.setLocation(dlLocation);

    }
}
