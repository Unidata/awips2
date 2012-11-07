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

package com.raytheon.viz.ui.dialogs.colordialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.viz.ui.color.BackgroundColor;
import com.raytheon.viz.ui.color.IBackgroundColorChangedListener.BGColorMode;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This is the main dialog for the Background Color Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1/8/2008     706        Dan Fitch   Initial Creation.
 * 10/16/2012   1229       rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */
public class BackgroundColorDialog extends CaveSWTDialog implements
        IColorWheelAction {

    /**
     * Upper color wheel (composite object).
     */
    private ColorWheelComp backgroundColorWheel;

    /**
     * RGB radio button.
     */
    private Button rgbRdo;

    /**
     * HSB radio button.
     */
    private Button hsbRdo;

    /**
     * OK button.
     */
    private Button okBtn;

    /**
     * Apply button.
     */
    private Button applyBtn;

    /**
     * Cancel button.
     */
    private Button cancelBtn;

    /**
     * Title for the upper color wheel.
     */
    private final String upperWheelTitle = " Upper Color ";

    private RGB theColor;

    private IDisplayPaneContainer container;

    private BGColorMode mode;

    private BackgroundColor backgroundColor;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public BackgroundColorDialog(Shell parent, IDisplayPaneContainer container,
            BGColorMode mode) {
        super(parent, SWT.DIALOG_TRIM | SWT.MIN, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("Set Background Color");
        this.container = container;
        this.mode = mode;
        if (mode != null) {
            backgroundColor = BackgroundColor.getActivePerspectiveInstance();
        }

        if (container != null
                && container.getActiveDisplayPane().getRenderableDisplay() instanceof AbstractRenderableDisplay) {
            theColor = ((AbstractRenderableDisplay) container
                    .getActiveDisplayPane().getRenderableDisplay())
                    .getBackgroundColor();
        } else {
            if (mode != null) {
                theColor = backgroundColor.getColor(mode);
            } else {
                theColor = new RGB(0, 0, 0);
            }
        }
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        // Create the RGB and the HSB radio buttons.
        createRgbHsbButtons();

        ColorData initial = new ColorData(theColor, 255);

        // Create the color wheel for the display.
        backgroundColorWheel = new ColorWheelComp(shell, upperWheelTitle, true);
        backgroundColorWheel.setColor(initial);

        // Create the bottom control buttons.
        createBottomButtons();
    }

    /**
     * Create the RGB and the HSB radio buttons.
     */
    private void createRgbHsbButtons() {
        // Create a group to contain the RGB and HSB radio buttons.
        Group colorGroup = new Group(shell, SWT.NONE);
        colorGroup.setText(" Use color model: ");

        RowLayout groupRowLayout = new RowLayout();
        groupRowLayout.marginLeft = 10;
        groupRowLayout.marginRight = 10;
        groupRowLayout.spacing = 10;
        colorGroup.setLayout(groupRowLayout);

        // Create the RGB radio button. When the radio button is selected
        // update the upper and lower color wheel objects to display the RGB
        // sliders.
        rgbRdo = new Button(colorGroup, SWT.RADIO);
        rgbRdo.setText("RGB");
        rgbRdo.setSelection(true);
        rgbRdo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                changeColorWheels();
            }
        });

        // Create the HSB radio button. When the radio button is selected
        // update the upper and lower color wheel objects to display the HSB
        // sliders.
        hsbRdo = new Button(colorGroup, SWT.RADIO);
        hsbRdo.setText("HSB");
        hsbRdo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                changeColorWheels();
            }
        });
    }

    /**
     * Create the bottom control buttons.
     */
    private void createBottomButtons() {
        // Create a composite that will contain the control buttons.
        Composite bottonBtnComposite = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, true);
        gl.horizontalSpacing = 10;
        bottonBtnComposite.setLayout(gl);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        bottonBtnComposite.setLayoutData(gd);

        // Create the OK button
        gd = new GridData(GridData.FILL_HORIZONTAL);
        okBtn = new Button(bottonBtnComposite, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                applyColor(backgroundColorWheel.getColorData().rgbColor);
                shell.dispose();
            }
        });

        // Create the Apply button
        gd = new GridData(GridData.FILL_HORIZONTAL);
        applyBtn = new Button(bottonBtnComposite, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                applyColor(backgroundColorWheel.getColorData().rgbColor);
            }
        });

        // Create the Cancel button
        gd = new GridData(GridData.FILL_HORIZONTAL);
        cancelBtn = new Button(bottonBtnComposite, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                applyColor(theColor);
                shell.dispose();
            }
        });

    }

    /**
     * Change the color wheel objects to display either RGB or HSB.
     */
    private void changeColorWheels() {
        if (rgbRdo.getSelection() == true) {
            backgroundColorWheel.showRgbSliders(true);
        } else {
            backgroundColorWheel.showRgbSliders(false);
        }
    }

    /**
     * 
     * 
     * @param colorData
     *            The color data object containing the RGB color and the alpha
     *            value.
     */
    public void fillColor(ColorData colorData) {

    }

    /**
     * 
     * 
     * @param colorData
     *            The color data object containing the RGB color and the alpha
     *            value.
     * @param colorWheelTitle
     *            The title of the color wheel that is calling the method.
     */
    public void setColor(ColorData colorData, String colorWheelTitle) {

    }

    private void applyColor(RGB color) {
        if (mode == null) {
            for (IDisplayPane pane : container.getDisplayPanes()) {
                pane.getRenderableDisplay().setBackgroundColor(color);
            }
            container.refresh();
        } else if (backgroundColor != null) {
            backgroundColor.setColor(mode, color);
        }
    }

    public void setMode(BGColorMode mode) {
        this.mode = mode;
    }
}
