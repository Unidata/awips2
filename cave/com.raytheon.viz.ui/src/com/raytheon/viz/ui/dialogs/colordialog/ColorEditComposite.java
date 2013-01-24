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
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

import com.raytheon.uf.common.colormap.ColorMap;

/**
 * Composite for colormap editing
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 18, 2010            mschenke     Initial creation
 * Jan 10, 2013 15648      ryu         Editing GFE discrete colormap: a check button
 *                                     is added and duplicate entries in the colormap
 *                                     are removed when it is selected.
 *
 * </pre>
 *
 * @author mschenke
 * @version 1.0
 */

public class ColorEditComposite extends Composite implements IColorWheelAction,
        IColorBarAction {

    /**
     * Upper color wheel (composite object).
     */
    private ColorWheelComp upperColorWheel;

    /**
     * Lower color wheel (composite object).
     */
    private ColorWheelComp lowerColorWheel;

    /**
     * Color bar (composite object).
     */
    private ColorBar colorBar;

    /**
     * RGB radio button.
     */
    private Button rgbRdo;

    /**
     * HSB radio button.
     */
    private Button hsbRdo;

    /**
     * GFE discrete check button.
     */
    private Button gfeDiscreteCheck;

    /**
     * Title for the upper color wheel.
     */
    private final String upperWheelTitle = " Upper Color ";

    /**
     * Title for the lower color wheel.
     */
    private final String lowerWheelTitle = " Lower Color ";

    private ColorMap colorMap;

    private IColorEditCompCallback callback;

    /**
     * @param parent
     * @param style
     */
    public ColorEditComposite(Composite parent, int style,
            IColorEditCompCallback callback) {
        super(parent, style);
        this.callback = callback;
        initializeComponents(parent);
    }

    private void initializeComponents(Composite parent) {
        // Initialize the components.
        // Create the RGB and the HSB radio buttons.
        createRgbHsbButtons();

        ColorData initial = new ColorData(new RGB(255, 255, 255), 255);

        // Create the upper color wheel for the display.
        upperColorWheel = new ColorWheelComp(parent, this, upperWheelTitle);
        upperColorWheel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));
        // upperColorWheel.setColor(colorArray.get(0));
        upperColorWheel.setColor(initial);

        // Create the color bar object that is displayed
        // in the middle of the dialog.
        colorBar = new ColorBar(parent, this, callback.getColorMapParameters());

        // Create the lower color wheel for the display.
        lowerColorWheel = new ColorWheelComp(parent, this, lowerWheelTitle);
        lowerColorWheel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));
        // lowerColorWheel.setColor(colorArray.get(colorArray.size() - 1));
        lowerColorWheel.setColor(initial);

        // Create the GFE discrete check button.
        createGFEDiscreteButton();
    }

    /**
     * Create the RGB and the HSB radio buttons.
     */
    private void createRgbHsbButtons() {
        // Create a group to contain the RGB and HSB radio buttons.
        Group colorGroup = new Group(getParent(), SWT.NONE);
        colorGroup.setText(" Use color model: ");

        RowLayout groupRowLayout = new RowLayout();
        groupRowLayout.marginLeft = 10;
        groupRowLayout.marginRight = 10;
        groupRowLayout.spacing = 10;
        colorGroup.setLayout(groupRowLayout);
        colorGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // Create the RGB radio button. When the radio button is selected
        // update the upper and lower color wheel objects to display the RGB
        // sliders.
        rgbRdo = new Button(colorGroup, SWT.RADIO);
        rgbRdo.setText("RGB");
        rgbRdo.setSelection(true);
        rgbRdo.addSelectionListener(new SelectionAdapter() {
            @Override
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
            @Override
            public void widgetSelected(SelectionEvent event) {
                changeColorWheels();
            }
        });
    }

    /**
     * Create the GFE discrete check button.
     */
    private void createGFEDiscreteButton() {
        // Create a group to contain the RGB and HSB radio buttons.
        Group discreteGroup = new Group(getParent(), SWT.NONE);

        RowLayout groupRowLayout = new RowLayout();
        groupRowLayout.marginLeft = 10;
        groupRowLayout.marginRight = 10;
        groupRowLayout.spacing = 10;
        discreteGroup.setLayout(groupRowLayout);
        discreteGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        // Create the discrete check button.
        gfeDiscreteCheck = new Button(discreteGroup, SWT.CHECK);
        gfeDiscreteCheck.setText("GFE Discrete");
        gfeDiscreteCheck.setSelection(false);
        gfeDiscreteCheck.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateColorMap();
            }
        });
    }

    /**
     * Change the upper and lower color wheel objects to display either RGB or
     * HSB.
     */
    private void changeColorWheels() {
        if (rgbRdo.getSelection() == true) {
            upperColorWheel.showRgbSliders(true);
            lowerColorWheel.showRgbSliders(true);
        } else {
            upperColorWheel.showRgbSliders(false);
            lowerColorWheel.showRgbSliders(false);
        }
    }

    /**
     * Fill the area between the sliders in the color bar using the color data
     * provided.
     *
     * @param colorData
     *            The color data object containing the RGB color and the alpha
     *            value.
     */
    public void fillColor(ColorData colorData) {
        colorBar.fillColorBarColor(colorData);
        updateColorMap();
        callback.fillColor(colorData);
    }

    /**
     * Set the color where the top or bottom slider is pointing. The color wheel
     * title is used to determine if the color is from the upper color wheel or
     * the lower color wheel.
     *
     * @param colorData
     *            The color data object containing the RGB color and the alpha
     *            value.
     * @param colorWheelTitle
     *            The title of the color wheel that is calling the method.
     */
    public void setColor(ColorData colorData, String colorWheelTitle) {
        if (colorWheelTitle.compareTo(upperWheelTitle) == 0) {
            colorBar.setColorBarColor(colorData, true);
        } else if (colorWheelTitle.compareTo(lowerWheelTitle) == 0) {
            colorBar.setColorBarColor(colorData, false);
        }
        updateColorMap();
        callback.setColor(colorData, colorWheelTitle);
    }

    /**
     * A callback method used by the ColorBar class. This method is called to
     * update the upper or lower color wheel when the mouse is clicked in the
     * color bar and moved around.
     *
     * @param colorData
     *            The color data object containing the RGB color and the alpha
     *            value.
     * @param upperFlag
     *            A flag indicating if the upper or lower color wheel is to be
     *            updated.
     */
    public void updateColor(ColorData colorData, boolean upperFlag) {
        if (upperFlag) {
            upperColorWheel.setColor(colorData);
        } else {
            lowerColorWheel.setColor(colorData);
        }
    }

    /**
     * Updates the color map currently displayed
     */
    public void updateColorMap() {
        colorMap = ColorUtil.buildColorMap(colorBar.getCurrentColors(), null);
        if (isGFEDiscrete()) {
            colorMap.removeDuplicates();
        }
        callback.updateColorMap(colorMap);
    }

    public ColorWheelComp getUpperColorWheel() {
        return upperColorWheel;
    }

    public ColorWheelComp getLowerColorWheel() {
        return lowerColorWheel;
    }

    public ColorBar getColorBar() {
        return colorBar;
    }

    public Button getRgbRdo() {
        return rgbRdo;
    }

    public Button getHsbRdo() {
        return hsbRdo;
    }

    public String getUpperWheelTitle() {
        return upperWheelTitle;
    }

    public String getLowerWheelTitle() {
        return lowerWheelTitle;
    }

    public ColorMap getColorMap() {
        return colorMap;
    }

    public boolean isGFEDiscrete() {
        return gfeDiscreteCheck.getSelection();
    }

}
