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
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

import com.raytheon.viz.ui.widgets.SpinScale;

/**
 * Composite containing the color wheel and the scales used to adjust the color
 * value.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20 JUN 2007  933        lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ColorWheelComp extends Composite implements MouseListener,
        MouseMoveListener {
    /**
     * Radius of the color wheel.
     */
    private static final int radius = 55;

    /**
     * Size of the image displaying the color wheel.
     */
    private static final int imageSize = radius * 2 + 10;

    /**
     * center of the X coordinate.
     */
    private static final int xCenter = imageSize / 2;

    /**
     * Center of the Y coordinate.
     */
    private static final int yCenter = imageSize / 2;

    /**
     * Radius of the color wheel cursor.
     */
    private static final int cursorRadius = 3;

    /**
     * Width of the scales.
     */
    private final int SCALE_WIDTH = 250;

    /**
     * Height of the scales.
     */
    private final int SCALE_HEIGHT = 30;

    /**
     * Parent shell.
     */
    private Composite parent;

    /**
     * Callback used to update the color bar.
     */
    private IColorWheelAction callback;

    /**
     * Callback used when the color changes using the color wheel/scales.
     */
    private IColorWheelChange changeCallback = null;

    /**
     * Palette data.
     */
    private PaletteData palette;

    /**
     * Image data used to draw the color wheel.
     */
    private ImageData colorWheelImageData;

    /**
     * Image data used to draw the preview color box.
     */
    private ImageData previewData;

    /**
     * Cursor position.
     */
    private Point cursorPos;

    /**
     * RGB object.
     */
    private RGB rgb;

    /**
     * Pixel value.
     */
    private int pix;

    /**
     * Red (RGB) scale.
     */
    private SpinScale redScale;

    /**
     * Green (RGB) scale.
     */
    private SpinScale greenScale;

    /**
     * Blue (RGB) scale.
     */
    private SpinScale blueScale;

    /**
     * Hue (HSB) scale.
     */
    private SpinScale hueScale;

    /**
     * Saturation (HSB) scale.
     */
    private SpinScale satScale;

    /**
     * Brightness (HSB) scale.
     */
    private SpinScale brightScale;

    /**
     * RGB alpha scale.
     */
    private SpinScale rgbAlphaScale;

    /**
     * HSB alpha scale.
     */
    private SpinScale hsbAlphaScale;

    /**
     * Color wheel where the user can choose a color by using the mouse.
     */
    private Control colorWheel;

    /**
     * A canvas to show the exact color selected by the color wheel, scales, or
     * the spinner controls.
     */
    private Canvas colorPreview;

    /**
     * The current selected color.
     */
    private Color currentColor;

    /**
     * The current alpha value.
     */
    private int alphaValue = 255;

    /**
     * Color of the cursor on the color wheel. The cursor will change colors so
     * it won't blend into the color wheel.
     */
    private Color cursorColor;

    /**
     * Flag indicating the the mouse button is pressed down.
     */
    private boolean mouseIsDown = false;

    /**
     * Composite with a stack layout. The RGB and HSB scales, spinners, and
     * labels will occupy the same space and will be visible when either RGB or
     * HSB is selected.
     */
    private Composite stackComposite;

    /**
     * Stack layout for the stack composite container.
     */
    private StackLayout stackLayout;

    /**
     * Set button used to set a single cell of the color bar.
     */
    private Button setButton;

    /**
     * Fill button used to fill a range with the same colors in the color bar.
     */
    private Button fillButton;

    /**
     * Composite to hold the RGB controls.
     */
    private Composite rgbComposite;

    /**
     * Composite to hold the HSB controls.
     */
    private Composite hsbComposite;

    /**
     * Main composite that holds all of the components of the color wheel.
     */
    private Group mainGroupComposite;

    /**
     * Color wheel image drawn on the color wheel canvas.
     */
    private Image colorWheelImage;

    /**
     * Color wheel image drawn on the color preview canvas.
     */
    private Image previewImage;

    /**
     * Flag to determine if the set, fill, and alpha scales are visible.
     */
    private boolean hideControls = false;

    /**
     * Color offset.
     */
    final int colorOffset = 127;

    /**
     * Width of the preview canvas.
     */
    private int previewCanvasWidth = 47;

    /**
     * Height of the preview canvas.
     */
    private int previewCanvasHeight = 37;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param title
     *            Group title.
     * @param hideControls
     *            Flag indicating if certain controls should be hidden.
     */
    public ColorWheelComp(Composite parent, String title, boolean hideControls) {
        super(parent, SWT.NONE);

        this.hideControls = hideControls;
        this.parent = parent;
        this.callback = null;

        init(title);
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param callback
     *            Callback used when updates are needed.
     * @param title
     *            Group title.
     */
    public ColorWheelComp(Composite parent, IColorWheelAction callback,
            String title) {
        super(parent, SWT.NONE);
        this.parent = parent;
        this.callback = callback;

        init(title);
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param callback
     *            Callback used when updates are needed.
     * @param title
     *            Group title.
     */
    public ColorWheelComp(Composite parent, IColorWheelChange changeCallback,
            String title, boolean hideControls) {
        super(parent, SWT.NONE);
        this.parent = parent;
        this.changeCallback = changeCallback;
        this.hideControls = hideControls;

        init(title);
    }

    /**
     * Initialize the components.
     * 
     * @param title
     *            Group title.
     */
    private void init(String title) {
        // Setup the color wheel and preview data.
        palette = new PaletteData(0xff, 0xff00, 0xff0000);
        colorWheelImageData = new ImageData(imageSize, imageSize, 24, palette);
        previewData = new ImageData(47, 37, 24, palette);
        setPreviewBackground();

        this.setLayout(new FillLayout());

        // Setup the main composite.
        mainGroupComposite = new Group(this, SWT.NONE);
        mainGroupComposite.setText(title);
        GridLayout gl = new GridLayout(3, false);
        mainGroupComposite.setLayout(gl);

        // Initialize the components.
        initComponents();

        // Set the starting color to white.
        setColor(new RGB(255, 255, 255));
    }

    // Initialize all of the components.
    private void initComponents() {
        // Create the stack layout that will contain the
        // RGB and the HSB controls.
        createRgbHsbControlsStack();

        // Create the color wheel image.
        createColorWheel();

        // Create the control buttons.
        createButtons();
    }

    /**
     * Create the color wheel.
     */
    private void createColorWheel() {
        Composite colorWheelComp = new Composite(mainGroupComposite, SWT.NONE);
        colorWheelComp.setLayout(new GridLayout(1, false));

        // Create the color wheel canvas and add a paint listener to the canvas
        // that will draw the color wheel.
        colorWheel = new Canvas(colorWheelComp, SWT.DOUBLE_BUFFERED);
        colorWheel.setLayoutData(new GridData(imageSize, imageSize));
        colorWheel.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent event) {
                if (colorWheelImage != null) {
                    colorWheelImage.dispose();
                }
                if (isEnabled()) {
                    colorWheelImage = new Image(parent.getDisplay(),
                            colorWheelImageData);
                    event.gc.drawImage(colorWheelImage, 0, 0);
                    event.gc.setForeground(cursorColor);
                    event.gc.setLineWidth(2);
                    event.gc.drawOval(cursorPos.x - cursorRadius, cursorPos.y
                            - cursorRadius, cursorRadius * 2, cursorRadius * 2);
                } else {
                    event.gc.setBackground(getDisplay().getSystemColor(
                            SWT.COLOR_BLACK));
                    event.gc.fillRectangle(0, 0, imageSize, imageSize);
                }
            }
        });

        // Add a dispose listener to the color wheel so when the control is
        // disposed objects will get cleaned up.
        colorWheel.addDisposeListener(new DisposeListener() {
            // @Override
            public void widgetDisposed(DisposeEvent arg0) {
                currentColor.dispose();
                cursorColor.dispose();

                if (colorWheelImage != null) {
                    colorWheelImage.dispose();
                }
            }
        });

        // Add mouse listeners to the color wheel.
        colorWheel.addMouseListener(this);
        colorWheel.addMouseMoveListener(this);

        // Create the start color wheel cursor position.
        cursorPos = new Point(xCenter, yCenter);
    }

    /**
     * Create the Set and Fill buttons and the preview canvas.
     */
    private void createButtons() {
        // Create the component to contain the preview canvas and
        // the Set & Fill buttons.
        Composite buttonsComp = new Composite(mainGroupComposite, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 10;
        buttonsComp.setLayout(gl);

        // Create the color preview canvas.
        GridData gd;

        if (hideControls == false) {
            gd = new GridData(45, 35);
        } else {
            gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
            gd.widthHint = 47;
            gd.heightHint = 100;
            previewCanvasWidth = 49;
            previewCanvasHeight = 102;
        }

        colorPreview = new Canvas(buttonsComp, SWT.DOUBLE_BUFFERED | SWT.BORDER);
        colorPreview.setLayoutData(gd);
        colorPreview.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent event) {
                if (previewImage != null) {
                    previewImage.dispose();
                }
                if (isEnabled()) {
                    previewImage = new Image(parent.getDisplay(), previewData);
                    event.gc.drawImage(previewImage, 0, 0);
                    event.gc.setAlpha(alphaValue);
                    event.gc.setBackground(currentColor);
                    event.gc.fillRectangle(0, 0, previewCanvasWidth,
                            previewCanvasHeight);

                    if (changeCallback != null) {
                        changeCallback.colorChange(currentColor.getRGB(),
                                mainGroupComposite.getText());
                    }
                } else {
                    event.gc.setBackground(getDisplay().getSystemColor(
                            SWT.COLOR_BLACK));
                    event.gc.fillRectangle(0, 0, previewCanvasWidth,
                            previewCanvasHeight);
                }
            }
        });

        // Add a dispose listener to the color preview canvas so when the
        // canvas is disposed the preview image is disposed.
        colorPreview.addDisposeListener(new DisposeListener() {
            // @Override
            public void widgetDisposed(DisposeEvent arg0) {
                if (previewImage != null && previewImage.isDisposed() == false) {
                    previewImage.dispose();
                }
            }
        });

        // Check if the Set and Fill button should be created.
        if (hideControls == false) {
            // Create the Set button.
            gd = new GridData(44, 28);
            setButton = new Button(buttonsComp, SWT.PUSH);
            setButton.setText("Set");
            setButton.setLayoutData(gd);
            setButton.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {

                    if (callback == null) {
                        return;
                    }

                    ColorData colorData = new ColorData(currentColor.getRGB(),
                            alphaValue);
                    callback.setColor(colorData, mainGroupComposite.getText());
                }
            });

            // Create the Fill button.
            gd = new GridData(45, 28);
            fillButton = new Button(buttonsComp, SWT.PUSH);
            fillButton.setText("Fill");
            fillButton.setLayoutData(gd);
            fillButton.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {

                    if (callback == null) {
                        return;
                    }

                    ColorData colorData = new ColorData(currentColor.getRGB(),
                            alphaValue);
                    callback.fillColor(colorData);
                }
            });
        }
    }

    /**
     * Set the color wheel, and RGB/HSB controls to the specified color and
     * alpha value.
     * 
     * @param colorData
     *            Color data containing the RGB color and the alpha value.
     */
    public void setColor(ColorData colorData) {
        alphaValue = colorData.alphaValue;
        setColor(colorData.rgbColor);
    }

    /**
     * Use the specified color to update all of the color controls and the color
     * wheel cursor to reflect the new color.
     * 
     * @param rgb
     */
    public void setColor(RGB rgb) {
        // Dispose of the current color (if not null).
        if (currentColor != null) {
            currentColor.dispose();
        }

        // Set the new color.
        currentColor = new Color(parent.getDisplay(), rgb);

        // Compute the new cursor color.
        computeCursorColor();

        // Redraw the color preview canvas.
        colorPreview.redraw();

        // Update the red scale and spinner
        redScale.setSelection(rgb.red);

        // Update the green scale and spinner
        greenScale.setSelection(rgb.green);

        // Update the blue scale and spinner
        blueScale.setSelection(rgb.blue);

        // Update the hue scale and spinner
        float hsb[] = rgb.getHSB();
        int hue = Math.round(hsb[0]);
        hueScale.setSelection(hue);

        // Update the saturation scale and spinner
        int sat = Math.round(hsb[1] * 100);
        satScale.setSelection(sat);

        // Update the brightness scale and spinner
        int brightness = Math.round(hsb[2] * 100);
        brightScale.setSelection(brightness);

        // Check if the RGB alpha controls need to be initialized.
        if (hideControls == false) {

            // Update the RGB & HSB alpha scales and spinners
            rgbAlphaScale.setSelection(alphaValue);
            hsbAlphaScale.setSelection(alphaValue);

        }

        // Update the color wheel and redraw the color wheel.
        updateColorWheel(hsb[2]);
        moveCursor(rgb);
    }

    /**
     * Get the current color and alpha value.
     * 
     * @return ColorData object containing the RGB and alpha values.
     */
    public ColorData getColorData() {
        ColorData colorData = new ColorData(currentColor.getRGB(), alphaValue);

        return colorData;
    }

    /**
     * Set the preview image data to have a black background with a white filled
     * square in the middle. When the alpha channel is modified the background
     * will start to show through so the user can visible see the alpha channel
     * being adjusted.
     */
    private void setPreviewBackground() {
        RGB rgb = new RGB(0, 0, 0);
        int pixVal = palette.getPixel(rgb);

        for (int y = 0; y < 37; ++y) {
            for (int x = 0; x < 47; ++x) {
                if (y > 8 && y < 27) {
                    if (x < 12 || x > 33) {
                        rgb = new RGB(0, 0, 0);
                        pixVal = palette.getPixel(rgb);
                    } else {
                        rgb = new RGB(255, 255, 255);
                        pixVal = palette.getPixel(rgb);
                    }
                }
                previewData.setPixel(x, y, pixVal);
            }
        }
    }

    /**
     * Redraw the color wheel with the updated brightness level.
     * 
     * @param brightness
     *            Brightness level.
     */
    private void updateColorWheel(float brightness) {
        float saturation;
        for (int x = 0; x <= radius; x++) {
            int x2 = x * x;
            for (int y = 0; y <= radius; y++) {
                int r = (int) Math.sqrt(x2 + y * y);
                if (r <= radius) {
                    double hue = Math.toDegrees(Math.atan2(x, y));
                    saturation = (float) r / radius;
                    rgb = new RGB((float) hue, saturation, brightness);
                    pix = palette.getPixel(rgb);
                    colorWheelImageData.setPixel(xCenter + x, yCenter - y, pix);
                    rgb = new RGB((float) (180.0 - hue), saturation, brightness);
                    pix = palette.getPixel(rgb);
                    colorWheelImageData.setPixel(xCenter + x, yCenter + y, pix);
                    rgb = new RGB((float) (180.0 + hue), saturation, brightness);
                    pix = palette.getPixel(rgb);
                    colorWheelImageData.setPixel(xCenter - x, yCenter + y, pix);
                    rgb = new RGB((float) (360.0 - hue), saturation, brightness);
                    pix = palette.getPixel(rgb);
                    colorWheelImageData.setPixel(xCenter - x, yCenter - y, pix);
                }
            }
        }
        colorWheel.redraw();
    }

    /**
     * Create container to hold the RGB and HSB controls in a stack layout.
     */
    private void createRgbHsbControlsStack() {
        // Create a composite with a stack layout.
        stackComposite = new Composite(mainGroupComposite, SWT.NONE);
        stackLayout = new StackLayout();
        stackComposite.setLayout(stackLayout);

        // Add the RGB and HSB containers to the stack composite.
        createRgbControls(stackComposite);
        createHsbControls(stackComposite);

        // Set the stack layout to show the RGB controls.
        stackLayout.topControl = rgbComposite;
        stackComposite.layout();
    }

    /**
     * Create the RGB controls.
     * 
     * @param stackComposite
     *            Stack composite where the controls will reside.
     */
    private void createRgbControls(Composite stackComposite) {
        rgbComposite = new Composite(stackComposite, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        rgbComposite.setLayout(gridLayout);

        GridData gd;

        // Create the Red label.
        Label redLbl = new Label(rgbComposite, SWT.NONE);
        redLbl.setText("Red:");

        // Create the Red scale.
        gd = new GridData(SCALE_WIDTH, SCALE_HEIGHT);
        redScale = new SpinScale(rgbComposite, SWT.HORIZONTAL);
        redScale.setMinimum(0);
        redScale.setMaximum(255);
        redScale.setIncrement(1);
        redScale.setPageIncrement(5);
        redScale.setLayoutData(gd);
        redScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RGB rgb = currentColor.getRGB();
                rgb.red = redScale.getSelection();
                setColor(rgb);
            }
        });

        // Create the Green label.
        Label greenLbl = new Label(rgbComposite, SWT.NONE);
        greenLbl.setText("Green:");

        // Create the Green scale.
        gd = new GridData(SCALE_WIDTH, SCALE_HEIGHT);
        greenScale = new SpinScale(rgbComposite, SWT.HORIZONTAL);
        greenScale.setMinimum(0);
        greenScale.setMaximum(255);
        greenScale.setIncrement(1);
        greenScale.setPageIncrement(5);
        greenScale.setLayoutData(gd);
        greenScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RGB rgb = currentColor.getRGB();
                rgb.green = greenScale.getSelection();
                setColor(rgb);
            }
        });

        // Create the Blue label.
        Label blueLbl = new Label(rgbComposite, SWT.NONE);
        blueLbl.setText("Blue :  ");

        // Create the Blue scale.
        gd = new GridData(SCALE_WIDTH, SCALE_HEIGHT);
        blueScale = new SpinScale(rgbComposite, SWT.HORIZONTAL);
        blueScale.setMinimum(0);
        blueScale.setMaximum(255);
        blueScale.setIncrement(1);
        blueScale.setPageIncrement(5);
        blueScale.setLayoutData(gd);
        blueScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RGB rgb = currentColor.getRGB();
                rgb.blue = blueScale.getSelection();
                setColor(rgb);
            }
        });

        // Check if the RGB alpha controls need to be initialized.
        if (hideControls == false) {
            // Create the RGB alpha label.
            Label alphaLbl = new Label(rgbComposite, SWT.NONE);
            alphaLbl.setText("Alpha :  ");

            // Create the alpha RGB scale.
            gd = new GridData(SCALE_WIDTH, SCALE_HEIGHT);
            rgbAlphaScale = new SpinScale(rgbComposite, SWT.HORIZONTAL);
            rgbAlphaScale.setMinimum(0);
            rgbAlphaScale.setMaximum(255);
            rgbAlphaScale.setSelection(255);
            rgbAlphaScale.setIncrement(1);
            rgbAlphaScale.setPageIncrement(5);
            rgbAlphaScale.setLayoutData(gd);
            rgbAlphaScale.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    alphaValue = rgbAlphaScale.getSelection();
                    hsbAlphaScale.setSelection(alphaValue);
                    colorPreview.redraw();
                }
            });
        }
    }

    /**
     * Create the HSB controls.
     * 
     * @param stackComposite
     *            Stack composite where the controls will reside.
     */
    private void createHsbControls(Composite stackComposite) {
        hsbComposite = new Composite(stackComposite, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        hsbComposite.setLayout(gridLayout);

        GridData gd;

        // Create Hue label.
        Label hueLbl = new Label(hsbComposite, SWT.NONE);
        hueLbl.setText("Hue:");

        SelectionListener commonHSBListener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                float[] hsb = new float[3];
                hsb[0] = hueScale.getSelection();
                hsb[1] = satScale.getSelection();
                hsb[2] = brightScale.getSelection();
                RGB rgb = new RGB(hsb[0], hsb[1] / 100.0f, hsb[2] / 100.0f);
                setColor(rgb);
                // the selection may have been changed in setColor since not all
                // hsb combinations make sense(for example hue and sat don't
                // matter when brightness is 0). This changes them back to the
                // user defined values for a more continuous user experience
                hueScale.setSelection((int) hsb[0]);
                satScale.setSelection((int) hsb[1]);
                brightScale.setSelection((int) hsb[2]);
            }
        };

        // Create Hue scale.
        gd = new GridData(SCALE_WIDTH, SCALE_HEIGHT);
        hueScale = new SpinScale(hsbComposite, SWT.HORIZONTAL);
        hueScale.setMinimum(0);
        hueScale.setMaximum(359);
        hueScale.setIncrement(1);
        hueScale.setPageIncrement(5);
        hueScale.setLayoutData(gd);
        hueScale.addSelectionListener(commonHSBListener);

        // Create Saturation label.
        Label satLbl = new Label(hsbComposite, SWT.NONE);
        satLbl.setText("Saturation:");

        // Create saturation scale.
        gd = new GridData(SCALE_WIDTH, SCALE_HEIGHT);
        satScale = new SpinScale(hsbComposite, SWT.HORIZONTAL);
        satScale.setMinimum(0);
        satScale.setMaximum(100);
        satScale.setIncrement(1);
        satScale.setPageIncrement(5);
        satScale.setLayoutData(gd);
        satScale.addSelectionListener(commonHSBListener);

        // Create Brightness label.
        Label brightLbl = new Label(hsbComposite, SWT.NONE);
        brightLbl.setText("Brightness:");

        // Create Brightness scale.
        gd = new GridData(SCALE_WIDTH, SCALE_HEIGHT);
        brightScale = new SpinScale(hsbComposite, SWT.HORIZONTAL);
        brightScale.setMinimum(0);
        brightScale.setMaximum(100);
        brightScale.setIncrement(1);
        brightScale.setPageIncrement(5);
        brightScale.setLayoutData(gd);
        brightScale.addSelectionListener(commonHSBListener);

        // Check if the HSB alpha controls need to be initialized.
        if (hideControls == false) {
            // Create the HSB alpha label.
            Label alphaLbl = new Label(hsbComposite, SWT.NONE);
            alphaLbl.setText("Alpha :  ");

            // Create the HSB alpha scale.
            gd = new GridData(SCALE_WIDTH, SCALE_HEIGHT);
            hsbAlphaScale = new SpinScale(hsbComposite, SWT.HORIZONTAL);
            hsbAlphaScale.setMinimum(0);
            hsbAlphaScale.setMaximum(255);
            hsbAlphaScale.setSelection(255);
            hsbAlphaScale.setIncrement(1);
            hsbAlphaScale.setPageIncrement(5);
            hsbAlphaScale.setLayoutData(gd);
            hsbAlphaScale.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    alphaValue = hsbAlphaScale.getSelection();
                    rgbAlphaScale.setSelection(alphaValue);
                    colorPreview.redraw();
                }
            });
        }
    }

    /**
     * Shows the RGB controls if the flag passed in is true, otherwise the HSB
     * controls are shown.
     * 
     * @param showRgbFlag
     *            If true show the RGB controls, if false show the HSB controls.
     */
    public void showRgbSliders(boolean showRgbFlag) {
        if (showRgbFlag == true) {
            // Set the stack layout to the RGB composite and then have
            // the stack composite show the RGB layer.
            stackLayout.topControl = rgbComposite;
            stackComposite.layout();
        } else {
            // Set the stack layout to the HSB composite and then have
            // the stack composite show the HSB layer.
            stackLayout.topControl = hsbComposite;
            stackComposite.layout();
        }
    }

    /**
     * Method called when the mouse is moved over the color wheel. *
     * 
     * @param e
     *            Mouse event.
     */
    public void mouseMove(MouseEvent e) {
        // If the mouse button is down then adjust the current color
        // according to the position of the mouse pointer.
        if (mouseIsDown) {
            adjustColor(e);
        }
    }

    /**
     * NOT USED
     */
    public void mouseDoubleClick(MouseEvent e) {
    }

    /**
     * Method called when the mouse button is pressed.
     * 
     * @param e
     *            Mouse event.
     */
    public void mouseDown(MouseEvent e) {
        // If the left mouse button is not pressed then return.
        if (e.button != 1) {
            return;
        }

        // Set the mouse button down flag and adjust the current color
        // according to the mouse position.
        mouseIsDown = true;
        adjustColor(e);
    }

    /**
     * Method called when the mouse button is released. The mouse button down
     * flag is reset to false.
     * 
     * @param e
     *            Mouse event.
     */
    public void mouseUp(MouseEvent e) {
        mouseIsDown = false;
    }

    /**
     * Get the color on the color wheel where the mouse is pointing at and then
     * have the RGB and HSB controls adjusted to reflect the selected color.
     * 
     * @param e
     *            Mouse event.
     */
    private void adjustColor(MouseEvent e) {
        // Initialize the local variables.
        int x = e.x - xCenter;
        int y = yCenter - e.y;
        int r = Math.min((int) Math.sqrt(x * x + y * y), radius);
        double theta = Math.toDegrees(Math.atan2(x, y));
        float[] hsb = currentColor.getRGB().getHSB();
        hsb[0] = (float) (theta);

        // Validate the range of the Hue value.
        if (hsb[0] < 0.0) {
            hsb[0] += 360.0;
        }

        // Set the saturation value.
        hsb[1] = (float) r / (float) radius;

        // Update the RGB and HSB controls to reflect the selected color.
        setColor(new RGB(hsb[0], hsb[1], hsb[2]));
    }

    /**
     * Move the cursor to the selected color.
     * 
     * @param rgb
     */
    private void moveCursor(RGB rgb) {
        float hsb[] = rgb.getHSB();
        double theta = Math.toRadians(hsb[0]);
        double r = hsb[1] * radius;

        // compute x and y in terms of r and theta;
        int x = (int) (r * Math.sin(theta));
        int y = (int) (-r * Math.cos(theta));
        cursorPos.x = x + xCenter;
        cursorPos.y = y + yCenter;

        // Redraw the color wheel that will redraw the cursor as well.
        colorWheel.redraw();
    }

    /**
     * Compute the cursor color. This will prevent the cursor from blending into
     * the color wheel colors.
     */
    private void computeCursorColor() {
        // Get the current color and change the RGB values using the color
        // offset
        // so the cursor color can be updated.
        RGB rgb = currentColor.getRGB();
        rgb.red += (rgb.red > 127 ? -colorOffset : colorOffset);
        rgb.green += (rgb.green > 127 ? -colorOffset : colorOffset);
        rgb.blue += (rgb.blue > 127 ? -colorOffset : colorOffset);

        // Dispose of the cursor color (if not null);
        if (cursorColor != null) {
            cursorColor.dispose();
        }

        // Set the new cursor color.
        cursorColor = new Color(parent.getDisplay(), rgb);
    }

    @Override
    public void setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        blueScale.setEnabled(enabled);
        redScale.setEnabled(enabled);
        greenScale.setEnabled(enabled);
        rgbAlphaScale.setEnabled(enabled);
        hueScale.setEnabled(enabled);
        satScale.setEnabled(enabled);
        brightScale.setEnabled(enabled);
        setButton.setEnabled(enabled);
        fillButton.setEnabled(enabled);
        colorWheel.setEnabled(enabled);
        colorPreview.setEnabled(enabled);
        colorWheel.redraw();
        colorPreview.redraw();
    }

}
