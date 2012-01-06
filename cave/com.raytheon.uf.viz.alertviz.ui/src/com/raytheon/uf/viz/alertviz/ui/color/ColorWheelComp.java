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

package com.raytheon.uf.viz.alertviz.ui.color;

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
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Spinner;

/**
 * Composite containing the color wheel and the sliders used to adjust the color
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
     * Radian to degree conversion.
     */
    private static final double rad2deg = 180.0 / Math.PI;

    /**
     * Width of the sliders.
     */
    private final int SLIDER_WIDTH = 200;

    /**
     * Height of the sliders.
     */
    private final int SLIDER_HEIGHT = 30;

    /**
     * Parent shell.
     */
    private Composite parent;

    /**
     * Callback used to update the color bar.
     */
    private IColorWheelAction callback;

    /**
     * Callback used when the color changes using the color wheel/sliders.
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
     * Red (RGB) slider.
     */
    private Scale redSlider;

    /**
     * Green (RGB) slider.
     */
    private Scale greenSlider;

    /**
     * Blue (RGB) slider.
     */
    private Scale blueSlider;

    /**
     * Hue (HSB) slider.
     */
    private Scale hueSlider;

    /**
     * Saturation (HSB) slider.
     */
    private Scale satSlider;

    /**
     * Brightness (HSB) slider.
     */
    private Scale brightSlider;

    /**
     * RGB alpha slider.
     */
    private Scale rgbAlphaSlider;

    /**
     * HSB alpha slider.
     */
    private Scale hsbAlphaSlider;

    /**
     * Red (RGB) spinner.
     */
    private Spinner redSpinner;

    /**
     * Green (RGB) spinner.
     */
    private Spinner greenSpinner;

    /**
     * Blue (RGB) spinner.
     */
    private Spinner blueSpinner;

    /**
     * RGB alpha spinner.
     */
    private Spinner rgbAlphaSpinner;

    /**
     * Hue (HSB) spinner.
     */
    private Spinner hueSpinner;

    /**
     * Saturation (HSB) spinner.
     */
    private Spinner satSpinner;

    /**
     * Brightness (HSB) spinner.
     */
    private Spinner brightSpinner;

    /**
     * HSB alpha spinner.
     */
    private Spinner hsbAlphaSpinner;

    /**
     * Color wheel where the user can choose a color by using the mouse.
     */
    private Control colorWheel;

    /**
     * A canvas to show the exact color selected by the color wheel, sliders, or
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
     * Composite with a stack layout. The RGB and HSB sliders, spinners, and
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
     * Flag to determine if the set, fill, and alpha sliders are visible.
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
                colorWheelImage = new Image(parent.getDisplay(),
                        colorWheelImageData);
                event.gc.drawImage(colorWheelImage, 0, 0);
                event.gc.setForeground(cursorColor);
                event.gc.setLineWidth(2);
                event.gc.drawOval(cursorPos.x - cursorRadius, cursorPos.y
                        - cursorRadius, cursorRadius * 2, cursorRadius * 2);
            }
        });

        // Add a dispose listener to the color wheel so when the control is
        // disposed objects will get cleaned up.
        colorWheel.addDisposeListener(new DisposeListener() {
            // @Override
            public void widgetDisposed(DisposeEvent arg0) {
                currentColor.dispose();
                cursorColor.dispose();
                colorWheelImage.dispose();
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
            }
        });

        // Add a dispose listener to the color preview canvas so when the
        // canvas is disposed the preview image is disposed.
        colorPreview.addDisposeListener(new DisposeListener() {
            // @Override
            public void widgetDisposed(DisposeEvent arg0) {
                previewImage.dispose();
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

        // Update the red slider and spinner
        redSlider.setSelection(rgb.red);
        redSpinner.setSelection(rgb.red);

        // Update the green slider and spinner
        greenSlider.setSelection(rgb.green);
        greenSpinner.setSelection(rgb.green);

        // Update the blue slider and spinner
        blueSlider.setSelection(rgb.blue);
        blueSpinner.setSelection(rgb.blue);

        // Update the hue slider and spinner
        float hsb[] = rgb.getHSB();
        int hue = Math.round(hsb[0]);
        hueSlider.setSelection(hue);
        hueSpinner.setSelection(hue);

        // Update the saturation slider and spinner
        int sat = Math.round(hsb[1] * 100);
        satSlider.setSelection(sat);
        satSpinner.setSelection(sat);

        // Update the brightness slider and spinner
        int brightness = Math.round(hsb[2] * 100);
        brightSlider.setSelection(brightness);
        brightSpinner.setSelection(brightness);

        // Check if the RGB alpha controls need to be initialized.
        if (hideControls == false) {

            // Update the RGB & HSB alpha sliders and spinners
            rgbAlphaSlider.setSelection(alphaValue);
            rgbAlphaSpinner.setSelection(alphaValue);
            hsbAlphaSlider.setSelection(alphaValue);
            hsbAlphaSpinner.setSelection(alphaValue);

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
                    double hue = Math.atan2(x, y) * rad2deg;
                    saturation = (float) r / radius;
                    rgb = new RGB((float) hue, saturation, brightness);
                    pix = palette.getPixel(rgb);
                    colorWheelImageData.setPixel(xCenter + (int) x, yCenter
                            - (int) y, pix);
                    rgb = new RGB((float) (180.0 - hue), saturation, brightness);
                    pix = palette.getPixel(rgb);
                    colorWheelImageData.setPixel(xCenter + (int) x, yCenter
                            + (int) y, pix);
                    rgb = new RGB((float) (180.0 + hue), saturation, brightness);
                    pix = palette.getPixel(rgb);
                    colorWheelImageData.setPixel(xCenter - (int) x, yCenter
                            + (int) y, pix);
                    rgb = new RGB((float) (360.0 - hue), saturation, brightness);
                    pix = palette.getPixel(rgb);
                    colorWheelImageData.setPixel(xCenter - (int) x, yCenter
                            - (int) y, pix);
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
        GridLayout gridLayout = new GridLayout(3, false);
        rgbComposite.setLayout(gridLayout);

        GridData gd;

        // Create the Red label.
        Label redLbl = new Label(rgbComposite, SWT.NONE);
        redLbl.setText("Red:");

        // Create the Red slider.
        gd = new GridData(SLIDER_WIDTH, SLIDER_HEIGHT);
        redSlider = new Scale(rgbComposite, SWT.HORIZONTAL);
        redSlider.setMinimum(0);
        redSlider.setMaximum(255);
        redSlider.setIncrement(5);
        redSlider.setLayoutData(gd);
        redSlider.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                redSpinner.setSelection(redSlider.getSelection());
                RGB rgb = currentColor.getRGB();
                rgb.red = redSlider.getSelection();
                setColor(rgb);
            }
        });

        // Create the Red color spinner.
        redSpinner = new Spinner(rgbComposite, SWT.BORDER);
        gd = new GridData(30, SWT.DEFAULT);
        redSpinner.setLayoutData(gd);
        redSpinner.setMinimum(0);
        redSpinner.setMaximum(255);
        redSpinner.setSelection(redSlider.getSelection());

        redSpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                redSlider.setSelection(redSpinner.getSelection());
                RGB rgb = currentColor.getRGB();
                rgb.red = redSlider.getSelection();
                setColor(rgb);
            }
        });

        // Create the Green label.
        Label greenLbl = new Label(rgbComposite, SWT.NONE);
        greenLbl.setText("Green:");

        // Create the Green slider.
        gd = new GridData(SLIDER_WIDTH, SLIDER_HEIGHT);
        greenSlider = new Scale(rgbComposite, SWT.HORIZONTAL);
        greenSlider.setMinimum(0);
        greenSlider.setMaximum(255);
        greenSlider.setIncrement(5);
        greenSlider.setLayoutData(gd);
        greenSlider.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                greenSpinner.setSelection(greenSlider.getSelection());
                RGB rgb = currentColor.getRGB();
                rgb.green = greenSlider.getSelection();
                setColor(rgb);
            }
        });

        // Create the Green color spinner.
        gd = new GridData(30, SWT.DEFAULT);
        greenSpinner = new Spinner(rgbComposite, SWT.BORDER);
        greenSpinner.setLayoutData(gd);
        greenSpinner.setMinimum(0);
        greenSpinner.setMaximum(255);
        greenSpinner.setSelection(greenSlider.getSelection());
        greenSpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                greenSlider.setSelection(greenSpinner.getSelection());
                RGB rgb = currentColor.getRGB();
                rgb.green = greenSlider.getSelection();
                setColor(rgb);
            }
        });

        // Create the Blue label.
        Label blueLbl = new Label(rgbComposite, SWT.NONE);
        blueLbl.setText("Blue :  ");

        // Create the Blue slider.
        gd = new GridData(SLIDER_WIDTH, SLIDER_HEIGHT);
        blueSlider = new Scale(rgbComposite, SWT.HORIZONTAL);
        blueSlider.setMinimum(0);
        blueSlider.setMaximum(255);
        blueSlider.setIncrement(5);
        blueSlider.setLayoutData(gd);
        blueSlider.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                blueSpinner.setSelection(blueSlider.getSelection());
                RGB rgb = currentColor.getRGB();
                rgb.blue = blueSlider.getSelection();
                setColor(rgb);
            }
        });

        // Create the Blue color spinner.
        gd = new GridData(30, SWT.DEFAULT);
        blueSpinner = new Spinner(rgbComposite, SWT.BORDER);
        blueSpinner.setLayoutData(gd);
        blueSpinner.setMinimum(0);
        blueSpinner.setMaximum(255);
        blueSpinner.setSelection(blueSlider.getSelection());
        blueSpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                blueSlider.setSelection(blueSpinner.getSelection());
                RGB rgb = currentColor.getRGB();
                rgb.blue = blueSlider.getSelection();
                setColor(rgb);
            }
        });

        // Check if the RGB alpha controls need to be initialized.
        if (hideControls == false) {
            // Create the RGB alpha label.
            Label alphaLbl = new Label(rgbComposite, SWT.NONE);
            alphaLbl.setText("Alpha :  ");

            // Create the alpha RGB slider.
            gd = new GridData(SLIDER_WIDTH, SLIDER_HEIGHT);
            rgbAlphaSlider = new Scale(rgbComposite, SWT.HORIZONTAL);
            rgbAlphaSlider.setMinimum(0);
            rgbAlphaSlider.setMaximum(255);
            rgbAlphaSlider.setSelection(255);
            rgbAlphaSlider.setIncrement(5);
            rgbAlphaSlider.setLayoutData(gd);
            rgbAlphaSlider.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                    alphaValue = rgbAlphaSlider.getSelection();
                    hsbAlphaSlider.setSelection(alphaValue);
                    rgbAlphaSpinner.setSelection(alphaValue);
                    hsbAlphaSpinner.setSelection(alphaValue);
                    colorPreview.redraw();
                }
            });

            // Create the RGB alpha spinner.
            gd = new GridData(30, SWT.DEFAULT);
            rgbAlphaSpinner = new Spinner(rgbComposite, SWT.BORDER);
            rgbAlphaSpinner.setLayoutData(gd);
            rgbAlphaSpinner.setMinimum(0);
            rgbAlphaSpinner.setMaximum(255);
            rgbAlphaSpinner.setSelection(rgbAlphaSlider.getSelection());
            rgbAlphaSpinner.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                    alphaValue = rgbAlphaSpinner.getSelection();
                    rgbAlphaSlider.setSelection(alphaValue);
                    hsbAlphaSlider.setSelection(alphaValue);
                    hsbAlphaSpinner.setSelection(alphaValue);
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
        GridLayout gridLayout = new GridLayout(3, false);
        hsbComposite.setLayout(gridLayout);

        GridData gd;

        // Create Hue label.
        Label hueLbl = new Label(hsbComposite, SWT.NONE);
        hueLbl.setText("Hue:");

        // Create Hue slider.
        gd = new GridData(SLIDER_WIDTH, SLIDER_HEIGHT);
        hueSlider = new Scale(hsbComposite, SWT.HORIZONTAL);
        hueSlider.setMinimum(0);
        hueSlider.setMaximum(359);
        hueSlider.setIncrement(5);
        hueSlider.setLayoutData(gd);
        hueSlider.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                hueSpinner.setSelection(hueSlider.getSelection());
                float[] hsb = currentColor.getRGB().getHSB();
                hsb[0] = hueSlider.getSelection();
                RGB rgb = new RGB(hsb[0], hsb[1], hsb[2]);
                setColor(rgb);
            }
        });

        // Create Hue spinner.
        gd = new GridData(30, SWT.DEFAULT);
        hueSpinner = new Spinner(hsbComposite, SWT.BORDER);
        hueSpinner.setLayoutData(gd);
        hueSpinner.setMinimum(0);
        hueSpinner.setMaximum(359);
        hueSpinner.setSelection(hueSlider.getSelection());
        hueSpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                hueSlider.setSelection(hueSpinner.getSelection());
                float[] hsb = currentColor.getRGB().getHSB();
                hsb[0] = hueSpinner.getSelection();
                RGB rgb = new RGB(hsb[0], hsb[1], hsb[2]);
                setColor(rgb);
            }
        });

        // Create Saturation label.
        Label satLbl = new Label(hsbComposite, SWT.NONE);
        satLbl.setText("Saturation:");

        // Create saturation slider.
        gd = new GridData(SLIDER_WIDTH, SLIDER_HEIGHT);
        satSlider = new Scale(hsbComposite, SWT.HORIZONTAL);
        satSlider.setMinimum(0);
        satSlider.setMaximum(100);
        satSlider.setIncrement(5);
        satSlider.setLayoutData(gd);
        satSlider.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                satSpinner.setSelection(satSlider.getSelection());
                float[] hsb = currentColor.getRGB().getHSB();
                hsb[1] = satSlider.getSelection() / 100.0f;
                RGB rgb = new RGB(hsb[0], hsb[1], hsb[2]);
                setColor(rgb);
            }
        });

        // Create Saturation spinner.
        gd = new GridData(30, SWT.DEFAULT);
        satSpinner = new Spinner(hsbComposite, SWT.BORDER);
        satSpinner.setLayoutData(gd);
        satSpinner.setMinimum(0);
        satSpinner.setMaximum(100);
        satSpinner.setSelection(satSlider.getSelection());
        satSpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                satSlider.setSelection(satSpinner.getSelection());
                float[] hsb = currentColor.getRGB().getHSB();
                hsb[1] = satSlider.getSelection() / 100.0f;
                RGB rgb = new RGB(hsb[0], hsb[1], hsb[2]);
                setColor(rgb);
            }
        });

        // Create Brightness label.
        Label brightLbl = new Label(hsbComposite, SWT.NONE);
        brightLbl.setText("Brightness:");

        // Create Brightness slider.
        gd = new GridData(SLIDER_WIDTH, SLIDER_HEIGHT);
        brightSlider = new Scale(hsbComposite, SWT.HORIZONTAL);
        brightSlider.setMinimum(0);
        brightSlider.setMaximum(100);
        brightSlider.setIncrement(5);
        brightSlider.setLayoutData(gd);
        brightSlider.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                brightSpinner.setSelection(brightSlider.getSelection());
                float[] hsb = currentColor.getRGB().getHSB();
                hsb[2] = brightSlider.getSelection() / 100.0f;
                RGB rgb = new RGB(hsb[0], hsb[1], hsb[2]);
                setColor(rgb);
            }
        });

        // Create Brightness spinner.
        gd = new GridData(30, SWT.DEFAULT);
        brightSpinner = new Spinner(hsbComposite, SWT.BORDER);
        brightSpinner.setLayoutData(gd);
        brightSpinner.setMinimum(0);
        brightSpinner.setMaximum(100);
        brightSpinner.setSelection(brightSlider.getSelection());
        brightSpinner.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                brightSlider.setSelection(brightSpinner.getSelection());
                float[] hsb = currentColor.getRGB().getHSB();
                hsb[2] = brightSlider.getSelection() / 100.0f;
                RGB rgb = new RGB(hsb[0], hsb[1], hsb[2]);
                setColor(rgb);
            }
        });

        // Check if the HSB alpha controls need to be initialized.
        if (hideControls == false) {
            // Create the HSB alpha label.
            Label alphaLbl = new Label(hsbComposite, SWT.NONE);
            alphaLbl.setText("Alpha :  ");

            // Create the HSB alpha slider.
            gd = new GridData(SLIDER_WIDTH, SLIDER_HEIGHT);
            hsbAlphaSlider = new Scale(hsbComposite, SWT.HORIZONTAL);
            hsbAlphaSlider.setMinimum(0);
            hsbAlphaSlider.setMaximum(255);
            hsbAlphaSlider.setSelection(255);
            hsbAlphaSlider.setIncrement(5);
            hsbAlphaSlider.setLayoutData(gd);
            hsbAlphaSlider.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                    alphaValue = hsbAlphaSlider.getSelection();
                    rgbAlphaSlider.setSelection(alphaValue);
                    rgbAlphaSpinner.setSelection(alphaValue);
                    hsbAlphaSpinner.setSelection(alphaValue);
                    colorPreview.redraw();
                }
            });

            // Create the HSB alpha spinner.
            gd = new GridData(30, SWT.DEFAULT);
            hsbAlphaSpinner = new Spinner(hsbComposite, SWT.BORDER);
            hsbAlphaSpinner.setLayoutData(gd);
            hsbAlphaSpinner.setMinimum(0);
            hsbAlphaSpinner.setMaximum(255);
            hsbAlphaSpinner.setSelection(255);
            hsbAlphaSpinner.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(SelectionEvent event) {
                    alphaValue = hsbAlphaSpinner.getSelection();
                    rgbAlphaSlider.setSelection(alphaValue);
                    hsbAlphaSlider.setSelection(alphaValue);
                    hsbAlphaSpinner.setSelection(alphaValue);
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
        double theta = Math.atan2(x, y);
        float[] hsb = currentColor.getRGB().getHSB();
        hsb[0] = (float) (theta * rad2deg);

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
        double theta = hsb[0] / rad2deg;
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
}
