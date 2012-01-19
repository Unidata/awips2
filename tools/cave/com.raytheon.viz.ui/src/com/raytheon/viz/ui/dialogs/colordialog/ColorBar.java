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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;

import javax.measure.converter.UnitConverter;

import org.eclipse.swt.SWT;
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
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;

public class ColorBar extends Composite implements MouseListener,
        MouseMoveListener {
    /**
     * Parent shell (composite).
     */
    protected Composite parent;

    /**
     * Callback used to update the color wheel.
     */
    protected IColorBarAction callBack;

    /**
     * Canvas where the color bar is drawn.
     */
    protected Canvas colorBarCanvas;

    /**
     * Palette information.
     */
    protected PaletteData palette;

    /**
     * Image data for the color bar.
     */
    protected ImageData colorBarImageData;

    /**
     * Width of the navigation buttons.
     */
    protected final int NAV_BTN_WIDTH = 33;

    /**
     * Height of the navigation buttons.
     */
    protected final int NAV_BTN_HEIGHT = 25;

    /**
     * Height of the slider arrows.
     */
    protected final int ARROW_HEIGHT = 28;

    /**
     * Width of the slider arrows.
     */
    protected final int ARROW_WIDTH = 10;

    /**
     * Horizontal margin - pixels from the left edge of the color bar canvas.
     */
    protected final int HORIZ_MARGIN = ARROW_WIDTH + 6;

    /**
     * Vertical margin - pixels from the top edge of the color bar to the top of
     * the arrow canvas.
     */
    protected final int VERT_MARGIN = 6;

    /**
     * Number of pixels between the top of the canvas to the top edge of the
     * color bar.
     */
    protected final int BAR_TOP = ARROW_HEIGHT + VERT_MARGIN;

    /**
     * The number of pixels per each color in a 256 color map. The color bar is
     * wider that 256 pixels so there is not a 1 to 1 mapping between colors and
     * pixels across the color bar.
     */
    protected final float PIXELS_PER_CELL = 1.34f;

    /**
     * Width of the color bar.
     */
    protected final int BAR_WIDTH = (int) (256 * PIXELS_PER_CELL);

    /**
     * Height of the color bar.
     */
    protected final int BAR_HEIGHT = 50;

    /**
     * Number of pixels between the bottom of the canvas to the bottom edge of
     * the color bar.
     */
    protected final int BAR_BOTTOM = BAR_TOP + BAR_HEIGHT;

    /**
     * Canvas width.
     */
    protected final int CANVAS_WIDTH = HORIZ_MARGIN * 2 + BAR_WIDTH;

    /**
     * Canvas height.
     */
    protected final int CANVAS_HEIGHT = BAR_BOTTOM + ARROW_HEIGHT + VERT_MARGIN;

    /**
     * Left bound limit for the X coordinate of the mouse.
     */
    protected final int LEFT_MOUSE_BOUNDS = HORIZ_MARGIN;

    /**
     * Right bound limit for the X coordinate of the mouse.
     */
    protected final int RIGHT_MOUSE_BOUNDS = HORIZ_MARGIN + BAR_WIDTH;

    /**
     * Middle of the color bar.
     */
    protected final int BAR_HORIZ_MID = RIGHT_MOUSE_BOUNDS / 2;

    protected Button topSkipLeft;

    protected Button topMoveLeft;

    protected Button bottomMoveLeft;

    protected Button bottomSkipLeft;

    protected Button topSkipRight;

    protected Button topMoveRight;

    protected Button bottomMoveRight;

    protected Button bottomSkipRight;

    /**
     * Point variable.
     */
    protected Point p;

    /**
     * Temporary RGB object.
     */
    protected RGB tmpRGB;

    /**
     * Current color.
     */
    protected Color currentColor;

    /**
     * Color bar image.
     */
    protected Image colorBarImage;

    /**
     * Rectangle area of the top slider.
     */
    protected Rectangle topSliderRect;

    /**
     * Rectangle area of the bottom slider.
     */
    protected Rectangle bottomSliderRect;

    /**
     * Rectangle area of the top color area of the color bar.
     */
    protected Rectangle topColorRect;

    /**
     * Rectangle area of the bottom color area of the color bar.
     */
    protected Rectangle bottomColorRect;

    /**
     * Mouse is down flag.
     */
    protected boolean mouseIsDown = false;

    /**
     * Move top slider flag.
     */
    protected boolean moveTopSlider = false;

    /**
     * Move bottom slider flag.
     */
    protected boolean moveBottomSlider = false;

    /**
     * Flag indicating if the color bar should be queried for the color where
     * the mouse is at (top of the color bar).
     */
    protected boolean queryTopColorBar = false;

    /**
     * Flag indicating if the color bar should be queried for the color where
     * the mouse is at (bottom of the color bar).
     */
    protected boolean queryBottomColorBar = false;

    /**
     * Top slider X coordinate.
     */
    protected int topSliderX = 0;

    /**
     * Bottom slider X coordinate.
     */
    protected int bottomSliderX = BAR_WIDTH;

    /**
     * Array of starting colors (original color map).
     */
    protected List<ColorData> startingColors;

    /**
     * Array of current colors. Reflects any current changes that have been
     * made.
     */
    protected List<ColorData> currentColors;

    /**
     * Index into the undo/redo color array.
     */
    protected int currentColorIndex = 0;

    /**
     * Array of text that will be displayed next to the top and bottom color bar
     * sliders.
     */
    protected String[] sliderText;

    /**
     * Array of color arrays to keep track of changes made to the color bar.
     */
    protected List<List<ColorData>> undoRedoArray;

    /**
     * The colormap parameters to use to initialize the bar
     */
    protected ColorMapParameters cmapParams;

    /**
     * When enabled, the canvas will look at the alphaMask in the
     * paintProperties
     */
    protected boolean enabledColorMask = false;

    /**
     * The color bar image
     */
    protected Image colorBar;

    /**
     * The color bar image with mask applied
     */
    protected Image colorBarWithMask;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param callback
     *            Callback to be called when color updates are needed.
     * @param cmapParams
     *            The colormap paramters to use to initalize the bar
     */
    public ColorBar(Composite parent, IColorBarAction callback,
            ColorMapParameters cmapParams) {
        this(parent, callback, cmapParams, false);
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param callback
     *            Callback to be called when color updates are needed.
     * @param cmapParams
     *            The colormap paramters to use to initalize the bar
     * @param enableColorMask
     *            Use the color mask values when painting the color bar
     */
    public ColorBar(Composite parent, IColorBarAction callback,
            ColorMapParameters cmapParams, boolean enableColorMask) {
        super(parent, SWT.NONE);
        this.parent = parent;
        this.enabledColorMask = enableColorMask;
        this.cmapParams = cmapParams;

        initializeColorData();

        this.callBack = callback;

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);
        this.setLayout(gl);
        this.setLayoutData(gd);

        initComponents();
    }

    private void initializeColorData() {
        List<ColorData> colors = createSliderData();

        this.startingColors = colors;

        // Create the undo/redo array and put the initial colors
        // at the beginning of the array.
        undoRedoArray = new ArrayList<List<ColorData>>();
        currentColors = new ArrayList<ColorData>(colors);
        undoRedoArray.add(startingColors);
    }

    /**
     * Initialize the components for the color bar.
     */
    private void initComponents() {
        // Create the palette and the initial color car image.
        palette = new PaletteData(0xff, 0xff00, 0xff0000);
        colorBarImageData = new ImageData(CANVAS_WIDTH, CANVAS_HEIGHT, 24,
                palette);

        // Create the starting color (white).
        currentColor = new Color(parent.getDisplay(), 255, 255, 255);

        initalizeColobars();

        // Create the slider navigation button on the left
        // side of the color bar.
        createLeftButtons();

        // Create the color bar canvas when the colors and
        // sliders are displayed.
        createColorBarArea();

        // Create the slider navigation button on the right
        // side of the color bar.
        createRightButtons();
    }

    public void updateColorMap(ColorMapParameters newParams) {
        this.cmapParams = newParams;
        this.currentColors = createSliderData();
        if (undoRedoArray.get(undoRedoArray.size() - 1).equals(currentColors) == false) {
            undoRedoArray.add(this.currentColors);
        }
        repaintColorbars();
    }

    /**
     * Create the slider navigation buttons on the left side of the color bar.
     */
    private void createLeftButtons() {
        // Create a composite to hold the buttons.
        Composite leftButtonsComp = new Composite(this, SWT.NONE);
        RowLayout rowLayout = new RowLayout(SWT.VERTICAL);
        rowLayout.spacing = 5;
        leftButtonsComp.setLayout(rowLayout);

        // Create the single step left navigation button for
        // the top slider.
        RowData rd = new RowData(NAV_BTN_WIDTH, NAV_BTN_HEIGHT);
        topMoveLeft = new Button(leftButtonsComp, SWT.PUSH);
        topMoveLeft.setText("<");
        topMoveLeft.setLayoutData(rd);
        topMoveLeft.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                // Move the top slider to the left 1 cell.
                int x = getXcoord(topSliderX);
                int y = topSliderX - 1;

                if (x == getXcoord(y)) {
                    --y;
                }

                drawTopSlider(y + HORIZ_MARGIN);
            }
        });

        // Create the skip color left navigation button for
        // the top slider.
        rd = new RowData(NAV_BTN_WIDTH, NAV_BTN_HEIGHT);
        topSkipLeft = new Button(leftButtonsComp, SWT.PUSH);
        topSkipLeft.setText("<<");
        topSkipLeft.setLayoutData(rd);
        topSkipLeft.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                skipTopSliderToLeft();
            }
        });

        // Create the skip color left navigation button for
        // the bottom slider.
        rd = new RowData(NAV_BTN_WIDTH, NAV_BTN_HEIGHT);
        bottomSkipLeft = new Button(leftButtonsComp, SWT.PUSH);
        bottomSkipLeft.setText("<<");
        bottomSkipLeft.setLayoutData(rd);
        bottomSkipLeft.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                skipBottomSliderToLeft();
            }
        });

        // Create the single step left navigation button for
        // the bottom slider.
        rd = new RowData(NAV_BTN_WIDTH, NAV_BTN_HEIGHT);
        bottomMoveLeft = new Button(leftButtonsComp, SWT.PUSH);
        bottomMoveLeft.setText("<");
        bottomMoveLeft.setLayoutData(rd);
        bottomMoveLeft.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                // Move the bottom slider to the left 1 cell.
                int x = getXcoord(bottomSliderX);
                int y = bottomSliderX - 1;

                if (x == getXcoord(y)) {
                    --y;
                }

                drawBottomSlider(y + HORIZ_MARGIN);
            }
        });
    }

    /**
     * Create the color bar canvas that will display the color bar and the top
     * and bottom sliders.
     */
    private void createColorBarArea() {
        // Do the initial calculation for the color bar.
        calculateColorBarAreas();

        // Set the font used for the top and bottom sliders.
        final Font font = new Font(parent.getDisplay(), "Courier", 10,
                SWT.NORMAL);

        // Create the composite that will hold the color bar canvas.
        Composite colorBarComp = new Composite(this, SWT.NONE);
        colorBarComp.setLayout(new GridLayout(1, false));

        // Create a new color bar canvas and set the mouse
        // and paint listeners.
        colorBarCanvas = new Canvas(colorBarComp, SWT.DOUBLE_BUFFERED);
        colorBarCanvas.setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));
        colorBarCanvas.addMouseListener(this);
        colorBarCanvas.addMouseMoveListener(this);
        colorBarCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent event) {
                if (isEnabled()) {

                    ColorData colorData;

                    // Set the font to be used.
                    event.gc.setFont(font);

                    // Draw the color bar background image.
                    if (colorBarImage != null) {
                        colorBarImage.dispose();
                    }

                    colorBarImage = new Image(parent.getDisplay(),
                            colorBarImageData);

                    event.gc.drawImage(colorBarImage, 0, 0);
                    event.gc.setForeground(parent.getDisplay().getSystemColor(
                            SWT.COLOR_WHITE));

                    // Draw the white color bar outline rectangle.
                    event.gc.drawRectangle(HORIZ_MARGIN, BAR_TOP, BAR_WIDTH,
                            BAR_HEIGHT);

                    // Draw colorbar image
                    synchronized (ColorBar.this) {
                        if (enabledColorMask && cmapParams.isUseMask()) {
                            event.gc.drawImage(colorBarWithMask,
                                    HORIZ_MARGIN + 1, BAR_TOP + 1);
                        } else {
                            event.gc.drawImage(colorBar, HORIZ_MARGIN + 1,
                                    BAR_TOP + 1);
                        }
                    }

                    // Reset the alpha to 255 (solid color).
                    event.gc.setAlpha(255);

                    // Draw a black line down the middle of the color bar.
                    event.gc.setForeground(parent.getDisplay().getSystemColor(
                            SWT.COLOR_BLACK));
                    event.gc.drawLine(HORIZ_MARGIN, BAR_HEIGHT / 2 + BAR_TOP,
                            BAR_WIDTH + HORIZ_MARGIN, BAR_HEIGHT / 2 + BAR_TOP);

                    // Draw a dashed white line down the middle of
                    // the color bar ( over the top of the black line).
                    event.gc.setForeground(parent.getDisplay().getSystemColor(
                            SWT.COLOR_WHITE));
                    event.gc.setLineStyle(SWT.LINE_DOT);
                    event.gc.drawLine(HORIZ_MARGIN, BAR_HEIGHT / 2 + BAR_TOP,
                            BAR_WIDTH + HORIZ_MARGIN, BAR_HEIGHT / 2 + BAR_TOP);

                    // Draw top slider arrow. The arrow will be filled with
                    // the color it is pointing to on the color bar.
                    event.gc.setLineStyle(SWT.LINE_SOLID);
                    currentColor.dispose();
                    colorData = currentColors.get(getXcoord(topSliderX));
                    currentColor = new Color(parent.getDisplay(),
                            colorData.rgbColor);
                    event.gc.setBackground(currentColor);
                    event.gc.setAlpha(colorData.alphaValue);
                    event.gc.fillPolygon(calcTopArrow(topSliderX));
                    event.gc.setAlpha(255);
                    event.gc.drawPolygon(calcTopArrow(topSliderX));

                    // Draw text that is displayed next to the top
                    // slider arrow.
                    event.gc.setForeground(parent.getDisplay().getSystemColor(
                            SWT.COLOR_WHITE));
                    if (topSliderX < BAR_HORIZ_MID) {
                        event.gc.drawString(sliderText[getXcoord(topSliderX)],
                                (HORIZ_MARGIN + topSliderX + 5),
                                ((int) (BAR_TOP - ARROW_HEIGHT)), true);
                    } else {
                        p = event.gc
                                .textExtent(sliderText[getXcoord(topSliderX)]);
                        event.gc.drawString(
                                sliderText[getXcoord(topSliderX)],
                                (HORIZ_MARGIN + topSliderX - ARROW_WIDTH - 5 - p.x),
                                ((int) (BAR_TOP - ARROW_HEIGHT)), true);
                    }

                    // Draw bottom slider arrow. The arrow will be filled with
                    // the color it is pointing to on the color bar.
                    currentColor.dispose();
                    colorData = currentColors.get(getXcoord(bottomSliderX));
                    currentColor = new Color(parent.getDisplay(),
                            colorData.rgbColor);
                    event.gc.setBackground(currentColor);
                    event.gc.setAlpha(colorData.alphaValue);
                    event.gc.fillPolygon(calcBottomArrow(bottomSliderX));
                    event.gc.setAlpha(255);
                    event.gc.drawPolygon(calcBottomArrow(bottomSliderX));

                    // Draw text that is displayed next to the bottom
                    // slider arrow.
                    event.gc.setForeground(parent.getDisplay().getSystemColor(
                            SWT.COLOR_WHITE));
                    if (bottomSliderX < BAR_HORIZ_MID) {
                        event.gc.drawString(
                                sliderText[getXcoord(bottomSliderX)],
                                (HORIZ_MARGIN + bottomSliderX + 13),
                                ((BAR_BOTTOM + 13)), true);
                    } else {
                        p = event.gc
                                .textExtent(sliderText[getXcoord(bottomSliderX)]);
                        event.gc.drawString(
                                sliderText[getXcoord(bottomSliderX)],
                                (HORIZ_MARGIN + bottomSliderX - ARROW_WIDTH - p.x),
                                ((BAR_BOTTOM + 13)), true);
                    }
                } else {
                    event.gc.setBackground(getDisplay().getSystemColor(
                            SWT.COLOR_BLACK));
                    event.gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);
                }
            }
        });

        // Add a dispose listener to the color bar canvas so we can clean up SWT
        // objects when the canvas is disposed.
        colorBarCanvas.addDisposeListener(new DisposeListener() {
            // @Override
            public void widgetDisposed(DisposeEvent arg0) {
                currentColor.dispose();
                font.dispose();
            }
        });
    }

    /**
     * Calculate the points of the top slider arrow.
     * 
     * @param xCoord
     *            X coordinate to draw the top slider arrow.
     * @return Array of points outlining the top slider arrow.
     */
    private int[] calcTopArrow(int xCoord) {
        int[] topArrow = { (HORIZ_MARGIN + xCoord), (BAR_TOP - 2),
                (HORIZ_MARGIN + xCoord), (BAR_TOP - ARROW_HEIGHT),
                (HORIZ_MARGIN + xCoord - ARROW_WIDTH / 2),
                (BAR_TOP - ARROW_HEIGHT),
                (HORIZ_MARGIN + xCoord - ARROW_WIDTH / 2),
                (BAR_TOP - ARROW_HEIGHT / 2),
                (HORIZ_MARGIN + xCoord - ARROW_WIDTH),
                (BAR_TOP - ARROW_HEIGHT / 2) };

        return topArrow;
    }

    /**
     * Calculate the points of the bottom slider arrow.
     * 
     * @param xCoord
     *            X coordinate to draw the bottom slider arrow.
     * @return Array of points outlining the bottom slider arrow.
     */
    private int[] calcBottomArrow(int xCoord) {
        int[] bottomArrow = { (HORIZ_MARGIN + xCoord), (BAR_BOTTOM + 2),
                (HORIZ_MARGIN + xCoord), (BAR_BOTTOM + ARROW_HEIGHT),
                (HORIZ_MARGIN + xCoord + ARROW_WIDTH / 2),
                (BAR_BOTTOM + ARROW_HEIGHT),
                (HORIZ_MARGIN + xCoord + ARROW_WIDTH / 2),
                (BAR_BOTTOM + ARROW_HEIGHT / 2),
                (HORIZ_MARGIN + xCoord + ARROW_WIDTH),
                (BAR_BOTTOM + ARROW_HEIGHT / 2) };

        return bottomArrow;
    }

    /**
     * Calculate the areas of the color bar.
     */
    private void calculateColorBarAreas() {
        topSliderRect = new Rectangle(0, 0, CANVAS_WIDTH, BAR_TOP);
        bottomSliderRect = new Rectangle(0, BAR_BOTTOM, CANVAS_WIDTH, BAR_TOP);
        topColorRect = new Rectangle((HORIZ_MARGIN + 1), (BAR_TOP + 1),
                (BAR_WIDTH - 1), ((int) (BAR_HEIGHT / 2)));
        bottomColorRect = new Rectangle((HORIZ_MARGIN + 1),
                (BAR_TOP + 1 + ((int) (BAR_HEIGHT / 2))), (BAR_WIDTH - 1),
                ((int) (BAR_HEIGHT / 2)));
    }

    /**
     * Create the slider navigation buttons on the right side of the color bar.
     */
    private void createRightButtons() {
        // Create a composite that will hold the buttons.
        Composite rightButtonsComp = new Composite(this, SWT.NONE);
        RowLayout rowLayout = new RowLayout(SWT.VERTICAL);
        rowLayout.spacing = 5;
        rightButtonsComp.setLayout(rowLayout);

        // Create the single step right button for the top slider.
        RowData rd = new RowData(NAV_BTN_WIDTH, NAV_BTN_HEIGHT);
        topMoveRight = new Button(rightButtonsComp, SWT.PUSH);
        topMoveRight.setText(">");
        topMoveRight.setLayoutData(rd);
        topMoveRight.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                // Move the top slider to the right 1 cell.
                int x = getXcoord(topSliderX + HORIZ_MARGIN);
                int y = topSliderX + 1 + HORIZ_MARGIN;

                if (x == getXcoord(y)) {
                    ++y;
                }

                drawTopSlider(y);
            }
        });

        // Create the skip color right navigation button for
        // the top slider.
        rd = new RowData(NAV_BTN_WIDTH, NAV_BTN_HEIGHT);
        topSkipRight = new Button(rightButtonsComp, SWT.PUSH);
        topSkipRight.setText(">>");
        topSkipRight.setLayoutData(rd);
        topSkipRight.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                skipTopSliderToRight();
            }
        });

        // Create the skip color right navigation button for
        // the bottom slider.
        rd = new RowData(NAV_BTN_WIDTH, NAV_BTN_HEIGHT);
        bottomSkipRight = new Button(rightButtonsComp, SWT.PUSH);
        bottomSkipRight.setText(">>");
        bottomSkipRight.setLayoutData(rd);
        bottomSkipRight.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                skipBottomSliderToRight();
            }
        });

        // Create the single step right button for the bottom slider.
        rd = new RowData(NAV_BTN_WIDTH, NAV_BTN_HEIGHT);
        bottomMoveRight = new Button(rightButtonsComp, SWT.PUSH);
        bottomMoveRight.setText(">");
        bottomMoveRight.setLayoutData(rd);
        bottomMoveRight.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                // Move the bottom slider to the right 1 cell.
                int x = getXcoord(bottomSliderX + HORIZ_MARGIN);
                int y = bottomSliderX + 1 + HORIZ_MARGIN;

                if (x == getXcoord(y)) {
                    ++y;
                }

                drawBottomSlider(y);
            }
        });
    }

    /**
     * Method called when the mouse is moved.
     * 
     * @param e
     *            Mouse event.
     */
    public void mouseMove(MouseEvent e) {
        // Check if the mouse button is held down.
        if (mouseIsDown) {
            if (moveTopSlider) {
                // Move the top slider arrow.
                drawTopSlider(e.x);
            } else if (moveBottomSlider) {
                // Move the bottom slider arrow.
                drawBottomSlider(e.x);
            } else if (queryTopColorBar) {
                // Update the upper color wheel with the color
                // the mouse is over on the top part of the color bar.
                callBack.updateColor(
                        currentColors.get(getXcoord(e.x - HORIZ_MARGIN)), true);
            } else if (queryBottomColorBar) {
                // Update the lower color wheel with the color
                // the mouse is over on the bottom part of the color bar.
                callBack.updateColor(
                        currentColors.get(getXcoord(e.x - HORIZ_MARGIN)), false);
            }
        }
    }

    /**
     * Mouse double click method -- NOT USED
     */
    public void mouseDoubleClick(MouseEvent e) {
    }

    /**
     * Method that is called when the mouse button is
     */
    public void mouseDown(MouseEvent e) {
        // If the left mouse button was not pressed then return.
        if (e.button != 1) {
            return;
        }

        mouseIsDown = true;

        // Check when the mouse is at in the color bar canvas. This
        // will determine what action is taken.
        if (topSliderRect.contains(e.x, e.y) == true) {
            moveTopSlider = true;
            drawTopSlider(e.x);
        } else if (bottomSliderRect.contains(e.x, e.y) == true) {
            moveBottomSlider = true;
            drawBottomSlider(e.x);
        } else if (topColorRect.contains(e.x, e.y) == true) {
            queryTopColorBar = true;
            callBack.updateColor(
                    currentColors.get(getXcoord(e.x - HORIZ_MARGIN)), true);
        } else if (bottomColorRect.contains(e.x, e.y) == true) {
            queryBottomColorBar = true;
            callBack.updateColor(
                    currentColors.get(getXcoord(e.x - HORIZ_MARGIN)), false);
        }
    }

    /**
     * Method called when the mouse button is released.
     * 
     * @param e
     *            Mouse event.
     */
    public void mouseUp(MouseEvent e) {
        mouseIsDown = false;
        moveBottomSlider = false;
        moveTopSlider = false;
        queryTopColorBar = false;
        queryBottomColorBar = false;
    }

    /**
     * Draw the top slider in the color bar. The actual "drawing" is done when
     * the redraw method on the color bar canvas is called.
     * 
     * @param mouseXcoord
     *            Mouse's X coordinate.
     */
    private void drawTopSlider(int mouseXcoord) {
        // Get the top slider position minus the horizontal offset.
        topSliderX = mouseXcoord - HORIZ_MARGIN;

        // Check to see if the mouse's X coordinate is out side
        // the range of the top slider.
        if (mouseXcoord > LEFT_MOUSE_BOUNDS && mouseXcoord < RIGHT_MOUSE_BOUNDS) {
            // If the top slider position is right of the bottom
            // slider then we need to move the bottom slider to
            // the same position as the top slider.
            if (topSliderX > bottomSliderX) {
                bottomSliderX = topSliderX;
                drawBottomSlider(mouseXcoord);
            }
        } else {
            // If the mouse's X coordinate is less than the mouse left bounds
            // limit then set the top slider and bottom slider to 0
            // (all the way to the left).
            // If the mouse's X coordinate is greater than the mouse right
            // bounds
            // limit then set the top slider to the width of the color bar
            // (all the way to the right).
            if (mouseXcoord <= LEFT_MOUSE_BOUNDS) {
                topSliderX = 0;
            } else if (mouseXcoord >= RIGHT_MOUSE_BOUNDS) {
                topSliderX = BAR_WIDTH;
                bottomSliderX = BAR_WIDTH;
                drawBottomSlider(mouseXcoord);
            }
        }

        // Redraw the color bar canvas.
        repaint();
    }

    /**
     * Draw the bottom slider in the color bar. The actual "drawing" is done
     * when the redraw method on the color bar canvas is called.
     * 
     * @param mouseXcoord
     *            Mouse's X coordinate.
     */
    private void drawBottomSlider(int mouseXcoord) {
        bottomSliderX = mouseXcoord - HORIZ_MARGIN;

        if (mouseXcoord > LEFT_MOUSE_BOUNDS && mouseXcoord < RIGHT_MOUSE_BOUNDS) {
            if (bottomSliderX < topSliderX) {
                topSliderX = bottomSliderX;
                drawTopSlider(mouseXcoord);
            }
        } else {
            // If the mouse's X coordinate is less than the mouse left bounds
            // limit then set the bottom slider and top slider to 0
            // (all the way to the left).
            // If the mouse's X coordinate is greater than the mouse right
            // bounds
            // limit then set the bottom slider to the width of the color bar
            // (all the way to the right).
            if (mouseXcoord <= LEFT_MOUSE_BOUNDS) {
                bottomSliderX = 0;
                topSliderX = 0;
                drawTopSlider(mouseXcoord);
            } else if (mouseXcoord >= RIGHT_MOUSE_BOUNDS) {
                bottomSliderX = BAR_WIDTH;
            }
        }

        repaint();
    }

    /**
     * Set the current cell (top or bottom depending on the flag passed in) to
     * the specified color in the color data object.
     * 
     * @param colorData
     *            Color data object containing the RGB and alpha values.
     * @param upperFlag
     *            If true the cell where the top slider is located will be
     *            updated, if false the cell where the bottom slider is located
     *            will be updated.
     */
    public void setColorBarColor(ColorData colorData, boolean upperFlag) {
        int xCoord = 0;

        // Create a new array of colors using the current list of colors.
        // This list will be modified and put in the undo/redo array.
        ArrayList<ColorData> newColors = new ArrayList<ColorData>(currentColors);

        // Check if the top or bottom slider X coordinate
        // will be used.
        if (upperFlag) {
            xCoord = getXcoord(topSliderX);
        } else {
            xCoord = getXcoord(bottomSliderX);
        }

        // Change the necessary color.
        newColors.set(xCoord, colorData);

        // Add the changed color array to the undo/redo array.
        if (currentColorIndex == (undoRedoArray.size() - 1)) {
            ++currentColorIndex;
            undoRedoArray.add(newColors);
        } else {
            ++currentColorIndex;
            undoRedoArray.add(currentColorIndex, newColors);
        }

        // Update the current colors array with the changed colors.
        currentColors = new ArrayList<ColorData>(newColors);

        repaintColorbars();
    }

    /**
     * Fill the area of the color bar with a solid color from the color wheel.
     * The area between the top and bottom sliders will be fill with the
     * specified color.
     * 
     * @param colorData
     *            The color to fill the color bar area with.
     */
    public void fillColorBarColor(ColorData colorData) {
        int topXCoord = getXcoord(topSliderX);
        int bottomXCoord = getXcoord(bottomSliderX);
        ArrayList<ColorData> newColors = new ArrayList<ColorData>(currentColors);

        // Fill the cells between the top slider and the bottom
        // slider with the fill color.
        for (int i = topXCoord; i <= bottomXCoord; ++i) {
            newColors.set(i, colorData);
        }

        // Update the undo/redo array.
        if (currentColorIndex == 0) {
            ++currentColorIndex;
            undoRedoArray.add(newColors);
        } else {
            ++currentColorIndex;
            undoRedoArray.add(currentColorIndex, newColors);
        }

        // Update the current colors array with the changed colors.
        currentColors = new ArrayList<ColorData>(newColors);

        repaintColorbars();
    }

    /**
     * Undo the last change to the color bar.
     * 
     * @return True if there are more "undos" that can take place, otherwise
     *         return false.
     */
    public boolean undoColorBar() {
        boolean rv = true;

        // Decrement the color index.
        --currentColorIndex;

        // If the color index is less than or equal zero set it to
        // zero and return false.
        if (currentColorIndex <= 0) {
            currentColorIndex = 0;
            rv = false;
        }

        // Update the current array of colors and redraw the color bar.
        currentColors = undoRedoArray.get(currentColorIndex);
        repaintColorbars();

        return rv;
    }

    /**
     * Redo the last change to the color bar.
     * 
     * @return True if there are more "redos" that can take place, otherwise
     *         return false.
     */
    public boolean redoColorBar() {
        boolean rv = true;

        // Increment the color index.
        ++currentColorIndex;

        // Check if the color index is at the last element
        // of the undo/redo array.
        if (currentColorIndex == undoRedoArray.size() - 1) {
            currentColorIndex = undoRedoArray.size() - 1;
            rv = false;
        }

        // Update the current array of colors and redraw the color bar.
        currentColors = undoRedoArray.get(currentColorIndex);
        repaintColorbars();

        return rv;
    }

    /**
     * Reverting will clear the undo/redo array and display the original colors
     * in the color bar.
     */
    public void revertColorBar() {
        undoRedoArray.clear();
        currentColors = new ArrayList<ColorData>(startingColors);
        undoRedoArray.add(currentColors);
        repaintColorbars();
        currentColorIndex = 0;
    }

    /**
     * The top slider will skip left to the next color in the color bar that is
     * not the same as the current color the slider is pointing at.
     */
    private void skipTopSliderToLeft() {
        // Get the X coordinate of the slider.
        int topXCoord = getXcoord(topSliderX);

        // Get the current color the top slider is "pointing" to.
        RGB currentRGB = currentColors.get(topXCoord).rgbColor;

        // Loop until a new color is found.
        for (int i = topXCoord - 1; i >= 0; i--) {
            tmpRGB = currentColors.get(i).rgbColor;

            // If a new color is found then move the slider
            // to the new color.
            if (currentRGB.red != tmpRGB.red
                    || currentRGB.green != tmpRGB.green
                    || currentRGB.blue != tmpRGB.blue) {
                int skipXCoord = Math.round(i * PIXELS_PER_CELL);

                drawTopSlider(skipXCoord + HORIZ_MARGIN);
                break;
            }
        }
    }

    /**
     * The bottom slider will skip left to the next color in the color bar that
     * is not the same as the current color the slider is pointing at.
     */
    private void skipBottomSliderToLeft() {
        // Get the X coordinate of the slider.
        int bottomXCoord = getXcoord(bottomSliderX);

        // Get the current color.
        RGB currentRGB = currentColors.get(bottomXCoord).rgbColor;

        // Loop until a new color is found.
        for (int i = bottomXCoord - 1; i >= 0; i--) {
            tmpRGB = currentColors.get(i).rgbColor;

            // If a new color is found then move the slider
            // to the new color.
            if (currentRGB.red != tmpRGB.red
                    || currentRGB.green != tmpRGB.green
                    || currentRGB.blue != tmpRGB.blue) {
                int skipXCoord = Math.round(i * PIXELS_PER_CELL);

                drawBottomSlider(skipXCoord + HORIZ_MARGIN);
                break;
            }
        }
    }

    /**
     * The top slider will skip right to the next color in the color bar that is
     * not the same as the current color the slider is pointing at.
     */
    private void skipTopSliderToRight() {
        // Get the X coordinate of the slider.
        int topXCoord = getXcoord(topSliderX);

        // Get the current color.
        RGB currentRGB = currentColors.get(topXCoord).rgbColor;

        // Loop until a new color is found.
        for (int i = topXCoord + 1; i < currentColors.size(); i++) {
            tmpRGB = currentColors.get(i).rgbColor;

            // If a new color is found then move the slider
            // to the new color.
            if (currentRGB.red != tmpRGB.red
                    || currentRGB.green != tmpRGB.green
                    || currentRGB.blue != tmpRGB.blue) {
                int skipXCoord = Math.round(i * PIXELS_PER_CELL);
                int newSliderCoord = topSliderX + 1;

                while ((getXcoord(newSliderCoord) < getXcoord(skipXCoord)) == true) {
                    ++newSliderCoord;
                }

                drawTopSlider(newSliderCoord + HORIZ_MARGIN);
                break;
            }
        }
    }

    /**
     * The bottom slider will skip right to the next color in the color bar that
     * is not the same as the current color the slider is pointing at.
     */
    private void skipBottomSliderToRight() {
        // Get the X coordinate of the slider.
        int bottomXCoord = getXcoord(bottomSliderX);

        // Get the current color.
        RGB currentRGB = currentColors.get(bottomXCoord).rgbColor;

        // Loop until a new color is found.
        for (int i = bottomXCoord + 1; i < currentColors.size(); i++) {
            tmpRGB = currentColors.get(i).rgbColor;

            // If a new color is found then move the slider
            // to the new color.
            if (currentRGB.red != tmpRGB.red
                    || currentRGB.green != tmpRGB.green
                    || currentRGB.blue != tmpRGB.blue) {
                int skipXCoord = Math.round(i * PIXELS_PER_CELL);
                int newSliderCoord = bottomSliderX + 1;

                while ((getXcoord(newSliderCoord) < getXcoord(skipXCoord)) == true) {
                    ++newSliderCoord;
                }

                drawBottomSlider(newSliderCoord + HORIZ_MARGIN);
                break;
            }
        }
    }

    /**
     * Interpolate between the start and end colors. If the colors are HSB: ---
     * Fills the specified range with an interpolation or blending of the lower
     * color with the upper color. This interpolation along the hue, saturation,
     * brightness color model is linear.
     * 
     * If the colors are RGB: --- Fills the specified range with an
     * interpolation or blending of the lower color with the upper color. This
     * interpolation along the red, green, blue color model is linear.
     * 
     * The alpha value is also interpolated.
     * 
     * @param startColorData
     *            Starting color.
     * @param endColorData
     *            Ending color.
     * @param isRGB
     *            If true the colors are interpolated as RGB, if false the
     *            colors are interpolated as HSB.
     */
    public void interpolate(ColorData startColorData, ColorData endColorData,
            boolean isRGB) {
        float numOfCells = (bottomSliderX - topSliderX) / PIXELS_PER_CELL;

        // Check if the interpolation should be RGB or HSB.
        if (isRGB) {
            // Get the delta between the starting and ending colors for
            // red, green, blue, as well as the alpha.
            float deltaRed = (endColorData.rgbColor.red - startColorData.rgbColor.red)
                    / numOfCells;
            float deltaGreen = (endColorData.rgbColor.green - startColorData.rgbColor.green)
                    / numOfCells;
            float deltaBlue = (endColorData.rgbColor.blue - startColorData.rgbColor.blue)
                    / numOfCells;
            float deltaAlpha = (endColorData.alphaValue - startColorData.alphaValue)
                    / numOfCells;

            // Create an array of new colors using the current colors.
            ArrayList<ColorData> newColors = new ArrayList<ColorData>(
                    currentColors);

            // Initialize local variables.
            ColorData colorData;
            RGB tmpRGB;
            int newRed = 0;
            int newGreen = 0;
            int newBlue = 0;
            int newAlpha = 255;
            int topXCoord = getXcoord(topSliderX);
            int bottomXCoord = getXcoord(bottomSliderX);
            int i = 0;

            // If the top and bottom sliders are in the same position then
            // only one cell color can be changed. We will fill the cell
            // using the starting color.
            if (topXCoord == bottomXCoord) {
                newColors.set(topXCoord, startColorData);
            } else {
                // Loop through all of the cells and fill them with the
                // interpolated colors.
                for (int x = topXCoord; x <= bottomXCoord; ++x) {
                    newRed = Math.round((deltaRed * i)
                            + startColorData.rgbColor.red);
                    newGreen = Math.round((deltaGreen * i)
                            + startColorData.rgbColor.green);
                    newBlue = Math.round((deltaBlue * i)
                            + startColorData.rgbColor.blue);
                    newAlpha = Math.round((deltaAlpha * i)
                            + startColorData.alphaValue);

                    tmpRGB = new RGB(checkRgbColor(newRed),
                            checkRgbColor(newGreen), checkRgbColor(newBlue));
                    colorData = new ColorData(tmpRGB, newAlpha);

                    // If we are at the last cell to be changed then set the
                    // last
                    // color to the ending color.
                    if (x == bottomXCoord) {
                        newColors.set(x, endColorData);
                    } else {
                        newColors.set(x, colorData);
                    }
                    ++i;
                }
            }

            // Put the new color array into the undo/redo list.
            if (currentColorIndex == 0) {
                ++currentColorIndex;
                undoRedoArray.add(newColors);
            } else {
                ++currentColorIndex;
                undoRedoArray.add(currentColorIndex, newColors);
            }

            // Set the current colors to the changed set of colors.
            currentColors = new ArrayList<ColorData>(newColors);

            // Redraw the color bar canvas.
            repaintColorbars();
        } else {
            // Get the starting and ending HSB colors.
            float[] startHSB = startColorData.rgbColor.getHSB();
            float[] endHSB = endColorData.rgbColor.getHSB();

            // Get the deltas for the hue, saturation, brightness, and alpha.
            float deltaHue = getMinHueDiff(startHSB[0], endHSB[0]) / numOfCells;
            float deltaSaturation = (endHSB[1] - startHSB[1]) / numOfCells;
            float deltaBrightness = (endHSB[2] - startHSB[2]) / numOfCells;

            float deltaAlpha = (endColorData.alphaValue - startColorData.alphaValue)
                    / numOfCells;

            // Create a new color array using the current set of colors.
            ArrayList<ColorData> newColors = new ArrayList<ColorData>(
                    currentColors);

            // Initialize local variables.
            ColorData colorData;
            RGB tmpRGB;
            float newHue = 0;
            float newSaturation = 0;
            float newBrightness = 0;
            int newAlpha = 255;
            int topXCoord = getXcoord(topSliderX);
            int bottomXCoord = getXcoord(bottomSliderX);
            int i = 0;

            // If the top slider and the bottom slider are at the same position
            // then set the cell to the starting color.
            if (topXCoord == bottomXCoord) {
                newColors.set(topXCoord, startColorData);
            } else {
                // Loop through all of the cells and fill them with the
                // interpolated colors.
                for (int x = topXCoord; x <= bottomXCoord; ++x) {
                    newHue = (float) (deltaHue * i) + startHSB[0];
                    newSaturation = (float) (deltaSaturation * i) + startHSB[1];
                    newBrightness = (float) (deltaBrightness * i) + startHSB[2];
                    newAlpha = Math.round((deltaAlpha * i)
                            + startColorData.alphaValue);

                    tmpRGB = new RGB(checkHue(newHue), newSaturation,
                            newBrightness);
                    colorData = new ColorData(tmpRGB, newAlpha);

                    // If we are at the last cell to be changed then
                    // set the last color to the ending color.
                    if (x == bottomXCoord) {
                        newColors.set(x, endColorData);
                    } else {
                        newColors.set(x, colorData);
                    }
                    ++i;
                }
            }

            // Put the new color array into the undo/redo list.
            if (currentColorIndex == 0) {
                ++currentColorIndex;
                undoRedoArray.add(newColors);
            } else {
                ++currentColorIndex;
                undoRedoArray.add(currentColorIndex, newColors);
            }

            // Set the current colors to the changed set of colors.
            currentColors = new ArrayList<ColorData>(newColors);

            // Redraw the color bar canvas.
            repaintColorbars();
        }
    }

    /**
     * Get the minimum difference between hue values. The hue values range from
     * 0 to 359.99 so we need to figure out the shortest "distance" between the
     * two colors.
     * 
     * Example: If the first color is 0 and the second is 170, the colors will
     * go "clockwise" from 0 to 170. If the second color is 190 the the colors
     * will go from 0 to 190 "counterclockwise" (0, 359, 358, ..., 191, 190)
     * 
     * 
     * @param startHue
     *            Starting hue value.
     * @param endHue
     *            Ending hue value.
     * @return The minimum difference between the hue values.
     */
    private float getMinHueDiff(float startHue, float endHue) {
        float diff = endHue - startHue;

        if (Math.abs(diff) > 180) {
            diff = (diff > 0) ? (diff - 360) : (diff + 360);
        }

        return diff;
    }

    /**
     * Valid that the hue value is within the 0 to 359.99 range.
     * 
     * @param hue
     *            Hue value.
     * @return Either the original hue value or the corrected hue value.
     */
    private float checkHue(float hue) {
        if (hue > 360f) {
            hue = hue - 360f;
        } else if (hue < 0f) {
            hue = hue + 360f;
        }

        return hue;
    }

    /**
     * Validate that the R/G/B color is within the 0 to 255 range.
     * 
     * @param color
     *            R/G/B color to validate.
     * @return Existing R/G/B color value or the adjusted color value.
     */
    private int checkRgbColor(int color) {
        if (color > 255) {
            return 255;
        } else if (color < 0) {
            return 0;
        }

        return color;
    }

    /**
     * The adjust X coordinate relative to the slider or mouse position.
     * 
     * @param xCoord
     * @return The converted X coordinate.
     */
    private int getXcoord(int xCoord) {
        int rv;

        if (xCoord < 0) {
            return 0;
        } else if (xCoord > BAR_WIDTH) {
            return 255;
        }

        rv = Math.round((xCoord / PIXELS_PER_CELL));

        if (rv < 0) {
            return 0;
        } else if (rv > 255) {
            return 255;
        }

        return rv;
    }

    public List<ColorData> getCurrentColors() {
        return currentColors;
    }

    public void updateRevertToCurrent() {
        startingColors = new ArrayList<ColorData>(currentColors);
    }

    private List<ColorData> createSliderData() {
        sliderText = new String[256];
        List<ColorData> colorArray = new ArrayList<ColorData>();

        DecimalFormat format = null;

        float difference = cmapParams.getColorMapMax()
                - cmapParams.getColorMapMin();
        float increment = difference / ColorUtil.MAX_VALUE;
        float start = cmapParams.getColorMapMin();
        String units = "";

        UnitConverter unitConv = cmapParams.getImageToDisplayConverter();

        Double lastVal = Double.NaN;

        // TODO: Handle piecewise pixel converts to show ranges (for radar)
        for (int i = 0; i < sliderText.length; ++i) {
            double value = start;

            String textStr = "";

            if (cmapParams.isLogarithmic()) {
                // TODO: Handle case where min/max go from neg to pos
                if (cmapParams.getColorMapMax() >= 0
                        && cmapParams.getColorMapMin() >= 0) {
                    double index = ((float) i) / ColorUtil.MAX_VALUE;
                    value = Math
                            .pow(Math.E,
                                    (Math.log(cmapParams.getColorMapMin()) + (index * (Math
                                            .log(cmapParams.getColorMapMax()) - Math
                                            .log(cmapParams.getColorMapMin())))));
                }
                if (format == null) {
                    format = new DecimalFormat("0.000");
                }
            }

            if (unitConv != null) {
                value = unitConv.convert(value);

                /*
                 * Check if the last value is non a number.
                 */
                if (lastVal.isNaN()) {
                    // If value is not a number then set the text to
                    // "NO DATA".
                    if (((Double) value).isNaN()) {
                        textStr = "NO DATA";
                    }
                    lastVal = value;
                } else {
                    // If value is not a number then prepend ">"
                    // to the value.
                    if (((Double) value).isNaN()) {
                        textStr = "> " + lastVal;
                    } else {
                        lastVal = value;
                    }
                }
            }

            if (format == null && new Double(value).isNaN() == false) {
                int zeros = 0;
                String val = "" + value;
                char[] vals = val.substring(val.indexOf(".") + 1).toCharArray();
                for (int j = 0; j < vals.length; ++j) {
                    if (vals[j] == '0') {
                        ++zeros;
                    } else {
                        ++zeros;
                        break;
                    }
                }
                zeros = Math.min(3, zeros);

                String f = "0.";
                for (int j = 0; j < zeros; ++j) {
                    f += "0";
                }
                format = new DecimalFormat(f);
            }

            String txt;

            /*
             * If textStr doesn't have any text then set txt to the value in the
             * value variable.
             */
            if (textStr.length() == 0) {
                txt = format.format(value);
            } else {
                txt = textStr;
            }

            if (units != null) {
                txt += " " + units;
            }

            sliderText[i] = txt;
            start += increment;
        }

        start = cmapParams.getColorMapMin();
        for (int i = 0; i < sliderText.length; ++i) {
            double value = start;

            if ( cmapParams.getDataMapping() != null ) {
                String dmLabel = cmapParams
                        .getDataMapping().getLabelValueForDataValue(value);
                if ( dmLabel != null && !"".equals(dmLabel.trim()) ) {
                    sliderText[i] = dmLabel;
                }
            }
            start += increment;
        }
            
        colorArray = ColorUtil.buildColorData((ColorMap) cmapParams
                .getColorMap());

        /*
         * If the colorArray is less then 256, then we spread all of the
         * elements of the colorArray out to fill a 256 element array.
         */
        if (colorArray.size() < 256) {
            ArrayList<ColorData> tmpColorArray = new ArrayList<ColorData>(256);
            double adjustNum = (colorArray.size() - 1) / 255.0;

            ColorData colorData;

            for (int j = 0; j < 256; j++) {

                int index = (int) Math.round(j * adjustNum);

                colorData = new ColorData(colorArray.get(index).rgbColor,
                        colorArray.get(index).alphaValue);
                tmpColorArray.add(colorData);
            }

            colorArray = new ArrayList<ColorData>(tmpColorArray);
        }
        return colorArray;
    }

    public Point getSliderRange() {
        return new Point(getXcoord(topSliderX), getXcoord(bottomSliderX));
    }

    /**
     * Repaint the colobar's canvas because if alpha mask state has changed
     */
    public void repaint() {
        colorBarCanvas.redraw();
    }

    /**
     * Paint the color bar images
     */
    public void repaintColorbars() {
        // Don't do this if I am disposed;
        if (isDisposed()) {
            return;
        }
        // initialize the color bars
        initalizeColobars();

        // repaint the canvas
        repaint();
    }

    private void initalizeColobars() {
        Image colorBar = new Image(getDisplay(), BAR_WIDTH - 1, BAR_HEIGHT - 1);
        Color black = getDisplay().getSystemColor(SWT.COLOR_BLACK);
        Color white = getDisplay().getSystemColor(SWT.COLOR_WHITE);
        GC gc = new GC(colorBar);

        gc.setBackground(black);
        gc.fillRectangle(0, 0, BAR_WIDTH - 1, BAR_HEIGHT - 1);
        // Draw 2 filled rectangles. One for the top and one for the
        // bottom of the color bar. These rectangles will show up when
        // the alpha
        // channel is set.
        gc.setBackground(white);
        gc.fillRectangle(0, 10, BAR_WIDTH, Math.round(BAR_HEIGHT / 6));
        gc.fillRectangle(0, (BAR_HEIGHT / 2) + 10, BAR_WIDTH,
                Math.round(BAR_HEIGHT / 6));

        ColorData colorData;
        for (int x = 0; x < BAR_WIDTH - 1; ++x) {
            currentColor.dispose();
            int idx = getXcoord(x);
            colorData = currentColors.get(idx);
            currentColor = new Color(parent.getDisplay(), colorData.rgbColor);

            gc.setAlpha(colorData.alphaValue);
            gc.setForeground(currentColor);

            gc.drawLine(x, 0, x, BAR_HEIGHT);
        }

        gc.dispose();

        Image colorBarWithMask = null;

        if (enabledColorMask) {
            colorBarWithMask = new Image(getDisplay(), BAR_WIDTH - 1,
                    BAR_HEIGHT - 1);
            gc = new GC(colorBarWithMask);

            gc.setBackground(black);
            gc.fillRectangle(0, 0, BAR_WIDTH - 1, BAR_HEIGHT - 1);
            // Draw 2 filled rectangles. One for the top and one for the
            // bottom of the color bar. These rectangles will show up when
            // the alpha
            // channel is set.
            gc.setBackground(white);
            gc.fillRectangle(0, 10, BAR_WIDTH, Math.round(BAR_HEIGHT / 6));
            gc.fillRectangle(0, (BAR_HEIGHT / 2) + 10, BAR_WIDTH,
                    Math.round(BAR_HEIGHT / 6));

            byte[] mask = cmapParams.getAlphaMask();

            for (int x = 0; x < BAR_WIDTH - 1; ++x) {
                currentColor.dispose();
                int idx = getXcoord(x);
                colorData = currentColors.get(idx);
                currentColor = new Color(parent.getDisplay(),
                        colorData.rgbColor);
                if (mask[idx] == 0) {
                    gc.setAlpha(colorData.alphaValue);
                    gc.setForeground(currentColor);
                } else {
                    gc.setAlpha(255);
                    gc.setForeground(black);
                }

                gc.drawLine(x, 0, x, BAR_HEIGHT);
            }

            gc.dispose();
        }

        synchronized (this) {
            if (this.colorBar != null) {
                this.colorBar.dispose();
            }
            this.colorBar = colorBar;

            if (colorBarWithMask != null) {
                if (this.colorBarWithMask != null) {
                    this.colorBarWithMask.dispose();
                }
                this.colorBarWithMask = colorBarWithMask;
            }
        }
    }

    @Override
    public void setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        if (!enabled) {
            revertColorBar();
        }
        topSkipLeft.setEnabled(enabled);
        bottomSkipLeft.setEnabled(enabled);
        topMoveLeft.setEnabled(enabled);
        bottomMoveLeft.setEnabled(enabled);
        topSkipRight.setEnabled(enabled);
        bottomSkipRight.setEnabled(enabled);
        topMoveRight.setEnabled(enabled);
        bottomMoveRight.setEnabled(enabled);
        colorBarCanvas.setEnabled(enabled);
        colorBarCanvas.redraw();
    }

    public boolean canUndo() {
        return undoRedoArray.size() > 1 && currentColorIndex > 0;
    }

    public boolean canRedo() {
        return undoRedoArray.size() > 1
                && currentColorIndex < (undoRedoArray.size() - 1);
    }
}
