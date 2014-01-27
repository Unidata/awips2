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
package com.raytheon.viz.aviation.climatology;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Region;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;

/**
 * TrendSliderComp class is the base class containing the draw canvas, range
 * text control, and the value text control. The canvas is sized for drawing
 * slider controls (Wind Speed, Ceiling, Visibility).
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 12 Aug 2013  #2256      lvenable    Added calcArrow() method and code to dispose of the region.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public abstract class TrendSliderComp extends Composite {
    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Dash line length
     */
    protected int dashLine = 5;

    /**
     * Drawing canvas.
     */
    protected Canvas drawingCanvas;

    /**
     * Font used on the canvas.
     */
    private Font canvasFont;

    /**
     * Group container to hold the controls.
     */
    private Group groupContainer;

    /**
     * Value text control.
     */
    protected Text valueTF;

    /**
     * Range text control.
     */
    protected Text rangeTF;

    /**
     * Group title.
     */
    private String title;

    /**
     * Area defined by points.
     */
    protected Region region;

    /**
     * Flag indicating if validation is taking place.
     */
    private boolean validationInProcess = false;

    /**
     * Invalid integer value.
     */
    protected final int INVALID_INT = -9999;

    /**
     * Canvas width.
     */
    protected final int canvasWidth = 300;

    /**
     * Canvas width.
     */
    protected final int canvasHeight = 50;

    /**
     * Y coordinate of the horizontal line.
     */
    protected int hLineYcoord = 25;

    /**
     * Starting X coordinate of the horizontal line.
     */
    protected int hLineStartXCoord = 10;

    /**
     * Ending X coordinate of the horizontal line.
     */
    protected int hLineEndXCoord = canvasWidth - hLineStartXCoord;

    /**
     * X coordinate of the arrow center.
     */
    protected int arrowCenterXCoord = 0;

    /**
     * Y coordinate of the arrow's top.
     */
    protected int arrowTopYCoord = 16;

    /**
     * Y coordinate of the arrow's bottom.
     */
    protected int arrowBottomYCoord = 25;

    /**
     * Points making up the shape of the arrow.
     */
    protected int[] arrowPoints;

    /**
     * Rectangle of the bar.
     */
    protected Rectangle barRect;

    /**
     * Width of the range bar.
     */
    protected int barWidth;

    /**
     * X coordinate of the right side of the range bar.
     */
    protected int barRightXCoord;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param title
     *            Group title.
     */
    public TrendSliderComp(Composite parent, String title) {
        super(parent, SWT.NONE);
        this.title = title;
        this.parent = parent;

        initComponents();
    }

    /**
     * Initialize the components on the composite.
     */
    private void initComponents() {
        canvasFont = new Font(parent.getDisplay(), "Monospace", 9, SWT.NORMAL);
        region = new Region(parent.getDisplay());

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        this.setLayout(gl);
        this.setLayoutData(gd);

        groupContainer = new Group(this, SWT.NONE);
        groupContainer.setText(title);
        groupContainer.setLayout(new GridLayout(1, false));
        groupContainer.setLayoutData(gd);

        setupCanvas();

        setupTextControls();

        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                canvasFont.dispose();
                region.dispose();
            }
        });
    }

    /**
     * Setup the drawing canvas.
     */
    private void setupCanvas() {
        drawingCanvas = new Canvas(groupContainer, SWT.DOUBLE_BUFFERED);
        drawingCanvas.setLayoutData(new GridData(canvasWidth, canvasHeight));
        drawingCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                GC gc = e.gc;
                gc.setFont(canvasFont);
                gc.setAntialias(SWT.ON);
                drawCanvas(gc);
            }
        });
    }

    /**
     * Setup the Value and Range text controls.
     */
    private void setupTextControls() {
        Composite textComp = new Composite(groupContainer, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        textComp.setLayout(gl);

        Label valueLbl = new Label(textComp, SWT.NONE);
        valueLbl.setText("Value:");

        GridData gd = new GridData(90, SWT.DEFAULT);
        valueTF = new Text(textComp, SWT.BORDER);
        valueTF.setLayoutData(gd);

        Label rangeLbl = new Label(textComp, SWT.NONE);
        rangeLbl.setText("Range:");

        gd = new GridData(90, SWT.DEFAULT);
        rangeTF = new Text(textComp, SWT.BORDER);
        rangeTF.setLayoutData(gd);
    }

    /**
     * Add listeners to the value text control.
     */
    public void validateValueListeners() {
        valueTF.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent ke) {
                if (ke.keyCode == SWT.KEYPAD_CR || ke.keyCode == SWT.CR) {
                    validationInProcess = true;
                    validateValueInputs();
                    validationInProcess = false;
                }
            }

            public void keyReleased(KeyEvent ke) {
                // Do nothing...
            }
        });

        valueTF.addFocusListener(new FocusAdapter() {
            public void focusLost(FocusEvent event) {
                // Check if the text field is being validated
                // by the 'Enter' key.
                if (validationInProcess == true) {
                    return;
                }
                validateValueInputs();
            }
        });
    }

    /**
     * Add listeners to the range text control.
     */
    public void validateRangeListeners() {
        rangeTF.addKeyListener(new KeyListener() {
            public void keyPressed(KeyEvent ke) {
                if (ke.keyCode == SWT.KEYPAD_CR || ke.keyCode == SWT.CR) {
                    validationInProcess = true;
                    validateRangeInputs();
                    validationInProcess = false;
                }
            }

            public void keyReleased(KeyEvent ke) {
                // Do nothing...
            }
        });

        rangeTF.addFocusListener(new FocusAdapter() {
            public void focusLost(FocusEvent event) {
                // Check if the text field is being validated
                // by the 'Enter' key.
                if (validationInProcess == true) {
                    return;
                }
                validateRangeInputs();
            }
        });
    }

    /**
     * Display an information box with a message.
     * 
     * @param information
     *            Message string.
     */
    public void userInformation(String information) {
        MessageBox mb = new MessageBox(parent.getShell(), SWT.ICON_ERROR
                | SWT.OK);
        mb.setText("Notice");
        mb.setMessage(information);
        mb.open();
    }

    /**
     * Calculate the arrow position.
     */
    protected void calcArrow() {
        if (region.isEmpty() == false) {
            region.subtract(arrowPoints);
        }

        arrowPoints = new int[] { arrowCenterXCoord, arrowBottomYCoord,
                arrowCenterXCoord + 5, arrowTopYCoord, arrowCenterXCoord - 5,
                arrowTopYCoord, arrowCenterXCoord, arrowBottomYCoord };

        region.add(arrowPoints);
    }

    /**
     * This method is implemented by the class extending this abstract class and
     * is used to draw information on the canvas.
     * 
     * @param gc
     *            The graphical context.
     */
    abstract void drawCanvas(GC gc);

    /**
     * Validate the user input in the value text control.
     */
    abstract void validateValueInputs();

    /**
     * Validate the user input in the range text control.
     */
    abstract void validateRangeInputs();
}
