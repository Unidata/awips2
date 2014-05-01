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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;

/**
 * TrendDialComp class is the base class containing the draw canvas, range text
 * control, and the value text control. The canvas is sized for drawing dials
 * (Date, Hour, Wind Direction).
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation. 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
abstract class TrendDialComp extends Composite {
    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * Font used on the canvas.
     */
    private Font canvasFont;

    /**
     * Canvas to draw on.
     */
    protected Canvas drawingCanvas;

    /**
     * Value text control.
     */
    protected Text valueTF;

    /**
     * Range text control.
     */
    protected Text rangeTF;

    /**
     * Title of the trend information.
     */
    private String title;

    /**
     * Length of the dash on the dial.
     */
    protected final int dashLength = 5;

    /**
     * Canvas width.
     */
    protected final int canvasWidth = 110;

    /**
     * Canvas height.
     */
    protected final int canvasHeight = 110;

    /**
     * The X,Y coordinate values of the dial.
     */
    protected int dialXYVal = 20;

    /**
     * X coordinate of the dial's center.
     */
    protected int centerX = canvasWidth / 2;

    /**
     * Y coordinate of the dial's center.
     */
    protected int centerY = canvasHeight / 2;

    /**
     * Radius of the dial arm..
     */
    protected double dialArmRadius = (canvasWidth - 20 * 2) / 2 - 4;

    /**
     * Radius for the range line.
     */
    protected final double circleRadiusForRangeLine = (canvasHeight - 20 * 2) / 2 - 1;

    /**
     * Zero radius value.
     */
    protected final double zeroRadius = 0.0;

    /**
     * Mouse position.
     */
    protected Point mousePos = new Point(centerX, centerY);

    /**
     * Degree of the dial arm.
     */
    protected double dialArmDegree = 0.0;

    /**
     * Degree of the start range.
     */
    protected double rangeStartDegree = 0.0;

    /**
     * Degree of the end range.
     */
    protected double rangeEndDegree = 90.0;

    /**
     * Flag indicating if the dial arm should be moved.
     */
    protected boolean moveDialArm = true;

    /**
     * Flag indicating if the start range should be moved.
     */
    protected boolean moveRangeStart = false;

    /**
     * Flag indicating if the end range should be moved.
     */
    protected boolean moveRangeEnd = false;

    /**
     * Flag indicating if the dial arm, start range, and end range should be
     * moved.
     */
    protected boolean moveAll = false;

    /**
     * Flag indicating if validation is taking place.
     */
    private boolean validationInProcess = false;

    /**
     * Radians to degrees value.
     */
    protected final double rad2deg = 180.0 / Math.PI;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param title
     *            Title of the trend information.
     */
    public TrendDialComp(Composite parent, String title) {
        super(parent, SWT.NONE);

        this.parent = parent;

        this.title = title;

        initComponents();
    }

    /**
     * Initialize the components on the composite.
     */
    private void initComponents() {
        canvasFont = new Font(parent.getDisplay(), "Monospace", 9, SWT.NORMAL);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(2, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        this.setLayout(gl);
        this.setLayoutData(gd);

        setupCanvas();

        setupTextControls();

        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                canvasFont.dispose();
            }
        });
    }

    /**
     * Setup the drawing canvas.
     */
    private void setupCanvas() {
        drawingCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);
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
        Composite textComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        textComp.setLayout(gl);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Label titleLbl = new Label(textComp, SWT.CENTER);
        titleLbl.setText(title);
        titleLbl.setLayoutData(gd);

        Label valueLbl = new Label(textComp, SWT.NONE);
        valueLbl.setText("Value:");

        gd = new GridData(100, SWT.DEFAULT);
        valueTF = new Text(textComp, SWT.BORDER);
        valueTF.setLayoutData(gd);

        Label rangeLbl = new Label(textComp, SWT.NONE);
        rangeLbl.setText("Range:");

        gd = new GridData(100, SWT.DEFAULT);
        rangeTF = new Text(textComp, SWT.BORDER);
        rangeTF.setLayoutData(gd);
    }

    /**
     * Get the font used on the canvas.
     * 
     * @return The canvas font.
     */
    public Font getFont() {
        return canvasFont;
    }

    /**
     * Get the dash length.
     * 
     * @return The dash length.
     */
    public int getDashLength() {
        return dashLength;
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
     * Calculate the dial arm degree given the radian.
     * 
     * @param radian
     *            Radian.
     */
    public void calculateDialArmDegree(double radian) {
        double neg180to180DegreeArm = Math.toDegrees(radian);

        double degree0to360 = neg180to180DegreeArm;

        if (degree0to360 < 0) {
            degree0to360 += 360.0;
        }

        dialArmDegree = degree0to360;
    }

    /**
     * Calculate the start range degree given the radian.
     * 
     * @param radian
     *            Radian.
     */
    public void calculateRangeStartDegree(double radian) {
        double neg180to180DegreeTmp = Math.toDegrees(radian);

        double degree0to360 = neg180to180DegreeTmp;

        if (degree0to360 < 0) {
            degree0to360 += 360.0;
        }

        rangeStartDegree = degree0to360;
    }

    /**
     * Calculate the end range degree given the radian.
     * 
     * @param radian
     *            Radian.
     */
    public void calculateRangeEndDegree(double radian) {
        double neg180to180DegreeTmp = Math.toDegrees(radian);

        double degree0to360 = neg180to180DegreeTmp;

        if (degree0to360 < 0) {
            degree0to360 += 360.0;
        }

        rangeEndDegree = degree0to360;
    }

    /**
     * Draw the range arc on the dial.
     * 
     * @param gc
     *            Graphics context.
     */
    public void drawRangeArc(GC gc) {
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_RED));
        gc.setAlpha(75);

        int startDegree = (int) Math.round(rangeEndDegree);
        int arcAngle = (int) Math.round(rangeStartDegree - rangeEndDegree);

        double degreeOffset = 360.0;
        if (rangeStartDegree == 0.0) {
            degreeOffset = 0.0;
        }
        startDegree = (int) Math.round(degreeOffset - rangeStartDegree + 90.0);

        if (rangeEndDegree < rangeStartDegree) {
            arcAngle = (int) Math
                    .round((rangeEndDegree + 360.0 - rangeStartDegree) * -1);
        } else {
            arcAngle = (int) Math.round((rangeEndDegree - rangeStartDegree)
                    * -1);
        }

        gc.fillArc((canvasWidth - (canvasWidth - dialXYVal * 2)) / 2,
                (canvasHeight - (canvasHeight - dialXYVal * 2)) / 2,
                canvasWidth - dialXYVal * 2, canvasHeight - dialXYVal * 2,
                startDegree, arcAngle);

        gc.setAlpha(255);

        drawRangeLines(gc);

        gc.setLineWidth(2);
        gc.drawArc((canvasWidth - (canvasWidth - dialXYVal * 2)) / 2,
                (canvasHeight - (canvasHeight - dialXYVal * 2)) / 2,
                canvasWidth - dialXYVal * 2, canvasHeight - dialXYVal * 2,
                startDegree, arcAngle);
        gc.setLineWidth(1);

        setRangeText(rangeStartDegree, rangeEndDegree);
    }

    /**
     * Draw the range lines that outline the range arc.
     * 
     * @param gc
     *            Graphics context.
     */
    public void drawRangeLines(GC gc) {
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_RED));

        gc.setLineCap(SWT.CAP_ROUND);
        gc.setLineWidth(2);

        double neg180to180DegreeArm = rangeStartDegree;
        if (neg180to180DegreeArm > 180.0) {
            neg180to180DegreeArm += -360.0;
        }

        double theta = neg180to180DegreeArm / rad2deg;

        int x = (int) (circleRadiusForRangeLine * Math.sin(theta));
        int y = (int) (-circleRadiusForRangeLine * Math.cos(theta));

        int x2 = (int) (zeroRadius * Math.sin(theta));
        int y2 = (int) (-zeroRadius * Math.cos(theta));

        gc.drawLine((int) Math.round(centerX + x2), (int) Math.round(centerY
                + y2), (int) Math.round(centerX + x), (int) Math.round(centerY
                + y));

        neg180to180DegreeArm = rangeEndDegree;
        if (neg180to180DegreeArm > 180.0) {
            neg180to180DegreeArm += -360.0;
        }

        theta = neg180to180DegreeArm / rad2deg;

        x = (int) (circleRadiusForRangeLine * Math.sin(theta));
        y = (int) (-circleRadiusForRangeLine * Math.cos(theta));

        x2 = (int) (zeroRadius * Math.sin(theta));
        y2 = (int) (-zeroRadius * Math.cos(theta));

        gc.drawLine((int) Math.round(centerX + x2), (int) Math.round(centerY
                + y2), (int) Math.round(centerX + x), (int) Math.round(centerY
                + y));

        gc.setLineWidth(1);
    }

    /**
     * Get a point on a circle.
     * 
     * @param x1
     *            X coordinate.
     * @param y1
     *            Y coordinate.
     * @param radius
     *            Circle radius.
     * @param angle
     *            Angle.
     * @return Array of points (x, y coordinates).
     */
    public int[] getPointOnCircle(double x1, double y1, double radius,
            double angle) {
        int pointOnCircle[] = new int[2];

        pointOnCircle[0] = (int) (x1 + radius * Math.cos(Math.toRadians(angle)));
        pointOnCircle[1] = (int) (y1 + radius * Math.sin(Math.toRadians(angle)));
        return pointOnCircle;
    }

    /**
     * Move the dial arm, one of the range lines, or all of the lines on the
     * dial.
     */
    public void moveLine() {
        if (moveAll == true) {
            double startingAng = dialArmDegree;
            double dialArmRadian = Math.atan2(mousePos.x - centerX, centerY
                    - mousePos.y);
            calculateDialArmDegree(dialArmRadian);

            double diff = startingAng - dialArmDegree;

            rangeStartDegree -= diff;
            rangeEndDegree -= diff;

            if (rangeStartDegree > 359.999) {
                rangeStartDegree -= 360.0;
            } else if (rangeStartDegree < 0.0) {
                rangeStartDegree += 360.0;
            }

            if (rangeEndDegree > 359.999) {
                rangeEndDegree -= 360.0;
            } else if (rangeEndDegree < 0.0) {
                rangeEndDegree += 360.0;
            }

            drawingCanvas.redraw();

            return;
        } else if (moveDialArm == true) {
            double dialArmRadian = Math.atan2(mousePos.x - centerX, centerY
                    - mousePos.y);
            calculateDialArmDegree(dialArmRadian);
        } else if (moveRangeStart == true) {
            double rangeStartRadian = Math.atan2(mousePos.x - centerX, centerY
                    - mousePos.y);
            calculateRangeStartDegree(rangeStartRadian);
        } else if (moveRangeEnd == true) {
            double rangeEndRadian = Math.atan2(mousePos.x - centerX, centerY
                    - mousePos.y);
            calculateRangeEndDegree(rangeEndRadian);
        }

        // Redraw the canvas.
        drawingCanvas.redraw();
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
     * Draw the arm of the dial.
     * 
     * @param gc
     *            Graphics context.
     */
    public void drawDialArm(GC gc) {
        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLUE));
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_BLUE));

        gc.fillOval(centerX - 4, centerY - 4, 8, 8);

        gc.setLineCap(SWT.CAP_ROUND);
        gc.setLineWidth(2);

        double neg180to180Degree = dialArmDegree;
        if (neg180to180Degree > 180.0) {
            neg180to180Degree += -360.0;
        }

        double theta = neg180to180Degree / rad2deg;

        int x = (int) (dialArmRadius * Math.sin(theta));
        int y = (int) (-dialArmRadius * Math.cos(theta));

        int x2 = (int) (zeroRadius * Math.sin(theta));
        int y2 = (int) (-zeroRadius * Math.cos(theta));

        gc.drawLine((int) Math.round(centerX + x2), (int) Math.round(centerY
                + y2), (int) Math.round(centerX + x), (int) Math.round(centerY
                + y));

        int[] tipEnd1 = getPointOnCircle(centerX, centerY, dialArmRadius - 15,
                neg180to180Degree - 75);

        int[] tipEnd2 = getPointOnCircle(centerX, centerY, dialArmRadius - 15,
                neg180to180Degree - 105);

        gc.drawLine((int) Math.round(centerX + x), (int) Math
                .round(centerY + y), tipEnd1[0], tipEnd1[1]);

        gc.drawLine((int) Math.round(centerX + x), (int) Math
                .round(centerY + y), tipEnd2[0], tipEnd2[1]);

        int[] polygonArray = { (int) Math.round(centerX + x),
                (int) Math.round(centerY + y), tipEnd1[0], tipEnd1[1],
                tipEnd2[0], tipEnd2[1] };

        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_BLUE));
        gc.fillPolygon(polygonArray);

        gc.setLineWidth(1);

        setValueText(dialArmDegree);
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
     * This method is implemented by the class extending this abstract class and
     * is used to set the text in the value text control.
     * 
     * @param degrees
     *            Dial arm in degrees.
     */
    abstract void setValueText(double degrees);

    /**
     * This method is implemented by the class extending this abstract class and
     * is used to set the text in the range text control.
     * 
     * @param startDegrees
     *            Start degree.
     * @param endDegrees
     *            End degree.
     */
    abstract void setRangeText(double startDegrees, double endDegrees);

    /**
     * Validate the user input in the value text control.
     */
    abstract void validateValueInputs();

    /**
     * Validate the user input in the range text control.
     */
    abstract void validateRangeInputs();
}
