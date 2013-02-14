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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.colordialog.ColorWheelComp;

/**
 * Display the Spell Checker dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 21 APR 2008  ###        lvenable    Initial creation
 * 08 NOV 2012  1298       rferrel     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ZoneColorEditorDlg extends CaveSWTDialog implements MouseListener,
        MouseMoveListener {

    /**
     * Color wheel composite used to select colors.
     */
    private ColorWheelComp zoneColorWheel;

    /**
     * Undo button.
     */
    private Button undoBtn;

    /**
     * Revert button.
     */
    private Button revertBtn;

    /**
     * Canvas displaying the Zone Group Colors.
     */
    private Canvas colorMapCanvas;

    /**
     * Canvas width.
     */
    private final int CANVAS_WIDTH = 560;

    /**
     * Color bar width.
     */
    private final int COLOR_BAR_WIDTH = 540;

    /**
     * Color bar height.
     */
    private final int COLOR_BAR_HEIGHT = 65;

    /**
     * Color bar X coordinate.
     */
    private final int COLOR_BAR_X_COORD = (CANVAS_WIDTH - COLOR_BAR_WIDTH) / 2;

    /**
     * Color bar Y coordinate.
     */
    private final int COLOR_BAR_Y_COORD = 30;

    /**
     * Array of RGB colors.
     */
    protected List<RGB> rgbArray;

    /**
     * Original RGB color array (starting colors).
     */
    private List<RGB> originalRgbArray;

    /**
     * Width of each cell.
     */
    private double cellWidth = 0.0;

    /**
     * Current color.
     */
    private Color currentColor;

    /**
     * Flag indicating if the mouse is over the
     */
    private boolean mouseOverColorBar = false;

    /**
     * Mouse X coordinate.
     */
    private int mouseXCoord = 0;

    /**
     * RGB of the cell before the RGB was changed.
     */
    private RGB lastRGB = null;

    /**
     * Index of the RGB cell that was last changed.
     */
    private int lastRGBIndex = 0;

    /**
     * Rectange area of the zone group color bar.
     */
    private Rectangle colorBarRect = new Rectangle(COLOR_BAR_X_COORD,
            COLOR_BAR_Y_COORD, COLOR_BAR_WIDTH, COLOR_BAR_HEIGHT);

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 130;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param colorArray
     *            Array of RGB colors.
     */
    public ZoneColorEditorDlg(Shell parent, List<RGB> colorArray) {
        super(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL,
                CAVE.DO_NOT_BLOCK);

        this.rgbArray = colorArray;

        originalRgbArray = new ArrayList<RGB>(colorArray);

        cellWidth = COLOR_BAR_WIDTH / (double) colorArray.size();
    }

    @Override
    protected void initializeComponents(Shell shell) {

        shell.setText("Zone Color Table Editor");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the controls on the display.
     */
    private void initializeComponents() {
        createColorControls();
        createBottomButtons();
    }

    /**
     * Create the color controls.
     */
    private void createColorControls() {
        // Create the color wheel.
        zoneColorWheel = new ColorWheelComp(shell, "Color Control", true);
        zoneColorWheel.showRgbSliders(false);

        // Create the Zone Group Color canvas.
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = CANVAS_WIDTH;
        gd.heightHint = CANVAS_HEIGHT;
        colorMapCanvas = new Canvas(shell, SWT.DOUBLE_BUFFERED);
        colorMapCanvas.setLayoutData(gd);
        colorMapCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });

        colorMapCanvas.addMouseMoveListener(this);
        colorMapCanvas.addMouseListener(this);

        // Create the Undo button.
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        undoBtn = new Button(shell, SWT.PUSH);
        undoBtn.setText("Undo");
        undoBtn.setEnabled(false);
        undoBtn.setLayoutData(gd);
        undoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                undoBtn.setEnabled(false);

                rgbArray.set(lastRGBIndex, lastRGB);

                colorMapCanvas.redraw();
            }
        });

        // Create the Revert button.
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        revertBtn = new Button(shell, SWT.PUSH);
        revertBtn.setText("Revert");
        revertBtn.setLayoutData(gd);
        revertBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                rgbArray.clear();
                rgbArray.addAll(originalRgbArray);

                colorMapCanvas.redraw();
            }
        });
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        buttonArea.setLayout(new GridLayout(1, false));

        // The intent is for this composite to be centered
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayoutData(gd);
        buttons.setLayout(new GridLayout(2, true));

        gd = new GridData(120, SWT.DEFAULT);
        Button applyBtn = new Button(buttons, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                originalRgbArray = rgbArray;
                close();
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        Button dismissBtn = new Button(buttons, SWT.PUSH);
        dismissBtn.setText("Dismiss");
        dismissBtn.setLayoutData(gd);
        dismissBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(null);
                rgbArray.clear();
                rgbArray.addAll(originalRgbArray);
                close();
            }
        });
    }

    /**
     * Draw (paint) the canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawCanvas(GC gc) {
        gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        gc.drawString("Zone Group Colors", 10, 5, true);

        // -------------------------------------------------
        // Draw the color cells
        // -------------------------------------------------

        drawColorCells(gc);

        if (mouseOverColorBar == true) {
            drawColorNumberLabel(gc);
        }
    }

    /**
     * Draw the color cells (the color across the bar).
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawColorCells(GC gc) {
        // -------------------------------------------------
        // Draw the color cells
        // -------------------------------------------------

        int cellXCoord = 0;
        for (int i = 0; i < rgbArray.size(); i++) {
            cellXCoord = COLOR_BAR_X_COORD + (int) Math.round(cellWidth * i);

            if (currentColor != null) {
                currentColor.dispose();
            }
            currentColor = new Color(getDisplay(), rgbArray.get(i));
            gc.setBackground(currentColor);

            gc.fillRectangle(cellXCoord, COLOR_BAR_Y_COORD,
                    (int) Math.round(cellWidth), COLOR_BAR_HEIGHT);

            if (i != 0) {
                gc.drawLine(cellXCoord, COLOR_BAR_Y_COORD, cellXCoord,
                        COLOR_BAR_Y_COORD + COLOR_BAR_HEIGHT);
            }
        }

        // -------------------------------------------------
        // Draw the color bar boundaries
        // -------------------------------------------------
        gc.drawRectangle(COLOR_BAR_X_COORD, COLOR_BAR_Y_COORD, COLOR_BAR_WIDTH,
                COLOR_BAR_HEIGHT);
    }

    /**
     * Draw the color cell number that appears below the color cell. The number
     * appears when the mouse is over the cell.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawColorNumberLabel(GC gc) {
        int fontAveWidth = gc.getFontMetrics().getAverageCharWidth();

        int x = (int) (((mouseXCoord - COLOR_BAR_X_COORD) / cellWidth) + 1);

        int labelXCoord = (int) (COLOR_BAR_X_COORD + cellWidth * x - cellWidth
                / 2 - fontAveWidth);

        gc.drawString(String.valueOf(x), labelXCoord, COLOR_BAR_Y_COORD
                + COLOR_BAR_HEIGHT + 8, true);
    }

    /**
     * Method called when the mouse moves over the Zone Group Color canvas.
     * 
     * @param MouseEvent
     *            e Mouse event.
     */
    public void mouseMove(MouseEvent e) {
        mouseXCoord = e.x;

        if (colorBarRect.contains(e.x, e.y) == true) {
            mouseOverColorBar = true;
            colorMapCanvas.redraw();
        } else {
            if (mouseOverColorBar == true) {
                colorMapCanvas.redraw();
                mouseOverColorBar = false;
            }
        }
    }

    /**
     * NOT IMPLEMENTED...
     * 
     * @param MouseEvent
     *            e Mouse event.
     */
    public void mouseDoubleClick(MouseEvent e) {
    }

    /**
     * Method called when the mouse button is pressed while over the Zone Group
     * Color canvas.
     * 
     * @param MouseEvent
     *            e Mouse event.
     */
    public void mouseDown(MouseEvent e) {
        mouseXCoord = e.x;

        // If the mouse press is inside the color bar
        // then change the color cell to the selected
        // color in the color wheel.
        if (colorBarRect.contains(e.x, e.y) == true) {
            undoBtn.setEnabled(true);
            int x = (int) ((mouseXCoord - COLOR_BAR_X_COORD) / cellWidth);

            lastRGB = rgbArray.get(x);
            lastRGBIndex = x;

            rgbArray.set(x, zoneColorWheel.getColorData().rgbColor);

            colorMapCanvas.redraw();
        }
    }

    /**
     * NOT IMPLEMENTED...
     * 
     * @param MouseEvent
     *            e Mouse event.
     */
    public void mouseUp(MouseEvent e) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        if (currentColor != null) {
            currentColor.dispose();
        }
    }
}
