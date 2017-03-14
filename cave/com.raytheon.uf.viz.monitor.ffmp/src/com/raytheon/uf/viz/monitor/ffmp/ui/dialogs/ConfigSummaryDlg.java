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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Display FFMP Basin Table's configuration summary data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * Dec 6, 2012  1353       rferrel     Convert to CaveSWTDialog and made non-blocking.
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class ConfigSummaryDlg extends CaveSWTDialog {

    /**
     * Canvas to display the information.
     */
    private Canvas canvas;

    /**
     * Canvas width.
     */
    private int canvasWidth = 50;

    /**
     * Canvas height.
     */
    private int canvasHeight = 50;

    /**
     * Color of the left side of the canvas.
     */
    private Color leftSideColor;

    /**
     * Color of the right side of the canvas.
     */
    private Color rightSideColor;

    /**
     * Text font.
     */
    private Font textFont;

    /**
     * Coordinates of the button that launched this dialog.
     */
    private Point controlLoc;

    /**
     * Configuration summary data to be displayed.
     */
    private ConfigSummaryData cfgSumData;

    /**
     * Maximum data label characters.
     */
    private int maxDataLabelLen = -1;

    /**
     * Maximum configuration data characters.
     */
    private int maxCfgDataLen = -1;

    /**
     * Width of the black colon band in the middle of the dialog.
     */
    private int colonBand = 10;

    /**
     * Size of the borer in pixels.
     */
    private int borderSize = 5;

    /**
     * Width of the text.
     */
    private int textWidth = 8;

    /**
     * Height of the text.
     */
    private int textHeight = 17;

    /**
     * Vertical space between the text.
     */
    private int spaceBetweenText = 5;

    /**
     * String array of config data.
     */
    private String[] cfgData;

    /**
     * Data labels.
     */
    private final String[] dataLabels = new String[] { "Layer",
            "Link to Frame", "Worst Case", "Zoom: Maintain Layer",
            "Zoom: only Basins in Parent", "Include CWAs", "D2D Click Action",
            "D2D Display Type", "Auto-Refresh" };

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param controlLocation
     *            Location of the control that launched this dialog.
     * @param cfgSumData
     *            Configuration summary dialog.
     */
    public ConfigSummaryDlg(Shell parent, Point controlLocation,
            ConfigSummaryData cfgSumData) {
        super(parent, SWT.NO_TRIM, CAVE.DO_NOT_BLOCK);

        this.controlLoc = controlLocation;
        this.cfgSumData = cfgSumData;

        cfgData = this.cfgSumData.getDisplayData();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    private void initializeComponents() {
        Display display = shell.getDisplay();
        /*
         * Setup the font and colors.
         */
        leftSideColor = new Color(display, 245, 245, 212);
        rightSideColor = new Color(display, 255, 238, 190);
        textFont = new Font(display, "Monospace", 10, SWT.BOLD);

        // Make the calculation for the canvas.
        makeCalculations();

        // Create the canvas.
        createCanvas();
    }

    private void createCanvas() {
        canvas = new Canvas(shell, SWT.DOUBLE_BUFFERED);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = canvasHeight;
        gd.widthHint = canvasWidth;

        canvas.setSize(canvasWidth, canvasHeight);

        canvas.setLayoutData(gd);
        canvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });

        /*
         * Add mouse listeners to the canvas so it will be closed when the user
         * moves the mouse outside the dialog or it is clicked.
         */
        canvas.addMouseTrackListener(new MouseTrackAdapter() {

            @Override
            public void mouseExit(MouseEvent e) {
                close();
            }
        });

        canvas.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseDown(MouseEvent e) {
                close();
            }
        });
    }

    /**
     * Draw on the canvas.
     * 
     * @param gc
     *            Graphic context.
     */
    private void drawCanvas(GC gc) {
        Display display = shell.getDisplay();
        gc.setFont(textFont);
        gc.setTextAntialias(SWT.ON);

        gc.setBackground(leftSideColor);
        int tmpWidth = (borderSize * 2) + (maxDataLabelLen * textWidth) + 3;
        gc.fillRectangle(0, 0, tmpWidth, canvasHeight);

        gc.setBackground(rightSideColor);
        int tmpX = (borderSize * 2) + (maxDataLabelLen * textWidth) + colonBand;
        tmpWidth = canvasWidth
                - ((borderSize * 2) + colonBand + (maxDataLabelLen * textWidth));
        gc.fillRectangle(tmpX, 0, tmpWidth, canvasHeight);

        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));

        /*
         * Draw data labels
         */
        int xCoord = (borderSize) + (maxDataLabelLen * textWidth);
        int yCoord = 0;

        for (int i = 0; i < dataLabels.length; i++) {
            xCoord = (borderSize) + (maxDataLabelLen * textWidth)
                    - (dataLabels[i].length() * textWidth);
            yCoord = borderSize + (textHeight * i) + (spaceBetweenText * i);
            gc.drawString(dataLabels[i], xCoord, yCoord, true);
        }

        /*
         * Draw color band
         */
        xCoord = (borderSize * 2) + (maxDataLabelLen * textWidth);
        yCoord = 0;
        gc.setBackground(display.getSystemColor(SWT.COLOR_BLACK));
        gc.fillRectangle(xCoord, yCoord, colonBand, canvasHeight);

        gc.setForeground(display.getSystemColor(SWT.COLOR_WHITE));
        xCoord = (borderSize * 2) + (maxDataLabelLen * textWidth) + colonBand
                / 2 - 4;
        yCoord = 0;
        for (int i = 0; i < cfgData.length; i++) {
            yCoord = borderSize + (textHeight * i) + (spaceBetweenText * i);
            gc.drawString(":", xCoord, yCoord, true);
        }

        /*
         * Draw config data labels
         */
        gc.setForeground(display.getSystemColor(SWT.COLOR_BLACK));
        xCoord = (borderSize * 3) + colonBand + (maxDataLabelLen * textWidth);
        yCoord = 0;

        for (int i = 0; i < cfgData.length; i++) {
            yCoord = borderSize + (textHeight * i) + (spaceBetweenText * i);
            gc.drawString(cfgData[i], xCoord, yCoord, true);
        }
    }

    /**
     * Make the calculation used for draw the data on the canvas.
     */
    private void makeCalculations() {
        Display display = shell.getDisplay();
        Image image = new Image(display, 100, 100);
        GC gc = new GC(image);
        gc.setFont(textFont);

        textWidth = gc.getFontMetrics().getAverageCharWidth();
        textHeight = gc.getFontMetrics().getHeight();

        gc.dispose();
        image.dispose();

        // Find largest data label
        for (String str : dataLabels) {
            if (maxDataLabelLen < str.length()) {
                maxDataLabelLen = str.length();
            }
        }

        // Find largest config data
        for (String str : cfgData) {
            if (maxCfgDataLen < str.length()) {
                maxCfgDataLen = str.length();
            }
        }

        // Calculate the canvas width & height
        canvasWidth = (maxDataLabelLen * textWidth) + (borderSize * 4)
                + (maxCfgDataLen * textWidth) + colonBand;
        canvasHeight = (borderSize * 2) + (dataLabels.length * textHeight)
                + (spaceBetweenText * dataLabels.length - 1);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();
        shell.setLocation(controlLoc.x - 100, controlLoc.y - 100);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        leftSideColor.dispose();
        rightSideColor.dispose();
        textFont.dispose();
    }
}
