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
package com.raytheon.uf.viz.monitor.ffmp.ffti;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Scale;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Total duration scale for the FFTI display.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ????                                Initial creation
 * Oct 7, 2013  #2437      lvenable     Fixed color memory leak
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class TotalDurScaleComp extends Composite {

    /** Parent composite. */
    private Composite parent;

    /** Scale value canvas. */
    private Canvas scaleValueCanvas;

    /** Scale range canvas. */
    private Canvas scaleRangeCanvas;

    /** Time duration scale control. */
    private Scale timeDurScale;

    /** Font for the canvas. */
    private Font canvasFont;

    /** Time duration hours. */
    private double timeDurHours = 0.0;

    /** Canvas height. */
    private final int CANVAS_HEIGHT = 20;

    /** Canvas width. */
    private int CANVAS_WIDTH = 650;

    /** X coordinate offset in pixels. */
    private int xCoordOffset = 5;

    /** Low value. */
    private double lowerVal = 0.00;

    /** Default retention time. */
    private final int DEFAULT_RETENTION_TIME = 24;

    /** Retention time. */
    private int retentiontime = DEFAULT_RETENTION_TIME;

    /** Range value. */
    private double rangeVal = 12;

    /** Array of display numbers. */
    private double[] displayNumbers;

    /** Duration interface. */
    private DurationInterface owner = null;

    /** Color yellow. */
    private Color yellowClr;

    /** Color grey. */
    private Color greyClr;

    /**
     * Constructor.
     * 
     * @param parent
     */
    public TotalDurScaleComp(Composite parent) {
        super(parent, 0);

        this.parent = parent;

        init();
    }

    private void init() {
        retentiontime = this.getFFMPRetentiontime();
        rangeVal = (Math.abs(lowerVal) + Math.abs(retentiontime));

        displayNumbers = new double[(int) rangeVal / 2 + 1];
        for (int i = 0; i < displayNumbers.length; i++)
            displayNumbers[i] = lowerVal + i * 2;

        // Set the font and colors.
        canvasFont = new Font(parent.getDisplay(), "Monospace", 8, SWT.NORMAL);
        yellowClr = new Color(parent.getDisplay(), 255, 255, 224);
        greyClr = new Color(parent.getDisplay(), 239, 239, 239);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, false, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 0;
        gl.marginHeight = 0;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createControls();

        this.pack();

        this.addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent arg0) {
                canvasFont.dispose();
                yellowClr.dispose();
                greyClr.dispose();
            }
        });
    }

    private void createControls() {
        scaleValueCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);
        scaleValueCanvas.setLayoutData(new GridData(CANVAS_WIDTH + 10,
                CANVAS_HEIGHT));
        scaleValueCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawScaleValueCanvas(e.gc);
            }
        });

        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gd.widthHint = CANVAS_WIDTH;
        timeDurScale = new Scale(this, SWT.HORIZONTAL);
        timeDurScale.setMinimum(0);
        timeDurScale.setMaximum(retentiontime * 4);
        timeDurScale.setIncrement(1);
        timeDurScale.setPageIncrement(1);

        timeDurScale.setLayoutData(gd);
        timeDurScale.getDisplay();
        timeDurScale.addMouseListener(new MouseListener() {

            @Override
            public void mouseDoubleClick(MouseEvent e) {
            }

            @Override
            public void mouseDown(MouseEvent e) {
            }

            @Override
            public void mouseUp(MouseEvent e) {
                owner.updateQPEDurHour(timeDurHours);
                owner.updateGuidDurHour(timeDurHours);
                owner.updateAccumAttrib(timeDurHours);
            }
        });
        timeDurScale.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                calcTimeDurHours();
                setTimeDuration(timeDurHours);
                owner.updateAccumAttrib(timeDurHours);
            }
        });

        calcTimeDurHours();

        scaleRangeCanvas = new Canvas(this, SWT.DOUBLE_BUFFERED);
        scaleRangeCanvas.setLayoutData(new GridData(CANVAS_WIDTH + 10,
                CANVAS_HEIGHT));
        scaleRangeCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawScaleRangeCanvas(e.gc);
            }
        });
    }

    public void setScaleToYellow() {
        timeDurScale.setForeground(yellowClr);
        timeDurScale.setBackground(yellowClr);
    }

    public void setScaleToGrey() {
        timeDurScale.setForeground(greyClr);
        timeDurScale.setBackground(greyClr);
    }

    private void drawScaleValueCanvas(GC gc) {
        int fontAveWidth = (int) gc.getFontMetrics().getAverageCharWidth();

        double pixPerInc = (CANVAS_WIDTH - 35 - xCoordOffset) / rangeVal;

        gc.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        int newXCoord = (int) Math.round(((this.timeDurHours + Math
                .abs(lowerVal)) * pixPerInc + xCoordOffset));

        if (Math.abs(this.timeDurHours) >= 10) {
            newXCoord -= fontAveWidth / 2 - 2;
        }

        if (this.timeDurHours < 0.0) {
            newXCoord -= 5;
        }

        gc.drawString(String.format("%2.2f", this.timeDurHours), newXCoord, 1,
                true);
    }

    private void drawScaleRangeCanvas(GC gc) {
        int fontAveWidth = (int) gc.getFontMetrics().getAverageCharWidth();

        double pixPerInc = (CANVAS_WIDTH - 35 - xCoordOffset) / rangeVal;

        gc.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        for (double dVal : displayNumbers) {
            int newXCoord = (int) Math.round(((dVal + Math.abs(lowerVal))
                    * pixPerInc + xCoordOffset));

            if (Math.abs(dVal) >= 10) {
                newXCoord -= fontAveWidth / 2 - 2;
            }

            if (dVal < 0.0) {
                newXCoord -= 5;
            }

            gc.drawString(String.format("%2.2f", dVal), newXCoord, 1, true);
        }
    }

    private void calcTimeDurHours() {
        timeDurHours = ((((double) timeDurScale.getSelection()) * .25));
    }

    private void setTimeDurationScale(double val) {
        timeDurScale.setSelection((int) (val / .25));
    }

    /**
     * An equavalant of changeTotalDur in FFTIhandler.C
     * 
     * @param val
     */
    public void setTimeDuration(double val) {
        timeDurHours = owner.adjustTotalDurationHr(val);

        setTimeDurationScale(timeDurHours);
        scaleValueCanvas.redraw();
    }

    /**
     * Get the selected value.
     * 
     * @return The selected value.
     */
    public double getSelectedValue() {
        return timeDurHours;
    }

    /**
     * get the retention time (purge hour) from metadata.plugin_info
     * 
     * @return retention time
     */
    private int getFFMPRetentiontime() {
        StringBuilder query = new StringBuilder();
        query.append("select retentiontime from plugin_info where name = 'ffmp'");

        List<Object[]> results;
        try {
            results = DirectDbQuery.executeQuery(query.toString(), "metadata",
                    QueryLanguage.SQL);
            if (results.size() == 1) { // there should be only one
                Object[] objs = results.get(0);
                Integer retentiontime = (Integer) objs[0];
                return retentiontime.intValue();
            }

        } catch (VizException e) {
            return DEFAULT_RETENTION_TIME;
        }

        return DEFAULT_RETENTION_TIME;
    }

    /**
     * Set the owner.
     * 
     * @param owner
     *            The owner.
     */
    public void setOwner(DurationInterface owner) {
        this.owner = owner;
    }
}
