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

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Scale;

import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFTIDataManager;
import com.raytheon.uf.common.monitor.xml.SourceXML;

public class DurHoursScaleComp extends Composite {
    private Composite parent;

    private Canvas scaleValueCanvas;

    private Canvas scaleRangeCanvas;

    private Scale timeDurScale;

    private Font canvasFont;

    private double timeDurHours = 0.0;

    private final int CANVAS_HEIGHT = 20;

    private int CANVAS_WIDTH = 100;

    private int xCoordOffset = 5;

    private double lowerVal = 0.50;

    private double upperVal = 6.00;

    private double rangeVal = upperVal - lowerVal;

    private double[] displayNumbers;

    private DurationInterface owner = null;

    public DurHoursScaleComp(Composite parent) {
        super(parent, 0);

        this.parent = parent;
        init();
    }

    private void init() {
        FFMPSourceConfigurationManager fscm = FFMPSourceConfigurationManager
                .getInstance();
        ArrayList<String> guidSources = fscm.getGuidances();

        FFTIDataManager fdm = FFTIDataManager.getInstance();

        lowerVal = Double.MAX_VALUE;
        upperVal = Double.MIN_VALUE;
        for (String sourceName : guidSources) {
            SourceXML source = fscm.getSource(sourceName);
            if (source.getDurationHour() < lowerVal) {
                lowerVal = source.getDurationHour();
            }

            if (source.getDurationHour() > upperVal) {
                upperVal = source.getDurationHour();
            }
        }

        if ((lowerVal == Double.MAX_VALUE) || (lowerVal <= 0.0)) {
            lowerVal = 0.0;
        }

        // get the smallest QPF value
        ArrayList<String> qpfSources = fscm.getQPFSources();
        double lowestQpfVal = Double.MAX_VALUE;
        if (qpfSources != null) {
            for (String sourceName : qpfSources) {
                SourceXML source = fscm.getSource(sourceName);
                String durHour = String.valueOf(source.getDurationHour());
                if (lowestQpfVal > Double.parseDouble(durHour))
                    lowestQpfVal = Double.parseDouble(durHour);
            }
        } else {
            lowestQpfVal = 1.0;
        }

        // if qpf is active, the use the lowest qpf value
        if (fdm.getSettingList().size() > 0) {
            // FFTISettingXML fsx = fdm.getSettingList().get(0);
            // if (fsx.getQPFSource().getDurationHour() > 0.0) { // QPF is
            // active
            if (lowestQpfVal < lowerVal) {
                lowerVal = lowestQpfVal;
            }
        }

        if (upperVal == Double.MIN_VALUE) {
            upperVal = 6.00;
        }

        timeDurHours = lowerVal * 4;
        displayNumbers = new double[] { lowerVal, upperVal };
        rangeVal = upperVal - lowerVal;

        canvasFont = new Font(parent.getDisplay(), "Monospace", 8, SWT.NORMAL);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
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
        timeDurScale.setMinimum((int) (lowerVal * 4));
        timeDurScale.setMaximum((int) (upperVal * 4));
        timeDurScale.setIncrement(1);
        timeDurScale.setPageIncrement(1);
        timeDurScale.setSelection((int) (lowerVal * 4));

        timeDurScale.setLayoutData(gd);
        timeDurScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateTimeDuration();
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

    private void drawScaleValueCanvas(GC gc) {
        double pixPerInc = (CANVAS_WIDTH - 25 - xCoordOffset) / rangeVal;

        gc.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        int newXCoord = (int) Math.round((this.timeDurHours - lowerVal)
                * pixPerInc);

        gc.drawString(String.format("%2.2f", this.timeDurHours), newXCoord, 1,
                true);
    }

    private void drawScaleRangeCanvas(GC gc) {
        double pixPerInc = (CANVAS_WIDTH - 25 - xCoordOffset) / rangeVal;

        gc.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        for (double dVal : displayNumbers) {
            int newXCoord = (int) Math.round((dVal - lowerVal) * pixPerInc);

            gc.drawString(String.format("%2.2f", dVal), newXCoord, 1, true);
        }
    }

    private void calcTimeDurHours() {
        timeDurHours = timeDurScale.getSelection() * .25;
    }

    private void setTimeDurationScale(double val) {
        timeDurScale.setSelection((int) ((val / .25)));
    }

    public void setOwner(DurationInterface owner) {
        this.owner = owner;
    }

    private void updateTimeDuration() {
        calcTimeDurHours();
        scaleValueCanvas.redraw();

        if (owner != null) {
            owner.updateQPEDurHour(timeDurHours);
            owner.updateTotalDurHour(timeDurHours);
        }
    }

    public void setTimeDuration(double val) {
        setTimeDurationScale(val);
        calcTimeDurHours();
        scaleValueCanvas.redraw();
    }

    public void setTimeDurationAndUpdate(double val) {
        setTimeDurationScale(val);
        calcTimeDurHours();
        scaleValueCanvas.redraw();
    }

    public double getSelectedValue() {
        return timeDurHours;
    }

    /**
     * enable the duration hour scale base on the state of enabled variable.
     * 
     * @param enabled
     */
    public void enableScale(boolean enabled) {
        this.timeDurScale.setEnabled(enabled);
        this.setEnabled(enabled);
    }

    public double getLowerVal() {
        return lowerVal;
    }

    public double getUpperVal() {
        return upperVal;
    }
}
