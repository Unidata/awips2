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
package com.raytheon.viz.ghg.monitor;

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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;

import com.raytheon.viz.ghg.monitor.data.GhgAlertData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;

/**
 * This class is a Composite containing the Enable Alert controls.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class GhgEnableAlertComp extends Composite {
    /**
     * Parent composite.
     */
    private Composite parent;

    /**
     * The alert data object to be updated.
     */
    private GhgAlertData alertData = null;

    /**
     * The alert type handled by this composite.
     */
    private GhgConfigData.AlertsEnum type = null;

    /**
     * Enable title.
     */
    private String enableTitle;

    /**
     * Number of minute to expire.
     */
    private int expireMinutes;

    /**
     * Canvas font.
     */
    private Font canvasFont;

    /**
     * Enable check box.
     */
    private Button enableChk;

    /**
     * Show banner chack box.
     */
    private Button showBannerChk;

    /**
     * Canvas that displays the current minutes scale value selected.
     */
    private Canvas scaleValueCanvas;

    /**
     * Canvas that displays the range of values for the minutes scale.
     */
    private Canvas scaleLabelCanvas;

    /**
     * Minutes scale.
     */
    private Scale minutesScale;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 20;

    /**
     * Canvas width.
     */
    private final int CANVAS_WIDTH = 240;

    /**
     * X coordinate offset.
     */
    private int xCoordOffset = 10;
    
    /**
     * Widget enabled flag.
     */
    private boolean enabled = true;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public GhgEnableAlertComp(Composite parent,
            GhgConfigData.AlertsEnum enableTitle, int expireMin, boolean enabled) {
        super(parent, SWT.BORDER);
        type = enableTitle;
        this.parent = parent;
        this.enableTitle = enableTitle.display;
        expireMinutes = expireMin;
        this.enabled = enabled;

        init();
    }

    /**
     * Initialize the composite.
     */
    private void init() {
        canvasFont = new Font(parent.getDisplay(), "Monospace", 10, SWT.NORMAL);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(2, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        setLayout(gl);
        setLayoutData(gd);

        initializeComponents();

        this.pack();

        addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent arg0) {
                canvasFont.dispose();
            }
        });
    }

    /**
     * Initialize the controls on the composite.
     */
    private void initializeComponents() {
        createCheckBoxes();

        createScaleControl();
    }

    /**
     * Create the alert check boxes.
     */
    private void createCheckBoxes() {
        Composite checkBoxComp = new Composite(this, SWT.NONE);
        checkBoxComp.setLayout(new GridLayout(1, false));

        GridData gd = new GridData(150, SWT.DEFAULT);
        enableChk = new Button(checkBoxComp, SWT.CHECK);
        enableChk.setText("Enable " + enableTitle);
        enableChk.setSelection(true);
        enableChk.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        showBannerChk = new Button(checkBoxComp, SWT.CHECK);
        showBannerChk.setText("Show Banner");
        showBannerChk.setSelection(true);
        showBannerChk.setLayoutData(gd);
    }

    /**
     * Create the scale canvases and control.
     */
    private void createScaleControl() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite alertComp = new Composite(this, SWT.NONE);
        alertComp.setLayout(new GridLayout(1, false));
        alertComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label alertLbl = new Label(alertComp, SWT.CENTER);
        alertLbl.setText("Alert minutes prior to Expire:");
        alertLbl.setLayoutData(gd);

        scaleValueCanvas = new Canvas(alertComp, SWT.DOUBLE_BUFFERED);
        scaleValueCanvas
                .setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));
        scaleValueCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                e.gc.setFont(canvasFont);
                drawScaleValueCanvas(e.gc);
            }
        });

        gd = new GridData(CANVAS_WIDTH, SWT.DEFAULT);
        minutesScale = new Scale(alertComp, SWT.HORIZONTAL);
        minutesScale.setMinimum(0);
        minutesScale.setMaximum(60);
        minutesScale.setIncrement(5);
        minutesScale.setPageIncrement(5);
        minutesScale.setSelection(expireMinutes);
        minutesScale.setLayoutData(gd);
        minutesScale.setEnabled(enabled);
        minutesScale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                expireMinutes = minutesScale.getSelection();
                scaleValueCanvas.redraw();
                alertData.setTime(expireMinutes);
            }
        });

        scaleLabelCanvas = new Canvas(alertComp, SWT.DOUBLE_BUFFERED);
        scaleLabelCanvas
                .setLayoutData(new GridData(CANVAS_WIDTH, CANVAS_HEIGHT));
        scaleLabelCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                e.gc.setFont(canvasFont);
                drawScaleLabelCanvas(e.gc);
            }
        });
    }

    /**
     * Draw the scale value on the scale value canvas. The value adjusts with
     * the value of the scale and moves with the thumb slider.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawScaleValueCanvas(GC gc) {
        int fontAveWidth = gc.getFontMetrics().getAverageCharWidth();

        double pixPerInc = (220 - xCoordOffset) / 60.0;

        gc.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        int newXCoord = (int) Math.round(expireMinutes * pixPerInc
                + xCoordOffset);

        if (expireMinutes >= 10) {
            newXCoord -= fontAveWidth / 2;
        }

        gc.drawString(String.valueOf(expireMinutes), newXCoord, 1, true);
    }

    /**
     * Draw the scale range labels on the scale label canvas.
     * 
     * @param gc
     *            Graphics context.
     */
    private void drawScaleLabelCanvas(GC gc) {
        int fontAveWidth = gc.getFontMetrics().getAverageCharWidth();

        gc.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));

        gc.fillRectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT);

        gc.setForeground(parent.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        gc.drawString("0", xCoordOffset, 1, true);

        gc.drawString("15", ((int) Math.round(CANVAS_WIDTH * .25))
                - fontAveWidth / 2 + 3, 1, true);

        gc.drawString("30", CANVAS_WIDTH / 2 - fontAveWidth, 1, true);

        gc.drawString("45", ((int) Math.round(CANVAS_WIDTH * .75))
                - fontAveWidth / 2 - 10, 1, true);

        gc.drawString("60", CANVAS_WIDTH - xCoordOffset - fontAveWidth * 2, 1,
                true);
    }

    public void setAlertData(GhgAlertData data) {
        alertData = data;
        type = data.getType();
        enableChk.setSelection(data.isEnabled());
        enableChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                alertData.setEnabled(enableChk.getSelection());
            }
        });
        showBannerChk.setSelection(data.isBanner());
        showBannerChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                alertData.setBanner(showBannerChk.getSelection());
            }
        });
        minutesScale.setSelection(data.getTime());
        expireMinutes = minutesScale.getSelection();
        scaleLabelCanvas.redraw();
    }

    public GhgConfigData.AlertsEnum getType() {
        return type;
    }
}