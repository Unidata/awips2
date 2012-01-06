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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;

/**
 * This class displays the composite/controls for the "By Month" tab.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation.
 * 7/29/2008    1337       grichard    Auto redraw checked by default.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class CigVisByMonthTabComp extends Composite implements ICigVisTabComp {
    /**
     * Hour spinner control.
     */
    private Spinner hourSpnr;

    /**
     * Number of hours spinner control.
     */
    private Spinner numHoursSpnr;

    /**
     * Auto redraw check box.
     */
    private Button autoRedrawChk;

    /**
     * Show legend check box.
     */
    private Button showLegendChk;

    /**
     * Ceiling & Visibility canvas composite to draw the graphs.
     */
    private CigVisByMonthCanvasComp cigVisCanvasComp;

    /**
     * By Month data.
     */
    private CigVisDistDataManager data;

    private CigVisDistributionDlg parentDialog;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public CigVisByMonthTabComp(Composite parent, CigVisDistDataManager data,
            CigVisDistributionDlg dialog) {
        super(parent, SWT.NONE);
        this.parentDialog = dialog;
        this.data = data;

        initComponents();
    }

    /**
     * Initialize the components on the display.
     */
    private void initComponents() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createControls();

        createCigVisCanvas();
    }

    /**
     * Create the hour and auto redraw controls.
     */
    private void createControls() {
        Composite controlComp = new Composite(this, SWT.NONE);
        controlComp.setLayout(new GridLayout(8, false));
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        controlComp.setLayoutData(gd);

        Label hourLbl = new Label(controlComp, SWT.NONE);
        hourLbl.setText("Hour:");

        gd = new GridData(30, SWT.DEFAULT);
        hourSpnr = new Spinner(controlComp, SWT.BORDER);
        hourSpnr.setDigits(0);
        hourSpnr.setIncrement(1);
        hourSpnr.setPageIncrement(3);
        hourSpnr.setMinimum(0);
        hourSpnr.setMaximum(23);
        hourSpnr.setSelection(1);
        hourSpnr.setLayoutData(gd);
        hourSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (autoRedrawChk.getSelection()) {
                    redrawCanvas();
                }
            }
        });

        Label numHoursLbl = new Label(controlComp, SWT.NONE);
        numHoursLbl.setText("Num Hours:");

        gd = new GridData(30, SWT.DEFAULT);
        numHoursSpnr = new Spinner(controlComp, SWT.BORDER);
        numHoursSpnr.setDigits(0);
        numHoursSpnr.setIncrement(1);
        numHoursSpnr.setPageIncrement(3);
        numHoursSpnr.setMinimum(1);
        numHoursSpnr.setMaximum(24);
        numHoursSpnr.setSelection(1);
        numHoursSpnr.setLayoutData(gd);
        numHoursSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (autoRedrawChk.getSelection()) {
                    redrawCanvas();
                }
            }
        });

        gd = new GridData(30, SWT.DEFAULT);
        Label filler = new Label(controlComp, SWT.NONE);
        filler.setLayoutData(gd);

        autoRedrawChk = new Button(controlComp, SWT.CHECK);
        autoRedrawChk.setText("Auto Redraw");
        autoRedrawChk.setSelection(true);

        gd = new GridData(30, SWT.DEFAULT);
        Label filler2 = new Label(controlComp, SWT.NONE);
        filler2.setLayoutData(gd);

        showLegendChk = new Button(controlComp, SWT.CHECK);
        showLegendChk.setText("Show Legend");
        showLegendChk.setSelection(true);
        showLegendChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                cigVisCanvasComp.drawLegend(showLegendChk.getSelection());
            }
        });
    }

    /**
     * Create the Ceiling & Visibility drawing canvas.
     */
    private void createCigVisCanvas() {
        Composite canvasComp = new Composite(this, SWT.NONE);
        canvasComp.setLayout(new GridLayout(1, false));
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        canvasComp.setLayoutData(gd);

        cigVisCanvasComp = new CigVisByMonthCanvasComp(canvasComp, data, this);
    }

    /**
     * Set the percent occurrence.
     * 
     * @param percentOccurrence
     *            Percent occurrence.
     */
    public void setMaxPercentOccurrence(int percentOccurrence) {
        cigVisCanvasComp.setMaxPercentOccurrence(percentOccurrence);
    }

    /**
     * Redraw the By Month canvas.
     */
    public void redrawCanvas() {
        cigVisCanvasComp.redrawCanvas();
    }

    /**
     * Set the data.
     * 
     * @param data
     *            Data to set.
     */
    public void setCigVisData(CigVisDistDataManager data) {
        this.data = data;
        cigVisCanvasComp.setCigVisData(data);
    }

    public int getStartHour() {
        return hourSpnr.getSelection();
    }

    public int getEndHour() {
        int endHour = hourSpnr.getSelection() + numHoursSpnr.getSelection();
        if (endHour > 24) {
            endHour -= 24;
        }

        return endHour;
    }

    public Image getCigVisDistImage() {
        return cigVisCanvasComp.getCigVisDistImage();
    }

    public void drawCanvas(GC gc) {
        cigVisCanvasComp.drawCanvas(gc);
    }

    @Override
    public CigVisDistributionDlg getDialog() {
        return parentDialog;
    }

    @Override
    public float getMaxPercentInData() {
        return cigVisCanvasComp.getMaxPercentInData();
    }
}
