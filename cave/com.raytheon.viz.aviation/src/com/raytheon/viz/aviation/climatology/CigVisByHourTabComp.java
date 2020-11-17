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
 * This class displays the composite/controls for the "By Hour" tab.
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
public class CigVisByHourTabComp extends Composite implements ICigVisTabComp {
    /**
     * Month spinner control.
     */
    private Spinner monthSpnr;

    /**
     * Number of months spinner control.
     */
    private Spinner numMonthsSpnr;

    /**
     * Auto Redraw check box.
     */
    private Button autoRedrawChk;

    /**
     * Show legend check box.
     */
    private Button showLegendChk;

    /**
     * Ceiling & Visibility canvas composite to draw the graphs.
     */
    private CigVisByHourCanvasComp cigVisCanvasComp;

    /**
     * Hour data to be graphed.
     */
    private CigVisDistDataManager data;

    private CigVisDistributionDlg parentDialog;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     */
    public CigVisByHourTabComp(Composite parent, CigVisDistDataManager data,
            CigVisDistributionDlg dialog) {
        super(parent, SWT.NONE);
        this.data = data;
        this.parentDialog = dialog;
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

        createCigVisByHourCanvas();
    }

    /**
     * Create the month and auto redraw controls.
     */
    private void createControls() {
        Composite controlComp = new Composite(this, SWT.NONE);
        controlComp.setLayout(new GridLayout(8, false));
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        controlComp.setLayoutData(gd);

        Label monthLbl = new Label(controlComp, SWT.NONE);
        monthLbl.setText("Month:");

        gd = new GridData(30, SWT.DEFAULT);
        monthSpnr = new Spinner(controlComp, SWT.BORDER);
        monthSpnr.setDigits(0);
        monthSpnr.setIncrement(1);
        monthSpnr.setPageIncrement(3);
        monthSpnr.setMinimum(1);
        monthSpnr.setMaximum(12);
        monthSpnr.setSelection(1);
        monthSpnr.setLayoutData(gd);
        monthSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (autoRedrawChk.getSelection()) {
                    redrawCanvas();
                }
            }
        });

        Label numMonthLbl = new Label(controlComp, SWT.NONE);
        numMonthLbl.setText("Num Months:");

        gd = new GridData(30, SWT.DEFAULT);
        numMonthsSpnr = new Spinner(controlComp, SWT.BORDER);
        numMonthsSpnr.setDigits(0);
        numMonthsSpnr.setIncrement(1);
        numMonthsSpnr.setPageIncrement(3);
        numMonthsSpnr.setMinimum(1);
        numMonthsSpnr.setMaximum(12);
        numMonthsSpnr.setSelection(1);
        numMonthsSpnr.setLayoutData(gd);
        numMonthsSpnr.addSelectionListener(new SelectionAdapter() {
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
    private void createCigVisByHourCanvas() {
        Composite canvasComp = new Composite(this, SWT.NONE);
        canvasComp.setLayout(new GridLayout(1, false));
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        canvasComp.setLayoutData(gd);

        cigVisCanvasComp = new CigVisByHourCanvasComp(canvasComp, data, this);
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
     * Redraw the By Hour canvas.
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

    public int getStartMonth() {
        return monthSpnr.getSelection();
    }

    public int getEndMonth() {
        int endMonth = monthSpnr.getSelection() + numMonthsSpnr.getSelection();
        if (endMonth > 12) {
            endMonth -= 12;
        }

        return endMonth;
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