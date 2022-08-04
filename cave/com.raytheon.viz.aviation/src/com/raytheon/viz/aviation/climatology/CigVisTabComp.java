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
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.viz.aviation.climatology.CigVisDistDataManager.Element;
import com.raytheon.viz.aviation.climatology.CigVisDistDataManager.GraphType;

/**
 * This class displays the composite/controls for the "By Wind Dir" tab.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ---------------------------------
 * Feb 28, 2008  938      lvenable  Initial creation.
 * Jul 29, 2008  1337     grichard  Auto redraw checked by default.
 * May 14, 2020  8067     randerso  Major refactor and code cleanup.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class CigVisTabComp extends Composite {
    /**
     * Month spinner control.
     */
    private Spinner monthSpnr;

    /**
     * Number of months spinner control.
     */
    private Spinner numMonthsSpnr;

    /**
     * Hour spinner control.
     */
    private Spinner hourSpnr;

    /**
     * Number of hours spinner control.
     */
    private Spinner numHoursSpnr;

    /**
     * Ceiling & Visibility canvas composite to draw the graphs.
     */
    private CigVisCanvasComp canvas;

    /**
     * Type of graph to be drawn
     */
    private GraphType graphType;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent composite.
     * @param graphType
     *            Type of graph
     * @param data
     *            the initial graph data
     * @param element
     *            the element to be initially displayed
     */
    public CigVisTabComp(Composite parent, GraphType graphType,
            CigVisDistDataManager data, Element element) {
        super(parent, SWT.NONE);
        this.graphType = graphType;

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createControls();

        createCanvas(data, element);
    }

    /**
     * Create the hours, months, and auto redraw controls.
     */
    private void createControls() {
        Composite controlComp = new Composite(this, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        controlComp.setLayout(gridLayout);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        controlComp.setLayoutData(gd);

        if (graphType.isShowMonthControls()) {
            gridLayout.numColumns += 4;

            Label monthLbl = new Label(controlComp, SWT.NONE);
            monthLbl.setText("Month:");

            gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
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
                    canvas.setStartMonth(getStartMonth());
                }
            });

            Label numMonthLbl = new Label(controlComp, SWT.NONE);
            numMonthLbl.setText("Num Months:");

            gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
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
                    canvas.setNumMonths(getNumMonths());
                }
            });
        }

        if (graphType.isShowHourControls()) {
            gridLayout.numColumns += 4;

            Label hourLbl = new Label(controlComp, SWT.NONE);
            hourLbl.setText("Hour:");

            gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
            hourSpnr = new Spinner(controlComp, SWT.BORDER);
            hourSpnr.setValues(0, 0, CigVisDistDataManager.NUM_HOURS - 1, 0, 1,
                    3);
            hourSpnr.setLayoutData(gd);
            hourSpnr.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    canvas.setStartHour(getStartHour());
                }
            });

            Label numHoursLbl = new Label(controlComp, SWT.NONE);
            numHoursLbl.setText("Num Hours:");

            gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
            numHoursSpnr = new Spinner(controlComp, SWT.BORDER);
            numHoursSpnr.setValues(1, 1, CigVisDistDataManager.NUM_HOURS, 0, 1,
                    3);
            numHoursSpnr.setLayoutData(gd);
            numHoursSpnr.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    canvas.setNumHours(getNumHours());
                }
            });
        }

        Button autoRedrawChk = new Button(controlComp, SWT.CHECK);
        autoRedrawChk.setText("Auto Redraw");
        autoRedrawChk.setSelection(true);
        autoRedrawChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                canvas.setAutoRedraw(((Button) e.widget).getSelection());
            }
        });

        Button showLegendChk = new Button(controlComp, SWT.CHECK);
        showLegendChk.setText("Show Legend");
        showLegendChk.setSelection(true);
        showLegendChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                canvas.drawLegend(((Button) e.widget).getSelection());
            }
        });
    }

    /**
     * Create the Ceiling & Visibility drawing canvas.
     *
     * @param element
     * @param data
     */
    private void createCanvas(CigVisDistDataManager data, Element element) {
        canvas = new CigVisCanvasComp(this, graphType, data, element);
        canvas.setStartHour(getStartHour());
        canvas.setNumHours(getNumHours());
        canvas.setStartMonth(getStartMonth());
        canvas.setNumMonths(getNumMonths());
    }

    /**
     * Set the maxPercent on the graph scale.
     *
     * @param maxPercent
     *            Maximum Percent
     */
    public void setMaxPercent(int maxPercent) {
        canvas.setMaxPercent(maxPercent);
    }

    /**
     * Set the data.
     *
     * @param data
     *            Data to set.
     */
    public void setCigVisData(CigVisDistDataManager data) {
        canvas.setCigVisData(data);
    }

    public void setElement(Element element) {
        canvas.setElement(element);
    }

    public void forceUpdate() {
        canvas.forceUpdate();
    }

    public int getStartMonth() {
        if (monthSpnr == null) {
            return 0;
        }
        return monthSpnr.getSelection() - 1;
    }

    public int getNumMonths() {
        if (numMonthsSpnr == null) {
            /* 12 months + 1 for annual */
            return CigVisDistDataManager.NUM_MONTHS + 1;
        }
        return numMonthsSpnr.getSelection();
    }

    public int getStartHour() {
        if (hourSpnr == null) {
            return 0;
        }
        return hourSpnr.getSelection();
    }

    public int getNumHours() {
        if (numHoursSpnr == null) {
            return CigVisDistDataManager.NUM_HOURS;
        }
        return numHoursSpnr.getSelection();
    }

    public Image getCigVisDistImage() {
        return canvas.getCigVisDistImage();
    }

    /**
     * @return the graphType
     */
    public GraphType getGraphType() {
        return graphType;
    }
}
