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
package com.raytheon.viz.hydro.riversummary;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.RiverDataPoint;
import com.raytheon.viz.hydrocommon.datamanager.RiverDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the River Summary dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation 
 * 08 NOV 2008  1628       dhladky     made it work.
 * 15 Jan 2008  1802       askripsk    Changed to draw gages with missing data.
 * 08 Mar 2010  2486       mpduff      Changed to open with the river for the 
 *                                     selected site automatically selected.
 * 15 Mar 2013  1790       rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class RiverSummaryDlg extends CaveSWTDialog {
    /**
     * Maximum stage difference.
     */
    private static final int MAX_STAGE_DIFF = 100;

    /**
     * Font used for SWT controls.
     */
    private Font font;

    /**
     * Font used on the canvas display.
     */
    private Font canvasFont;

    /**
     * Stream list control.
     */
    private List streamList;

    /**
     * List of stream names that coincide with the streamList widget.
     */
    private java.util.List<String> streamNameList = new ArrayList<String>();

    /**
     * Stage basis combo box.
     */
    private Combo stageBasisCbo;

    /**
     * Canvas displaying the labels for the river summaries.
     */
    private Canvas labelCanvas;

    /**
     * Canvas displaying the river summaries.
     */
    private Canvas riverSumCanvas;

    /**
     * Canvas height.
     */
    private final int CANVAS_HEIGHT = 620;

    /**
     * Y coordinate of the dashed flood line.
     */
    private final int FLOOD_LINE_YCOORD = (CANVAS_HEIGHT / 2) - 100;

    /**
     * Width of the label canvas.
     */
    private final int LABEL_CANVAS_WIDTH = 120;

    /**
     * Width of the river summary canvas.
     */
    private final int RIVER_SUM_CANVAS_WIDTH = 2000;

    /**
     * Width of the scrolled composite.
     */
    private final int SCROLLED_COMP_WIDTH = 800;

    /**
     * Height of the scrolled composite.
     */
    private final int SCROLLED_COMP_HEIGHT = CANVAS_HEIGHT;

    /**
     * First time flag indicating if the canvases have been drawn on.
     */
    private boolean firstTime = true;

    /**
     * Decimal Formatter
     */
    private DecimalFormat df = new DecimalFormat();

    /**
     * Height of the canvas font.
     */
    private int canvasFontHeight;

    /**
     * Y coordinate of the flood stage.
     */
    private int floodStgYCoord;

    /**
     * Y coordinate of the name.
     */
    private int nameYCoord;

    /**
     * Y coordinate of the ID.
     */
    private int idYCoord;

    /**
     * Y coordinate of the date.
     */
    private int dateYCoord;

    /**
     * Y coordinate of the stage.
     */
    private int stageYCoord;

    /**
     * All rivers Data structure
     */
    private Map<String, LinkedHashMap<String, RiverDataPoint>> riversData = null;

    /**
     * River Summary Data structure
     */
    private Map<String, RiverDataPoint> riverData = null;

    /**
     * River datamanager instance
     */
    private RiverDataManager rsdm = null;

    /**
     * Location and size of the dialog.
     */
    Rectangle bounds;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public RiverSummaryDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("River Summary");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        font.dispose();
        canvasFont.dispose();
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
        setReturnValue(false);

        font = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        canvasFont = new Font(shell.getDisplay(), "Monospace", 8, SWT.NORMAL);

        // Initialize all of the controls and layouts
        createStreamListLabel();
        createStreamListAndOptions();
        fillStreamList();
        createCanvasLabel();
        createCanvasComposite();
        createCloseButton();
        setSelection();

    }

    /**
     * Create label for the the stream list control.
     */
    private void createStreamListLabel() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        labelComp.setLayout(gl);

        Label streamListLbl = new Label(labelComp, SWT.NONE);
        streamListLbl.setText("Stream List");
    }

    /**
     * Create the stream list control and the options group and control.
     */
    private void createStreamListAndOptions() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite listOptionsComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 10;
        listOptionsComp.setLayout(gl);
        listOptionsComp.setLayoutData(gd);

        gd = new GridData(475, 100);
        streamList = new List(listOptionsComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        streamList.setLayoutData(gd);
        streamList.setFont(font);
        streamList.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                int index = ((List) e.getSource()).getSelectionIndex();
                int i = 0;
                String riverKey = null;
                for (String key : riversData.keySet()) {
                    if (i == index) {
                        riverKey = key;
                        break;
                    }
                    i++;
                }
                setRiverData(rsdm.populateRiverData(riverKey,
                        riversData.get(riverKey)));
                // issue a paint event
                riverSumCanvas.redraw();
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = ((List) e.getSource()).getSelectionIndex();
                int i = 0;
                String riverKey = null;
                for (String key : riversData.keySet()) {
                    if (i == index) {
                        riverKey = key;
                        break;
                    }
                    i++;
                }
                setRiverData(rsdm.populateRiverData(riverKey,
                        riversData.get(riverKey)));
                // issue a paint event
                riverSumCanvas.redraw();
            }
        });

        // -------------------------------------------
        // Create the Options group
        // -------------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group productInfoGroup = new Group(listOptionsComp, SWT.NONE);
        gl = new GridLayout(2, false);
        productInfoGroup.setLayout(gl);
        productInfoGroup.setLayoutData(gd);
        productInfoGroup.setText(" Options ");

        Label stageLbl = new Label(productInfoGroup, SWT.NONE);
        stageLbl.setText("Stage Basis:");

        gd = new GridData(150, SWT.DEFAULT);
        stageBasisCbo = new Combo(productInfoGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        stageBasisCbo.add("Max Obs/Fcst");
        stageBasisCbo.add("Observed");
        stageBasisCbo.add("Forecast");
        stageBasisCbo.select(0);
        stageBasisCbo.setLayoutData(gd);

        stageBasisCbo.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                // issue a paint event
                riverSumCanvas.redraw();
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                // issue a paint event
                riverSumCanvas.redraw();
            }
        });
    }

    /**
     * Create label above the label & river summary canvases.
     */
    private void createCanvasLabel() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        labelComp.setLayout(gl);

        Label canvasLbl = new Label(labelComp, SWT.NONE);
        canvasLbl.setText("Stations ordered by river mile");
    }

    /**
     * Create the composite for the label & river summary canvases.
     */
    private void createCanvasComposite() {
        Composite canvasComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 0;
        canvasComp.setLayout(gl);

        addLabelCanvas(canvasComp);
        addProfileCanvas(canvasComp);
    }

    /**
     * Add the label canvas to the canvas composite.
     * 
     * @param canvasComp
     *            Canvas composite.
     */
    private void addLabelCanvas(Composite canvasComp) {
        labelCanvas = new Canvas(canvasComp, SWT.DOUBLE_BUFFERED);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = LABEL_CANVAS_WIDTH;

        labelCanvas.setSize(LABEL_CANVAS_WIDTH, CANVAS_HEIGHT);

        labelCanvas.setLayoutData(gd);

        labelCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawLabelCanvas(e);
            }
        });

    }

    /**
     * Add the river summary profile canvas to the canvas composite.
     * 
     * @param canvasComp
     *            Canvas composite.
     */
    private void addProfileCanvas(Composite canvasComp) {
        ScrolledComposite scrolledComp = new ScrolledComposite(canvasComp,
                SWT.H_SCROLL | SWT.V_SCROLL);
        GridLayout gl = new GridLayout(1, false);
        scrolledComp.setLayout(gl);
        GridData gd = new GridData(SCROLLED_COMP_WIDTH, SCROLLED_COMP_HEIGHT);
        scrolledComp.setLayoutData(gd);

        riverSumCanvas = new Canvas(scrolledComp, SWT.DOUBLE_BUFFERED);
        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = RIVER_SUM_CANVAS_WIDTH;

        riverSumCanvas.setSize(RIVER_SUM_CANVAS_WIDTH, CANVAS_HEIGHT);

        riverSumCanvas.setLayoutData(gd);
        riverSumCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawRiverSummaryCanvas(e);
            }
        });

        scrolledComp.setContent(riverSumCanvas);
    }

    /**
     * Draw the labels on the label canvas.
     * 
     * @param e
     *            Paint event.
     */
    private void drawLabelCanvas(PaintEvent e) {
        e.gc.setFont(canvasFont);

        if (firstTime == true) {
            calculateCoordinates(e.gc);
        }

        e.gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));

        e.gc.fillRectangle(0, 0, LABEL_CANVAS_WIDTH, CANVAS_HEIGHT);

        // -------------------------------------
        // Draw dashed flood line
        // -------------------------------------
        e.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        e.gc.setLineStyle(SWT.LINE_DOT);
        e.gc.drawLine(0, FLOOD_LINE_YCOORD, RIVER_SUM_CANVAS_WIDTH,
                FLOOD_LINE_YCOORD);

        e.gc.drawString("Flood Stage", 2, floodStgYCoord, true);

        // -------------------------------------------
        // Draw bottom labels
        // -------------------------------------------
        e.gc.drawString("Name", 2, nameYCoord, true);
        e.gc.drawString("Id (river mile)", 2, idYCoord, true);
        e.gc.drawString("Date/Time", 2, dateYCoord, true);
        e.gc.drawString("Stage (if no graph)", 2, stageYCoord, true);
    }

    /**
     * Draw the river summary profile canvas.
     * 
     * @param e
     *            Paint event.
     */
    private void drawRiverSummaryCanvas(PaintEvent e) {
        e.gc.setFont(canvasFont);

        // ticInterval is used to determine max and min stages to be
        // displayed by a particular station.
        int ticInterval = 5;

        if (firstTime == true) {
            calculateCoordinates(e.gc);
        }

        e.gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));

        e.gc.fillRectangle(0, 0, RIVER_SUM_CANVAS_WIDTH, CANVAS_HEIGHT);

        // -------------------------------------
        // Draw dashed flood line
        // -------------------------------------
        e.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        e.gc.setLineStyle(SWT.LINE_DOT);
        e.gc.drawLine(0, FLOOD_LINE_YCOORD, RIVER_SUM_CANVAS_WIDTH,
                FLOOD_LINE_YCOORD);

        if (getRiverData() != null) {
            // must deal with x coordinate
            // hardcoding an x offset of 175 to bring stations
            // closer together, but this leaves empty space to the right
            int xoffset = 135;
            int x = 30; // starting point
            df.setMinimumIntegerDigits(1);
            df.setMaximumFractionDigits(2);

            for (String key : getRiverData().keySet()) {
                if (getRiverData().containsKey(key)) {
                    RiverDataPoint rdp = getRiverData().get(key);
                    double value = HydroConstants.MISSING_VALUE;
                    Calendar cal = null;
                    String calString = "";
                    String valString = "";
                    String mileString = "";
                    String lidString = rdp.getLid();

                    // Get the max/min values
                    MaxMin maxMin = new MaxMin();
                    maxMin.checkValue(rdp.getFloodStage());

                    if (rdp.getActionStage() > 0.0) {
                        maxMin.checkValue(rdp.getActionStage());
                    }

                    maxMin.checkValue(rdp.getObsValue());
                    maxMin.checkValue(rdp.getFcstValue());

                    // adjust minStage down to nearest number divisible by
                    // ticInterval
                    double minStage = maxMin.getMinValue();

                    if (rdp.getFloodStage() > 100) {
                        minStage -= ticInterval;
                        long longStage = (long) minStage / ticInterval;
                        minStage = longStage * ticInterval;
                    } else {
                        minStage = 0.0;
                    }

                    // adjust maxStage up to nearest number div by ticInterval
                    double maxStage = maxMin.getMaxValue();
                    maxStage += 2 * ticInterval;
                    long longStage = (long) maxStage / ticInterval;
                    maxStage = longStage * ticInterval;

                    // -------------------------------------------
                    // Draw bottom labels
                    // -------------------------------------------
                    e.gc.drawString(rdp.getLocName(), x, nameYCoord, true);
                    if (Double.compare(rdp.getMile(),
                            HydroConstants.MISSING_VALUE) != 0) {
                        mileString = df.format(rdp.getMile());
                    } else {
                        mileString = "MSG River Mile";
                    }
                    e.gc.drawString(lidString + " (" + mileString + ")", x,
                            idYCoord, true);

                    if (stageBasisCbo.getSelectionIndex() == 0) {
                        // max of fcst and obs
                        if (rdp.getObsValue() > rdp.getFcstValue()) {
                            value = rdp.getObsValue();
                            cal = rdp.getObsTime();
                        } else {
                            value = rdp.getFcstValue();
                            cal = rdp.getFcstTime();
                        }
                    } else if (stageBasisCbo.getSelectionIndex() == 1) {
                        // obs max
                        value = rdp.getObsValue();
                        cal = rdp.getObsTime();
                    } else {
                        // fcst max
                        value = rdp.getFcstValue();
                        cal = rdp.getFcstTime();
                    }

                    // draw the date
                    if (value != HydroConstants.MISSING_VALUE) {
                        calString = HydroConstants.DATE_FORMAT.format(cal
                                .getTime());
                    } else {
                        calString = "MSG Stage Data";
                    }
                    e.gc.drawString(calString, x, dateYCoord, true);

                    double gageDiff = maxStage - minStage;

                    //
                    // check that station can be drawn
                    //
                    if ((gageDiff < MAX_STAGE_DIFF)
                            && (rdp.getFloodStage() > 0.0)) {
                        // NOW!!!! do the call to draw the gage here.
                        RiverGage.drawRiverGage(e.gc, rdp, x,
                                FLOOD_LINE_YCOORD, value, minStage, maxStage,
                                maxMin);
                    } else {
                        // draw the gage value if no graph is drawn
                        if (Double.compare(value, HydroConstants.MISSING_VALUE) != 0) {
                            valString = df.format(value);
                        } else {
                            valString = "Missing";
                        }
                        e.gc.drawString(valString, x, stageYCoord, true);

                        String errorText = "";
                        if (rdp.getFloodStage() == 0.0) {
                            errorText = "MSG Flood Stage";
                        } else if (gageDiff >= MAX_STAGE_DIFF) {
                            errorText = "Stage diffs too big.";
                        }
                        e.gc.drawString(errorText, x, stageYCoord - 15, true);
                    }

                    x += xoffset;
                }
            }
        }
    }

    /**
     * Calculate font height and label coordinates.
     * 
     * @param gc
     *            Graphic component.
     */
    private void calculateCoordinates(GC gc) {
        canvasFontHeight = (gc.getFontMetrics().getHeight());

        floodStgYCoord = FLOOD_LINE_YCOORD - canvasFontHeight - 2;

        nameYCoord = CANVAS_HEIGHT - canvasFontHeight - 2;
        idYCoord = nameYCoord - canvasFontHeight - 2;
        dateYCoord = idYCoord - canvasFontHeight - 2;
        stageYCoord = dateYCoord - canvasFontHeight - 2;

        firstTime = false;
    }

    /**
     * Create the Close button.
     */
    private void createCloseButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        centeredComp.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                bounds = shell.getBounds();
                close();
            }
        });
    }

    /**
     * Populate teh stream list.
     */
    private void fillStreamList() {
        rsdm = RiverDataManager.getInstance();
        riversData = rsdm.getRiverSummaryData();
        String tmpStr;
        String tmpStr2;
        streamNameList.clear();

        for (String id : riversData.keySet()) {
            tmpStr2 = String.format("(%d stations)", riversData.get(id)
                    .keySet().size());
            String name = null;
            // extract the river name for display
            for (String key : riversData.get(id).keySet()) {
                name = riversData.get(id).get(key).getRiverName();
                break;
            }
            tmpStr = String.format("%-40s %-13s", name, tmpStr2);
            streamList.add(tmpStr);
            streamNameList.add(name);
        }
    }

    /**
     * Set up data for the desired selection and redraw the canvas.
     */
    private void setSelection() {
        String lid = HydroDisplayManager.getInstance().getCurrentLid();
        RiverDataPoint riverPoint = RiverDataManager.getInstance()
                .getRiverDataPoint(lid);

        for (int i = 0; i < streamNameList.size(); i++) {
            if (streamNameList.get(i).equalsIgnoreCase(
                    riverPoint.getStreamName())) {
                streamList.select(i);
                streamList.showSelection();
                break;
            }
        }

        int index = streamList.getSelectionIndex();
        int i = 0;
        String riverKey = null;
        for (String key : riversData.keySet()) {
            if (i == index) {
                riverKey = key;
                break;
            }
            i++;
        }
        setRiverData(rsdm.populateRiverData(riverKey, riversData.get(riverKey)));

        // issue a paint event
        riverSumCanvas.redraw();
    }

    /**
     * Sets the data structure for this river.
     * 
     * @param riverData
     */
    private void setRiverData(Map<String, RiverDataPoint> riverData) {
        this.riverData = riverData;
    }

    /**
     * gets the data structure for this river.
     * 
     * @return
     */
    private Map<String, RiverDataPoint> getRiverData() {
        return riverData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();
        shell.addShellListener(new ShellAdapter() {

            @Override
            public void shellClosed(ShellEvent e) {
                bounds = shell.getBounds();
            }
        });
        if (bounds != null) {
            shell.setBounds(bounds);
        }
    }

}
