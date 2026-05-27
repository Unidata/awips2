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

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.RiverDataPoint;
import com.raytheon.viz.hydrocommon.datamanager.RiverDataManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

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
 * 08 Apr 2016  5483       dgilling    Re-factor to fix hi-dpi issues.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class RiverSummaryDlg extends CaveJFACEDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String[] STAGE_BASIS_OPTIONS = { "Max Obs/Fcst",
            "Observed", "Forecast" };

    private static final int CANVAS_HEIGHT = 620;

    private static final String STAGE_LABEL_TEXT = "Stage (if no graph)";

    /**
     * Y coordinate of the dashed flood line.
     */
    private static final int FLOOD_LINE_YCOORD = (CANVAS_HEIGHT / 2) - 100;

    private static final int INITIAL_X_POS = 30;

    /**
     * Maximum stage difference.
     */
    private static final int MAX_STAGE_DIFF = 100;

    /**
     * Font used on the canvas display.
     */
    private Font canvasFont;

    /**
     * Stream list control.
     */
    private List streamList;

    /**
     * Stage basis combo box.
     */
    private Combo stageBasisCbo;

    /**
     * Canvas displaying the river summaries.
     */
    private Canvas riverSumCanvas;

    /**
     * Width of the label canvas.
     */
    private int labelCanvasWidth;

    /**
     * Width of the river summary canvas.
     */
    private int riverSumCanvasWidth;

    private int itemWidth;

    /**
     * First time flag indicating if the canvases have been drawn on.
     */
    private boolean firstTime;

    /**
     * Decimal Formatter
     */
    private NumberFormat df;

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
    private Map<String, LinkedHashMap<String, RiverDataPoint>> riversData;

    /**
     * River Summary Data structure
     */
    private Map<String, RiverDataPoint> riverData;

    /**
     * River datamanager instance
     */
    private RiverDataManager rsdm;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public RiverSummaryDlg(Shell parent) {
        super(parent);
        setBlockOnOpen(false);
        setShellStyle(SWT.DIALOG_TRIM | SWT.RESIZE);

        this.rsdm = RiverDataManager.getInstance();
        this.riverData = null;
        this.firstTime = true;

        this.df = NumberFormat.getNumberInstance();
        this.df.setMinimumIntegerDigits(1);
        this.df.setMaximumFractionDigits(2);
    }

    private void disposed(DisposeEvent e) {
        if (canvasFont != null) {
            canvasFont.dispose();
        }
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);
        composite.setLayout(new GridLayout(2, false));

        // Initialize all of the controls and layouts
        createStreamListAndOptions(composite);
        createCanvasComposite(composite);
        setSelection();

        return composite;
    }

    /**
     * Create the stream list control and the options group and control.
     * 
     * @param composite
     */
    private void createStreamListAndOptions(Composite parent) {
        Label listLabel = new Label(parent, SWT.NONE);
        listLabel.setText("Stream List");
        listLabel.setLayoutData(new GridData(SWT.LEFT, SWT.FILL, false, false,
                2, 1));

        streamList = new List(parent, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        streamList.setFont(JFaceResources.getTextFont());
        streamList.setItems(getStreamList());
        GC gc = new GC(streamList);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = streamList.getItemHeight() * 5;
        gd.widthHint = gc.getFontMetrics().getAverageCharWidth() * 55;
        gc.dispose();
        streamList.setLayoutData(gd);
        streamList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateRiverDataFromSelection(((List) e.getSource())
                        .getSelectionIndex());
                riverSumCanvas.redraw();
            }
        });

        Group optionsGroup = new Group(parent, SWT.NONE);
        optionsGroup.setText("Options");
        optionsGroup.setLayout(new GridLayout(2, false));
        optionsGroup.setLayoutData(new GridData(SWT.RIGHT, SWT.FILL, false,
                true));

        Label stageBasisLabel = new Label(optionsGroup, SWT.NONE);
        stageBasisLabel.setText("Stage Basis:");
        stageBasisLabel.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false,
                false));

        stageBasisCbo = new Combo(optionsGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        stageBasisCbo.setItems(STAGE_BASIS_OPTIONS);
        stageBasisCbo.select(0);
        stageBasisCbo.setLayoutData(new GridData(SWT.DEFAULT, SWT.DEFAULT,
                false, false));
        stageBasisCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                riverSumCanvas.redraw();
            }
        });
    }

    /**
     * Create the composite for the label & river summary canvases.
     * 
     * @param composite
     */
    private void createCanvasComposite(Composite parent) {
        Composite plotComposite = new Composite(parent, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 0;
        plotComposite.setLayout(gl);
        plotComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true, 2, 1));

        Label stationsLabel = new Label(plotComposite, SWT.NONE);
        stationsLabel.setText("Stations ordered by river mile");
        stationsLabel.setLayoutData(new GridData(SWT.LEFT, SWT.FILL, true,
                false, 2, 1));

        canvasFont = new Font(plotComposite.getDisplay(), "Monospace", 8,
                SWT.NORMAL);

        Canvas labelCanvas = new Canvas(plotComposite, SWT.DOUBLE_BUFFERED);
        labelCanvas.setFont(canvasFont);
        GridData gd = new GridData(SWT.CENTER, SWT.FILL, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        GC gc = new GC(labelCanvas);
        labelCanvasWidth = gc.textExtent(STAGE_LABEL_TEXT + "   ").x;
        gd.widthHint = labelCanvasWidth;
        gc.dispose();
        labelCanvas.setLayoutData(gd);
        labelCanvas.addPaintListener(new PaintListener() {

            @Override
            public void paintControl(PaintEvent e) {
                drawLabelCanvas(e);
            }
        });

        ScrolledComposite scrolledComp = new ScrolledComposite(plotComposite,
                SWT.H_SCROLL);
        scrolledComp.setLayout(new FillLayout());

        riverSumCanvas = new Canvas(scrolledComp, SWT.DOUBLE_BUFFERED);
        riverSumCanvas.setFont(canvasFont);
        gd = new GridData(SWT.LEFT, SWT.FILL, false, true);
        gc = new GC(riverSumCanvas);
        itemWidth = gc.textExtent(HydroConstants.DATE_FORMAT.toPattern()
                + "   ").x;
        gc.dispose();
        riverSumCanvasWidth = (itemWidth * 15) + INITIAL_X_POS;
        riverSumCanvas.setSize(riverSumCanvasWidth, CANVAS_HEIGHT);
        riverSumCanvas.setLayoutData(gd);
        riverSumCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawRiverSummaryCanvas(e);
            }
        });

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = (itemWidth * 6) + INITIAL_X_POS;
        scrolledComp.setLayoutData(gd);
        scrolledComp.setContent(riverSumCanvas);
    }

    /**
     * Draw the labels on the label canvas.
     * 
     * @param e
     *            Paint event.
     */
    private void drawLabelCanvas(PaintEvent e) {
        if (firstTime) {
            calculateCoordinates(e.gc);
        }

        e.gc.setBackground(e.display.getSystemColor(SWT.COLOR_BLACK));
        e.gc.fillRectangle(0, 0, labelCanvasWidth, CANVAS_HEIGHT);

        // -------------------------------------
        // Draw dashed flood line
        // -------------------------------------
        e.gc.setForeground(e.display.getSystemColor(SWT.COLOR_WHITE));
        e.gc.setLineStyle(SWT.LINE_DOT);
        e.gc.drawLine(0, FLOOD_LINE_YCOORD, labelCanvasWidth, FLOOD_LINE_YCOORD);

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
        /*
         * ticInterval is used to determine max and min stages to be displayed
         * by a particular station.
         */
        int ticInterval = 5;

        if (firstTime) {
            calculateCoordinates(e.gc);
        }

        e.gc.setBackground(e.display.getSystemColor(SWT.COLOR_BLACK));
        e.gc.fillRectangle(0, 0, riverSumCanvasWidth, CANVAS_HEIGHT);

        // -------------------------------------
        // Draw dashed flood line
        // -------------------------------------
        e.gc.setForeground(e.display.getSystemColor(SWT.COLOR_WHITE));
        e.gc.setLineStyle(SWT.LINE_DOT);
        e.gc.drawLine(0, FLOOD_LINE_YCOORD, riverSumCanvasWidth,
                FLOOD_LINE_YCOORD);

        if (getRiverData() != null) {
            int x = INITIAL_X_POS; // starting point

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

                    x += itemWidth;
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
        int fontHeight = gc.getFontMetrics().getHeight();

        floodStgYCoord = FLOOD_LINE_YCOORD - fontHeight - 2;
        nameYCoord = CANVAS_HEIGHT - fontHeight - 2;
        idYCoord = nameYCoord - fontHeight - 2;
        dateYCoord = idYCoord - fontHeight - 2;
        stageYCoord = dateYCoord - fontHeight - 2;

        firstTime = false;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.CLOSE_ID,
                IDialogConstants.CLOSE_LABEL, true);
    }

    @Override
    protected void buttonPressed(int buttonId) {
        switch (buttonId) {
        case IDialogConstants.CLOSE_ID:
            close();
            break;
        default:
            statusHandler.warn(String.format(
                    "Unrecognized button ID [%d] pressed.", buttonId));
            break;
        }
    }

    /**
     * Populate the stream list.
     */
    private String[] getStreamList() {
        riversData = rsdm.getRiverSummaryData();

        Collection<String> streamList = new ArrayList<>(riversData.size());
        for (String id : riversData.keySet()) {
            String first = riversData.get(id).keySet().iterator().next();
            String name = riversData.get(id).get(first).getRiverName();
            int numRecords = riversData.get(id).keySet().size();
            String tmp = String.format("(%d stations)", numRecords);
            streamList.add(String.format("%-40s %-13s", name, tmp));
        }

        return streamList.toArray(new String[0]);
    }

    /**
     * Set up data for the desired selection and redraw the canvas.
     */
    private void setSelection() {
        String lid = HydroDisplayManager.getInstance().getCurrentLid();
        RiverDataPoint riverPoint = RiverDataManager.getInstance()
                .getRiverDataPoint(lid);

        if ((riverPoint != null) && (riverPoint.getStreamName() != null)) {
            for (int i = 0; i < streamList.getItemCount(); i++) {
                if (streamList.getItem(i)
                        .startsWith(riverPoint.getStreamName())) {
                    streamList.select(i);
                    streamList.showSelection();
                    break;
                }
            }
        }

        updateRiverDataFromSelection(streamList.getSelectionIndex());

        // issue a paint event
        riverSumCanvas.redraw();
    }

    private void updateRiverDataFromSelection(int index) {
        if (index < 0) {
            return;
        }

        int i = 0;
        String riverKey = null;
        for (String key : riversData.keySet()) {
            if (i == index) {
                riverKey = key;
                break;
            }
            i++;
        }
        riverData = rsdm.populateRiverData(riverKey, riversData.get(riverKey));
    }

    /**
     * gets the data structure for this river.
     * 
     * @return
     */
    private Map<String, RiverDataPoint> getRiverData() {
        return riverData;
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("River Summary");
        newShell.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                disposed(e);
            }
        });
    }
}
