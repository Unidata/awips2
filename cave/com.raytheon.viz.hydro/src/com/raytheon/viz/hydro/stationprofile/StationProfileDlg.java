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
package com.raytheon.viz.hydro.stationprofile;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.shef.tables.Statprof;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.data.HydroDataReport;
import com.raytheon.viz.hydrocommon.data.MaxObsFcst;
import com.raytheon.viz.hydrocommon.datamanager.RiverDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Station Profile dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation 
 * 15 Nov 2008  1628       dhladky     Made it work.
 * 15 Jun 2010  4304       mpduff      Added some null checks.
 * 30 Nov 2011  11253      lbousaidi   used List instead of TreeMap
 * 29 Mar 2013  1790       rferrel     Make dialog non-blocking.
 * 23 Oct 2013  15183      wkwock      Fix scales and value format
 * 18 Jan 2018  6822       mduff       Refactored the drawing calculations for better drawing.
 * Sep 21, 2018 7379       mduff       Support PDC Refactor.
 * 
 * </pre>
 * 
 * @author lvenable
 * 
 */
public class StationProfileDlg extends CaveSWTDialog {
    /**
     * Font used when drawing the canvases.
     */
    private Font font;

    /**
     * Station profile data.
     */
    private StationProfileData stationProfData;

    /**
     * Canvas height.
     */
    private static final int CANVAS_HEIGHT = 450;

    /**
     * Station profile canvas width.
     */
    private int profileCanvasWidth = 1600;

    /**
     * Label canvas width.
     */
    private static final int LABEL_CANVAS_WIDTH = 100;

    /**
     * Scrolled composite width.
     */
    private static final int SCROLLED_COMP_WIDTH = 800;

    /**
     * Scrolled composite height.
     */
    private static final int SCROLLED_COMP_HEIGHT = CANVAS_HEIGHT;

    /**
     * Hash length on the River Miles horizontal line.
     */
    private static final int RIVER_MILES_HASH = 8;

    /**
     * Hash length on the Elevation In Feet vertical line.
     */
    private static final int ELEVATION_HASH = 8;

    /**
     * Hash length of the black hash mark where the stations are drawn.
     */
    private static final int POINT_HASH = 10;

    /**
     * Height of the station bar.
     */
    private static final int BAR_HEIGHT = 40;

    /**
     * Width of the station bar.
     */
    private static final int BAR_WIDTH = 10;

    /**
     * Y coordinate of the horizontal graph line.
     */
    private static final int BOTTOM_Y_COORD = 400;

    /**
     * Number of pixel from the top of the canvas to the top of the vertical
     * graph line.
     */
    private static final int TOP_Y_COORD = 60;

    /**
     * "ELEVATION IN FEET" string.
     */
    private static final String ELEVATION_FEET = "ELEVATION IN FEET";

    /**
     * Offset between the elevation points.
     */
    private int elevationOffset = 0;

    /**
     * The pixels between the elevation hashes.
     */
    private int elevationPixelOffset = 0;

    /**
     * Number of pixels per Y increment.
     */
    private double pixelsPerIncY = 0.0;

    /**
     * Color of the station profile.
     */
    private Color profileColor;

    /**
     * Station combo box.
     */
    private Combo stationCbo;

    /**
     * Station name text control.
     */
    private Text nameTF;

    /**
     * Reach text control.
     */
    private Text reachTF;

    /**
     * Action text control.
     */
    private Text actionTF;

    /**
     * Flood text control.
     */
    private Text floodTF;

    /**
     * selectedLid
     */
    private String selectedLid = null;

    private Map<String, HydroDataReport> allReports = new HashMap<>();

    private boolean displayDlg = true;

    private final DecimalFormat decimalFormat = new DecimalFormat("#.##");

    private Point screenDPI;

    private int pixelsPerMile;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public StationProfileDlg(Shell parent, String selectedLid) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Station Profile");
        this.selectedLid = selectedLid;
        screenDPI = Display.getCurrent().getDPI();
        setReturnValue(selectedLid);
        loadStationProfileData();
        if (stationProfData != null) {
            calculateValues();
        } else {
            displayDlg = false;
        }
    }

    @Override
    protected void disposed() {
        font.dispose();
        profileColor.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        profileColor = new Color(getDisplay(), 216, 212, 100);
        font = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        initializeComponents();
    }

    @Override
    protected boolean shouldOpen() {
        if (displayDlg) {
            return true;
        } else {
            MessageBox mb = new MessageBox(getParent(),
                    SWT.ICON_ERROR | SWT.OK);
            mb.setText("Data Not Available");
            mb.setMessage("Unable to retrieve station information.");
            mb.open();
            setReturnValue(null);
            return false;
        }
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        createTopLabel();
        createCanvases();
        createInformationFields();

        // add a separator line
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createCloseButton();

        updateInformationFields();
    }

    /**
     * Calculate pixel and offset values.
     */
    private void calculateValues() {
        // 25 miles per inch
        int pixelsPerInch = screenDPI.x;
        pixelsPerMile = pixelsPerInch * 2 / 50;
        double totalElevInc = stationProfData.getElevationScaleMax()
                - stationProfData.getElevationScaleMin();

        // Calculate the offset between the elevation points
        double offsetDbl = totalElevInc / 5;

        elevationOffset = (int) Math.round(offsetDbl);

        // Calculate the pixels between the elevation hashes
        elevationPixelOffset = Math.round((BOTTOM_Y_COORD - TOP_Y_COORD) / 5);

        // Calculate the number of pixels per increment
        pixelsPerIncY = (BOTTOM_Y_COORD - TOP_Y_COORD) / totalElevInc;
        // Add 150 pixel buffer to the right end of canvas for text
        profileCanvasWidth = (int) (stationProfData.getMileRange())
                * pixelsPerMile + 150;

        // Minimum canvas size
        if (profileCanvasWidth < 800) {
            profileCanvasWidth = 800;
        }
    }

    /**
     * Create the stream label at the top of the dialog.
     */
    private void createTopLabel() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        labelComp.setLayout(gl);

        Label streamLbl = new Label(labelComp, SWT.NONE);
        String streamName = stationProfData.getStreamName();
        if (streamName != null) {
            streamLbl.setText("Stream: " + streamName);
        } else {
            streamLbl.setText("Stream: No Data Available");
        }
    }

    /**
     * Create the composite that will contain the 2 canvases.
     */
    private void createCanvases() {
        Composite canvasComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 0;
        canvasComp.setLayout(gl);

        addLabelCanvas(canvasComp);
        addProfileCanvas(canvasComp);
    }

    /**
     * Create the station profile information fields.
     */
    private void createInformationFields() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite infoComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(6, false);
        gl.horizontalSpacing = 0;
        infoComp.setLayout(gl);
        infoComp.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        Label stationLBl = new Label(infoComp, SWT.RIGHT);
        stationLBl.setText("Station: ");
        stationLBl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        stationCbo = new Combo(infoComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        populateStationCbo();
        int j = stationCbo.indexOf(selectedLid);
        if (j >= 0) {
            stationCbo.select(j);
        }
        stationCbo.setLayoutData(gd);
        stationCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateInformationFields();
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        Label nameLBl = new Label(infoComp, SWT.RIGHT);
        nameLBl.setText("Name: ");
        nameLBl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        nameTF = new Text(infoComp, SWT.BORDER);
        nameTF.setEditable(false);
        nameTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label filler1 = new Label(infoComp, SWT.NONE);
        filler1.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        Label reachLBl = new Label(infoComp, SWT.RIGHT);
        reachLBl.setText("Reach: ");
        reachLBl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 35;
        gd.horizontalSpan = 3;
        reachTF = new Text(infoComp, SWT.BORDER | SWT.MULTI);
        reachTF.setEditable(false);
        reachTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label filler2 = new Label(infoComp, SWT.NONE);
        filler2.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        Label actionLBl = new Label(infoComp, SWT.RIGHT);
        actionLBl.setText("Action: ");
        actionLBl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        actionTF = new Text(infoComp, SWT.BORDER);
        actionTF.setEditable(false);
        actionTF.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Label floodLBl = new Label(infoComp, SWT.RIGHT);
        floodLBl.setText("Flood: ");
        floodLBl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        floodTF = new Text(infoComp, SWT.BORDER);
        floodTF.setEditable(false);
        floodTF.setLayoutData(gd);
    }

    /**
     * Create the Close button at the bottom of the dialog.
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
                close();
            }
        });
    }

    /**
     * Add the label canvas to the display.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void addLabelCanvas(Composite parentComp) {
        Canvas labelCanvas = new Canvas(parentComp, SWT.DOUBLE_BUFFERED);
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = LABEL_CANVAS_WIDTH;

        labelCanvas.setSize(LABEL_CANVAS_WIDTH, CANVAS_HEIGHT);

        labelCanvas.setLayoutData(gd);
        labelCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawLabelCanvas(e);
            }
        });
    }

    /**
     * Add the station profile canvas to the display.
     * 
     * @param parentComp
     *            Parent component.
     */
    private void addProfileCanvas(Composite parentComp) {
        ScrolledComposite scrolledComp = new ScrolledComposite(parentComp,
                SWT.H_SCROLL | SWT.V_SCROLL);
        GridLayout gl = new GridLayout(1, false);
        scrolledComp.setLayout(gl);
        GridData gd = new GridData(SCROLLED_COMP_WIDTH, SCROLLED_COMP_HEIGHT);
        scrolledComp.setLayoutData(gd);

        Canvas profileCanvas = new Canvas(scrolledComp, SWT.DOUBLE_BUFFERED);
        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.heightHint = CANVAS_HEIGHT;
        gd.widthHint = profileCanvasWidth;

        profileCanvas.setSize(profileCanvasWidth, CANVAS_HEIGHT);

        profileCanvas.setLayoutData(gd);
        profileCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                drawProfileCanvas(e);
            }
        });

        scrolledComp.setContent(profileCanvas);
    }

    /**
     * Draw the data on the label canvas.
     * 
     * @param e
     *            Paint event.
     */
    private void drawLabelCanvas(PaintEvent e) {
        e.gc.setFont(font);
        int fontHeight = (e.gc.getFontMetrics().getHeight());

        e.gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));

        e.gc.fillRectangle(0, 0, LABEL_CANVAS_WIDTH, CANVAS_HEIGHT);

        // ----------------------------------------
        // Draw "ELEVEATION IN FEET" label
        // ----------------------------------------
        e.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));

        char[] charArray = ELEVATION_FEET.toCharArray();

        int tmpY = 50;
        for (int i = 0; i < charArray.length; i++) {
            e.gc.drawString(String.valueOf(charArray[i]), 5, tmpY, true);
            tmpY += fontHeight;
        }

        // ----------------------------------------------------
        // Draw the elevation line, hashes, and labels
        // ----------------------------------------------------
        e.gc.drawLine(LABEL_CANVAS_WIDTH - 1, BOTTOM_Y_COORD,
                LABEL_CANVAS_WIDTH - 1, TOP_Y_COORD);

        // Draw bottom hash mark & label
        e.gc.drawLine(LABEL_CANVAS_WIDTH - 1, BOTTOM_Y_COORD,
                LABEL_CANVAS_WIDTH - ELEVATION_HASH - 1, BOTTOM_Y_COORD);

        // Draw top hash mark & label
        e.gc.drawLine(LABEL_CANVAS_WIDTH - 1, TOP_Y_COORD,
                LABEL_CANVAS_WIDTH - ELEVATION_HASH - 1, TOP_Y_COORD);

        e.gc.drawString(
                String.format("%5d", stationProfData.getElevationScaleMin()),
                40, BOTTOM_Y_COORD - fontHeight / 2, true);
        e.gc.drawString(
                String.format("%5d", stationProfData.getElevationScaleMax()),
                40, TOP_Y_COORD - fontHeight / 2, true);

        // Draw remaining "in between" hash marks & labels
        int tmpYCoord = BOTTOM_Y_COORD - elevationPixelOffset;
        int tmpElev = stationProfData.getElevationScaleMin() + elevationOffset;

        for (int i = 0; i < 5; i++) {
            e.gc.drawLine(LABEL_CANVAS_WIDTH - 1, tmpYCoord,
                    LABEL_CANVAS_WIDTH - ELEVATION_HASH - 1, tmpYCoord);
            e.gc.drawString(String.format("%5d", tmpElev), 40,
                    tmpYCoord - fontHeight / 2, true);

            tmpYCoord -= elevationPixelOffset;
            tmpElev += elevationOffset;
        }

    }

    /**
     * Draw the data on the station profile canvas.
     * 
     * @param e
     *            Paint event.
     */
    private void drawProfileCanvas(PaintEvent e) {
        e.gc.setFont(font);
        int fontHeight = (e.gc.getFontMetrics().getHeight());
        int fontAveWidth = (e.gc.getFontMetrics().getAverageCharWidth());

        List<LabelPosition> labelList = new ArrayList<>();

        e.gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        e.gc.fillRectangle(0, 0, profileCanvasWidth, CANVAS_HEIGHT);

        // Draw "RIVER MILES" label
        e.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        e.gc.drawString("RIVER MILES",
                profileCanvasWidth / 2 - fontAveWidth * 5,
                CANVAS_HEIGHT - fontHeight - 3, true);

        // Y-Axis line
        e.gc.drawLine(0, BOTTOM_Y_COORD, profileCanvasWidth, BOTTOM_Y_COORD);

        // Draw X axis hash and labels
        double maxMile = this.stationProfData.getMaxMile();
        double minMile = stationProfData.getMinMile();
        double mileScaleValue = maxMile;
        int x;
        int y;

        int numTics = (int) (mileScaleValue - minMile) / 50;
        for (int i = 0; i <= numTics; i++) {
            // 10 pixel offset to the right
            x = i * pixelsPerMile * 50 + 10;
            e.gc.drawLine(x, BOTTOM_Y_COORD, x,
                    BOTTOM_Y_COORD + RIVER_MILES_HASH);
            e.gc.drawString(String.valueOf(mileScaleValue), x - fontAveWidth,
                    BOTTOM_Y_COORD + RIVER_MILES_HASH + 3, true);
            mileScaleValue -= 50;
        }

        // ------------------------------------------
        // Draw the station profile
        // ------------------------------------------
        e.gc.setBackground(profileColor);

        List<Integer> points = new ArrayList<>();
        List<Statprof> stations = stationProfData.getStations();

        // Bottom right x,y coordinate
        points.add(profileCanvasWidth);
        points.add(BOTTOM_Y_COORD - 2);

        // Bottom left x,y coordinate
        points.add(2);
        points.add(BOTTOM_Y_COORD - 2);

        // Top left x,y coordinate
        points.add(2);
        points.add(calcElevationYCoord(stations.get(0).getId().getZd()));

        // The last y value used
        int lastY = points.get(5);

        // Get gage bar points
        for (Statprof station : stations) {
            // Skip gage if the river mile is not valid
            if (station.getId().getMile() == HydroConstants.MISSING_VALUE) {
                continue;
            }
            x = (int) ((maxMile - station.getId().getMile()) * pixelsPerMile
                    + 10);
            y = calcElevationYCoord(station.getId().getZd());
            points.add(x);
            points.add(y);

            lastY = y;
        }

        /*
         * Top right x,y coordinate Need to use the last y for this coordinate
         * in case there is a site without a river mile, which can throw off the
         * drawing.
         */
        points.add(profileCanvasWidth);
        points.add(lastY);

        // profile background
        int i = 0;
        int[] pointArray = new int[points.size()];
        for (Integer pt : points) {
            pointArray[i] = pt;
            i++;
        }
        e.gc.fillPolygon(pointArray);

        // ================================================
        // NOTE: The color bars that are drawn below can
        // change color depending on certain values.
        // This needs to be accounted for...
        // ================================================

        // Draw vertical hashes and bars at the plot points
        e.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        e.gc.setLineWidth(2);

        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm MM/dd");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        for (Statprof station : stations) {
            // Skip gage if the river mile is not valid
            if (station.getId().getMile() == HydroConstants.MISSING_VALUE) {
                continue;
            }

            e.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
            x = (int) ((maxMile - station.getId().getMile()) * pixelsPerMile
                    + 10);

            y = calcElevationYCoord(station.getId().getZd());

            // hash mark at each site
            e.gc.drawLine(x, y, x, y + POINT_HASH);

            /*
             * determine color of staff, based on proximity to action or flood
             * STAGE or DISCHARGE as appropriate.
             */
            HydroDataReport report = allReports.get(station.getId().getLid());
            Display display = getDisplay();
            Color barColor = display.getSystemColor(SWT.COLOR_GREEN);
            if (report != null) {
                if (station.getId().getPrimaryPe().startsWith("H")) {
                    if ((report.getValue() > station.getId().getFs())
                            && (station.getId().getFs() > 0)) {
                        barColor = display.getSystemColor(SWT.COLOR_RED);
                    } else if ((report.getValue() > station.getId().getWstg())
                            && (station.getId().getWstg() > 0)) {
                        barColor = display.getSystemColor(SWT.COLOR_YELLOW);
                    }
                    // else use default green
                } else {
                    if ((report.getValue() > station.getId().getFq())
                            && (station.getId().getFs() > 0)) {
                        barColor = display.getSystemColor(SWT.COLOR_RED);
                    } else if ((report.getValue() > station.getId()
                            .getActionFlow())
                            && (station.getId().getWstg() > 0)) {
                        barColor = display.getSystemColor(SWT.COLOR_YELLOW);
                    }
                    // else use default green
                }
            }

            e.gc.setBackground(barColor);
            e.gc.fillRectangle(x - BAR_WIDTH / 2, y - BAR_HEIGHT, BAR_WIDTH,
                    BAR_HEIGHT);

            // label at each site
            e.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
            StringBuilder label = new StringBuilder();
            label.append(station.getId().getLid()).append(" (");

            HydroDataReport rpt = allReports.get(station.getId().getLid());
            if (rpt.getValue() != HydroConstants.MISSING_VALUE) {
                label.append(decimalFormat.format(rpt.getValue()))
                        .append(" - ");
                label.append(sdf.format(rpt.getValidTime())).append(")");
            } else {
                label.append("MSG/MSG)");
            }
            LabelPosition lp = new LabelPosition();

            // lid and value
            lp.setLidX(x - fontAveWidth);
            lp.setLidY(y - BAR_HEIGHT - 20);
            lp.setLidBuffer(label.toString());

            // elevation
            String feet = String.valueOf(station.getId().getZd());
            int offset = feet.length() * fontAveWidth / 2;
            lp.setElevX(x - offset);
            lp.setElevY(y + POINT_HASH);
            lp.setElevBuffer(feet);

            labelList.add(lp);
        }

        // Draw the elevation labels, left to right
        if (labelList.isEmpty()) {
            return;
        }

        for (int j = 0; j < labelList.size() - 1; j++) {
            LabelPosition lp = labelList.get(j);
            LabelPosition lp2 = labelList.get(j + 1);

            boolean elevCollision = false;

            int elevX = lp.getElevX();
            int elevY = lp.getElevY();

            int elevX2 = lp2.getElevX();
            int elevY2 = lp2.getElevY();
            int elevLength = lp.getElevBuffer().length();
            int pixelOverlap = 0;

            int offset = elevLength * fontAveWidth / 2;
            int offset2 = lp2.getElevBuffer().length() * fontAveWidth / 2;

            // right pixel value of elevation label 1
            int rightPixValElev = ((elevX - offset)
                    + (elevLength * fontAveWidth));

            // left pixel value of elevation label 2
            int leftPixValElev = (elevX2 - offset2);

            if (rightPixValElev >= leftPixValElev) {
                elevCollision = true;
            }

            // if collision try moving labels
            if (elevCollision) {
                pixelOverlap = elevY2 - (elevY + fontHeight);

                if (pixelOverlap < 0) {
                    lp2.setElevY(elevY - pixelOverlap);
                }
            }

            // draw elevation label
            e.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
            e.gc.drawString(lp.getElevBuffer(), lp.getElevX(), lp.getElevY(),
                    true);
        }

        // Reset to white
        e.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));

        // Draw the lid labels, right to left
        LabelPosition lp = labelList.get(labelList.size() - 1);
        e.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        e.gc.drawString(lp.getLidBuffer(), lp.getLidX(), lp.getLidY(), true);

        for (int j = labelList.size() - 2; j >= 0; j--) {
            lp = labelList.get(j);

            // one point left of lp
            LabelPosition lp2 = labelList.get(j + 1);

            boolean lidCollision = false;

            int lidX = lp.getLidX();
            int lidY = lp.getLidY();

            int lidX2 = lp2.getLidX();
            int lidY2 = lp2.getLidY();
            int lidLength = lp2.getLidBuffer().length();
            int pixelOverlap = 0;

            // right pixel value of lid label 2
            int rightPixValLid = ((lidX - fontAveWidth)
                    + (lidLength * fontAveWidth));

            // left pixel value of lid label 1
            int leftPixValLid = lidX2 - fontAveWidth;

            if (rightPixValLid >= leftPixValLid) {
                lidCollision = true;
            }

            // if collision try moving labels, but not the last one in the
            // list
            if (lidCollision) {
                pixelOverlap = lidY2 - (lidY + fontHeight) + 5;

                if (pixelOverlap < 0) {
                    lp.setLidY((lidY + pixelOverlap));
                }
            }

            e.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
            e.gc.drawString(lp.getLidBuffer(), lp.getLidX(), lp.getLidY(),
                    true);
        }

        // draw last label in list
        lp = labelList.get(labelList.size() - 1);

        // draw elevation label
        e.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        e.gc.drawString(lp.getElevBuffer(), lp.getElevX(), lp.getElevY(), true);
    }

    /**
     * Calculate the Y coordinate of the elevation.
     * 
     * @param elevation
     *            Elevation.
     * @return Y coordinate of the elevation.
     */
    private int calcElevationYCoord(double elevation) {
        double yCoordDbl = BOTTOM_Y_COORD
                - (elevation - stationProfData.getElevationScaleMin())
                        * pixelsPerIncY;
        int yCoord = (int) Math.round(yCoordDbl);

        return yCoord;
    }

    /**
     * Populate the station combo box.
     */
    private void populateStationCbo() {
        for (Statprof station : stationProfData.getStations()) {
            stationCbo.add(station.getId().getLid());
        }
    }

    /**
     * Update the station profile information fields using the selected station.
     */
    private void updateInformationFields() {
        if (stationCbo.getSelectionIndex() >= 0) {
            Statprof data = stationProfData
                    .getStationData(stationCbo.getText());
            String name = null;
            if (data.getId().getProximity() == null) {
                name = String.format("%s %s %s", data.getId().getStream(), "AT",
                        data.getId().getName());
            } else {
                name = String.format("%s %s %s", data.getId().getStream(),
                        data.getId().getProximity(), data.getId().getName());
            }

            if (name != null) {
                nameTF.setText(name);
            }

            if (data.getId().getReach() != null) {
                reachTF.setText(data.getId().getReach());
            }

            if (data.getId().getPrimaryPe().startsWith("H")) {
                actionTF.setText(
                        String.format("%5.2f", data.getId().getWstg()));
                floodTF.setText(String.format("%5.2f", data.getId().getFs()));
            } else {
                actionTF.setText(
                        String.format("%5.2f", data.getId().getActionFlow()));
                floodTF.setText(String.format("%5.2f", data.getId().getFq()));
            }
        }
    }

    /**
     * populate your data objects for display
     */
    private void loadStationProfileData() {
        HydroDisplayManager manager = HydroDisplayManager.getInstance();

        if (manager.getCurrentData() != null) {
            // based on lid, retrieve the map for this river.
            RiverDataManager rdm = RiverDataManager.getInstance();

            /* get the stream name for the selected station. */
            String stream = rdm.getStreamName(manager.getCurrentLid());
            if (stream == null) {
                // Error dialog "Unable to retrieve station information."
                stationProfData = null;
            }

            /* get the static station data */
            List<Statprof> stationList = rdm.getStatProf(stream);
            if ((stationList == null) || (stationList.isEmpty())) {
                // Error dialog
                stationProfData = null;
                return;
            }

            /* get the dynamic river station data */
            loadStatProfReports(stationList);

            stationProfData = new StationProfileData(stream, stationList);
        }
    }

    /**
     * Load the data reports into memory.
     * 
     * @param dataList
     *            - List of Statprof objects
     */
    private void loadStatProfReports(List<Statprof> dataList) {
        int hoursBack = 72;
        int fcstBasisHoursAgo = 72;

        /* loop on the stations and get their data */
        for (Statprof sp : dataList) {
            if (sp == null) {
                continue;
            }

            /* get the river data for the current station */
            MaxObsFcst mof = new MaxObsFcst(sp.getId().getLid(),
                    sp.getId().getPrimaryPe(), hoursBack, fcstBasisHoursAgo);
            HydroDataReport[] dataReports = mof.calcMaxObsFcst();
            HydroDataReport obsReport = dataReports[0];
            HydroDataReport fcstReport = dataReports[1];

            if ((obsReport != null) && (fcstReport != null)) {
                if (obsReport.getValue() >= fcstReport.getValue()) {
                    allReports.put(sp.getId().getLid(), obsReport);
                } else {
                    allReports.put(sp.getId().getLid(), fcstReport);
                }
            } else if (obsReport != null) {
                allReports.put(sp.getId().getLid(), obsReport);
            } else if (fcstReport != null) {
                allReports.put(sp.getId().getLid(), fcstReport);
            } else {
                allReports.put(sp.getId().getLid(), new HydroDataReport());
            }
        }
    }

    /**
     * Select the desired station and update the display.
     * 
     * @param station
     */
    public void setStation(String station) {
        if (stationCbo != null && !shell.isDisposed()) {
            int j = stationCbo.indexOf(station);
            stationCbo.select(j);
            updateInformationFields();
        }
    }
}
