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
package com.raytheon.viz.hydro.stationlegend;

import java.awt.image.BufferedImage;
import java.awt.image.DirectColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.WritableRaster;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.hydro.gagedisplay.HydroImageMaker;
import com.raytheon.viz.hydro.gagedisplay.HydroImageMaker.ImageSize;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.GageData.ThreatIndex;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 12, 2009            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class StationLegendDlg extends CaveSWTDialog {
    private static String STATION_ICON_TEXT = "Station Icons";

    private static String STATION_COLOR_TEXT = "River/Reservoir Station Colors";

    private static String RIVER_POINT_TEXT = "River Data Point";

    private static String FORECAST_POINT_TEXT = "River Forecast Point";

    private static String RESERVOIR_DATA_POINT_TEXT = "Reservoir Data Point";

    private static String RESERVOIR_FORECAST_POINT_TEXT = "Reservoir Forecast Point";

    private static String MET_STATION_TEXT = "Meteorological Station";

    private static String RIVER_FORECAST_MET_POINT_TEXT = "River Forecast Point with Meteorological Station";

    private static String MISSING_DATA_TEXT = "River Forecast Point with missing data";

    private static String FLOOD_STAGE_TEXT = "River Forecast Point above flood stage";

    private static String ACTION_STAGE_TEXT = "River Forecast Point above action stage";

    private static String BELOW_ACTION_STAGE_TEXT = "River Forecast Point below action stage";

    private static String MISSING_STAGE_TEXT = "River Forecast Point missing action/flood stage values";

    private static String STATION_DATA_TEXT = "Station Data";

    /**
     * The width of the dialog.
     */
    private final int windowWidth = 775;

    /**
     * The height of the dialog.
     */
    private final int windowHeight = 750;

    private int[] columns = new int[4];

    private int[] rows = new int[9];

    private Canvas canvas;

    /**
     * Constructor.
     * 
     * @param shell
     *            The parent shell
     */
    public StationLegendDlg(Shell shell) {
        super(shell, SWT.DIALOG_TRIM | SWT.MIN);
        setText("Station Legend");
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createCanvas();

        setRowsColumns();

        createCloseButton();

        canvas.redraw();
    }

    private void createCanvas() {
        /* Create the canvas for drawing */
        GridData gd = new GridData(windowWidth, windowHeight);
        canvas = new Canvas(shell, SWT.BORDER);
        canvas.setLayoutData(gd);
        canvas.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        canvas.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));

        canvas.addListener(SWT.Paint, new Listener() {
            public void handleEvent(Event event) {
                drawDialog(event);
            }
        });
    }

    private void drawDialog(Event evt) {
        int fontHeight = (evt.gc.getFontMetrics().getHeight());
        int fontAveWidth = evt.gc.getFontMetrics().getAverageCharWidth();

        int centerPt = windowWidth / 2;

        /* Shift the y value so the text is centerd on the icon */
        int yShift = (fontHeight - ImageSize.SMALL.getHeight()) / 2;
        if (yShift < 0) {
            yShift *= -1;
        }

        int rowCount = 0;
        
        evt.gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        evt.gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));
        
        evt.gc.fillRectangle(0, 0, windowWidth, windowHeight);

        /* Column 1 Title */
        evt.gc.drawString(STATION_ICON_TEXT, columns[0], rows[rowCount++]);

        /* Set the data to create the icons for column 1 */
        GageData gd = new GageData();
        gd.setDispClass("R"); // River point
        gd.setThreatIndex(ThreatIndex.THREAT_NONE); // Creates the light green
        // color
        BufferedImage icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);

        /* Convert the icon to SWT */
        ImageData imageData = convertToSWT(icon);
        Image swtIcon = new Image(evt.display, imageData);

        evt.gc.drawImage(swtIcon, columns[0], rows[rowCount]);
        evt.gc
                .drawString(RIVER_POINT_TEXT, columns[1], rows[rowCount]
                        + yShift);

        gd.setDispClass("RF"); // River forecast point
        icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
        imageData = convertToSWT(icon);
        swtIcon = new Image(evt.display, imageData);
        evt.gc.drawImage(swtIcon, columns[0], rows[++rowCount]);
        evt.gc.drawString(FORECAST_POINT_TEXT, columns[1], rows[rowCount]
                + yShift);

        gd.setDispClass("RD"); // Reservoir point
        icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
        imageData = convertToSWT(icon);
        swtIcon = new Image(evt.display, imageData);
        evt.gc.drawImage(swtIcon, columns[0], rows[++rowCount]);
        evt.gc.drawString(RESERVOIR_DATA_POINT_TEXT, columns[1], rows[rowCount]
                + yShift);

        gd.setDispClass("RFD"); // Reservoir forecast point
        icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
        imageData = convertToSWT(icon);
        swtIcon = new Image(evt.display, imageData);
        evt.gc.drawImage(swtIcon, columns[0], rows[++rowCount]);
        evt.gc.drawString(RESERVOIR_FORECAST_POINT_TEXT, columns[1],
                rows[rowCount] + yShift);

        gd.setDispClass("O"); // Meteoroligical station
        icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
        imageData = convertToSWT(icon);
        swtIcon = new Image(evt.display, imageData);
        evt.gc.drawImage(swtIcon, columns[0], rows[++rowCount]);
        evt.gc
                .drawString(MET_STATION_TEXT, columns[1], rows[rowCount]
                        + yShift);

        gd.setDispClass("RFO"); // Meteoroligical station
        icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
        imageData = convertToSWT(icon);
        swtIcon = new Image(evt.display, imageData);
        evt.gc.drawImage(swtIcon, columns[0], rows[++rowCount]);
        evt.gc.drawString(RIVER_FORECAST_MET_POINT_TEXT, columns[1],
                rows[rowCount] + yShift);

        rowCount = 0;

        /* Column 2 title */
        evt.gc.drawString(STATION_COLOR_TEXT, columns[3], rows[rowCount]);

        /* Set the data to create the icons for column 2 */
        gd.setDispClass("RF");
        gd.setThreatIndex(ThreatIndex.THREAT_MISSING_DATA);
        icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
        imageData = convertToSWT(icon);
        swtIcon = new Image(evt.display, imageData);
        evt.gc.drawImage(swtIcon, columns[2], rows[++rowCount]);
        evt.gc.drawString(MISSING_DATA_TEXT, columns[3], rows[rowCount]
                + yShift);

        gd.setThreatIndex(ThreatIndex.THREAT_FLOOD);
        icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
        imageData = convertToSWT(icon);
        swtIcon = new Image(evt.display, imageData);
        evt.gc.drawImage(swtIcon, columns[2], rows[++rowCount]);
        evt.gc
                .drawString(FLOOD_STAGE_TEXT, columns[3], rows[rowCount]
                        + yShift);

        gd.setThreatIndex(ThreatIndex.THREAT_ACTION);
        icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
        imageData = convertToSWT(icon);
        swtIcon = new Image(evt.display, imageData);
        evt.gc.drawImage(swtIcon, columns[2], rows[++rowCount]);
        evt.gc.drawString(ACTION_STAGE_TEXT, columns[3], rows[rowCount]
                + yShift);

        gd.setThreatIndex(ThreatIndex.THREAT_NONE);
        icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
        imageData = convertToSWT(icon);
        swtIcon = new Image(evt.display, imageData);
        evt.gc.drawImage(swtIcon, columns[2], rows[++rowCount]);
        evt.gc.drawString(BELOW_ACTION_STAGE_TEXT, columns[3], rows[rowCount]
                + yShift);

        gd.setThreatIndex(ThreatIndex.THREAT_MISSING_STAGE);
        icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
        imageData = convertToSWT(icon);
        swtIcon = new Image(evt.display, imageData);
        evt.gc.drawImage(swtIcon, columns[2], rows[++rowCount]);
        evt.gc.drawString(MISSING_STAGE_TEXT, columns[3], rows[rowCount]
                + yShift);

        /* Draw the station Data and center the String */
        int pixelWidth = STATION_DATA_TEXT.length() * fontAveWidth;
        int startX = centerPt - (pixelWidth / 2);
        evt.gc.drawString(STATION_DATA_TEXT, startX, rows[rowCount += 2]); // Skip
        // a
        // row

        gd.setThreatIndex(ThreatIndex.THREAT_NONE);
        gd.setDispClass("RF");
        icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
        imageData = convertToSWT(icon);
        swtIcon = new Image(evt.display, imageData);
        startX = centerPt - (swtIcon.getImageData().width / 2);
        evt.gc.drawImage(swtIcon, columns[2], rows[++rowCount]);

        String label = "[Fld Stg/Flow]";
        startX = centerPt - (label.length() * fontAveWidth) - 10;
        evt.gc.drawString(label, startX, rows[rowCount] - 17 - fontHeight);

        label = "Value";
        startX = centerPt - (label.length() * fontAveWidth) - 10;
        evt.gc.drawString(label, startX, rows[rowCount] - fontHeight);

        label = "Id";
        startX = centerPt - (label.length() * fontAveWidth) - 10;
        evt.gc.drawString(label, startX, rows[rowCount] + 10 + fontHeight);

        label = "MM/DD";
        startX = centerPt + 25;
        evt.gc.drawString(label, startX, rows[rowCount] - 17 - fontHeight);

        label = "hh:mm";
        startX = centerPt + 25;
        evt.gc.drawString(label, startX, rows[rowCount] - fontHeight);

        label = "Name";
        startX = centerPt + 25;
        evt.gc.drawString(label, startX, rows[rowCount] + 10 + fontHeight);

    }

    // private void drawGage(Event evt, GageData gd, int yShift, int column, int
    // rowCount, String text) {
    // /* Create the icon */
    // BufferedImage icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
    //        
    // /* Convert the icon to SWT */
    // ImageData imageData = convertToSWT(icon);
    // Image swtIcon = new Image(evt.display, imageData);
    //        
    // evt.gc.drawImage(swtIcon, column, rows[rowCount]);
    // evt.gc.drawString(text, column + 1, rows[rowCount] + yShift);
    // }
    //
    /**
     * Convert an AWT BufferedImage to a SWT image.
     * 
     * @param bufferedImage
     *            The image to convert
     * @return An ImageData object
     */
    private static ImageData convertToSWT(BufferedImage bufferedImage) {
        if (bufferedImage.getColorModel() instanceof DirectColorModel) {
            DirectColorModel colorModel = (DirectColorModel) bufferedImage
                    .getColorModel();
            PaletteData palette = new PaletteData(colorModel.getRedMask(),
                    colorModel.getGreenMask(), colorModel.getBlueMask());
            ImageData data = new ImageData(bufferedImage.getWidth(),
                    bufferedImage.getHeight(), colorModel.getPixelSize(),
                    palette);
            for (int y = 0; y < data.height; y++) {
                for (int x = 0; x < data.width; x++) {
                    int rgb = bufferedImage.getRGB(x, y);
                    int pixel = palette.getPixel(new RGB((rgb >> 16) & 0xFF,
                            (rgb >> 8) & 0xFF, rgb & 0xFF));
                    data.setPixel(x, y, pixel);
                }
            }
            return data;
        } else if (bufferedImage.getColorModel() instanceof IndexColorModel) {
            IndexColorModel colorModel = (IndexColorModel) bufferedImage
                    .getColorModel();
            int size = colorModel.getMapSize();
            byte[] reds = new byte[size];
            byte[] greens = new byte[size];
            byte[] blues = new byte[size];
            colorModel.getReds(reds);
            colorModel.getGreens(greens);
            colorModel.getBlues(blues);
            RGB[] rgbs = new RGB[size];
            for (int i = 0; i < rgbs.length; i++) {
                rgbs[i] = new RGB(reds[i] & 0xFF, greens[i] & 0xFF,
                        blues[i] & 0xFF);
            }
            PaletteData palette = new PaletteData(rgbs);
            ImageData data = new ImageData(bufferedImage.getWidth(),
                    bufferedImage.getHeight(), colorModel.getPixelSize(),
                    palette);
            data.transparentPixel = colorModel.getTransparentPixel();
            WritableRaster raster = bufferedImage.getRaster();
            int[] pixelArray = new int[1];
            for (int y = 0; y < data.height; y++) {
                for (int x = 0; x < data.width; x++) {
                    raster.getPixel(x, y, pixelArray);
                    data.setPixel(x, y, pixelArray[0]);
                }
            }
            return data;
        }
        return null;
    }

    private void createCloseButton() {
        /* Add the close button */
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData mainGD = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout buttonGl = new GridLayout(1, false);
        buttonComp.setLayoutData(mainGD);
        buttonComp.setLayout(buttonGl);

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Set the rows and columns xy values.
     */
    private void setRowsColumns() {
        columns[0] = 40;
        columns[1] = 75;
        columns[2] = 380;
        columns[3] = 415;

        rows[0] = 25;
        rows[1] = 60;
        rows[2] = 95;
        rows[3] = 125;
        rows[4] = 155;
        rows[5] = 190;
        rows[6] = 225;
        rows[7] = 400;
        rows[8] = 475;
    }
}
