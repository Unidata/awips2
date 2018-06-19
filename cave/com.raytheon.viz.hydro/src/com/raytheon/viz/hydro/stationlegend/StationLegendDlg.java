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
import org.eclipse.swt.graphics.GC;
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
 * Dialog to display the various station Legends.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ---------------------------------
 * Jan 12, 2009           mpduff    Initial creation
 * Mar 15, 2013  1790     rferrel   Changes for non-blocking dialog.
 * Sep 04, 2013  2324     lvenable  Fixed image memory leaks
 * May 16, 2016  5483     randerso  Reworked to fix GUI sizing issues
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class StationLegendDlg extends CaveSWTDialog {

    private class LegendData {
        String displayClass;

        ThreatIndex threatIndex;

        String legend;

        Image image;

        /**
         * Constructor
         * 
         * @param displayClass
         * @param threatIndex
         * @param legend
         */
        public LegendData(String displayClass, ThreatIndex threatIndex,
                String legend) {
            this.displayClass = displayClass;
            this.threatIndex = threatIndex;
            this.legend = legend;

            this.image = createImage();
        }

        private Image createImage() {
            GageData gd = new GageData();
            gd.setDispClass(displayClass);
            gd.setThreatIndex(threatIndex);

            BufferedImage icon = HydroImageMaker.getImage(gd, ImageSize.SMALL);
            ImageData imageData = convertToSWT(icon);
            return new Image(getDisplay(), imageData);
        }
    }

    /** Header for station icon column. */
    private final String STATION_ICON_TEXT = "Station Icons";

    /** Header for column to display river/reservoir colors. */
    private final String STATION_COLOR_TEXT = "River/Reservoir Station Colors";

    /** Title for river point icon. */
    private final String RIVER_POINT_TEXT = "River Data Point";

    /** Title for forecast point icon. */
    private final String FORECAST_POINT_TEXT = "River Forecast Point";

    /** Title for reservoir data point icon. */
    private final String RESERVOIR_DATA_POINT_TEXT = "Reservoir Data Point";

    /** Title for reservoir forecast point icon. */
    private final String RESERVOIR_FORECAST_POINT_TEXT = "Reservoir Forecast Point";

    /** Title for meteorological station icon. */
    private final String MET_STATION_TEXT = "Meteorological Station";

    /** Title for combined river forecast point and meteorological station icon. */
    private final String RIVER_FORECAST_MET_POINT_TEXT = "River Forecast Point with Meteorological Station";

    /** Title for river forecast point with missing data icon. */
    private final String MISSING_DATA_TEXT = "River Forecast Point with missing data";

    /** Title for river forecast point above flood stage icon. */
    private final String FLOOD_STAGE_TEXT = "River Forecast Point above flood stage";

    /** Title for river forecast point above action stage icon. */
    private final String ACTION_STAGE_TEXT = "River Forecast Point above action stage";

    /** Title for river forecast point below action stage icon. */
    private final String BELOW_ACTION_STAGE_TEXT = "River Forecast Point below action stage";

    /** Title for river forecast poine missing action/flood stage value icon. */
    private final String MISSING_STAGE_TEXT = "River Forecast Point missing action/flood stage values";

    /** Title for station data display. */
    private final String STATION_DATA_TEXT = "Station Data";

    /**
     * The width of the dialog.
     */
    private int windowWidth;

    /**
     * The height of the dialog.
     */
    private int windowHeight;

    /** The canvas use for the display of icons/titles. */
    private Canvas canvas;

    private String[] columnTitle = { STATION_ICON_TEXT, STATION_COLOR_TEXT };

    private LegendData[][] legendData = {
            // Column 1
            {
                    new LegendData("R", ThreatIndex.THREAT_NONE,
                            RIVER_POINT_TEXT),
                    new LegendData("RF", ThreatIndex.THREAT_NONE,
                            FORECAST_POINT_TEXT),
                    new LegendData("RD", ThreatIndex.THREAT_NONE,
                            RESERVOIR_DATA_POINT_TEXT),
                    new LegendData("RFD", ThreatIndex.THREAT_NONE,
                            RESERVOIR_FORECAST_POINT_TEXT),
                    new LegendData("O", ThreatIndex.THREAT_NONE,
                            MET_STATION_TEXT),
                    new LegendData("RFO", ThreatIndex.THREAT_NONE,
                            RIVER_FORECAST_MET_POINT_TEXT), },

            // Column 2
            {
                    new LegendData("RF", ThreatIndex.THREAT_MISSING_DATA,
                            MISSING_DATA_TEXT),
                    new LegendData("RF", ThreatIndex.THREAT_FLOOD,
                            FLOOD_STAGE_TEXT),
                    new LegendData("RF", ThreatIndex.THREAT_ACTION,
                            ACTION_STAGE_TEXT),
                    new LegendData("RF", ThreatIndex.THREAT_NONE,
                            BELOW_ACTION_STAGE_TEXT),
                    new LegendData("RF", ThreatIndex.THREAT_MISSING_STAGE,
                            MISSING_STAGE_TEXT) } };

    /**
     * Constructor.
     * 
     * @param shell
     *            The parent shell
     */
    public StationLegendDlg(Shell shell) {
        super(shell, SWT.DIALOG_TRIM | SWT.MIN, CAVE.DO_NOT_BLOCK);
        setText("Station Legend");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayoutData()
     */
    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
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

        createCanvas();

        createCloseButton();

        canvas.redraw();
    }

    @Override
    protected void disposed() {
        for (LegendData[] column : legendData) {
            for (LegendData data : column) {
                data.image.dispose();
                data.image = null;
            }
        }
    }

    /**
     * Set up canvas for display.
     */
    private void createCanvas() {
        /* Create the canvas for drawing */
        canvas = new Canvas(shell, SWT.BORDER);
        canvas.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        canvas.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));

        canvas.addListener(SWT.Paint, new Listener() {
            @Override
            public void handleEvent(Event event) {
                drawCanvas(event.gc);
            }
        });

        GC gc = new GC(canvas);
        drawCanvas(gc);
        gc.dispose();
        GridData gd = new GridData(windowWidth, windowHeight);
        canvas.setLayoutData(gd);
    }

    /**
     * Performs the work of displaying the canvas.
     * 
     * @param gc
     */
    private void drawCanvas(GC gc) {
        int fontHeight = (gc.getFontMetrics().getHeight());
        int imageWidth = ImageSize.SMALL.getWidth();
        int imageHeight = ImageSize.SMALL.getHeight();

        int xSpace = gc.textExtent("   ").x;
        int rowHeight = Math.max(imageHeight, fontHeight * 2);

        /* Shift the y value so the text is centered on the icon */
        int yShift = (imageHeight - fontHeight) / 2;

        gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));
        gc.setForeground(getDisplay().getSystemColor(SWT.COLOR_WHITE));

        int x = xSpace;

        int maxWidth = 0;
        int maxHeight = 0;
        for (int column = 0; column < 2; column++) {
            int y = rowHeight;

            gc.drawString(columnTitle[column], x, y);

            for (LegendData data : legendData[column]) {
                y += rowHeight;
                int x1 = x + imageWidth + xSpace;

                maxWidth = Math
                        .max(maxWidth, x1 + gc.textExtent(data.legend).x);
                maxHeight = Math.max(maxHeight, y + imageHeight);
                maxHeight = Math.max(maxHeight, y + yShift + fontHeight);

                gc.drawImage(data.image, x, y);
                gc.drawString(data.legend, x1, y + yShift);
            }

            x = maxWidth + xSpace * 2;
        }

        windowWidth = maxWidth + xSpace;

        /* Draw the station Data and center the String */
        int centerX = windowWidth / 2;
        int y = maxHeight + rowHeight * 2;
        int textWidth = gc.textExtent(STATION_DATA_TEXT).x;
        gc.drawString(STATION_DATA_TEXT, centerX - textWidth / 2, y);

        y += fontHeight * 4;
        gc.drawImage(legendData[0][1].image, centerX - imageWidth / 2, y);

        String label = "[Fld Stg/Flow]";
        x = centerX - gc.textExtent(label).x - xSpace;
        gc.drawString(label, x, y - fontHeight * 2);

        label = "Value";
        x = centerX - gc.textExtent(label).x - xSpace;
        gc.drawString(label, x, y - fontHeight);

        label = "Id";
        x = centerX - gc.textExtent(label).x - xSpace;
        gc.drawString(label, x, y + imageHeight);

        label = "MM/DD";
        x = centerX + imageWidth / 2 + xSpace;
        gc.drawString(label, x, y - fontHeight * 2);

        label = "hh:mm";
        gc.drawString(label, x, y - fontHeight);

        label = "Name";
        gc.drawString(label, x, y + imageHeight);

        windowHeight = y + imageHeight + fontHeight * 3;

    }

    /**
     * Convert an AWT BufferedImage to a SWT image.
     * 
     * @param bufferedImage
     *            The image to convert
     * @return An ImageData object
     */
    private ImageData convertToSWT(BufferedImage bufferedImage) {
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

    /**
     * Make the close button.
     */
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
                close();
            }
        });
    }
}
