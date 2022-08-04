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

package com.raytheon.viz.awipstools.ui.dialog;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.util.Calendar;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.awipstools.common.SunriseSunsetCalculator;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import org.locationtech.jts.geom.Coordinate;

/**
 * Tool for viewing the sunrise/sunset time and associated Azimuths for a
 * specified latitude, longitude, day, month and year.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ----------   ----------  ----------- --------------------------
 * 08/15/07     426         Eric Babin  Initial Creation.
 * 23Oct2007    499         Eric Babin  Added error check for latitude between -90 &amp; 90.
 * 03Dec2007    461         bphillip    Modified to use VizTime
 *  10-21-09    #1711       bsteffen    Modified to use datamanager for home location
 * 25May2010    5603        bkowal      Added additional timezones based on the timezones
 *                                      that were present in AWIPSI.
 * 11Jul2012    875         rferrel     Now uses PointsDataManager
 * 25 Jul 2014  #3400       lvenable    Modified the shell style to remove the application modal setting.
 * Feb 23, 2017 6115        tgurney     Calculate spacing based on font size
 * Nov 27, 2018 7633        tgurney     Fix index out of bounds when lat/lon are empty
 * Nov 27, 2018 7633        tgurney     Cleanup
 *
 * </pre>
 *
 * @author Eric Babin
 */
public class SunriseToolDialog extends CaveJFACEDialog {

    private static final int CALCULATE_ID = IDialogConstants.CLIENT_ID;

    private Shell parentShell;

    private String title;

    private Composite top;

    private Text latText;

    private Text longitudeText;

    private Spinner yearSpinner;

    private Spinner monthSpinner;

    private Spinner daySpinner;

    private Combo zoneCombo;

    private int startY = 0;

    private Canvas sunriseCanvas;

    private String lengthOfDay;

    private double azimuthRise;

    private double azimuthSet;

    private String sunrise;

    private String sunset;

    private int startAngle;

    private int arcAngle;

    private Color bgColor;

    private SunriseSunsetCalculator sunriseCalculator = new SunriseSunsetCalculator();

    private double latitudeValue;

    private double longitudeValue;

    private int charWidth;

    private int charHeight;

    private int circleSize;

    private DecimalFormat decimalFormat = new DecimalFormat("0.00");

    private VerifyListener latLonVerifyListener;

    private static final String timeZoneIDs[] = { "Pacific/Guam", "HST", "AST",
            "AST", "PST", "PST", "MST", "MST", "CST", "CST", "EST", "EST",
            "SystemV/AST4", "GMT" };

    private static final boolean daylightIndicators[] = { false, false, false,
            true, false, true, false, true, false, true, false, true, false,
            false };

    private static final LinkedHashMap<String, String> timezonesMap;

    static {
        TimeZone timeZone = null;
        String displayName = null;
        String displayText = null;
        boolean dlIndicator = false;

        int additionalOffset = -1;
        timezonesMap = new LinkedHashMap<>();
        for (int i = 0; i < timeZoneIDs.length; i++) {
            dlIndicator = daylightIndicators[i];

            timeZone = TimeZone.getTimeZone(timeZoneIDs[i]);

            additionalOffset = 0;
            if (dlIndicator) {
                additionalOffset = (int) (1000 * TimeUtil.MILLIS_PER_SECOND);
            }

            displayName = timeZone.getDisplayName(dlIndicator, TimeZone.SHORT);
            displayText = displayName + "("
                    + (timeZone.getRawOffset() + additionalOffset)
                            / TimeUtil.MILLIS_PER_HOUR
                    + ")";

            timezonesMap.put(displayText, displayName);
        }
    }

    public SunriseToolDialog(Shell parShell, String dialogTitle) {
        super(parShell);

        // Remove Application Modal from the JFace shell style.
        setShellStyle(getShellStyle() & ~SWT.APPLICATION_MODAL);

        parentShell = parShell;
        title = dialogTitle;

        Coordinate center = PointsDataManager.getInstance().getWfoCenter();
        latitudeValue = center.y;
        longitudeValue = center.x;
        GC gc = new GC(parShell);
        charWidth = gc.getCharWidth('x');
        charHeight = gc.getFontMetrics().getHeight();
        gc.dispose();
        latLonVerifyListener = (e) -> {
            for (char c : e.text.toCharArray()) {
                if (c != '-' && c != '.' && !Character.isDigit(c)) {
                    e.doit = false;
                }
            }
        };

    }

    @Override
    public Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);
        GridLayout gridLayout = new GridLayout(2, false);
        top.setLayout(gridLayout);

        Font smallFont = new Font(top.getFont().getDevice(), "Sans", 7, 1);
        top.setFont(smallFont);
        createToolControls();
        createSunriseDisplay();
        calculateSunriseSunset();

        return top;
    }

    private boolean verifyLatLon() {
        String s = latText.getText();
        try {
            latitudeValue = Double.parseDouble(s);
            if (latitudeValue < -90 || latitudeValue > 90) {
                MessageDialog.openError(parentShell, "Invalid Number Specified",
                        "Latitude must be between -90 and 90.  Please correct.");
                latText.setFocus();
                return false;
            }
        } catch (NumberFormatException nfe) {
            MessageDialog.openError(parentShell, "Invalid Number Specified",
                    "The input for the Latitude is not a number.  Please correct.");
            latText.setFocus();
            return false;
        }

        s = longitudeText.getText();
        try {
            longitudeValue = Double.parseDouble(s);
        } catch (NumberFormatException nfe) {
            MessageDialog.openError(parentShell, "Invalid Number Specified",
                    "The input for the Longitude is not a number.  Please correct.");
            longitudeText.setFocus();
            return false;
        }

        return true;

    }

    private boolean verifyDate() {
        try {
            LocalDate.of(yearSpinner.getSelection(),
                    monthSpinner.getSelection(), daySpinner.getSelection());
        } catch (DateTimeException e) {
            MessageDialog.openError(parentShell, "Invalid Date Specified",
                    "The date specified is not a valid date.  Please correct.");
            return false;
        }
        return true;
    }

    /**
     * Method for calling calculate on sunrise calculator.
     */
    private void calculateSunriseSunset() {
        if (!verifyLatLon() || !verifyDate()) {
            return;
        }

        int year = Integer.parseInt(yearSpinner.getText());
        int month = Integer.parseInt(monthSpinner.getText());
        int day = Integer.parseInt(daySpinner.getText());

        sunriseCalculator.calculate(longitudeValue, latitudeValue,
                timezonesMap.get(zoneCombo.getText()), year, month, day);

        azimuthRise = sunriseCalculator.getSunriseAzimuth();
        azimuthSet = sunriseCalculator.getSunsetAzimuth();
        sunrise = sunriseCalculator.getSunRiseTime();
        sunset = sunriseCalculator.getSunSetTime();
        lengthOfDay = sunriseCalculator.getDayLength();

        sunriseCanvas.redraw();

    }

    /**
     * Method for getting next location for screen layout
     *
     * @param isLabel
     *            boolean Pass true, to signify left column.
     * @return a FormData object
     */
    private FormData createFormLayoutLoc(boolean isLabel) {
        FormData data = new FormData();

        if (isLabel) {
            data.top = new FormAttachment(0, startY);
            data.left = new FormAttachment(0, 0);
        } else {
            data.top = new FormAttachment(0, startY);
            data.left = new FormAttachment(0, charWidth * 10);
            startY += (int) (charHeight * 1.6);
        }
        return data;
    }

    /**
     * Method for creating sunrise display.
     */
    private void createSunriseDisplay() {
        startY = 0;

        Composite sunriseDisplayComposite = new Composite(top, SWT.NONE);
        sunriseDisplayComposite.setLayout(new FormLayout());

        GridData gridData = new GridData(GridData.FILL_BOTH);
        sunriseDisplayComposite.setLayoutData(gridData);

        createFormLayoutLoc(false);
        FormData mDat = createFormLayoutLoc(true);

        mDat.height = charWidth * 35;
        mDat.width = charWidth * 38;
        mDat.top = new FormAttachment(0, 0);
        sunriseCanvas = createSunriseChart(sunriseDisplayComposite, mDat.height,
                mDat.width);
        sunriseCanvas.setLayoutData(mDat);
        circleSize = mDat.width / 2;
    }

    /**
     * Method for creating the month, day, etc labels and fields.
     */
    private void createToolControls() {
        String[] zoneStrings = null;
        Iterator<String> tzIterator = null;

        Composite sunriseFieldComposite = new Composite(top, SWT.NONE);
        sunriseFieldComposite.setLayout(new FormLayout());

        GridData gridData = new GridData(GridData.FILL_BOTH);
        sunriseFieldComposite.setLayoutData(gridData);

        Label latitudeLabel = new Label(sunriseFieldComposite, SWT.NONE);
        latitudeLabel.setText("Latitude:");
        latitudeLabel.setLayoutData(createFormLayoutLoc(true));

        latText = new Text(sunriseFieldComposite,
                SWT.SINGLE | SWT.RIGHT | SWT.BORDER);
        FormData data = createFormLayoutLoc(false);
        data.width = charWidth * 10;
        latText.setLayoutData(data);
        latText.setText(Double.toString(latitudeValue));
        latText.addVerifyListener(latLonVerifyListener);

        Label longitudeLabel = new Label(sunriseFieldComposite, SWT.NONE);
        longitudeLabel.setText("Longitude:");
        longitudeLabel.setLayoutData(createFormLayoutLoc(true));

        longitudeText = new Text(sunriseFieldComposite,
                SWT.SINGLE | SWT.RIGHT | SWT.BORDER);
        data = createFormLayoutLoc(false);
        data.width = charWidth * 10;
        longitudeText.setLayoutData(data);
        longitudeText.setText(Double.toString(longitudeValue));
        latText.addVerifyListener(latLonVerifyListener);

        Button setHomeLocButton = new Button(sunriseFieldComposite, SWT.PUSH);
        setHomeLocButton.setText("Set at Home location");
        data = createFormLayoutLoc(true);
        data.top = new FormAttachment(0, startY += charWidth / 2);
        setHomeLocButton.setLayoutData(data);
        setHomeLocButton.addListener(SWT.MouseUp, new Listener() {

            @Override
            public void handleEvent(Event event) {
                Coordinate home = PointsDataManager.getInstance().getHome();
                longitudeValue = home.x;
                latitudeValue = home.y;
                longitudeText.setText(String.valueOf(longitudeValue));
                latText.setText(String.valueOf(latitudeValue));
            }

        });
        createFormLayoutLoc(false);

        data = createFormLayoutLoc(true);
        data.top = new FormAttachment(0, startY += charWidth * 2);

        Label yearLabel = new Label(sunriseFieldComposite, SWT.NONE);
        yearLabel.setText("Year:");
        yearLabel.setLayoutData(data);

        Calendar c = Calendar.getInstance();
        c.setTime(SimulatedTime.getSystemTime().getTime());

        yearSpinner = new Spinner(sunriseFieldComposite, SWT.BORDER);
        yearSpinner.setValues(c.get(Calendar.YEAR), 1, 9999, 0, 1, 5);
        data = createFormLayoutLoc(false);
        data.left = new FormAttachment(0, charWidth * 9);
        yearSpinner.setLayoutData(data);

        Label monthLabel = new Label(sunriseFieldComposite, SWT.NONE);
        monthLabel.setText("Month:");
        monthLabel.setLayoutData(createFormLayoutLoc(true));

        monthSpinner = new Spinner(sunriseFieldComposite, SWT.BORDER);
        monthSpinner.setValues(c.get(Calendar.MONTH) + 1, 1, 12, 0, 1, 5);
        data = createFormLayoutLoc(false);
        data.left = new FormAttachment(0, charWidth * 9);
        monthSpinner.setLayoutData(data);

        Label dayLabel = new Label(sunriseFieldComposite, SWT.NONE);
        dayLabel.setText("Day:");
        dayLabel.setLayoutData(createFormLayoutLoc(true));

        daySpinner = new Spinner(sunriseFieldComposite, SWT.BORDER);
        daySpinner.setValues(c.get(Calendar.DAY_OF_MONTH), 1, 31, 0, 1, 5);
        data = createFormLayoutLoc(false);
        data.left = new FormAttachment(0, charWidth * 9);
        daySpinner.setLayoutData(data);

        Label zoneLabel = new Label(sunriseFieldComposite, SWT.NONE);
        zoneLabel.setText("Zone:");
        data = createFormLayoutLoc(true);
        data.top = new FormAttachment(0, startY += charWidth);
        zoneLabel.setLayoutData(data);

        zoneStrings = new String[timezonesMap.size()];
        tzIterator = timezonesMap.keySet().iterator();
        for (int i = 0; i < zoneStrings.length; i++) {
            zoneStrings[i] = tzIterator.next();
        }

        zoneCombo = new Combo(sunriseFieldComposite,
                SWT.DROP_DOWN | SWT.READ_ONLY);
        zoneCombo.setItems(zoneStrings);
        data = createFormLayoutLoc(false);
        data.left = new FormAttachment(0, charWidth * 8);
        zoneCombo.setLayoutData(data);

        /* Set Display Text To First Element In Combo Box */
        zoneCombo.setText(zoneStrings[0]);
        setCurrentTimeZone(zoneStrings);

        startY = 0;
    }

    /**
     * Method for examining the current time, and setting the zone drop down to
     * the current.
     */
    private void setCurrentTimeZone(String[] zoneStrings) {
        String lTimeZoneString = null;

        lTimeZoneString = localTimeZoneString();
        if (timezonesMap.containsValue(lTimeZoneString)) {
            for (String zoneString : zoneStrings) {
                if (zoneString.contains(lTimeZoneString + "(")) {
                    zoneCombo.setText(zoneString);
                }
            }
        }
    }

    /**
     * @return
     */
    private String localTimeZoneString() {
        String localTimeZone = new SimpleDateFormat("z")
                .format(SimulatedTime.getSystemTime().getTime());
        return localTimeZone;
    }

    /**
     *
     * @param comp
     *            The parent composite
     * @param height
     *            The height of the parent composite.
     * @param canvasWidth
     *            The width of the parent composite.
     * @return The canvas with the sunrise chart.
     */
    private Canvas createSunriseChart(Composite comp, final int height,
            final int width) {

        final Canvas canvas = new Canvas(comp, SWT.NONE | SWT.BORDER);
        canvas.addPaintListener(new PaintListener() {

            @Override
            public void paintControl(PaintEvent e) {
                bgColor = e.gc.getBackground();

                startAngle = 0;
                arcAngle = 360;

                if (latitudeValue > 0) {
                    if (azimuthRise <= 0 && azimuthSet >= 360) {
                        // using defaults
                    } else if (azimuthRise <= 90) {
                        startAngle = (int) (90 - azimuthRise);
                        arcAngle = -(int) (azimuthSet - azimuthRise);
                    } else {
                        startAngle = (int) (450 - azimuthRise);
                        arcAngle = -(int) (azimuthSet - azimuthRise);
                    }
                } else {
                    if (azimuthRise <= 0 && azimuthSet >= 360) {
                        // using defaults
                    } else if (azimuthRise <= 90) {
                        startAngle = (int) (90 - azimuthRise);
                        arcAngle = (int) (360 - azimuthSet + azimuthRise);
                    } else {
                        startAngle = (int) (450 - azimuthRise);
                        arcAngle = (int) (360 - azimuthSet + azimuthRise);
                    }
                }

                e.gc.setBackground(
                        Display.getCurrent().getSystemColor(SWT.COLOR_YELLOW));
                int circleX = width / 2 - circleSize / 2;
                int circleY = height / 2 - circleSize / 2;
                e.gc.fillArc(circleX, circleY, circleSize, circleSize,
                        startAngle, arcAngle);
                e.gc.setBackground(
                        Display.getCurrent().getSystemColor(SWT.COLOR_BLACK));

                e.gc.drawArc(circleX, circleY, circleSize, circleSize,
                        startAngle, arcAngle);

                e.gc.setBackground(bgColor);
                e.gc.setBackground(e.gc.getBackground());
                e.gc.setLineStyle(SWT.LINE_SOLID);

                e.gc.drawString(sunrise, 5, 0);
                e.gc.drawString("Azimuth: " + decimalFormat.format(azimuthRise),
                        5, charHeight);
                e.gc.drawString(sunset, width - charWidth * 16, 0);
                e.gc.drawString("Azimuth: " + decimalFormat.format(azimuthSet),
                        width - charWidth * 16, charHeight);

                // Center the Length of day string
                int fontMid = e.gc.getFontMetrics().getAverageCharWidth()
                        * lengthOfDay.length();
                e.gc.drawString(lengthOfDay,
                        circleX + circleSize / 2 - fontMid / 2,
                        circleY + circleSize + charHeight);
                e.gc.setLineWidth(2);
                // N-S line.
                e.gc.drawString("N",
                        circleX + circleSize / 2 - e.gc.getCharWidth('N') / 2,
                        circleY - charHeight);
                e.gc.drawLine(circleX + circleSize / 2, circleY,
                        circleX + circleSize / 2, circleY + circleSize);
                e.gc.drawString("S",
                        circleX + circleSize / 2 - e.gc.getCharWidth('S') / 2,
                        circleY + circleSize);
                // W-E line.
                e.gc.drawString("W", circleX - e.gc.getCharWidth('W'),
                        circleY + circleSize / 2 - charHeight / 2);
                e.gc.drawLine(circleX, circleY + circleSize / 2,
                        circleX + circleSize, circleY + circleSize / 2);
                e.gc.drawString("E",
                        circleX + circleSize + e.gc.getCharWidth('E') / 2,
                        circleY + circleSize / 2 - charHeight / 2);
            }
        });
        return canvas;
    }

    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        if (title != null) {
            shell.setText(title);
        }
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        // override, so can add calculate as default button.
        Button calculateButton = createButton(parent, CALCULATE_ID, "Calculate",
                false);
        calculateButton.addListener(SWT.MouseUp, new Listener() {

            @Override
            public void handleEvent(Event event) {
                calculateSunriseSunset();
            }

        });
    }
}
