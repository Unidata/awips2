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
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
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
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.awipstools.common.SunriseSunsetCalculator;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * SunriseToolDialog.
 * 
 * Tool for viewing the sunrise/sunset time and associated Azimuth's for a
 * specified latitude, longitude, day, month and year.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ----------	----------	-----------	--------------------------
 * 08/15/07     426         Eric Babin    Initial Creation.
 * 23Oct2007    499         Eric Babin  Added error check for latitude between -90 &amp; 90.
 * 03Dec2007    461         bphillip    Modified to use VizTime
 *  10-21-09    #1711       bsteffen    Modified to use datamanager for home location
 * 25May2010    5603        bkowal      Added additional timezones based on the timezones
 *                                      that were present in AWIPSI.
 * 11Jul2012    875         rferrel     Now uses PointsDataManager
 * 25 Jul 2014  #3400       lvenable    Modified the shell style to remove the application modal setting.
 * 
 * </pre>
 * 
 * @author Eric Babin
 * @version 1
 */
public class SunriseToolDialog extends CaveJFACEDialog {

    public static final int CalculateID = 987;

    private static final int CIRCLE_SIZE = 150;

    private static final int CIRCLE_X = 50;

    private static final int CIRCLE_Y = 60;

    private Shell parentShell;

    private String title;

    private Composite top;

    private Label latitudeLabel;

    private Label longitudeLabel;

    private Label yearLabel;

    private Label monthLabel;

    private Label dayLabel;

    private Label zoneLabel;

    private Text latitudeText;

    private Text longitudeText;

    private Text yearText;

    private Text monthText;

    private Text dayText;

    private Combo zoneCombo;

    private Button setHomeLocButton;

    private Button calculateButton;

    private int startX = 0;

    private int startY = 0;

    private Composite sunriseFieldComposite;

    private Composite sunriseDisplayComposite;

    private Canvas sunriseCanvas;

    private String yyyyMMdd;

    private String lengthOfDay;

    private double azimuthRise;

    private double azimuthSet;

    private int yearValue = 0;

    private int monthValue = 0;

    private int dayValue = 0;

    private String sunrise;

    private String sunset;

    private String zone;

    private int startAngle;

    private int arcAngle;

    private Color bgColor;

    private SunriseSunsetCalculator suniseCalculator = new SunriseSunsetCalculator();

    private float latitudeValue;

    private float longitudeValue;

    private DecimalFormat decimalFormat = new DecimalFormat("0.00");

    // for daylight savings entries, add 1000000 (1 hour, to show the offset
    // right.

    /*
     * String zoneStrings[] = {
     * TimeZone.getTimeZone("Pacific/Guam").getDisplayName(false,
     * TimeZone.SHORT) + " (+" +
     * TimeZone.getTimeZone("Pacific/Guam").getRawOffset() / 3600000 + ")",
     * TimeZone.getTimeZone("HST").getDisplayName(false, TimeZone.SHORT) + " ("
     * + TimeZone.getTimeZone("HST").getRawOffset() / 3600000 + ")",
     * TimeZone.getTimeZone("AST").getDisplayName(false, TimeZone.SHORT) + " ("
     * + TimeZone.getTimeZone("AST").getRawOffset() / 3600000 + ")",
     * TimeZone.getTimeZone("AST").getDisplayName(true, TimeZone.SHORT) + " (" +
     * (TimeZone.getTimeZone("AST").getRawOffset() + 100000) / 3600000 + ")",
     * TimeZone.getTimeZone("PST").getDisplayName(false, TimeZone.SHORT) + " ("
     * + TimeZone.getTimeZone("PST").getRawOffset() / 3600000 + ")",
     * TimeZone.getTimeZone("PST").getDisplayName(true, TimeZone.SHORT) + " (" +
     * (TimeZone.getTimeZone("PST").getRawOffset() + 100000) / 3600000 + ")",
     * TimeZone.getTimeZone("MST").getDisplayName(false, TimeZone.SHORT) + " ("
     * + TimeZone.getTimeZone("MST").getRawOffset() / 3600000 + ")",
     * TimeZone.getTimeZone("MST").getDisplayName(true, TimeZone.SHORT) + " (" +
     * (TimeZone.getTimeZone("MST").getRawOffset() + 100000) / 3600000 + ")",
     * TimeZone.getTimeZone("CST").getDisplayName(false, TimeZone.SHORT) + " ("
     * + TimeZone.getTimeZone("CST").getRawOffset() / 3600000 + ")",
     * TimeZone.getTimeZone("CST").getDisplayName(true, TimeZone.SHORT) + " (" +
     * (TimeZone.getTimeZone("CST").getRawOffset() + 100000) / 3600000 + ")",
     * TimeZone.getTimeZone("EST").getDisplayName(false, TimeZone.SHORT) + " ("
     * + TimeZone.getTimeZone("EST").getRawOffset() / 3600000 + ")",
     * TimeZone.getTimeZone("EST").getDisplayName(true, TimeZone.SHORT) + " (" +
     * (TimeZone.getTimeZone("EST").getRawOffset() + 100000) / 3600000 + ")",
     * TimeZone.getTimeZone("SystemV/AST4").getDisplayName(false,
     * TimeZone.SHORT) + " (" +
     * TimeZone.getTimeZone("SystemV/AST4").getRawOffset() / 3600000 + ")",
     * TimeZone.getTimeZone("GMT").getDisplayName(false, TimeZone.SHORT) +
     * " (0)" };
     */
    final String timeZoneIDs[] = { "Pacific/Guam", "HST", "AST", "AST", "PST",
            "PST", "MST", "MST", "CST", "CST", "EST", "EST", "SystemV/AST4",
            "GMT" };

    final boolean daylightIndicators[] = { false, false, false, true, false,
            true, false, true, false, true, false, true, false, false };

    private LinkedHashMap<String, String> timezonesMap;

    public SunriseToolDialog(Shell parShell, String dialogTitle)
            throws VizException {
        super(parShell);

        // Remove Application Modal from the JFace shell style.
        setShellStyle(getShellStyle() & ~SWT.APPLICATION_MODAL);

        parentShell = parShell;
        this.title = dialogTitle;
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddzZ");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        yyyyMMdd = sdf.format(SimulatedTime.getSystemTime().getTime());

        Coordinate center = PointsDataManager.getInstance().getWfoCenter();
        latitudeValue = (float) center.y;
        longitudeValue = (float) center.x;

        this.setupTimezones();
        zone = localTimeZoneString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
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

    private void setupTimezones() {
        TimeZone timeZone = null;
        String displayName = null;
        String displayText = null;
        boolean dlIndicator = false;

        int additionalOffset = -1;

        timezonesMap = new LinkedHashMap<String, String>();
        for (int i = 0; i < timeZoneIDs.length; i++) {
            dlIndicator = daylightIndicators[i];

            timeZone = TimeZone.getTimeZone(timeZoneIDs[i]);

            additionalOffset = 0;
            if (dlIndicator) {
                additionalOffset = 1000000;
            }

            displayName = timeZone.getDisplayName(dlIndicator, TimeZone.SHORT);
            displayText = displayName + "("
                    + (timeZone.getRawOffset() + additionalOffset) / 3600000
                    + ")";

            timezonesMap.put(displayText, displayName);
        }
    }

    /**
     * Method for calling calculate on sunrise calculator.
     */
    private void calculateSunriseSunset() {

        suniseCalculator.calculate(longitudeValue, latitudeValue, zone,
                yearValue, monthValue, dayValue);

        azimuthRise = suniseCalculator.getSunriseAzimuth();
        azimuthSet = suniseCalculator.getSunsetAzimuth();
        sunrise = suniseCalculator.getSunRiseTime();
        sunset = suniseCalculator.getSunSetTime();
        lengthOfDay = suniseCalculator.getDayLength();

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
            startX += 3;
            data.top = new FormAttachment(0, startY);
            data.left = new FormAttachment(0, 0);
        } else {
            data.top = new FormAttachment(0, startY);
            data.left = new FormAttachment(0, 80);
            startY += 34;
        }
        return data;
    }

    /**
     * Method for creating sunrise display.
     */
    private void createSunriseDisplay() {
        startX = 0;
        startY = 0;

        sunriseDisplayComposite = new Composite(top, SWT.NONE);
        sunriseDisplayComposite.setLayout(new FormLayout());

        GridData gridData = new GridData(GridData.FILL_BOTH);
        sunriseDisplayComposite.setLayoutData(gridData);

        createFormLayoutLoc(false);
        FormData mDat = createFormLayoutLoc(true);

        mDat.height = 260;
        mDat.width = 250;
        mDat.top = new FormAttachment(0, 0);
        sunriseCanvas = createSunriseChart(sunriseDisplayComposite,
                mDat.height, mDat.width);
        sunriseCanvas.setLayoutData(mDat);
    }

    /**
     * Method for creating the month, day, ect labels and fields.
     */
    private void createToolControls() {
        String[] zoneStrings = null;
        Iterator<String> tzIterator = null;

        sunriseFieldComposite = new Composite(top, SWT.NONE);
        sunriseFieldComposite.setLayout(new FormLayout());
        sunriseFieldComposite.setSize(80, 360);

        GridData gridData = new GridData(GridData.FILL_BOTH);
        sunriseFieldComposite.setLayoutData(gridData);

        latitudeLabel = new Label(sunriseFieldComposite, SWT.NONE);
        latitudeLabel.setText("Latitude:");
        latitudeLabel.setLayoutData(createFormLayoutLoc(true));

        latitudeText = new Text(sunriseFieldComposite, SWT.SINGLE | SWT.RIGHT
                | SWT.BORDER);
        FormData data = createFormLayoutLoc(false);
        data.width = 52;
        latitudeText.setLayoutData(data);
        latitudeText.setText("" + latitudeValue);
        latitudeText.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                String s = latitudeText.getText();

                try {
                    latitudeValue = Float.parseFloat(s);
                    if (latitudeValue < -90 || latitudeValue > 90) {
                        MessageDialog
                                .openError(parentShell,
                                        "Invalid Number Specified",
                                        "Latitude must be between -90 and 90.  Please correct.");

                        latitudeText.setText("0.0");
                        latitudeText.setFocus();
                    }
                } catch (NumberFormatException nfe) {
                    if (s.charAt(0) != '-') {
                        MessageDialog
                                .openError(parentShell,
                                        "Invalid Number Specified",
                                        "The input for the Latitude not a number.  Please correct.");
                        latitudeText.setFocus();
                    }
                }
            }
        });

        longitudeLabel = new Label(sunriseFieldComposite, SWT.NONE);
        longitudeLabel.setText("Longitude:");
        longitudeLabel.setLayoutData(createFormLayoutLoc(true));

        longitudeText = new Text(sunriseFieldComposite, SWT.SINGLE | SWT.RIGHT
                | SWT.BORDER);
        data = createFormLayoutLoc(false);
        data.width = 52;
        longitudeText.setLayoutData(data);
        longitudeText.setText("" + longitudeValue);
        longitudeText.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                String s = longitudeText.getText();

                try {
                    longitudeValue = Float.parseFloat(s);
                } catch (NumberFormatException nfe) {
                    if (s.charAt(0) != '-') {
                        MessageDialog
                                .openError(parentShell,
                                        "Invalid Number Specified",
                                        "The input for the Longitude not a number.  Please correct.");
                        longitudeText.setFocus();
                    }
                }
            }
        });

        setHomeLocButton = new Button(sunriseFieldComposite, SWT.PUSH);
        setHomeLocButton.setText("Set at Home location");
        setHomeLocButton.setLayoutData(createFormLayoutLoc(true));
        setHomeLocButton.addListener(SWT.MouseUp, new Listener() {

            public void handleEvent(Event event) {
                Coordinate home = PointsDataManager.getInstance().getHome();
                longitudeValue = (float) home.x;
                latitudeValue = (float) home.y;
                longitudeText.setText(String.valueOf(longitudeValue));
                latitudeText.setText(String.valueOf(latitudeValue));
            }

        });
        createFormLayoutLoc(false);

        data = createFormLayoutLoc(true);
        data.top = new FormAttachment(0, startY += 10);

        yearLabel = new Label(sunriseFieldComposite, SWT.NONE);
        yearLabel.setText("Year:");
        yearLabel.setLayoutData(data);

        yearText = new Text(sunriseFieldComposite, SWT.SINGLE | SWT.BORDER);
        data = createFormLayoutLoc(false);
        data.left = new FormAttachment(0, 70);
        yearText.setLayoutData(data);
        yearText.setText(yyyyMMdd.substring(0, 4));
        yearValue = Integer.valueOf(yyyyMMdd.substring(0, 4));

        yearText.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                String s = yearText.getText();

                try {
                    yearValue = Integer.parseInt(s);
                } catch (NumberFormatException nfe) {
                    MessageDialog
                            .openError(parentShell, "Invalid Number Specified",
                                    "The input for the Year is not valid.  Please correct.");
                    yearText.setFocus();
                }
            }
        });
        monthLabel = new Label(sunriseFieldComposite, SWT.NONE);
        monthLabel.setText("Month:");
        monthLabel.setLayoutData(createFormLayoutLoc(true));

        monthText = new Text(sunriseFieldComposite, SWT.SINGLE | SWT.BORDER);
        monthText.setText(yyyyMMdd.substring(4, 6));
        monthValue = Integer.valueOf(yyyyMMdd.substring(4, 6));
        data = createFormLayoutLoc(false);
        data.left = new FormAttachment(0, 70);

        monthText.setLayoutData(data);

        monthText.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                String s = monthText.getText();

                try {
                    monthValue = Integer.parseInt(s);
                } catch (NumberFormatException nfe) {
                    MessageDialog
                            .openError(parentShell, "Invalid Number Specified",
                                    "The input for the Month is not valid.  Please correct.");
                    monthText.setFocus();
                }
            }
        });
        dayLabel = new Label(sunriseFieldComposite, SWT.NONE);
        dayLabel.setText("Day:");
        dayLabel.setLayoutData(createFormLayoutLoc(true));

        dayText = new Text(sunriseFieldComposite, SWT.SINGLE | SWT.BORDER);
        dayText.setText(yyyyMMdd.substring(6, 8));
        dayValue = Integer.valueOf(yyyyMMdd.substring(6, 8));
        data = createFormLayoutLoc(false);
        data.left = new FormAttachment(0, 70);

        dayText.setLayoutData(data);

        dayText.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                String s = dayText.getText();

                try {
                    dayValue = Integer.parseInt(s);
                } catch (NumberFormatException nfe) {
                    MessageDialog
                            .openError(parentShell, "Invalid Number Specified",
                                    "The input for the Day not valid.  Please correct.");
                    dayText.setFocus();
                }
            }
        });
        zoneLabel = new Label(sunriseFieldComposite, SWT.NONE);
        zoneLabel.setText("Zone:");
        zoneLabel.setLayoutData(createFormLayoutLoc(true));

        zoneStrings = new String[timezonesMap.size()];
        tzIterator = timezonesMap.keySet().iterator();
        String key = null;
        for (int i = 0; i < zoneStrings.length; i++) {
            zoneStrings[i] = tzIterator.next();
            key = tzIterator.toString();
        }

        zoneCombo = new Combo(sunriseFieldComposite, SWT.DROP_DOWN);
        zoneCombo.setItems(zoneStrings);
        data = createFormLayoutLoc(false);
        data.left = new FormAttachment(0, 60);
        zoneCombo.setLayoutData(data);

        zoneCombo.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            public void widgetSelected(SelectionEvent e) {
                if (zoneCombo.getSelectionIndex() != -1) {
                    zone = timezonesMap.get(zoneCombo.getText());
                } else {
                    zone = "GMT";
                }
            }

        });
        /* Set Display Text To First Element In Combo Box */
        zoneCombo.setText(zoneStrings[0]);
        setCurrentTimeZone(zoneStrings);
        ((FormData) zoneCombo.getLayoutData()).width = 90;

        startX = 0;
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
            for (int i = 0; i < zoneStrings.length; i++) {
                if (zoneStrings[i].contains(lTimeZoneString + "(")) {
                    zoneCombo.setText(zoneStrings[i]);
                }
            }
        }
    }

    /**
     * @return
     */
    private String localTimeZoneString() {
        String localTimeZone = new SimpleDateFormat("z").format(SimulatedTime
                .getSystemTime().getTime());
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
    private Canvas createSunriseChart(Composite comp, int height,
            int canvasWidth) {

        final Canvas canvas = new Canvas(comp, SWT.NONE | SWT.BORDER);
        final int width = canvasWidth;

        canvas.addPaintListener(new PaintListener() {

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

                e.gc.setBackground(Display.getCurrent().getSystemColor(
                        SWT.COLOR_YELLOW));
                e.gc.fillArc(CIRCLE_X, CIRCLE_Y, CIRCLE_SIZE, CIRCLE_SIZE,
                        startAngle, arcAngle);
                e.gc.setBackground(Display.getCurrent().getSystemColor(
                        SWT.COLOR_BLACK));

                e.gc.drawArc(CIRCLE_X, CIRCLE_Y, CIRCLE_SIZE, CIRCLE_SIZE,
                        startAngle, arcAngle);

                e.gc.setBackground(bgColor);
                e.gc.setBackground(e.gc.getBackground());
                e.gc.setLineStyle(SWT.LINE_SOLID);

                e.gc.drawString(sunrise, 5, 0);
                e.gc.drawString(
                        "Azimuth: " + decimalFormat.format(azimuthRise), 5, 20);
                e.gc.drawString(sunset, width / 2 + 5, 0);
                e.gc.drawString("Azimuth: " + decimalFormat.format(azimuthSet),
                        width / 2 + 5, 20);

                // Center the Length of day string
                int fontMid = e.gc.getFontMetrics().getAverageCharWidth()
                        * lengthOfDay.length();
                e.gc.drawString(lengthOfDay, CIRCLE_X + CIRCLE_SIZE / 2
                        - fontMid / 2, CIRCLE_Y + CIRCLE_SIZE + 20);
                // e.gc.setFont(originalFont);
                e.gc.setLineWidth(2);
                // N-S line.
                e.gc.drawString(
                        "N",
                        CIRCLE_X + CIRCLE_SIZE / 2 - e.gc.getCharWidth('N') / 2,
                        CIRCLE_Y - e.gc.getFontMetrics().getHeight());
                e.gc.drawLine(CIRCLE_X + CIRCLE_SIZE / 2, CIRCLE_Y, CIRCLE_X
                        + CIRCLE_SIZE / 2, CIRCLE_Y + CIRCLE_SIZE);
                e.gc.drawString(
                        "S",
                        CIRCLE_X + CIRCLE_SIZE / 2 - e.gc.getCharWidth('S') / 2,
                        CIRCLE_Y + CIRCLE_SIZE);
                // W-E line.
                e.gc.drawString("W", CIRCLE_X - e.gc.getCharWidth('W'),
                        CIRCLE_Y + CIRCLE_SIZE / 2
                                - e.gc.getFontMetrics().getHeight() / 2);
                e.gc.drawLine(CIRCLE_X, CIRCLE_Y + CIRCLE_SIZE / 2, CIRCLE_X
                        + CIRCLE_SIZE, CIRCLE_Y + CIRCLE_SIZE / 2);
                e.gc.drawString("E",
                        CIRCLE_X + CIRCLE_SIZE + e.gc.getCharWidth('E') / 2,
                        CIRCLE_Y + CIRCLE_SIZE / 2
                                - e.gc.getFontMetrics().getHeight() / 2);
            }
        });
        return canvas;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
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
        calculateButton = createButton(parent, CalculateID, "Calculate", false);
        calculateButton.addListener(SWT.MouseUp, new Listener() {

            public void handleEvent(Event event) {
                calculateSunriseSunset();
            }

        });
    }
}
