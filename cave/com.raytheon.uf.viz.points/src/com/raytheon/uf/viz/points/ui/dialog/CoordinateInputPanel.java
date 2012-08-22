package com.raytheon.uf.viz.points.ui.dialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.points.PointUtilities;
import com.raytheon.uf.viz.points.data.Point;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This panel creates the layout and performs the logic for latitude and
 * longitude information.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  October-2010              epolster    Initial Creation.
 *  July 21 2012 875         rferrel     Made sizing dynamic
 * 
 * </pre>
 * 
 * @author epolster
 * @version 1
 * 
 * 
 */

public class CoordinateInputPanel {

    static private String DEGREES_MINUTES_SECONDS = "   Degrees : Minutes : Seconds";

    static private String DEGREES_MINUTES = "   Degrees : Minutes";

    static private String DEGREES_ONLY = "   Degrees Only";

    private Text latDegreesText;

    private Text latMinutesText;

    private Text latSecondsText;

    private Text lonDegreesText;

    private Text lonMinutesText;

    private Text lonSecondsText;

    private Button cardinalNorthRadioButton;

    private Button cardinalSouthRadioButton;

    private Button cardinalEastRadioButton;

    private Button cardinalWestRadioButton;

    Combo resolutionCombo;

    Group rootContainer;

    PointEditDialog parent;

    private CoordinateInputOptions currInputMode = CoordinateInputOptions.DEGREES_MINUTES_SECONDS;

    public CoordinateInputPanel(PointEditDialog p, Group container) {
        rootContainer = container;
        parent = p;
        init();
    }

    private void init() {
        GridData gd = null;
        rootContainer.setLayout(new GridLayout(5, false));
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.minimumWidth = 340;
        rootContainer.setLayoutData(gd);

        Label lblLat = new Label(rootContainer, SWT.NONE);
        lblLat.setFont(FontManager.getFont("Tahoma",
                PointEditDialog.PREFERRED_FONT_SIZE_MEDIUM, SWT.BOLD));
        lblLat.setAlignment(SWT.CENTER);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
        lblLat.setLayoutData(gd);
        lblLat.setText("Lat:");

        latDegreesText = new Text(rootContainer, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        gd.minimumWidth = 64;
        latDegreesText.setLayoutData(gd);

        latMinutesText = new Text(rootContainer, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        gd.minimumWidth = 64;
        latMinutesText.setLayoutData(gd);

        latSecondsText = new Text(rootContainer, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        gd.minimumWidth = 64;
        latSecondsText.setLayoutData(gd);

        latDegreesText.addFocusListener(new FocusListener() {

            @Override
            public void focusGained(FocusEvent e) {
                latDegreesText.selectAll();
            }

            @Override
            public void focusLost(FocusEvent e) {
                String degreesStr = latDegreesText.getText();
                if ((degreesStr == null) || (degreesStr.length() == 0))
                    return;
                float degrees = Float.parseFloat(degreesStr);
                if (degrees < 0.0) {
                    cardinalSouthRadioButton.setSelection(true);
                    cardinalNorthRadioButton.setSelection(false);
                    degrees = Math.abs(degrees);
                    degreesStr = Float.toString(degrees);
                    latDegreesText.setText(degreesStr);
                }
            }

        });

        Composite latRadioGroupComposite = new Composite(rootContainer,
                SWT.NONE);
        latRadioGroupComposite.setLayout(new GridLayout(2, true));

        cardinalNorthRadioButton = new Button(latRadioGroupComposite, SWT.RADIO);
        cardinalNorthRadioButton.setFont(FontManager.getFont("Tahoma",
                PointEditDialog.PREFERRED_FONT_SIZE_SMALL, SWT.BOLD));
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        gd.minimumWidth = 35;
        cardinalNorthRadioButton.setLayoutData(gd);
        cardinalNorthRadioButton.setText("N");

        cardinalSouthRadioButton = new Button(latRadioGroupComposite, SWT.RADIO);
        cardinalSouthRadioButton.setFont(FontManager.getFont("Tahoma",
                PointEditDialog.PREFERRED_FONT_SIZE_SMALL, SWT.BOLD));
        cardinalSouthRadioButton.setText("S");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        gd.minimumWidth = 35;
        cardinalSouthRadioButton.setLayoutData(gd);

        new Label(rootContainer, SWT.NONE);

        Label lblDegrees = new Label(rootContainer, SWT.NONE);
        lblDegrees.setFont(FontManager.getFont("Tahoma",
                PointEditDialog.PREFERRED_FONT_SIZE_SMALL, SWT.NORMAL));
        lblDegrees.setAlignment(SWT.CENTER);
        gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        gd.horizontalSpan = 1;
        lblDegrees.setLayoutData(gd);
        lblDegrees.setText("degrees");

        Label lblMinutes = new Label(rootContainer, SWT.NONE);
        lblMinutes.setFont(FontManager.getFont("Tahoma",
                PointEditDialog.PREFERRED_FONT_SIZE_SMALL, SWT.NORMAL));
        lblMinutes.setAlignment(SWT.CENTER);
        gd = new GridData(SWT.CENTER, SWT.CENTER, false, false);
        lblMinutes.setLayoutData(gd);
        lblMinutes.setText("minutes");

        Label lblSeconds = new Label(rootContainer, SWT.NONE);
        lblSeconds.setFont(FontManager.getFont("Tahoma",
                PointEditDialog.PREFERRED_FONT_SIZE_SMALL, SWT.NORMAL));
        lblSeconds.setAlignment(SWT.CENTER);
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        lblSeconds.setLayoutData(gd);
        lblSeconds.setText("seconds");

        new Label(rootContainer, SWT.NONE);

        Label lblLon = new Label(rootContainer, SWT.NONE);
        lblLon.setFont(FontManager.getFont("Tahoma",
                PointEditDialog.PREFERRED_FONT_SIZE_MEDIUM, SWT.BOLD));
        lblLon.setAlignment(SWT.CENTER);
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        lblLon.setLayoutData(gd);
        lblLon.setText("Lon:");

        lonDegreesText = new Text(rootContainer, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        gd.minimumWidth = 64;
        lonDegreesText.setLayoutData(gd);

        lonDegreesText.addFocusListener(new FocusListener() {

            @Override
            public void focusGained(FocusEvent e) {
                lonDegreesText.selectAll();
            }

            @Override
            public void focusLost(FocusEvent e) {
                String degreesStr = lonDegreesText.getText();
                if ((degreesStr == null) || (degreesStr.length() == 0))
                    return;
                float degrees = Float.parseFloat(degreesStr);
                if (degrees < 0.0) {
                    cardinalWestRadioButton.setSelection(true);
                    cardinalEastRadioButton.setSelection(false);
                    degrees = Math.abs(degrees);
                    degreesStr = Float.toString(degrees);
                    lonDegreesText.setText(degreesStr);
                }
            }
        });

        lonMinutesText = new Text(rootContainer, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        gd.minimumWidth = 64;
        lonMinutesText.setLayoutData(gd);

        lonSecondsText = new Text(rootContainer, SWT.BORDER | SWT.SINGLE);
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        gd.minimumWidth = 64;
        lonSecondsText.setLayoutData(gd);

        Composite lonRadioGroupComposite = new Composite(rootContainer,
                SWT.NONE);
        lonRadioGroupComposite.setLayout(new GridLayout(2, true));

        cardinalWestRadioButton = new Button(lonRadioGroupComposite, SWT.RADIO);
        cardinalWestRadioButton.setFont(FontManager.getFont("Tahoma",
                PointEditDialog.PREFERRED_FONT_SIZE_SMALL, SWT.BOLD));
        cardinalWestRadioButton.setText("W");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        gd.minimumWidth = 35;
        cardinalWestRadioButton.setLayoutData(gd);

        cardinalEastRadioButton = new Button(lonRadioGroupComposite, SWT.RADIO);
        cardinalEastRadioButton.setFont(FontManager.getFont("Tahoma",
                PointEditDialog.PREFERRED_FONT_SIZE_SMALL, SWT.BOLD));
        cardinalEastRadioButton.setText("E");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, false);
        cardinalEastRadioButton.setLayoutData(gd);
        gd.minimumWidth = 35;

        cardinalNorthRadioButton.setSelection(true);
        cardinalWestRadioButton.setSelection(true);

        new Label(rootContainer, SWT.NONE);
        resolutionCombo = new Combo(rootContainer, SWT.SINGLE | SWT.CENTER
                | SWT.READ_ONLY);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        resolutionCombo.setLayoutData(gd);
        resolutionCombo.setFont(FontManager.getFont("Tahoma",
                PointEditDialog.PREFERRED_FONT_SIZE_MEDIUM, SWT.NORMAL));
        resolutionCombo.add(DEGREES_MINUTES_SECONDS);
        resolutionCombo.add(DEGREES_MINUTES);
        resolutionCombo.add(DEGREES_ONLY);
        resolutionCombo.select(0);

        resolutionCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleInputConstraintEvent();
            }
        });
        new Label(rootContainer, SWT.NONE);
    }

    protected void setLatDegrees(double d) {
        String cs = String.format("%.5f", d);
        latDegreesText.setText(cs);
    }

    protected void setLatDegrees(int i) {
        latDegreesText.setText(Integer.toString(i));
    }

    protected void setLatMinutes(double d) {
        String cs = String.format("%.4f", d);
        latMinutesText.setText(cs);
    }

    protected void setLatMinutes(int i) {
        latMinutesText.setText(Integer.toString(i));
    }

    protected void setLatMinutes(String s) {
        latMinutesText.setText(s);
    }

    protected void setLatSeconds(double d) {
        String cs = String.format("%.3f", d);
        latSecondsText.setText(cs);
    }

    protected void setLatSeconds(String s) {
        latSecondsText.setText(s);
    }

    protected void setLonDegrees(double d) {
        String cs = String.format("%.5f", d);
        lonDegreesText.setText(cs);
    }

    protected void setLonMinutes(double d) {
        String cs = String.format("%.4f", d);
        lonMinutesText.setText(cs);
    }

    protected void setLonMinutes(String s) {
        lonMinutesText.setText(s);
    }

    protected void setLonDegrees(int d) {
        lonDegreesText.setText(Integer.toString(d));
    }

    protected void setLonMinutes(int d) {
        lonMinutesText.setText(Integer.toString(d));
    }

    protected void setLonSeconds(double d) {
        String cs = String.format("%.3f", d);
        lonSecondsText.setText(cs);
    }

    protected void setLonSeconds(String s) {
        lonSecondsText.setText(s);
    }

    protected void setWest(boolean f) {
        cardinalWestRadioButton.setSelection(f);
    }

    protected void setNorth(boolean f) {
        cardinalNorthRadioButton.setSelection(f);
    }

    protected boolean isWest() {
        return cardinalWestRadioButton.getSelection();
    }

    protected boolean isNorth() {
        return cardinalNorthRadioButton.getSelection();
    }

    protected double getLatDegrees() {
        double deg = 0.0;
        try {
            deg = Double.parseDouble(latDegreesText.getText());
        } catch (Exception e) {
            deg = 0.0;
        }
        return deg;
    }

    protected double getLatMinutes() {
        double min = 0.0;
        try {
            min = Double.parseDouble(latMinutesText.getText());
        } catch (Exception e) {
            min = 0.0;
        }
        return min;
    }

    protected double getLatSeconds() {
        double sec = 0.0;
        try {
            sec = Double.parseDouble(latSecondsText.getText());
        } catch (Exception e) {
            sec = 0.0;
        }
        return sec;
    }

    protected double getLonDegrees() {
        double deg = 0.0;
        try {
            deg = Double.parseDouble(lonDegreesText.getText());
        } catch (Exception e) {
            deg = 0.0;
        }
        return deg;
    }

    protected double getLonMinutes() {
        double min = 0.0;
        try {
            min = Double.parseDouble(lonMinutesText.getText());
        } catch (Exception e) {
            min = 0.0;
        }
        return min;
    }

    protected double getLonSeconds() {
        double sec = 0.0;
        try {
            sec = Double.parseDouble(lonSecondsText.getText());
        } catch (Exception e) {
            sec = 0.0;
        }
        return sec;
    }

    public double getLatAsDegreesOnly() {
        double degrees = 0.0;

        CoordinateInputOptions c = getInputMode();

        if (c == CoordinateInputOptions.DEGREES_MINUTES_SECONDS) {
            degrees = getLatDegrees()
                    + (getLatMinutes() / PointUtilities.MINUTE_PER_DEGREE)
                    + (getLatSeconds() / (PointUtilities.SECOND_PER_MINUTE * PointUtilities.MINUTE_PER_DEGREE));
        } else if (c == CoordinateInputOptions.DEGREES_MINUTES) {
            degrees = getLatDegrees()
                    + (getLatMinutes() / PointUtilities.MINUTE_PER_DEGREE);
        } else if (c == CoordinateInputOptions.DEGREES_ONLY) {
            degrees = getLatDegrees();
        }

        if (!isNorth()) {
            degrees *= -1;
        }

        return degrees;
    }

    public double getLonAsDegreesOnly() {
        double degrees = 0.0;

        CoordinateInputOptions c = getInputMode();

        if (c == CoordinateInputOptions.DEGREES_MINUTES_SECONDS) {
            degrees = getLonDegrees()
                    + (getLonMinutes() / PointUtilities.MINUTE_PER_DEGREE)
                    + (getLonSeconds() / (PointUtilities.MINUTE_PER_DEGREE * PointUtilities.SECOND_PER_MINUTE));
        } else if (c == CoordinateInputOptions.DEGREES_MINUTES) {
            degrees = getLonDegrees()
                    + (getLonMinutes() / PointUtilities.MINUTE_PER_DEGREE);
        } else if (c == CoordinateInputOptions.DEGREES_ONLY) {
            degrees = getLonDegrees();
        }

        if (isWest()) {
            degrees *= -1;
        }

        return degrees;
    }

    protected void normalizeLatToDegreesOnly(double origLat, boolean isRaw) {

        if (isRaw) {
            if (origLat < 0) {
                setNorth(false);
            } else {
                setNorth(true);
            }
        }
        double absLat = Math.abs(origLat);

        setLatDegrees(absLat);
        setLatMinutes(PointEditDialog.NOT_ENABLED);
        setLatSeconds(PointEditDialog.NOT_ENABLED);
    }

    protected void normalizeLatToDegreesMinutes(double origLat, boolean isRaw) {

        if (isRaw) {
            if (origLat < 0) {
                setNorth(false);
            } else {
                setNorth(true);
            }
        }

        double deg = 0.0;
        double min = 0.0;
        double absLat = Math.abs(origLat);

        if ((absLat != 0) && (absLat != PointUtilities.MAX_LATITUDE)) {
            deg = Math.floor(absLat);
            min = ((absLat - deg) * PointUtilities.MINUTE_PER_DEGREE);
        } else {
            deg = absLat;
        }

        int iDeg = (int) deg;
        setLatDegrees(iDeg);
        setLatMinutes(min);
        setLatSeconds(PointEditDialog.NOT_ENABLED);
    }

    protected void normalizeLatToDegreesMinutesSeconds(double origLat,
            boolean isRaw) {

        if (isRaw) {
            if (origLat < 0) {
                setNorth(false);
            } else {
                setNorth(true);
            }
        }

        double deg = 0.0;
        double min = 0.0;
        double sec = 0.0;
        double absLat = Math.abs(origLat);

        if ((absLat != 0) && (absLat != PointUtilities.MAX_LATITUDE)) {
            deg = Math.floor(absLat);
            min = Math.floor((absLat - deg) * PointUtilities.MINUTE_PER_DEGREE);
            sec = (absLat - deg - (min / PointUtilities.MINUTE_PER_DEGREE))
                    * (PointUtilities.MINUTE_PER_DEGREE * PointUtilities.SECOND_PER_MINUTE);
        } else {
            deg = absLat;
        }

        int iDeg = (int) deg;
        int iMin = (int) min;
        setLatDegrees(iDeg);
        setLatMinutes(iMin);
        setLatSeconds(sec);
    }

    protected void normalizeLonToDegreesOnly(double origLon, boolean isRaw) {

        if (isRaw) {
            if (origLon < 0) {
                setWest(true);
            } else {
                setWest(false);
            }
        }

        double absLon = Math.abs(origLon);
        setLonDegrees(absLon);
        setLonMinutes(PointEditDialog.NOT_ENABLED);
        setLonSeconds(PointEditDialog.NOT_ENABLED);
    }

    protected void normalizeLonToDegreesMinutes(double origLon, boolean isRaw) {

        if (isRaw) {
            if (origLon < 0) {
                setWest(true);
            } else {
                setWest(false);
            }
        }

        double deg = 0.0;
        double min = 0.0;
        double absLon = Math.abs(origLon);

        if ((absLon != 0) && (absLon != PointUtilities.MAX_LONGITUDE)) {
            deg = Math.floor(absLon);
            min = (absLon - deg) * PointUtilities.MINUTE_PER_DEGREE;
        } else {
            deg = absLon;
        }

        int iDeg = (int) deg;
        setLonDegrees(iDeg);
        setLonMinutes(min);
        setLonSeconds(PointEditDialog.NOT_ENABLED);

    }

    protected void normalizeLonToDegreesMinutesSeconds(double origLon,
            boolean isRaw) {

        if (isRaw) {
            if (origLon < 0) {
                setWest(true);
            } else {
                setWest(false);
            }
        }

        double deg = 0.0;
        double min = 0.0;
        double sec = 0.0;
        double absLon = Math.abs(origLon);

        if ((absLon != 0) && (absLon != PointUtilities.MAX_LONGITUDE)) {
            deg = Math.floor(absLon);
            min = Math.floor((absLon - deg) * PointUtilities.MINUTE_PER_DEGREE);
            sec = (absLon - deg - (min / PointUtilities.MINUTE_PER_DEGREE))
                    * (PointUtilities.MINUTE_PER_DEGREE * PointUtilities.SECOND_PER_MINUTE);
        } else {
            deg = absLon;
        }

        int iDeg = (int) deg;
        int iMin = (int) min;
        setLonDegrees(iDeg);
        setLonMinutes(iMin);
        setLonSeconds(sec);

    }

    protected CoordinateInputOptions getInputMode() {
        return currInputMode;
    }

    private void handleInputConstraintEvent() {

        int index = resolutionCombo.getSelectionIndex();
        if (index == CoordinateInputOptions.DEGREES_MINUTES_SECONDS
                .getOrdinal()) {
            currInputMode = CoordinateInputOptions.DEGREES_MINUTES_SECONDS;
            recalculateCoordinateFields();
            latDegreesText.setEnabled(true);
            latMinutesText.setEnabled(true);
            latSecondsText.setEnabled(true);
            lonDegreesText.setEnabled(true);
            lonMinutesText.setEnabled(true);
            lonSecondsText.setEnabled(true);
        } else if (index == CoordinateInputOptions.DEGREES_MINUTES.getOrdinal()) {
            currInputMode = CoordinateInputOptions.DEGREES_MINUTES;
            recalculateCoordinateFields();
            latDegreesText.setEnabled(true);
            latMinutesText.setEnabled(true);
            latSecondsText.setEnabled(false);
            lonDegreesText.setEnabled(true);
            lonMinutesText.setEnabled(true);
            lonSecondsText.setEnabled(false);
        } else if (index == CoordinateInputOptions.DEGREES_ONLY.getOrdinal()) {
            currInputMode = CoordinateInputOptions.DEGREES_ONLY;
            recalculateCoordinateFields();
            latDegreesText.setEnabled(true);
            latMinutesText.setEnabled(false);
            latSecondsText.setEnabled(false);
            lonDegreesText.setEnabled(true);
            lonMinutesText.setEnabled(false);
            lonSecondsText.setEnabled(false);
        }
        if (resolutionCombo.isFocusControl()) {
            parent.resetFocus();
        }

    }

    public void recalculateCoordinateFields(Coordinate latLon) {
        recalculateCoordinateFields(latLon.y, latLon.x);
    }

    protected void recalculateCoordinateFields(Point currentPoint) {
        recalculateCoordinateFields(currentPoint.getLatitude(),
                currentPoint.getLongitude());
    }

    protected void recalculateCoordinateFields(double lat, double lon) {
        CoordinateInputOptions c = getInputMode();

        if (c == CoordinateInputOptions.DEGREES_MINUTES_SECONDS) {
            normalizeLatToDegreesMinutesSeconds(lat, true);
            normalizeLonToDegreesMinutesSeconds(lon, true);
        } else if (c == CoordinateInputOptions.DEGREES_MINUTES) {
            normalizeLatToDegreesMinutes(lat, true);
            normalizeLonToDegreesMinutes(lon, true);

        } else if (c == CoordinateInputOptions.DEGREES_ONLY) {
            normalizeLatToDegreesOnly(lat, true);
            normalizeLonToDegreesOnly(lon, true);
        }
    }

    protected void recalculateCoordinateFields() {
        double origLat = getLatDegrees() + (getLatMinutes() / 60)
                + (this.getLatSeconds() / 3600);
        double origLon = getLonDegrees() + (getLonMinutes() / 60)
                + (this.getLonSeconds() / 3600);
        if (currInputMode == CoordinateInputOptions.DEGREES_MINUTES_SECONDS) {
            normalizeLatToDegreesMinutesSeconds(origLat, false);
            normalizeLonToDegreesMinutesSeconds(origLon, false);
        } else if (currInputMode == CoordinateInputOptions.DEGREES_MINUTES) {
            normalizeLatToDegreesMinutes(origLat, false);
            normalizeLonToDegreesMinutes(origLon, false);

        } else if (currInputMode == CoordinateInputOptions.DEGREES_ONLY) {
            normalizeLatToDegreesOnly(origLat, false);
            normalizeLonToDegreesOnly(origLon, false);
        }
    }

    private enum CoordinateInputOptions {
        DEGREES_MINUTES_SECONDS(0), DEGREES_MINUTES(1), DEGREES_ONLY(2);

        private int _ordinal = 0;

        CoordinateInputOptions(int index) {
            _ordinal = index;
        }

        public int getOrdinal() {
            return _ordinal;
        }
    }
}
