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
package com.raytheon.uf.viz.grib;

import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.MercatorGridCoverage;
import com.raytheon.uf.common.gridcoverage.PolarStereoGridCoverage;

/**
 * 
 * Composite which allows the user to create or modify a {@link GridCoverage}.
 * Allows the user to choose between {@link LatLonGridCoverage},
 * {@link LambertConformalGridCoverage}, {@link MercatorGridCoverage}, and
 * {@link PolarStereoGridCoverage} and provides combo boxes and text entry for
 * all fields in each type of coverage.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 19, 2016  5572     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class DefineGridCoverageComposite extends Composite {

    /*
     * For each projection the number is the grib 2 GDS template number which is
     * provided to assist the user if they are reading the output from a grib
     * tool.
     */
    private static final String PROJECTION_LATLON = "#0: Equidistant Cylindrical";

    private static final String PROJECTION_MERC = "#10: Mercator";

    private static final String PROJECTION_POLAR = "#20: North Polar Stereographic";

    private static final String PROJECTION_LAMBERT = "#30: Lambert Conformal";

    private final Combo projectionCombo;

    private final Text nameText;

    private final Combo cornerCombo;

    private final Text descriptionText;

    private final Text nxText;

    private final Text nyText;

    /**
     * The dx label is kept around so it can display the spacing units of the
     * projection
     */
    private final Label dxLabel;

    /**
     * The dy label is kept around so it can display the spacing units of the
     * projection
     */
    private final Label dyLabel;

    private final Text dxText;

    private final Text dyText;

    private final Text la1Text;

    private final Text lo1Text;

    /**
     * The extra labels are used to display the names of the parameters that are
     * not in all projections but are needed by each specific type of
     * projection.
     */
    private Label[] extraLabels = new Label[3];

    /**
     * The extra texts allow user entry for the parameters that are not in all
     * projections but are needed by each specific type of projection.
     */
    private Text[] extraText = new Text[3];

    public DefineGridCoverageComposite(Composite parent) {
        super(parent, SWT.NONE);
        setLayout(new GridLayout(4, true));
        Label label = new Label(this, SWT.NONE);
        label.setText("GDS Template:");
        projectionCombo = new Combo(this, SWT.READ_ONLY);
        projectionCombo.add(PROJECTION_LATLON);
        projectionCombo.add(PROJECTION_MERC);
        projectionCombo.add(PROJECTION_POLAR);
        projectionCombo.add(PROJECTION_LAMBERT);


        GridData gridData = new GridData();
        gridData.horizontalSpan = 3;
        projectionCombo.setLayoutData(gridData);

        label = new Label(this, SWT.NONE);
        label.setText("Name:");
        nameText = new Text(this, SWT.BORDER | SWT.SINGLE);
        gridData = new GridData();
        gridData.horizontalSpan = 3;
        gridData.horizontalAlignment = SWT.FILL;
        nameText.setLayoutData(gridData);

        label = new Label(this, SWT.NONE);
        label.setText("Description:");
        descriptionText = new Text(this, SWT.BORDER | SWT.SINGLE);
        gridData = new GridData();
        gridData.horizontalSpan = 3;
        gridData.horizontalAlignment = SWT.FILL;
        descriptionText.setLayoutData(gridData);

        label = new Label(this, SWT.NONE);
        label.setText("First Corner:");
        cornerCombo = new Combo(this, SWT.READ_ONLY);
        gridData = new GridData();
        gridData.horizontalSpan = 3;
        cornerCombo.setLayoutData(gridData);
        for (Corner corner : Corner.values()) {
            cornerCombo.add(corner.name());
        }

        label = new Label(this, SWT.NONE);
        label.setText("nx:");
        label.setToolTipText("Number of horizontal grid points");

        nxText = new Text(this, SWT.BORDER | SWT.SINGLE);
        nxText.setToolTipText(label.getToolTipText());

        label = new Label(this, SWT.NONE);
        label.setText("ny:");
        label.setToolTipText("Number of vertical grid points");
        nyText = new Text(this, SWT.BORDER | SWT.SINGLE);
        nyText.setToolTipText(label.getToolTipText());

        dxLabel = new Label(this, SWT.NONE);
        dxLabel.setText("dx:");
        dxLabel.setToolTipText("Vertical distance between grid points");
        dxText = new Text(this, SWT.BORDER | SWT.SINGLE);
        dxText.setToolTipText(dxLabel.getToolTipText());


        dyLabel = new Label(this, SWT.NONE);
        dyLabel.setText("dy:");
        dyLabel.setToolTipText("Vertical distance between grid points");
        dyText = new Text(this, SWT.BORDER | SWT.SINGLE);
        dyText.setToolTipText(dyLabel.getToolTipText());

        label = new Label(this, SWT.NONE);
        label.setText("la1:");
        label.setToolTipText("Latitude of the first grid point");
        la1Text = new Text(this, SWT.BORDER | SWT.SINGLE);
        la1Text.setToolTipText(label.getToolTipText());

        label = new Label(this, SWT.NONE);
        label.setText("lo1:");
        label.setToolTipText("Longitude of the first grid point");
        lo1Text = new Text(this, SWT.BORDER | SWT.SINGLE);
        lo1Text.setToolTipText(label.getToolTipText());

        for (int i = 0; i < extraLabels.length; i += 1) {
            extraLabels[i] = new Label(this, SWT.NONE);
            extraLabels[i].setVisible(false);
            extraText[i] = new Text(this, SWT.BORDER | SWT.SINGLE);
            extraText[i].setVisible(false);
            gridData = new GridData();
            gridData.horizontalSpan = 3;
            extraText[i].setLayoutData(gridData);
        }

        projectionCombo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                updateExtraFields();
            }

        });

    }

    private void updateExtraFields() {
        int index = projectionCombo.getSelectionIndex();
        if (index < 0) {
            for (int i = 0; i < extraLabels.length; i += 1) {
                extraLabels[i].setVisible(false);
                extraText[i].setVisible(false);
            }
            return;
        }
        String selection = projectionCombo.getItem(index);
        if (PROJECTION_LATLON.equals(selection)) {
            for (int i = 0; i < extraLabels.length; i += 1) {
                extraLabels[i].setVisible(false);
                extraText[i].setVisible(false);
            }
            dxLabel.setText("dx (degrees):");
            dyLabel.setText("dy (degrees):");
        } else if (PROJECTION_MERC.equals(selection)) {
            for (int i = 0; i < extraLabels.length; i += 1) {
                extraLabels[i].setVisible(i < 1);
                extraText[i].setVisible(i < 1);
            }
            extraLabels[0].setText("latin:");
            extraLabels[0].setToolTipText("Latitude of Origin");
            extraText[0].setText("");
            extraText[0].setToolTipText(extraLabels[0].getToolTipText());
            dxLabel.setText("dx (km):");
            dyLabel.setText("dy (km):");
        } else if (PROJECTION_POLAR.equals(selection)) {
            for (int i = 0; i < extraLabels.length; i += 1) {
                extraLabels[i].setVisible(i < 2);
                extraText[i].setVisible(i < 2);
            }
            extraLabels[0].setText("lad:");
            extraLabels[0].setToolTipText("Latitude of Origin");
            extraText[0].setText("");
            extraText[0].setToolTipText(extraLabels[0].getToolTipText());
            extraLabels[1].setText("lov:");
            extraLabels[1].setToolTipText("Central Meridian");
            extraText[1].setText("");
            extraText[1].setToolTipText(extraLabels[1].getToolTipText());
            dxLabel.setText("dx (km):");
            dyLabel.setText("dy (km):");
        } else if (PROJECTION_LAMBERT.equals(selection)) {
            for (int i = 0; i < extraLabels.length; i += 1) {
                extraLabels[i].setVisible(i < 3);
                extraText[i].setVisible(i < 3);
            }
            extraLabels[0].setText("latin1:");
            extraLabels[0].setToolTipText("First Latitude of Origin");
            extraText[0].setText("");
            extraText[0].setToolTipText(extraLabels[0].getToolTipText());
            extraLabels[1].setText("latin2:");
            extraLabels[1].setToolTipText("Second Latitude of Origin");
            extraText[1].setText("");
            extraText[1].setToolTipText(extraLabels[1].getToolTipText());
            extraLabels[2].setText("lov:");
            extraLabels[2].setToolTipText("Central Meridian");
            extraText[2].setText("");
            extraText[2].setToolTipText(extraLabels[2].getToolTipText());
            dxLabel.setText("dx (km):");
            dyLabel.setText("dy (km):");
        }
        Control[] changed = Arrays.copyOf(extraLabels, extraLabels.length + 2);
        changed[extraLabels.length] = dxLabel;
        changed[extraLabels.length + 1] = dyLabel;
        layout(changed);
    }

    @Override
    public void setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        projectionCombo.setEnabled(enabled);
        nameText.setEnabled(enabled);
        descriptionText.setEnabled(enabled);
        cornerCombo.setEnabled(enabled);
        nxText.setEnabled(enabled);
        nyText.setEnabled(enabled);
        dxText.setEnabled(enabled);
        dyText.setEnabled(enabled);
        la1Text.setEnabled(enabled);
        lo1Text.setEnabled(enabled);
        for (int i = 0; i < extraText.length; i += 1) {
            extraText[i].setEnabled(enabled);
        }
    }

    /**
     * @return a friendly error message if any fields are not valid or null if
     *         it all looks good.
     */
    public String validate() {
        int index = projectionCombo.getSelectionIndex();
        if (index < 0) {
            return "Select a GDS Template";
        }
        if (nameText.getText().isEmpty()) {
            return "Must provide a name";
        }
        if (descriptionText.getText().isEmpty()) {
            return "Must provide a description";
        }
        if (cornerCombo.getSelectionIndex() < 0) {
            return "Must select first corner";
        }
        try {
            int nx = Integer.parseInt(nxText.getText());
            if (nx <= 0) {
                return "nx must be a positive integer.";
            }
        } catch (NumberFormatException e) {
            return "nx must be a positive integer.";
        }
        try {
            int ny = Integer.parseInt(nyText.getText());
            if (ny <= 0) {
                return "ny must be a positive integer.";
            }
        } catch (NumberFormatException e) {
            return "ny must be a positive integer.";
        }
        if (!validateDouble(dxText, Math.nextUp(0), Double.POSITIVE_INFINITY)) {
            return "dx must be a positive number.";
        }
        if (!validateDouble(dyText, Math.nextUp(0), Double.POSITIVE_INFINITY)) {
            return "dy must be a positive number.";
        }
        if (!validateDouble(la1Text, -90, 90)) {
            return "la1 must be a number between -90 and 90.";
        }
        if (!validateDouble(lo1Text, -180, 180)) {
            return "lo1 must be number between -180 and 180.";
        }
        String projection = projectionCombo.getItem(index);
        if (PROJECTION_MERC.equals(projection)) {
            if (!validateDouble(extraText[0], -90, 90)) {
                return "latin must be a number between -90 and 90.";
            }
        } else if (PROJECTION_POLAR.equals(projection)) {
            if (!validateDouble(extraText[0], -90, 90)) {
                return "lad must be a number between -90 and 90.";
            }
            if (!validateDouble(extraText[1], -180, 180)) {
                return "lov must be a number between -180 and 180.";
            }
        } else if (PROJECTION_LAMBERT.equals(projection)) {
            if (!validateDouble(extraText[0], -90, 90)) {
                return "latin1 must be a number between -90 and 90.";
            }
            if (!validateDouble(extraText[1], -90, 90)) {
                return "latin2 must be a number between -90 and 90.";
            }
            if (!validateDouble(extraText[2], -180, 180)) {
                return "lov must be a number between -180 and 180.";
            }

        }
        return null;
    }

    private boolean validateDouble(Text text, double min, double max) {
        try {
            double d = Double.parseDouble(text.getText());
            if (d < min || d > max) {
                return false;
            }
        } catch (NumberFormatException e) {
            return false;
        }
        return true;
    }

    /**
     * Add listeners that will be notified any time the user changes the value
     * of any widget in this composite.
     */
    public void addListeners(SelectionListener sListener,
            ModifyListener mListener) {
        projectionCombo.addSelectionListener(sListener);
        nameText.addModifyListener(mListener);
        descriptionText.addModifyListener(mListener);
        cornerCombo.addSelectionListener(sListener);
        nxText.addModifyListener(mListener);
        nyText.addModifyListener(mListener);
        dxText.addModifyListener(mListener);
        dyText.addModifyListener(mListener);
        la1Text.addModifyListener(mListener);
        lo1Text.addModifyListener(mListener);
        for (Text text : extraText) {
            text.addModifyListener(mListener);
        }
    }

    /**
     * @return a new coverage object with the values selected in this composite.
     *         This will throw exceptions if the values are not valid so the
     *         caller should ensure {@link #validate()} has been called before
     *         attempting to get the coverage.
     */
    public GridCoverage getCoverage() {
        int index = projectionCombo.getSelectionIndex();
        String selection = projectionCombo.getItem(index);
        GridCoverage coverage = null;
        if (PROJECTION_LATLON.equals(selection)) {
            coverage = new LatLonGridCoverage();
            coverage.setSpacingUnit("degree");
        } else if (PROJECTION_MERC.equals(selection)) {
            MercatorGridCoverage mCoverage = new MercatorGridCoverage();
            mCoverage.setLatin(Double.parseDouble(extraText[0].getText()));
            mCoverage.setSpacingUnit("km");
            coverage = mCoverage;
        } else if (PROJECTION_POLAR.equals(selection)) {
            PolarStereoGridCoverage pCoverage = new PolarStereoGridCoverage();
            pCoverage.setLad(Double.parseDouble(extraText[0].getText()));
            pCoverage.setLov(Double.parseDouble(extraText[1].getText()));
            pCoverage.setSpacingUnit("km");
            coverage = pCoverage;
        } else if (PROJECTION_LAMBERT.equals(selection)) {
            LambertConformalGridCoverage lCoverage = new LambertConformalGridCoverage();
            lCoverage.setLatin1(Double.parseDouble(extraText[0].getText()));
            lCoverage.setLatin2(Double.parseDouble(extraText[1].getText()));
            lCoverage.setLov(Double.parseDouble(extraText[2].getText()));
            lCoverage.setSpacingUnit("km");
            coverage = lCoverage;
        }
        coverage.setName(nameText.getText());
        coverage.setDescription(descriptionText.getText());
        coverage.setFirstGridPointCorner(Corner.valueOf(cornerCombo
                .getItem(cornerCombo.getSelectionIndex())));
        coverage.setNx(Integer.parseInt(nxText.getText()));
        coverage.setNy(Integer.parseInt(nyText.getText()));
        coverage.setDx(Double.parseDouble(dxText.getText()));
        coverage.setDy(Double.parseDouble(dyText.getText()));
        coverage.setLa1(Double.parseDouble(la1Text.getText()));
        coverage.setLo1(Double.parseDouble(lo1Text.getText()));
        return coverage;
    }

    /**
     * Populate the widgets in this composite with values from an existing
     * coverage.
     */
    public void setCoverage(GridCoverage coverage) {
        if (coverage == null) {
            projectionCombo.deselectAll();
            updateExtraFields();
            nameText.setText("");
            descriptionText.setText("");
            cornerCombo.deselectAll();
            nxText.setText("");
            nyText.setText("");
            dxText.setText("");
            dyText.setText("");
            la1Text.setText("");
            lo1Text.setText("");
            return;
        } else if (coverage instanceof LatLonGridCoverage) {
            projectionCombo.select(projectionCombo
                    .indexOf(PROJECTION_LATLON));
            updateExtraFields();
        } else if (coverage instanceof MercatorGridCoverage) {
            projectionCombo.select(projectionCombo
                    .indexOf(PROJECTION_MERC));
            updateExtraFields();
            MercatorGridCoverage mCoverage = (MercatorGridCoverage) coverage;
            extraText[0].setText(Double.toString(mCoverage.getLatin()));
        } else if (coverage instanceof PolarStereoGridCoverage) {
            projectionCombo.select(projectionCombo
                    .indexOf(PROJECTION_POLAR));
            updateExtraFields();
            PolarStereoGridCoverage pCoverage = (PolarStereoGridCoverage) coverage;
            extraText[0].setText(Double.toString(pCoverage.getLad()));
            extraText[1].setText(Double.toString(pCoverage.getLov()));
        } else if (coverage instanceof LambertConformalGridCoverage) {
            projectionCombo.select(projectionCombo
                    .indexOf(PROJECTION_LAMBERT));
            updateExtraFields();
            LambertConformalGridCoverage lCoverage = (LambertConformalGridCoverage) coverage;
            extraText[0].setText(Double.toString(lCoverage.getLatin1()));
            extraText[1].setText(Double.toString(lCoverage.getLatin2()));
            extraText[2].setText(Double.toString(lCoverage.getLov()));
        }
        if (coverage.getName() == null) {
            nameText.setText("");
        } else {
            nameText.setText(coverage.getName());
        }
        if (coverage.getDescription() == null) {
            descriptionText.setText("");
        } else {
            descriptionText.setText(coverage.getDescription());
        }
        cornerCombo.select(cornerCombo.indexOf(coverage
                .getFirstGridPointCorner().name()));
        nxText.setText(coverage.getNx().toString());
        nyText.setText(coverage.getNy().toString());
        dxText.setText(Double.toString(coverage.getDx()));
        dyText.setText(Double.toString(coverage.getDy()));
        la1Text.setText(Double.toString(coverage.getLa1()));
        lo1Text.setText(Double.toString(coverage.getLo1()));

    }

}
