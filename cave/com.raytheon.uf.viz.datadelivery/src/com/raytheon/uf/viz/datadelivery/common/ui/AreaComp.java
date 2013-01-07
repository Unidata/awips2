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
package com.raytheon.uf.viz.datadelivery.common.ui;

import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.datadelivery.registry.EnvelopeUtils;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.d2d.core.map.MapScales;
import com.raytheon.uf.viz.d2d.core.map.MapScales.MapScale;
import com.raytheon.uf.viz.datadelivery.common.xml.AreaXML;
import com.raytheon.uf.viz.datadelivery.subscription.subset.IDataSize;
import com.raytheon.uf.viz.datadelivery.subscription.subset.ISubset;
import com.raytheon.uf.viz.datadelivery.subscription.subset.SpatialSubsetMapDlg;
import com.raytheon.uf.viz.datadelivery.subscription.subset.SubsetFileManager;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * This is a common composite that is used to hold the area controls.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 28, 2012            lvenable     Initial creation.
 * Jun  1, 2012   645      jpiatt       Added tooltips & code cleanup.
 * Jun  8, 2012   684      jpiatt       Radio selection corrections.
 * Aug 10, 2012  1002      mpduff       Implementing dataset size estimation.
 * Aug 21, 2012   837      jpiatt       Corrected region coordinate conversion.
 * Aug 21, 2012  1113      jpiatt       Corrected longitude validation.
 * Oct 22, 2012   684      mpduff       Added showMyRegion method.
 * Oct 31, 2012  1278      mpduff       Integrated SpatialUtils class.
 * Nov 19, 2012  1289      bgonzale     added methods to determine if editing predefined
 *                                      user region. fixed regionMap clear.
 * Dec 07, 2012  1278      bgonzale     Added Coordinate[] to ctors. Use value of coords
 *                                      to determine if starting in manual entry mode.
 * Dec 10, 2012  1259      bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Dec 11, 2012  1264      mpduff       Fix validaiton of lat/lon text fields.
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */

public class AreaComp extends Composite implements ISubset {
    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AreaComp.class);

    /**
     * Region Combo Box options.
     */
    private enum REGION_GROUPS {
        PRE_DEFINED("Pre-defined"), MY_REGIONS("My Regions");

        private final String regionGroup;

        REGION_GROUPS(String value) {
            regionGroup = value;
        }

        public String getRegionGroup() {
            return regionGroup;
        }
    }

    /** Default group text. */
    private String groupText = "Area";

    /** Array of pre-defined regions. */
    public String[] predefinedRegions = null;

    /** Upper left lon text control. */
    private Text upperLeftLonTxt;

    /** Lower right lon text control. */
    private Text lowerRightLonTxt;

    /** Upper left lat text control. */
    private Text upperLeftLatTxt;

    /** Lower right lat text control. */
    private Text lowerRightLatTxt;

    /** Bounding box button. */
    private Button boundingBoxBtn;

    /** Custom label. */
    private Label manualLbl;

    /** Radio button for custom entry. */
    private Button manualRdo;

    /** Region label. */
    private Label regionLbl;

    /** Region combo box. */
    public Combo regionCombo;

    /** Selection combo box. */
    public Combo selectCombo;

    /** Radio button for selecting the bounding box. */
    private Button boundingBoxRdo;

    /** Radio button for selecting the regions. */
    private Button regionRdo;

    /** Envelope describing full area where requests are possible */
    private ReferencedEnvelope fullEnvelope;

    /** Envelope describing a subsection of fullEnvelope */
    private ReferencedEnvelope subEnvelope;

    /** Formatter for decimal. */
    private final NumberFormat formatter = new DecimalFormat("0.0000");

    /** Region tree map. */
    private final Map<String, LocalizationFile> regionMap = new TreeMap<String, LocalizationFile>();

    /** SpatialSubsetMapDlg object. */
    private SpatialSubsetMapDlg dlg = null;

    /** File extension */
    private final String extension = ".xml";

    /** callback */
    private final IDataSize callback;

    /** Envelope valid flag */
    private boolean envelopeValid = false;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent composite.
     * @param groupTxt
     *            Text to be displayed in the group title.
     * @param callback
     *            IDataSize callback
     * @param fullEnvelope
     */
    public AreaComp(Composite parent, String groupTxt, IDataSize callback,
            ReferencedEnvelope fullEnvelope) {
        this(parent, groupTxt, callback, fullEnvelope, null);
    }

    public AreaComp(Composite parent, String groupTxt, IDataSize callback,
            ReferencedEnvelope fullEnvelope, ReferencedEnvelope subEnvelope) {
        super(parent, SWT.NONE);

        if (groupTxt != null) {
            this.groupText = groupTxt;
        }

        this.callback = callback;
        if (fullEnvelope == null) {
            this.fullEnvelope = EnvelopeUtils.WORLD_WIDE_EQ_CYLINDRICAL_CENTER180;
        } else {
            this.fullEnvelope = fullEnvelope;
        }
        this.subEnvelope = subEnvelope;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        /*
         * Setup the layout for the composite
         */
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, true);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        this.setLayout(gl);
        this.setLayoutData(gd);

        setupData();

        createControls();
        if (regionRdo.getSelection()) {
            regionCombo.select(regionCombo.getItemCount() - 1);
            handleRegionSelection();
        } else if (manualRdo.getSelection()) {
            updateBounds(subEnvelope);
        }
    }

    /**
     * Setup the data.
     */
    private void setupData() {
        MapScales mapScales = MapScales.getInstance();
        MapScale[] scales = mapScales.getScales();
        predefinedRegions = new String[scales.length];
        for (int i = 0; i < predefinedRegions.length; i++) {
            predefinedRegions[i] = scales[i].getDisplayName();
        }
    }

    /**
     * Create the main group layout and the controls.
     */
    private void createControls() {
        /*
         * Create the group composite
         */
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        Group grp = new Group(this, SWT.NONE);
        grp.setText(" " + this.groupText + " ");
        grp.setLayout(gl);
        grp.setLayoutData(gd);

        createBoundControls(grp);
        addSeparator(grp);
        createRegionAndBoundingBox(grp);
        updateRegionControls();
    }

    /**
     * Create the bound controls.
     *
     * @param group
     *            Group container.
     */
    private void createBoundControls(Group group) {
        GridLayout gl = new GridLayout(2, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite spatialComp = new Composite(group, SWT.NONE);
        spatialComp.setLayout(gl);
        spatialComp.setLayoutData(gd);

        int controlWidth = 175;

        /*
         * Upper Left Lat
         */
        Label ulLatLbl = new Label(spatialComp, SWT.LEFT);
        ulLatLbl.setText("Upper Left Latitude:");
        ulLatLbl.setToolTipText("Enter upper left latitude");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = controlWidth;
        upperLeftLatTxt = new Text(spatialComp, SWT.BORDER);
        upperLeftLatTxt.setLayoutData(gd);
        upperLeftLatTxt.addFocusListener(new ValidatingFocusListener());

        /*
         * Upper Left Lon
         */
        Label ulLonLbl = new Label(spatialComp, SWT.LEFT);
        ulLonLbl.setText("Upper Left Longitude:");
        ulLonLbl.setToolTipText("Enter upper left longitude");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = controlWidth;
        upperLeftLonTxt = new Text(spatialComp, SWT.BORDER);
        upperLeftLonTxt.setLayoutData(gd);
        upperLeftLonTxt.addFocusListener(new ValidatingFocusListener());

        /*
         * Lower Right Lat
         */
        Label lrLatLbl = new Label(spatialComp, SWT.LEFT);
        lrLatLbl.setText("Lower Right Latitude:");
        lrLatLbl.setToolTipText("Enter lower right latitude");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = controlWidth;
        lowerRightLatTxt = new Text(spatialComp, SWT.BORDER);
        lowerRightLatTxt.setLayoutData(gd);
        lowerRightLatTxt.addFocusListener(new ValidatingFocusListener());

        /*
         * Lower Right Lon
         */
        Label lrLonLbl = new Label(spatialComp, SWT.LEFT);
        lrLonLbl.setText("Lower Right Longitude:");
        lrLonLbl.setToolTipText("Enter lower right longitude");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = controlWidth;
        lowerRightLonTxt = new Text(spatialComp, SWT.BORDER);
        lowerRightLonTxt.setLayoutData(gd);
        lowerRightLonTxt.addFocusListener(new ValidatingFocusListener());
    }

    /**
     * Create the region and bounding box controls.
     *
     * @param group
     *            Group container.
     */
    private void createRegionAndBoundingBox(Group group) {
        Composite regionComp = new Composite(group, SWT.NONE);
        regionComp.setLayout(new GridLayout(4, false));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        regionComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));
        boolean startInManualEntryMode = (subEnvelope != null);

        /*
         * Custom controls.
         */
        gd = new GridData(18, SWT.DEFAULT);
        manualRdo = new Button(regionComp, SWT.RADIO);
        manualRdo.setSelection(startInManualEntryMode);
        manualRdo.setLayoutData(gd);
        manualRdo.setToolTipText("Select to allow for manual lat/lon entry");
        manualRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (manualRdo.getSelection() == false) {
                    return;
                }

                updateRegionControls();
            }
        });

        gd = new GridData();
        gd.horizontalSpan = 3;
        manualLbl = new Label(regionComp, SWT.LEFT);
        manualLbl.setText("Manual Lat/Lon Edit");
        manualLbl.setLayoutData(gd);

        /*
         * Predefined controls.
         */
        gd = new GridData(18, SWT.DEFAULT);
        regionRdo = new Button(regionComp, SWT.RADIO);
        regionRdo.setSelection(!startInManualEntryMode);
        regionRdo.setLayoutData(gd);
        regionRdo.setToolTipText("Select a pre-defined region");
        regionRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (regionRdo.getSelection() == false) {
                    return;
                }
                updateRegionControls();
            }
        });

        regionLbl = new Label(regionComp, SWT.LEFT);
        regionLbl.setText("Pre-defined Regions:");

        REGION_GROUPS[] groups = REGION_GROUPS.values();
        String[] groupArr = new String[groups.length];
        for (int i = 0; i < groups.length; i++) {
            groupArr[i] = groups[i].getRegionGroup();
        }

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        selectCombo = new Combo(regionComp, SWT.READ_ONLY);
        selectCombo.setItems(groupArr);
        selectCombo.select(0);
        selectCombo.setLayoutData(gd);
        selectCombo.setToolTipText("Select from pre-defined or user-defined");
        selectCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleRegionChange();
            }
        });

        gd = new GridData(250, SWT.DEFAULT);
        regionCombo = new Combo(regionComp, SWT.READ_ONLY);
        regionCombo.setItems(predefinedRegions);
        regionCombo.setLayoutData(gd);
        regionCombo.select(1);
        regionCombo.setToolTipText("Select a region");
        regionCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleRegionSelection();
            }
        });

        /*
         * Bounding box controls.
         */
        gd = new GridData(18, SWT.DEFAULT);
        boundingBoxRdo = new Button(regionComp, SWT.RADIO);
        boundingBoxRdo.setLayoutData(gd);
        boundingBoxRdo.setToolTipText("Select to draw a bounding box");
        boundingBoxRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (boundingBoxRdo.getSelection() == false) {
                    return;
                }

                updateRegionControls();
            }
        });

        gd = new GridData();
        gd.horizontalSpan = 2;
        boundingBoxBtn = new Button(regionComp, SWT.PUSH);
        boundingBoxBtn.setText("Draw Bounding Box... ");
        boundingBoxBtn.setToolTipText("Click to draw a bounding box");
        boundingBoxBtn.setLayoutData(gd);
        boundingBoxBtn.setEnabled(false);
        boundingBoxBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayMapDialog();
            }
        });
    }

    /**
     * Add a separator line to the display.
     *
     * @param parentComp
     *            Parent component.
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Enable/Disable the region and bounding box controls. The bounding box
     * control will always be the disabled if the region controls are enabled.
     *
     */
    public void updateRegionControls() {
        enableCustomControls(manualRdo.getSelection());
        enableRegionContols(regionRdo.getSelection());
        enableBoundingBoxContols(boundingBoxRdo.getSelection());
        enableTextControls(manualRdo.getSelection());
    }

    /**
     * Set controls enabled based on flag.
     *
     * @param flag
     *            true if controls should be enabled
     */
    private void enableCustomControls(boolean flag) {
        manualLbl.setEnabled(flag);
        enableTextControls(flag);
    }

    /**
     * Set text controls enabled based on flag.
     *
     * @param flag
     *            true if text controls should be enabled
     */
    private void enableTextControls(boolean flag) {
        upperLeftLonTxt.setEditable(flag);
        upperLeftLonTxt.setBackground(getTextBackgroundColor(flag));

        lowerRightLonTxt.setEditable(flag);
        lowerRightLonTxt.setBackground(getTextBackgroundColor(flag));

        upperLeftLatTxt.setEditable(flag);
        upperLeftLatTxt.setBackground(getTextBackgroundColor(flag));

        lowerRightLatTxt.setEditable(flag);
        lowerRightLatTxt.setBackground(getTextBackgroundColor(flag));
    }

    /**
     * Get the text background based on if the control is enabled or not
     *
     * @param enabled
     *            enabled flag
     */
    private Color getTextBackgroundColor(boolean enabled) {
        Color returnColor = this.getDisplay().getSystemColor(SWT.COLOR_WHITE);
        if (!enabled) {
            returnColor = this.getDisplay().getSystemColor(
                    SWT.COLOR_WIDGET_BACKGROUND);
        }

        return returnColor;
    }

    /**
     * Enable the regions control based on the flag.
     *
     * @param flag
     *            enabled state of the controls
     */
    private void enableRegionContols(boolean flag) {
        regionLbl.setEnabled(flag);
        regionCombo.setEnabled(flag);
        selectCombo.setEnabled(flag);
        if (flag) {
            handleRegionSelection();
        }
    }

    /**
     * Enable the bounding box controls base on the flag.
     *
     * @param flag
     *            enabled state of the controls
     */
    private void enableBoundingBoxContols(boolean flag) {
        boundingBoxBtn.setEnabled(flag);
    }

    /**
     * Enable all controls using flag.
     *
     * @param flag
     *            on/off toggle for controls.
     */
    public void enableAllControls(boolean flag) {
        manualLbl.setEnabled(flag);
        enableTextControls(flag);
        regionLbl.setEnabled(flag);
        regionCombo.setEnabled(flag);
        selectCombo.setEnabled(flag);
        boundingBoxBtn.setEnabled(flag);

        manualRdo.setEnabled(flag);
        regionRdo.setEnabled(flag);
        boundingBoxRdo.setEnabled(flag);

        if (flag == true) {
            updateRegionControls();
        }
    }

    /**
     * Action taken when the region type has changed.
     */
    public void handleRegionChange() {
        regionCombo.removeAll();
        if (selectCombo.getEnabled()
                && selectCombo.getItem(selectCombo.getSelectionIndex()).equals(
                        REGION_GROUPS.PRE_DEFINED.getRegionGroup())) {
            regionCombo.setItems(predefinedRegions);
            regionCombo.select(0);
            handleRegionSelection();
        } else {
            regionCombo.setItems(getUserRegions());
            if (regionCombo.getItemCount() > 0) {
                regionCombo.select(0);
                handleRegionSelection();
            }
        }
    }

    public boolean isUserDefinedRegion() {
        return (selectCombo.getEnabled() && selectCombo.getItem(
                selectCombo.getSelectionIndex()).equals(
                REGION_GROUPS.MY_REGIONS.getRegionGroup()));
    }

    public boolean isPredefinedRegion() {
        return !(boundingBoxRdo.getSelection() || manualRdo.getSelection());
    }

    public boolean doAreaEditBoxesHaveFocus() {
        return upperLeftLatTxt.isFocusControl()
                || upperLeftLonTxt.isFocusControl()
                || lowerRightLatTxt.isFocusControl()
                || lowerRightLonTxt.isDisposed();
    }

    /**
     * Region selection action handler
     */
    private void handleRegionSelection() {
        if (!regionRdo.getSelection()) {
            return;
        }
        ReferencedEnvelope regionEnvelope = null;
        String name = regionCombo.getItem(regionCombo.getSelectionIndex());

        if (!isUserDefinedRegion()) {
            MapScale mapScale = MapScales.getInstance().getScaleByName(name);

            if (mapScale != null) {
                File scaleBundle = mapScale.getFile();
                try {
                    Bundle b = SerializationUtil.jaxbUnmarshalFromXmlFile(
                            Bundle.class, scaleBundle);

                    AbstractRenderableDisplay[] displays = b.getDisplays();
                    IMapDescriptor descriptor = (IMapDescriptor) displays[0]
                            .getDescriptor();

                    regionEnvelope = new ReferencedEnvelope(descriptor
                            .getGridGeometry().getEnvelope());
                } catch (SerializationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        } else {
            String path = name;
            if (!path.endsWith(extension)) {
                path = path + extension;
            }

            LocalizationFile locFile = regionMap.get(path);

            AreaXML area = SubsetFileManager.getInstance().getArea(locFile);
            regionEnvelope = area.getEnvelope();
            enableTextControls(true);
        }
        if (regionEnvelope != null) {
            try {
                ReferencedEnvelope intersection = MapUtil
                        .reprojectAndIntersect(regionEnvelope, fullEnvelope);
                if (intersection == null || intersection.isEmpty()) {
                    StringBuilder errorText = new StringBuilder();
                    errorText.append(name);
                    errorText.append(" does not intersect the dataset area.");
                    DataDeliveryUtils.showMessage(getShell(), SWT.OK,
                            "Validation Error", errorText.toString());
                    envelopeValid = false;
                    return;
                }

                envelopeValid = true;
                updateBounds(regionEnvelope);
                enableTextControls(!isPredefinedRegion());

            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }

        }
    }

    public boolean isRegionChanged() {
        return (regionRdo != null && regionRdo.getSelection());
    }

    /**
     * Display the map dialog.
     */
    private void displayMapDialog() {
        ReferencedEnvelope dlgEnvelope = this.subEnvelope;
        if (!dlgEnvelope.getCoordinateReferenceSystem().equals(
                fullEnvelope.getCoordinateReferenceSystem())) {
            // the dialog should always use an envelope in the same crs as the
            // data.
            try {
                dlgEnvelope = MapUtil.reprojectAndIntersect(dlgEnvelope,
                        fullEnvelope);
            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        if (dlg == null || dlg.isDisposed()) {
            dlg = new SpatialSubsetMapDlg(this.getShell(), this, fullEnvelope,
                    dlgEnvelope);
            dlg.open();
        } else {
            if (fullEnvelope != null) {
                dlg.setFullEnvelope(fullEnvelope);
                dlg.setSubEnvelope(dlgEnvelope);
            }
            dlg.bringToTop();
        }
    }

    /**
     * Get the user Regions.
     *
     * @return array of user regions
     */
    private String[] getUserRegions() {
        LocalizationFile[] areas = SubsetFileManager.getInstance().getAreas();
        // this.regionMap

        this.regionMap.clear();
        for (LocalizationFile lf : areas) {
            regionMap.put(lf.getFile().getName(), lf);
        }

        List<String> regions = new ArrayList<String>();
        Collections.sort(regions);
        for (String region : regionMap.keySet()) {

            // remove file extension
            int extensionIndex = region.lastIndexOf(".");
            String newStr = region.substring(0, extensionIndex);

            regions.add(newStr);
        }

        return regions.toArray(new String[regions.size()]);
    }

    /**
     * Validate the bounds.
     * 
     * @return true if the bounds are valid
     */
    private boolean validateBoundsText() {
        if (!upperLeftLatTxt.getEditable()) {
            this.envelopeValid = true;
            return true;
        }

        Coordinate ul = new Coordinate();
        ul.x = getDouble(upperLeftLonTxt.getText());
        ul.y = getDouble(upperLeftLatTxt.getText());

        Coordinate lr = new Coordinate();
        lr.x = getDouble(lowerRightLonTxt.getText());
        lr.y = getDouble(lowerRightLatTxt.getText());

        // Allow up to 5% error to handle rounding problems with formatted
        // LatLon values.
        boolean ulValid = EnvelopeUtils.envelopeContainsLatLon(fullEnvelope,
                ul, 0.05);
        boolean lrValid = EnvelopeUtils.envelopeContainsLatLon(fullEnvelope,
                lr, 0.05);

        envelopeValid = ulValid && lrValid;
        if (envelopeValid) {
            subEnvelope = EnvelopeUtils.createSubenvelopeFromLatLon(
                    fullEnvelope, ul, lr);
            updateDataSize();
        } else {
            StringBuilder errorText = new StringBuilder();
            errorText.append("The ");
            if (ulValid) {
                errorText.append("Lower Right Coordinate is");
            } else if (lrValid) {
                errorText.append("Upper Left Coordinate is");
            } else {
                errorText.append("Lower Right and Upper Left Coordinates are");
            }
            errorText.append(" not within the dataset area.");
            DataDeliveryUtils.showMessage(getShell(), SWT.OK,
                    "Validation Error", errorText.toString());
        }

        // Entries are valid so save them off.
        this.envelopeValid = true;

        return true;
    }

    /**
     * Get a double from text.
     *
     * @param text
     *            The double String
     * @return double, Double.NaN if unable to parse
     */
    private double getDouble(String text) {
        try {
            return Double.parseDouble(text);
        } catch (NumberFormatException e) {
            return Double.NaN;
        }
    }

    public ReferencedEnvelope getEnvelope() {
        if (!envelopeValid) {
            return null;
        }

        return subEnvelope;
    }

    public void setFullEnvelope(ReferencedEnvelope fullEnvelope) {
        if (fullEnvelope != null) {
            this.fullEnvelope = fullEnvelope;
        }
    }

    public void updateBounds(ReferencedEnvelope envelope) {
        Coordinate ul = EnvelopeUtils.getUpperLeftLatLon(envelope);
        Coordinate lr = EnvelopeUtils.getLowerRightLatLon(envelope);

        upperLeftLonTxt.setText(formatter.format(lr.x));
        upperLeftLatTxt.setText(formatter.format(ul.y));
        lowerRightLonTxt.setText(formatter.format(ul.x));
        lowerRightLatTxt.setText(formatter.format(lr.y));
        this.subEnvelope = envelope;

        updateDataSize();

    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.viz.datadelivery.subscription.subset.ISubset#
     * updateSelectionState(boolean, java.lang.String)
     */
    @Override
    public void updateSelectionState(boolean selected, String id) {
        // Not used
    }

    /**
     * Select the custom radio button
     *
     * @param regionName
     */
    public void setRegion(String regionName) {

        String[] regions = getUserRegions();
        int index = 0;
        String subRegionName = regionName;

        if (regionName.endsWith(extension)) {
            int extensionIndex = regionName.lastIndexOf(".");
            subRegionName = regionName.substring(0, extensionIndex);
        }

        // set radio buttons
        manualRdo.setSelection(false);
        regionRdo.setSelection(true);
        boundingBoxRdo.setSelection(false);

        // Enable features
        regionLbl.setEnabled(true);
        selectCombo.setEnabled(true);
        regionCombo.setEnabled(true);

        regionCombo.setItems(regions);
        selectCombo.select(1);

        for (String region : regions) {

            if (region.equals(subRegionName)) {
                regionCombo.select(index);
                break;
            }
            index++;
        }

    }

    /**
     * Select the custom radio button.
     *
     * @return region name
     */
    public String getRegionName() {

        if (regionCombo.getSelectionIndex() > -1) {
            return regionCombo.getItem(regionCombo.getSelectionIndex());
        }

        return null;
    }

    /**
     * Select the custom radio button
     */
    public void setCustom() {
        manualRdo.setSelection(true);
        regionRdo.setSelection(false);
        boundingBoxRdo.setSelection(false);
        updateRegionControls();
    }

    /**
     * Update the data size.
     */
    private void updateDataSize() {
        if (callback != null) {
            callback.updateDataSize();
        }
    }

    /**
     * Switch to "My Regions" as the active selection and select the provided
     * region name.
     *
     * @param regionName
     */
    public void showMyRegions(String regionName) {
        regionCombo.removeAll();
        String[] userRegions = getUserRegions();
        regionCombo.setItems(userRegions);
        selectCombo.select(REGION_GROUPS.MY_REGIONS.ordinal());

        if (regionCombo.getItemCount() > 0) {
            if (regionName == null) {
                regionCombo.select(0);
                return;
            }

            for (int i = 0; i < userRegions.length; i++) {
                if (userRegions[i].equals(regionName)) {
                    regionCombo.select(i);
                    return;
                }
            }
            regionCombo.select(0);
            handleRegionSelection();
        }
    }

    /**
     * @return the envelopeValid
     */
   public boolean isEnvelopeValid() {
        return envelopeValid;
    }

    private class ValidatingFocusListener implements FocusListener {

        private String prev;

        @Override
        public void focusGained(FocusEvent e) {
            Text t = (Text) e.widget;
            prev = t.getText();
        }

        @Override
        public void focusLost(FocusEvent e) {
            Text t = (Text) e.widget;
            if (!t.getEditable() || !manualRdo.getSelection()
                    || t.getText().equals(prev)) {
                return;
            }
            validateBoundsText();
        }

   }
   
}
