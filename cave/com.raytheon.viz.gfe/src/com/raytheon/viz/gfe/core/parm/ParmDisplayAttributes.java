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
package com.raytheon.viz.gfe.core.parm;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.RefType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.point.display.PointWindDisplay.DisplayType;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.IConfigurationChangeListener;
import com.raytheon.viz.gfe.rsc.GFEResource;

/**
 * Parm Display Attributes
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 20, 2008           chammack  Initial creation.
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Mar 15, 2018  6967     randerso  Improved error handling in setDisplayMask()
 *
 * </pre>
 *
 * @author chammack
 */
public class ParmDisplayAttributes {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParmDisplayAttributes.class);

    public static enum EditorType {
        SPATIAL, TEMPORAL, GRIDMGR
    };

    public static enum VisMode {
        IMAGE("Image"), GRAPHIC("Graphic");

        private String str;

        private VisMode(String mode) {
            this.str = mode;
        }

        @Override
        public String toString() {
            return this.str;
        }

    }

    public static enum VisualizationType {
        WIND_BARB("WindBarb"),
        WIND_ARROW("WindArrow"),
        CONTOUR("Contour"),
        IMAGE("Image"),
        BOUNDED_AREA("BoundedArea"),
        TE_COLOR_BAR("TEColorBar"),
        TIME_BAR("TimeBar"),
        TE_COLOR_RANGE_BAR("TEColorRangeBar"),
        RANGE_BAR("RangeBar");

        private String visualizationType;

        private VisualizationType(String visualizationType) {
            this.visualizationType = visualizationType;
        }

        /**
         * @return the visualizationType
         */
        public String getVizualizationType() {
            return visualizationType;
        }

        /**
         * @param string
         * @return the VisualizationType for the string
         */
        public static VisualizationType fromString(String string) {
            for (VisualizationType visType : VisualizationType.values()) {
                if (visType.visualizationType.equals(string)) {
                    return visType;
                }
            }
            throw new IllegalArgumentException(
                    string + " is not a valid VisualizationType.");
        }
    }

    protected static String[] genericColors;

    protected static int genericNext;

    static {
        GFEPreference.addConfigurationChangeListener(
                new IConfigurationChangeListener() {

                    @Override
                    public void configurationChanged(String config) {
                        genericColors = null;
                        genericNext = 0;
                    }
                });
    }

    /** The available image visualization types */
    private Set<VisualizationType> graphicSEVisType, imageSEVisType;

    private Set<VisualizationType> graphicTEVisType, imageTEVisType;

    private Set<VisualizationType> availGraphicSETypes, availGraphicTETypes;

    private Set<VisualizationType> availImageSETypes, availImageTETypes;

    /** The Editor type. */
    protected EditorType editorType = EditorType.SPATIAL;

    /** The Visual Mode */
    protected VisMode visMode = VisMode.GRAPHIC;

    /** The displayable. */
    protected boolean displayable;

    /** The contour values. */
    protected float[] baseContourValues;

    protected float[] contourValues;

    /** The line width. */
    protected int lineWidth;

    /** The line width. */
    protected LineStyle lineStyle;

    /** The line width. */
    protected LineStyle initLineStyle;

    /** The font offset. */
    protected int fontOffset;

    /** The density. */
    protected int density = 0;

    /** The base color */
    protected RGB baseColor;

    /** Reference to the Parm */
    protected Parm parm;

    protected VisualizationType vizType;

    protected DisplayType displayType = null;

    private ReferenceData displayMaskRefData;

    /**
     * Instantiates a new parm display attributes.
     *
     * @param displayable
     * @param parm
     */
    public ParmDisplayAttributes(boolean displayable, Parm parm) {
        this.displayable = displayable;
        this.parm = parm;

        // Get the graphic color information and allocate the color
        // get preferred color from config
        String pn = parm.getParmID().compositeNameUI();
        String graphicColor = GFEPreference.getString(pn + "_graphicColor");
        if (graphicColor.isEmpty()) {
            baseColor = getDefaultColor();
        } else {
            baseColor = RGBColors.getRGBColor(graphicColor);
        }

        initMask();

        availImageSETypes = EnumSet.noneOf(VisualizationType.class);
        availImageTETypes = EnumSet.noneOf(VisualizationType.class);
        availGraphicSETypes = EnumSet.noneOf(VisualizationType.class);
        availGraphicTETypes = EnumSet.noneOf(VisualizationType.class);
        imageSEVisType = EnumSet.noneOf(VisualizationType.class);
        imageTEVisType = EnumSet.noneOf(VisualizationType.class);
        graphicSEVisType = EnumSet.noneOf(VisualizationType.class);
        graphicTEVisType = EnumSet.noneOf(VisualizationType.class);

        setAvailableVisTypes();
        initVisualTypes();
        initGraphicStyles();
        initContourInfo();

    }

    /**
     * Uses parmName_contourValues from the config or parmName_contourInterval.
     * If not found, then calculates some defaults based on the min/max limits
     * and the precision. Ordering is contourValues first, then contourInterval.
     *
     * NOTE: in porting this code, I restructured the routine greatly, so it
     * only does one set of contour value calculations.
     */
    private void initContourInfo() {
        if (parm.getGridInfo().getGridType().equals(GridType.WEATHER)
                || parm.getGridInfo().getGridType().equals(GridType.DISCRETE)) {
            return;
        }

        String pn = parm.getParmID().getParmName();
        // look for contour values in config file
        float[] contourValueObj = GFEPreference
                .getFloatArray(pn + "_contourValues");

        // check for contourInterval from config file
        float interval = GFEPreference.getFloat(pn + "_contourInterval");

        if (contourValueObj.length > 0) {
            baseContourValues = contourValueObj;
        } else if (interval > 0.0f) {
            baseContourValues = calcContourValuesFromInterval(interval);
        }

        // do automatic calculation
        if (baseContourValues == null || baseContourValues.length == 0) {
            // compute the interval
            final float multStep[] = { 1, 2, 5 };

            // get the parm min and max values
            float parmMin = parm.getGridInfo().getMinValue();
            float parmMax = parm.getGridInfo().getMaxValue();
            float parmExtent = parmMax - parmMin;

            // how many intervals are desired?
            int noLabels = 60;

            // integer to step up multipliers by factor of 10
            float decade = 1.00f;
            for (int p = 1; p <= parm.getGridInfo().getPrecision(); p++) {
                decade *= 0.1;
            }

            boolean done = false;

            // which of the possible intervals is the closest?
            float intv = 0.0f;
            int noInterval;
            while (!done) {
                for (int i = 0; i < 2; i++) {
                    intv = decade * multStep[i];
                    noInterval = (int) (parmExtent / intv);
                    if (noInterval <= noLabels) {
                        done = true;
                        break;
                    }
                }
                if (done) {
                    break;
                } else {
                    // try again, after increasing multipliers
                    decade *= 10;
                }
            }

            // the interval has now been calculated. Now we need to calculate
            // the base value
            int iStartValue;
            float startValue;
            startValue = parmMin;
            iStartValue = (int) ((int) (parmMin / intv) * intv);
            if (iStartValue != (int) parmMin) {
                startValue = iStartValue + intv;
            } else {
                startValue = iStartValue;
            }

            // now calculate the values and store them
            List<Float> contourValList = new ArrayList<>();
            float value = startValue;
            while (value < parmMax) {
                contourValList.add(new Float(value));
                value += intv;
            }
            baseContourValues = new float[contourValList.size()];
            for (int i = 0; i < contourValList.size(); i++) {
                baseContourValues[i] = contourValList.get(i).floatValue();
            }
        }

        // now take into account the density
        contourValues = calcContourValuesFromDensity(baseContourValues,
                density);
    }

    /**
     * Routine takes an input set of contour values along with a contour density
     * value. It returns a set of contour values based on these values.
     *
     * @param baseValues
     * @param contourDensity
     * @return
     */
    private float[] calcContourValuesFromDensity(float[] baseValues,
            int contourDensity) {
        if (contourDensity == 0) {
            return Arrays.copyOf(baseValues, baseValues.length);
        }

        // increased number of contours
        List<Float> ret = new ArrayList<>();
        if (contourDensity > 0) {
            for (int i = 0; i < baseValues.length - 1; i++) {
                float interval = baseValues[i + 1] - baseValues[i];
                float delta = interval / (contourDensity + 1);
                for (int j = 0; j < contourDensity + 1; j++) {
                    ret.add(baseValues[i] + delta * j);
                }
            }
            ret.add(baseValues[baseValues.length - 1]);
        }

        // decreased number of contours
        else {
            double skip = -contourDensity + 1;
            for (int i = 0; i < baseValues.length; i++) {
                if (i % skip == 0) {
                    ret.add(baseValues[i]);
                }
            }
            ret.add(baseValues[baseValues.length - 1]);
        }

        float[] retArr = new float[ret.size()];
        for (int i = 0; i < retArr.length; i++) {
            retArr[i] = ret.get(i);
        }
        return retArr;
    }

    /**
     * @param interval
     */
    public void setContourInterval(float interval) {
        if (interval > 0.0f) {
            contourValues = calcContourValuesFromInterval(interval);
        }

        refresh();
    }

    /**
     * @param interval
     */
    private float[] calcContourValuesFromInterval(float interval) {
        List<Float> temp = new ArrayList<>();

        float min = parm.getGridInfo().getMinValue();
        float max = parm.getGridInfo().getMaxValue();

        // do the positive values;
        float base = 0;
        int i = 0;
        while (base <= max) {
            if (base >= min) {
                temp.add(base);
            }
            i++;
            base = i * interval;
        }
        // do the negative numbers
        base = 0.0f - interval;
        i = -1;
        while (base >= min) {
            if (base <= max) {
                temp.add(base);
            }
            i--;
            base = i * interval;
        }
        Collections.sort(temp);
        float[] newContourValues = new float[temp.size()];
        i = 0;
        for (Float f : temp) {
            newContourValues[i] = f.floatValue();
            i++;
        }

        return newContourValues;
    }

    private void initGraphicStyles() {
        String pn = parm.getParmID().getParmName();
        lineWidth = GFEPreference.getInt(pn + "_lineWidth", 1);

        fontOffset = GFEPreference.getInt(pn + "_fontOffset");

        density = GFEPreference.getInt(pn + "_density");

        try {
            lineStyle = LineStyle.valueOf(
                    GFEPreference.getString(pn + "_linePattern", "SOLID"));
        } catch (IllegalArgumentException e) {
            statusHandler.error("GFE config file \""
                    + GFEPreference.getConfigName()
                    + "\" contains an invalid line style for " + pn + ": "
                    + GFEPreference.getString(pn + "_linePattern"), e);
        }

        initLineStyle = lineStyle;
    }

    private void initVisualTypes(EditorType et, VisMode vm) {
        String pn = parm.getParmID().compositeNameUI();
        switch (et) {
        case SPATIAL:
            pn += "_spatial";
            break;
        case TEMPORAL:
            pn += "_temporal";
            break;
        case GRIDMGR:
            return;
        default:
            // logBug << "initVisualTypes(): unknown editor type :"
            // << (unsigned long)et << std::endl;
            return;
        }

        switch (vm) {
        case IMAGE:
            pn += "ImageType";
            break;
        case GRAPHIC:
            pn += "GraphicType";
            break;
        // case NONE:
        // return;
        default:
            // logBug << "initVisualTypes(): unknown visual mode: "
            // << (unsigned long)vm << std::endl;
            return;
        }

        Set<VisualizationType> visTypes = null;
        String[] visStrings = GFEPreference.getStringArray(pn);
        if (visStrings.length > 0) {
            visTypes = new HashSet<>();
            for (int i = 0; i < visStrings.length; i++) {
                visTypes.add(VisualizationType.fromString(visStrings[i]));
            }
        } else {
            visTypes = getDefaultVisualizationTypes(et, vm);
        }

        // remove entries that are not allowed, there are two lists, the
        // images and the graphics.
        Set<VisualizationType> available = EnumSet
                .noneOf(VisualizationType.class);
        available.addAll(getAvailableVisualizationType(et, vm));
        if (vm.equals(VisMode.IMAGE)) {
            available
                    .addAll(getAvailableVisualizationType(et, VisMode.GRAPHIC));
        }

        visTypes.retainAll(available);

        setVisualizationType(et, vm, visTypes);
    }

    /**
     * Determines the default visualization types to use based on the editor
     * type, visual mode, and parm type. These values are not taken from config.
     *
     * @param et
     * @param vm
     * @return
     */
    private Set<VisualizationType> getDefaultVisualizationTypes(EditorType et,
            VisMode vm) {
        Set<VisualizationType> types = EnumSet.noneOf(VisualizationType.class);
        if (et.equals(EditorType.SPATIAL)) {
            // IMAGE MODE
            if (vm.equals(VisMode.IMAGE)) {
                types.add(VisualizationType.IMAGE);
                if (parm.getGridInfo().getGridType().equals(GridType.VECTOR)) {
                    types.add(VisualizationType.WIND_BARB);
                }
            }
            // GRAPHICS MODE
            else if (vm.equals(VisMode.GRAPHIC)) {
                if (parm.getGridInfo().getGridType().equals(GridType.SCALAR)) {
                    types.add(VisualizationType.CONTOUR);
                } else if (parm.getGridInfo().getGridType()
                        .equals(GridType.VECTOR)) {
                    types.add(VisualizationType.WIND_BARB);
                } else if (parm.getGridInfo().getGridType()
                        .equals(GridType.WEATHER)) {
                    types.add(VisualizationType.BOUNDED_AREA);
                } else if (parm.getGridInfo().getGridType()
                        .equals(GridType.DISCRETE)) {
                    types.add(VisualizationType.BOUNDED_AREA);
                }

            }
        }

        else if (et.equals(EditorType.TEMPORAL)) {
            // IMAGE MODE
            if (vm.equals(VisMode.IMAGE)) {
                types.add(VisualizationType.TE_COLOR_BAR);
                if (parm.getGridInfo().getGridType().equals(GridType.VECTOR)) {
                    types.add(VisualizationType.WIND_BARB);
                }
            }
            // GRAPHICS MODE
            else if (vm.equals(VisMode.GRAPHIC)) {
                if (parm.getGridInfo().getGridType().equals(GridType.SCALAR)
                        || parm.getGridInfo().getGridType()
                                .equals(GridType.VECTOR)) {
                    types.add(VisualizationType.TIME_BAR);
                }
                if (parm.getGridInfo().getGridType().equals(GridType.VECTOR)) {
                    types.add(VisualizationType.WIND_BARB);
                }
            }
        }

        return types;
    }

    private void initVisualTypes() {
        initVisualTypes(EditorType.SPATIAL, VisMode.IMAGE);
        initVisualTypes(EditorType.SPATIAL, VisMode.GRAPHIC);
        initVisualTypes(EditorType.TEMPORAL, VisMode.IMAGE);
        initVisualTypes(EditorType.TEMPORAL, VisMode.GRAPHIC);
    }

    /**
     * Initialization routine to set the available visualization types.
     *
     * The entries are the ONLY image and ONLY graphic types, thus the actual
     * available ones for image are the image+graphic.
     *
     */
    private void setAvailableVisTypes() {
        // image is allowed on all spatial editor data types
        availImageSETypes.add(VisualizationType.IMAGE);

        // color bar is allowed on all temporal editor data types
        availImageTETypes.add(VisualizationType.TE_COLOR_BAR);

        switch (parm.getGridInfo().getGridType()) {
        case SCALAR:
            availImageTETypes.add(VisualizationType.TE_COLOR_RANGE_BAR);
            availGraphicSETypes.add(VisualizationType.CONTOUR);
            availGraphicTETypes.add(VisualizationType.TIME_BAR);
            availGraphicTETypes.add(VisualizationType.RANGE_BAR);
            break;

        case VECTOR:
            availImageTETypes.add(VisualizationType.TE_COLOR_RANGE_BAR);
            availGraphicSETypes.add(VisualizationType.WIND_BARB);
            availGraphicSETypes.add(VisualizationType.WIND_ARROW);
            availGraphicTETypes.add(VisualizationType.TIME_BAR);
            availGraphicTETypes.add(VisualizationType.RANGE_BAR);
            availGraphicTETypes.add(VisualizationType.WIND_BARB);
            availGraphicTETypes.add(VisualizationType.WIND_ARROW);
            break;

        case WEATHER:
            availGraphicSETypes.add(VisualizationType.BOUNDED_AREA);
            break;

        case DISCRETE:
            availGraphicSETypes.add(VisualizationType.BOUNDED_AREA);
            break;

        default:
            break;
        }

    }

    private void initMask() {
        // Set the parms visual mask if there is a config entry
        String siteID = parm.getParmID().getDbId().getSiteId();
        String pn = parm.getParmID().getParmName();
        String confVal = GFEPreference.getString(siteID + "_" + pn + "_mask");
        if (confVal.isEmpty()) {
            confVal = GFEPreference.getString(siteID + "_mask");
        }

        if (confVal.isEmpty()) {
            confVal = GFEPreference.getString(pn + "_mask");
        }
        if (confVal.isEmpty()) {
            confVal = GFEPreference.getString("mask");
        }

        if (confVal.isEmpty()) {
            // set all bits set
            setEmptyMask();
        } else {
            ReferenceID rid = new ReferenceID(confVal);
            setDisplayMask(rid);
        }
    }

    /**
     * Checks if is displayable.
     *
     * @return true, if is displayable
     */
    public boolean isDisplayable() {
        return displayable;
    }

    /**
     * Gets a copy of the base contour values.
     *
     * @return the contourValues
     */
    public float[] getBaseContourValues() {
        return baseContourValues.clone();
    }

    /**
     * Gets the contour values.
     *
     * @return the contourValues
     */
    public float[] getContourValues() {
        return contourValues;
    }

    /**
     * Sets the contour values.
     *
     * @param contourValues
     *            the contourValues to set
     */
    public void setContourValues(float[] contourValues) {
        this.contourValues = contourValues;
        refresh();
    }

    /**
     * @return the lineStyle
     */
    public LineStyle getLineStyle() {
        return lineStyle;
    }

    /**
     * @param lineStyle
     *            the lineStyle to set
     */
    public void setLineStyle(LineStyle lineStyle) {
        if (lineStyle.equals(LineStyle.DEFAULT)) {
            lineStyle = initLineStyle;
        }
        this.lineStyle = lineStyle;

        ResourcePair rp = parm.getDataManager().getSpatialDisplayManager()
                .getResourcePair(parm);

        if (rp != null) {
            GFEResource rsc = (GFEResource) rp.getResource();
            rsc.getCapability(OutlineCapability.class)
                    .setLineStyle(this.lineStyle);
            refresh(rsc);
        }
    }

    /**
     * Gets the line width.
     *
     * @return the lineWidth
     */
    public int getLineWidth() {
        return lineWidth;
    }

    /**
     * Sets the line width.
     *
     * @param lineWidth
     *            the lineWidth to set
     */
    public void setLineWidth(int lineWidth) {
        this.lineWidth = lineWidth;

        ResourcePair rp = parm.getDataManager().getSpatialDisplayManager()
                .getResourcePair(parm);

        if (rp != null) {
            GFEResource rsc = (GFEResource) rp.getResource();
            rsc.getCapability(OutlineCapability.class)
                    .setOutlineWidth(this.lineWidth);
            refresh(rsc);
        }
    }

    /**
     * Gets the font offset.
     *
     * @return the fontOffset
     */
    public int getFontOffset() {
        return fontOffset;
    }

    /**
     * Sets the font offset.
     *
     * @param fontOffset
     *            the fontOffset to set
     */
    public void setFontOffset(int fontOffset) {
        this.fontOffset = fontOffset;
        refresh();
    }

    /**
     * Gets the density.
     *
     * @return the density
     */
    public int getDensity() {
        return density;
    }

    /**
     * Sets the density.
     *
     * @param density
     *            the density to set
     */
    public void setDensity(int density) {
        if (density != this.density) {
            this.density = density;

            if (parm.getGridInfo().getGridType().equals(GridType.SCALAR)) {
                contourValues = calcContourValuesFromDensity(baseContourValues,
                        density);
            }

            refresh();
        }
    }

    /**
     * Sets the displayable.
     *
     * @param displayable
     *            the displayable to set
     */
    public void setDisplayable(boolean displayable) {
        this.displayable = displayable;
        refresh();
    }

    /**
     * Return the base color
     *
     * @return the baseColor
     */
    public RGB getBaseColor() {
        return baseColor;
    }

    /**
     * Set the base color
     *
     * @param baseColor
     *            the baseColor to set
     */
    public void setBaseColor(RGB baseColor) {
        this.baseColor = baseColor;

        ResourcePair rp = parm.getDataManager().getSpatialDisplayManager()
                .getResourcePair(parm);

        if (rp != null) {
            GFEResource rsc = (GFEResource) rp.getResource();
            rsc.getCapability(ColorableCapability.class).setColor(baseColor);
            refresh(rsc);
        }
    }

    /**
     * Return the visualization mode
     *
     * @return the visMode
     */
    public VisMode getVisMode() {
        if (visMode == null) {
            return VisMode.IMAGE;
        }
        return visMode;
    }

    /**
     * Set the visualization mode
     *
     * @param visMode
     *            the visMode to set
     */
    public void setVisMode(VisMode visMode) {
        this.visMode = visMode;
        refresh();
    }

    /**
     * Return the editor type
     *
     * @return the editorType
     */
    public EditorType getEditorType() {
        if (editorType == null) {
            return EditorType.SPATIAL;
        }
        return editorType;
    }

    /**
     * @param editorType
     *            the editorType to set
     */
    public void setEditorType(EditorType editorType) {
        this.editorType = editorType;
    }

    /**
     * @param editorType
     *            Spatial or Temporal
     * @param visualMode
     *            Image or Graphic
     * @return the visualization type(s)
     */
    public Set<VisualizationType> getVisualizationType(EditorType editorType,
            VisMode visualMode) {

        // if (visualMode.equals(VisMode.NONE)) {
        // return emptyList;
        // }

        Set<VisualizationType> retval = null;
        if (editorType.equals(EditorType.SPATIAL)) {
            if (visualMode.equals(VisMode.IMAGE)) {
                retval = imageSEVisType;
            } else if (visualMode.equals(VisMode.GRAPHIC)) {
                retval = graphicSEVisType;
            }
        } else if (editorType.equals(EditorType.TEMPORAL)) {
            if (visualMode.equals(VisMode.IMAGE)) {
                retval = imageTEVisType;
            } else if (visualMode.equals(VisMode.GRAPHIC)) {
                retval = graphicTEVisType;
            }
        }

        if (retval == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid editorType in availableVisualizationType()");
            retval = new HashSet<>();
        }
        return Collections.unmodifiableSet(retval);
    }

    /**
     * Sets the visualization type for the given editor type and visual mode to
     * those types specified.
     *
     * Verifies that the visual types specified are actually part of the
     * available list. Stores the new visual type(s) in private data. Sends out
     * a notification to parmclients that the visualization type has changed.
     * For NONE type visualMode, this is a no op.
     *
     * @param editorType
     * @param visualMode
     * @param visualType
     */
    public void setVisualizationType(EditorType editorType, VisMode visualMode,
            Set<VisualizationType> visualType) {
        // get the available types and validate request
        // image consists of availableImage + availableGraphic
        Set<VisualizationType> available = EnumSet
                .noneOf(VisualizationType.class);
        available.addAll(getAvailableVisualizationType(editorType, visualMode));
        if (visualMode.equals(VisMode.IMAGE)) {
            available.addAll(
                    getAvailableVisualizationType(editorType, VisMode.GRAPHIC));
        }

        for (VisualizationType type : visualType) {
            if (!available.contains(type)) {
                statusHandler.handle(Priority.PROBLEM,
                        parm.getParmID().toString()
                                + " Attempt to setVisualizationType to invalid: "
                                + type.toString()
                                + "\nAllowable vis types are: "
                                + Arrays.toString(available.toArray()));
                // no action performed
                return;
            }
        }

        if (editorType.equals(EditorType.SPATIAL)) {
            if (visualMode.equals(VisMode.IMAGE)) {
                imageSEVisType = visualType;
            } else if (visualMode.equals(VisMode.GRAPHIC)) {
                graphicSEVisType = visualType;
            }
        } else if (editorType.equals(EditorType.TEMPORAL)) {
            if (visualMode.equals(VisMode.IMAGE)) {
                imageTEVisType = visualType;
            } else if (visualMode.equals(VisMode.GRAPHIC)) {
                graphicTEVisType = visualType;
            }
        } else {
            // logBug << "Invalid editorType in setVisualizationType()" <<
            // std::endl;
            return;
        }

        // send out notification to the parm clients
        // const SeqOfPtr<ParmClient*>& pc = _parm->parmClients();
        // for (i = 0; i < pc.length(); i++)
        // pc[i]->visualizationTypeChanged(_parm, editorType, visualMode,
        // visualType);

        refresh();
    }

    private void refresh() {
        ResourcePair rp = parm.getDataManager().getSpatialDisplayManager()
                .getResourcePair(parm);

        if (rp != null) {
            ((GFEResource) rp.getResource()).reset();
        }
    }

    private void refresh(GFEResource rsc) {
        rsc.reset();
    }

    /**
     * Returns the list of available visualization types for the given editor
     * type (SPATIAL, TEMPORAL) and visual mode (IMAGE, GRAPHIC, NONE). The
     * image list only contains the image ones, you must also access the graphic
     * ones for a complete list.
     *
     * @param editorType
     *            Spatial or Temporal
     * @param visualMode
     *            Image or Graphic
     * @return the available visualization types
     */
    public Set<VisualizationType> getAvailableVisualizationType(
            EditorType editorType, VisMode visualMode) {

        // if (visualMode.equals(VisMode.NONE)) {
        // return emptyList; // no visuals available for the NONE-type
        // }

        Set<VisualizationType> retval = null;
        if (editorType.equals(EditorType.SPATIAL)) {
            if (visualMode.equals(VisMode.IMAGE)) {
                retval = availImageSETypes;
            } else if (visualMode.equals(VisMode.GRAPHIC)) {
                retval = availGraphicSETypes;
            }
        } else if (editorType.equals(EditorType.TEMPORAL)) {
            if (visualMode.equals(VisMode.IMAGE)) {
                retval = availImageTETypes;
            } else if (visualMode.equals(VisMode.GRAPHIC)) {
                retval = availGraphicTETypes;
            }
        }

        if (retval == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Invalid editorType in availableVisualizationType()");
            retval = new HashSet<>();
        }
        return Collections.unmodifiableSet(retval);
    }

    /**
     * @param visMode
     *            the visMode to set
     */
    public void setVisMode(String visMode) {
        this.visMode = VisMode.valueOf(visMode);
        refresh();
    }

    /**
     * @return the display mask
     */
    public Grid2DBit getDisplayMask() {
        if (displayMaskRefData == null) {
            return null;
        }
        return displayMaskRefData.getGrid();
    }

    /**
     * Sets the display mask for this parm. If the new mask does not match the
     * dimensions of the GridInfo then you will get a logBug and the mask will
     * not be changed.
     *
     * Note that this function clears the display mask saved reference data.
     *
     * @param bits
     *            the display mask
     */
    public void setDisplayMask(final Grid2DBit bits) {
        if (bits.getXdim() != parm.getGridInfo().getGridLoc().gridSize().x
                || bits.getYdim() != parm.getGridInfo().getGridLoc()
                        .gridSize().y) {
            statusHandler.handle(Priority.PROBLEM,
                    "ParmDspAttr::setMask(): mask dimensions [" + bits.getXdim()
                            + "," + bits.getYdim() + "] don't match parm "
                            + parm.getGridInfo().getGridLoc().gridSize());
            return;
        }

        if (bits.equals(getDisplayMask())) {
            // no change
            return;
        }

        displayMaskRefData = new ReferenceData(parm.getGridInfo().getGridLoc(),
                new ReferenceID("_PDA_"), bits);

        refresh();
    }

    /**
     * @return the displayMaskRefData
     */
    public ReferenceData getDisplayMaskRefData() {
        return displayMaskRefData;
    }

    /**
     * Sets the display mask for this parm. If the reference id isn't valid or
     * doesn't exist, then the mask is cleared.
     *
     * @param id
     */
    public void setDisplayMask(final ReferenceID id) {
        String message = null;

        /* If name is empty we just set empty display mask */

        /* Otherwise try to find an available ReferenceID with same name */
        if (!id.getName().isEmpty()) {
            ReferenceID refId = null;
            for (ReferenceID refset : parm.getDataManager().getRefManager()
                    .getAvailableSets()) {
                if (refset.getName().equals(id.getName())) {
                    refId = refset;
                    break;
                }
            }

            /* If a matching ReferenceID is found */
            if (refId != null) {
                // get the new mask
                ReferenceData newMask = parm.getDataManager().getRefManager()
                        .loadRefSet(refId);
                /*
                 * If newMask is null there was some kind of problem loading the
                 * reference set that was already logged by MapManager so
                 * nothing to do
                 */

                if (newMask != null) {
                    /* Check if ReferenceData is a query */
                    if (newMask.refType().equals(RefType.QUERY) || newMask
                            .refType().equals(RefType.QUERY_POLYGON)) {
                        message = String.format(
                                "Attempt to setDisplayMask on query-based reference set %s",
                                refId);
                    } else {
                        displayMaskRefData = newMask;
                        setDisplayMask(newMask.getGrid());
                        return;
                    }
                }
            } else {
                /* No matching ReferenceID was found */
                message = String.format(
                        "Attempt to setDisplayMask on unknown ReferenceID [%s]",
                        id);
            }
        }

        if (message != null) {
            statusHandler.error(message);
        }

        setEmptyMask();
    }

    /**
     * Clear the display mask
     */
    public void setEmptyMask() {
        setDisplayMask(
                parm.getDataManager().getRefManager().fullRefSet().getGrid());
    }

    /**
     * Get one of the generic colors.
     * <p>
     * AWIPS 1 kept counts of generic colors in use and always supplied the
     * least used one. We just round-robin them.
     *
     * @return
     */
    private synchronized RGB getDefaultColor() {
        if (genericColors == null) {
            genericColors = GFEPreference.getStringArray("Generic_colors");
            genericNext = 0;
        }

        String color = genericColors[genericNext];

        genericNext++;
        if (genericNext >= genericColors.length) {
            genericNext = 0;
        }

        return RGBColors.getRGBColor(color);
    }
}
