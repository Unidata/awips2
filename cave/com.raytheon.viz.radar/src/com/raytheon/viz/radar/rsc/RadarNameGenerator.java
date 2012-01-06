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
package com.raytheon.viz.radar.rsc;

import java.util.Arrays;
import java.util.regex.Pattern;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfo;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendedCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.VizStyleException;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.raytheon.viz.radar.rsc.graphic.RadarGraphicsResource;

/**
 * Name generator for radar products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 21, 2010            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class RadarNameGenerator extends AbstractNameGenerator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarNameGenerator.class);

    private static final Pattern SITE = Pattern.compile("\\{S\\}");

    private static final Pattern UNIT = Pattern.compile("\\{U\\}");

    private static final Pattern BIT = Pattern.compile("\\{B\\}");

    private static final Pattern TILT = Pattern.compile("\\{T\\}");

    private static final Pattern SEGMENT = Pattern.compile("\\{Sg\\}");

    private static final Pattern PAGE = Pattern.compile("\\{P\\}");

    private static final Pattern PAGE_COUNT = Pattern.compile("\\{P#\\}");

    private static final Pattern CELL_TREND_POINT = Pattern
            .compile("\\{Ctp\\}");

    private static final Pattern HOUR = Pattern.compile("\\{H\\}");

    private final static String MODE = "MODE";

    private final static String MODE_LABEL_SEPARATOR = "=";

    private final static String MODE_DEFINITION_SEPARATOR = ":";

    private final static String MODE_SEPARATOR = ";";

    private String unit = null;

    @SuppressWarnings("unchecked")
    @Override
    public String getName(AbstractVizResource<?, ?> vizRsc) {
        String outputString = "";

        if (vizRsc instanceof AbstractRadarResource<?>) {
            AbstractRadarResource<?> rsc = null;
            if (vizRsc.getDescriptor() instanceof MapDescriptor) {
                rsc = (AbstractRadarResource<MapDescriptor>) vizRsc;
            } else {
                rsc = (AbstractRadarResource<AbstractDescriptor>) vizRsc;
            }

            RadarRecord record = rsc.getRadarRecord(rsc.getDescriptor()
                    .getTimeForResource(rsc));
            if (record == null) {
                return "Radar";
            }
            RadarInfoDict dict = AbstractRadarResource.infoDict;
            RadarInfo info = dict.getInfo(record.getProductCode());
            if (info == null) {
                return "Radar";
            }
            String bit = Integer
                    .toString((int) (Math.log(record.getNumLevels()) / Math
                            .log(2)));
            String hours = "";
            int segment = 0;
            if (record != null && record.getLayer() != null) {
                segment = record.getLayer().intValue();
                switch (segment) {
                case 1:
                case 2:
                case 3:
                    hours = segment + "hr";
                    break;
                case 4:
                    hours = "6hr";
                    break;
                case 5:
                    hours = "12hr";
                    break;
                case 6:
                    hours = "24hr";
                    break;
                default:
                    hours = "Sel";
                }
            }
            int page = 1;
            int pageCount = 1;

            if (rsc instanceof RadarGraphicsResource) {
                RadarGraphicsResource rgRsc = (RadarGraphicsResource) rsc;
                DataTime displayedDate = rgRsc.getDescriptor()
                        .getTimeForResource(rgRsc);
                if (displayedDate == null) {
                    return "Radar";
                }
                if (rgRsc.getRadarGraphicsDisplay().get(displayedDate) != null) {
                    page = rgRsc.getRadarGraphicsDisplay().get(displayedDate)
                            .getCurrentPage() + 1;
                    pageCount = rgRsc.getRadarGraphicsDisplay()
                            .get(displayedDate).getNumPages();
                }
            }
            String point = rsc.getResourceData().getPointID();

            outputString = getResourceLabel(rsc, info);

            outputString = SITE.matcher(outputString).replaceFirst(rsc.icao);
            if (unit == null) {
                unit = getUnit(rsc, record);
            }
            outputString = UNIT.matcher(outputString).replaceFirst(unit);
            outputString = BIT.matcher(outputString).replaceFirst(bit);
            outputString = SEGMENT.matcher(outputString).replaceFirst(
                    Integer.toString(segment));
            outputString = PAGE.matcher(outputString).replaceFirst(
                    Integer.toString(page));
            outputString = PAGE_COUNT.matcher(outputString).replaceFirst(
                    Integer.toString(pageCount));
            outputString = CELL_TREND_POINT.matcher(outputString).replaceFirst(
                    point);
            outputString = TILT.matcher(outputString).replaceFirst(
                    getTilt(rsc, record));
            outputString = HOUR.matcher(outputString).replaceFirst(hours);
        }
        return outputString;

    }

    private static String getTilt(AbstractRadarResource<?> rsc,
            RadarRecord record) {
        String tilt = "";
        if (record != null && record.getTrueElevationAngle() != 0) {
            tilt = String.format("%1.1f ", record.getTrueElevationAngle());
        } else {
            if (rsc.actualLevel.equals("") || rsc.actualLevel.equals("0")
                    || rsc.actualLevel.equals("0.0")) {
                if (rsc.displayedLevel > 0.0) {
                    tilt = String.format("%1.1f ", rsc.displayedLevel);
                }
            } else {
                tilt = rsc.actualLevel;
            }
        }
        return tilt;
    }

    private static String getResourceLabel(AbstractRadarResource<?> rsc,
            RadarInfo info) {
        boolean isBlended = rsc.hasCapability(BlendedCapability.class);
        String outputString = isBlended ? info.getAbrevNameFormat() : info
                .getNameFormat();

        // check for mode specific labeling.
        if (outputString.startsWith(MODE)) {
            String rscMode = rsc.getResourceData().mode;
            boolean useFirstModeAsDefault = (rscMode == null || rscMode
                    .isEmpty());
            String[] modeLabels = outputString.split(MODE_SEPARATOR);

            for (String label : modeLabels) {
                String[] labelParts = label.split(MODE_DEFINITION_SEPARATOR);

                if (labelParts.length == 2) {
                    String[] modeParts = labelParts[0]
                            .split(MODE_LABEL_SEPARATOR);
                    String mode = modeParts[1];

                    if (useFirstModeAsDefault || rscMode.equals(mode)) {
                        outputString = labelParts[1];
                        break;
                    }
                }
            }
        }
        return outputString;
    }

    private static String getUnit(AbstractRadarResource<?> rsc,
            RadarRecord record) {
        boolean colormappable = false;
        if (rsc.hasCapability(ColorMapCapability.class)) {
            // for removing the colormap capability later (for graphics and
            // similar)
            colormappable = true;
        }
        ColorMapParameters params = rsc.getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        String unitName = "";

        // Try to get the unit Name from the Style Rules
        try {
            ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
            match.setParameterName(Arrays.asList(Integer.toString(record
                    .getProductCode())));
            StyleRule sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.IMAGERY, match);
            if (sr != null) {
                ImagePreferences preferences = (ImagePreferences) sr
                        .getPreferences();
                if (preferences.getDisplayUnitLabel() != null
                        && !preferences.getDisplayUnits().equals(Unit.ONE)) {
                    unitName = preferences.getDisplayUnitLabel();
                    // unitName = "(" + unitName + ") ";
                }
            }
        } catch (VizStyleException e1) {
            // Don't worry about it just grab the units from the record.
        }

        // If there was no style rules format the unit in the color map
        // parameters
        if (unitName.equals("") && params != null
                && params.getDisplayUnit() != null) {
            try {
                if (params.getDisplayUnit() != Unit.ONE) {
                    unitName = UnitFormat.getUCUMInstance().format(
                            params.getDisplayUnit());
                    // unitName = "(" + unitName + ") ";
                } else {
                    unitName = "";
                }
            } catch (Throwable e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error formatting display unit", e);
                unitName = "";
            }
        }
        // same as above, save screen space, or set unitName if it is null
        if ((unitName == null) || rsc.hasCapability(BlendedCapability.class)) {
            unitName = "";
        }

        if (!colormappable) {
            rsc.getCapabilities().removeCapability(ColorMapCapability.class);
        }
        return unitName;
    }
}
