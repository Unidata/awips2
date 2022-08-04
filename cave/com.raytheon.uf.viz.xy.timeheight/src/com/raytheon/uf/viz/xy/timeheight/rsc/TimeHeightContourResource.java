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
package com.raytheon.uf.viz.xy.timeheight.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.contour.ContourPreferences;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.rsc.groups.ICombinedResourceData.CombineOperation;
import com.raytheon.uf.viz.xy.timeheight.display.TimeHeightDescriptor;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.viz.core.contours.ContourSupport;
import com.raytheon.viz.core.contours.ContourSupport.ContourGroup;

/**
 * Resource for displaying cross sections as contours
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 04, 2007  625      njensen   Initial creation
 * Feb 20, 2009  1960     njensen   Refactored to new rsc architecture
 * Feb 09, 2011  8244     bkowal    Enabled the magnification capability.
 * Aug 12, 2016  5822     bsteffen  add disposeRenderables for handling data
 *                                  refresh.
 * Aug 12, 2016  5822     bsteffen  implement disposeRenderables
 * Feb 28, 2018  7231     njensen   Use source as creating entity to get style rule
 * Apr 12, 2018  7264     njensen   Use OutlineCapability/LineStyle
 * 
 * </pre>
 * 
 * @author njensen
 */
public class TimeHeightContourResource extends AbstractTimeHeightResource {

    private static final double ZOOM_REACTION_FACTOR = .45;

    private static final int NUMBER_CONTOURING_LEVELS = 5;

    private static final double[] ZOOM_THRESHOLDS = new double[NUMBER_CONTOURING_LEVELS];

    private ContourPreferences contourPrefs;

    private ContourGroup[] contours = null;

    private IFont timeHeightFont = null;

    static {
        for (int i = 0; i < NUMBER_CONTOURING_LEVELS; i++) {
            ZOOM_THRESHOLDS[i] = Math.pow(ZOOM_REACTION_FACTOR, i);
        }
    }

    public TimeHeightContourResource(TimeHeightResourceData data,
            LoadProperties props, AbstractVarHeightAdapter<?> adapter) {
        super(data, props, adapter);

        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(null);
        List<String> paramList = new ArrayList<>();
        paramList.add(resourceData.getParameter());
        match.setParameterName(paramList);
        List<String> creatingEntities = new ArrayList<>();
        creatingEntities.add(resourceData.getSource());
        match.setCreatingEntityNames(creatingEntities);
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance()
                    .getStyleRule(StyleManager.StyleType.CONTOUR, match);
        } catch (StyleException e) {
            statusHandler.error("Error getting contour style rules", e);
        }
        if (sr != null) {
            prefs = contourPrefs = (ContourPreferences) sr.getPreferences();
        }
        getCapability(DisplayTypeCapability.class)
                .setAlternativeDisplayTypes(Arrays.asList(DisplayType.IMAGE));
    }

    @Override
    protected void disposeRenderables() {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (timeHeightFont != null) {

                    timeHeightFont.dispose();
                    timeHeightFont = null;
                }
                if (contours != null) {
                    for (ContourGroup cg : contours) {
                        if (cg != null) {
                            cg.posValueShape.dispose();
                            cg.negValueShape.dispose();
                        }
                    }
                    contours = null;
                }
                issueRefresh();
            }

        });
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        super.paintInternal(target, paintProps);
        if (interpolatedData == null) {
            return;
        }
        if (secondaryResource != null) {
            secondaryResource.paint(target, paintProps);
            if (secondaryResource.interpolatedData == null) {
                return;
            }
        }

        OutlineCapability lineCap = getCapability(OutlineCapability.class);

        LineStyle posLineStyle = null;
        LineStyle negLineStyle = null;
        if (lineCap.getLineStyle() == LineStyle.DEFAULT) {
            posLineStyle = LineStyle.SOLID;
            negLineStyle = LineStyle.DASHED_LARGE;
        } else {
            posLineStyle = lineCap.getLineStyle();
            negLineStyle = lineCap.getLineStyle();
        }

        if (contours == null) {
            contours = new ContourGroup[NUMBER_CONTOURING_LEVELS];
        }

        double density = getCapability(DensityCapability.class).getDensity();
        int level = 0;
        double zoom = paintProps.getZoomLevel();
        for (level = NUMBER_CONTOURING_LEVELS - 1; level > 0; level--) {
            if (zoom < ZOOM_THRESHOLDS[level]) {
                break;
            }
        }

        if (contours[level] == null || density != contours[level].lastDensity) {
            if (contours[level] != null) {
                contours[level].posValueShape.dispose();
                contours[level].negValueShape.dispose();
            }

            if (secondaryResource != null
                    && combineOperation != CombineOperation.NONE) {
                interpolatedData = combineResourceData(interpolatedData,
                        secondaryResource.interpolatedData);
            }

            if (descriptor.getGraph(this).isReady()) {
                contours[level] = ContourSupport.createContours(
                        interpolatedData, level,
                        descriptor.getGraph(this).getExtent(), density,
                        geometry, target, contourPrefs);
            }
        }
        RGB color = getCapability(ColorableCapability.class).getColor();

        if (combineOperation != CombineOperation.NONE
                && contours[level] != null) {
            // Determine the magnification for the contour text
            Double magnification = getCapability(MagnificationCapability.class)
                    .getMagnification();
            if (this.timeHeightFont == null) {
                this.timeHeightFont = target.getDefaultFont()
                        .deriveWithSize(target.getDefaultFont().getFontSize());
            }

            this.timeHeightFont.setMagnification(magnification.floatValue());
            contours[level].drawContours(target, color,
                    lineCap.getOutlineWidth(), posLineStyle, negLineStyle,
                    this.timeHeightFont, null);
        }
    }

    @Override
    public void setDescriptor(TimeHeightDescriptor descriptor) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (contours != null) {
                    for (ContourGroup cg : contours) {
                        if (cg != null) {
                            cg.posValueShape.dispose();
                            cg.negValueShape.dispose();
                        }
                    }
                    contours = null;
                }
            }
        });
        super.setDescriptor(descriptor);
    }

}
