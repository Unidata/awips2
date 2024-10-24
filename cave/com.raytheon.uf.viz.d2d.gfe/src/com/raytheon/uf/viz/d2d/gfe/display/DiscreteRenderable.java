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
package com.raytheon.uf.viz.d2d.gfe.display;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.JTSCompiler;
import com.raytheon.uf.viz.core.drawables.JTSCompiler.JTSGeometryData;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.d2d.gfe.rsc.data.DiscreteGridData;
import com.raytheon.viz.gfe.colortable.ColorTable.ImageAttr;
import com.raytheon.viz.gfe.colortable.DiscreteColorTable;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 *
 * Renderable for Discrete GFE products being displayed in D2D
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2019  71896      tjensen     Initial creation
 * Dec 13, 2019 72475      tjensen     Refactor colorbar to be a separate resource
 *
 * </pre>
 *
 * @author tjensen
 */
public class DiscreteRenderable implements IRenderable {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DiscreteRenderable.class);

    private final DiscreteGridData discreteData;

    protected Map<Object, IWireframeShape> outlineShapes = new HashMap<>();

    protected Map<Object, Collection<IShadedShape>> shadedShapes = new HashMap<>();

    private final Parm parm;

    private final IMapDescriptor descriptor;

    private final ImagingCapability imagingCap;

    private final DiscreteColorTable colorTable;

    private boolean compiled = false;

    public DiscreteRenderable(DiscreteGridData discreteData, Parm parm,
            IMapDescriptor descriptor, ImagingCapability imagingCap,
            DiscreteColorTable colorTable) {
        this.discreteData = discreteData;
        this.parm = parm;
        this.descriptor = descriptor;
        this.imagingCap = imagingCap;
        this.colorTable = colorTable;
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        if (!compiled) {
            compileShapes(target);
        }

        for (Collection<IShadedShape> shapes : shadedShapes.values()) {
            for (IShadedShape shadedShape : shapes) {
                target.drawShadedShape(shadedShape, paintProps.getAlpha(),
                        imagingCap.getBrightness());
            }
        }

        LineStyle lineStyle = parm.getDisplayAttributes().getLineStyle();
        int lineWidth = parm.getDisplayAttributes().getLineWidth();

        for (Entry<Object, IWireframeShape> entry : outlineShapes.entrySet()) {
            Object defaultKey = DiscreteWxValue.defaultValue(parm)
                    .getDiscreteKey();

            if (!entry.getKey().equals(defaultKey)) {
                target.drawWireframeShape(entry.getValue(),
                        this.parm.getDisplayAttributes().getBaseColor(),
                        lineWidth, lineStyle);
            }

        }
    }

    private void compileShapes(IGraphicsTarget target) throws VizException {

        // Dispose all of the outlineShapes and shadedShapes
        for (IWireframeShape shape : outlineShapes.values()) {
            shape.dispose();
        }
        outlineShapes.clear();

        for (Collection<IShadedShape> shapeList : shadedShapes.values()) {
            for (IShadedShape shadedShape : shapeList) {
                shadedShape.dispose();
            }
            shapeList.clear();
        }
        shadedShapes.clear();

        Grid2DBit mask = parm.getDisplayAttributes().getDisplayMask();

        for (DiscreteKey discreteKey : discreteData.getKeys()) {

            if (discreteKey.isValid()) {
                outlineShapes.put(discreteKey,
                        target.createWireframeShape(false, this.descriptor));

                Collection<IShadedShape> shapeList = new ArrayList<>();
                shadedShapes.put(discreteKey, shapeList);

                WxValue wxValue = new DiscreteWxValue(discreteKey, parm);

                List<ImageAttr> fillAttrs = colorTable.map(wxValue);

                boolean first = true;
                for (ImageAttr attr : fillAttrs) {
                    IShadedShape shadedShape = target.createShadedShape(false,
                            this.descriptor.getGridGeometry());
                    shapeList.add(shadedShape);

                    IWireframeShape outlineShape = first
                            ? outlineShapes.get(discreteKey) : null;
                    first = false;

                    JTSCompiler jtsCompiler = new JTSCompiler(shadedShape,
                            outlineShape, this.descriptor);

                    byte[] fillPattern = FillPatterns
                            .getGLPattern(attr.getFillPatternName());

                    RGB fillColor = RGBColors.getRGBColor(attr.getColorName());
                    JTSGeometryData jtsData = jtsCompiler.createGeometryData();
                    jtsData.setGeometryColor(fillColor);
                    Grid2DBit tmpBit = discreteData.eq(discreteKey).and(mask);

                    ReferenceData refData = new ReferenceData(
                            parm.getGridInfo().getGridLoc(),
                            new ReferenceID("temp"), tmpBit);

                    jtsCompiler.handle(
                            refData.getPolygons(CoordinateType.LATLON),
                            jtsData);
                    shadedShape.compile();
                    shadedShape.setFillPattern(fillPattern);
                }

                outlineShapes.get(discreteKey).compile();
            }
        }
        compiled = true;
    }

    public void dispose() {
        if (outlineShapes != null) {
            for (IWireframeShape shape : outlineShapes.values()) {
                shape.dispose();
            }
            outlineShapes.clear();
        }

        if (shadedShapes != null) {
            for (Collection<IShadedShape> shadedShapeCol : shadedShapes
                    .values()) {
                for (IShadedShape shadedShape : shadedShapeCol) {
                    shadedShape.dispose();
                }
            }
            shadedShapes.clear();
        }
    }
}
