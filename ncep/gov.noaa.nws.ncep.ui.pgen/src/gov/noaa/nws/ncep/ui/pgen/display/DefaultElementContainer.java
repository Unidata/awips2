/*
 * DefaultElementContainer
 * 
 * Date created: 08 DECEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.gfa.IGfa;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.map.IMapDescriptor;

/**
 * A Default Element Container that can be used for most Drawable Elements.
 * Recreation of the IDisplayable objects is only done when zooming or if the
 * layer DisplayProperties change.
 * 
 * @author sgilbert
 * 
 */
public class DefaultElementContainer extends AbstractElementContainer {

    private DisplayProperties saveProps = null;

    private float zoomLevel = 0;

    /**
     * @param element
     * @param mapDescriptor
     * @param target
     */
    public DefaultElementContainer(DrawableElement element,
            IMapDescriptor mapDescriptor, IGraphicsTarget target) {
        super(element, mapDescriptor, target);
        //
    }

    /*
     * Draws to the given graphics target. Recreates the IDisplayable objects if
     * zooming or if the Layer properties change.
     * 
     * @see
     * gov.noaa.nws.ncep.ui.pgen.display.AbstractTBNL#draw(com.raytheon.uf.viz
     * .core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public void draw(IGraphicsTarget target, PaintProperties paintProps,
            DisplayProperties dprops) {
        draw(target, paintProps, dprops, false);
    }

    /*
     * Draws to the given graphics target. Recreates the IDisplayable objects if
     * zooming or if the Layer properties change
     * 
     * @see
     * gov.noaa.nws.ncep.ui.pgen.display.AbstractTBNL#draw(com.raytheon.uf.viz
     * .core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties, boolean)
     */
    @Override
    public void draw(IGraphicsTarget target, PaintProperties paintProps,
            DisplayProperties dprops, boolean needsCreate) {

        if ((displayEls == null) || paintProps.isZooming())
            needsCreate = true;

        if (paintProps.getZoomLevel() != zoomLevel) {
            needsCreate = true;
            zoomLevel = paintProps.getZoomLevel();
        }

        if ((dprops != null) && !dprops.equals(saveProps)) {
            def.setLayerDisplayAttr(dprops.getLayerMonoColor(),
                    dprops.getLayerColor(), dprops.getLayerFilled());
            needsCreate = true;
        } else if (element instanceof IMidCloudText
                || element instanceof IAvnText
                || (element instanceof IText && ((IText) element)
                        .getDisplayType().equals(DisplayType.BOX))
                || element instanceof IGfa || isCCFPArrow(element)) {
            if (PgenSession.getInstance().getPgenResource().isNeedsDisplay()) {
                needsCreate = true;
            }
        }

        if (needsCreate) {
            createDisplayables(paintProps);
        }

        saveProps = dprops;

        for (IDisplayable each : displayEls) {
            each.draw(target, paintProps);
        }

    }

}
