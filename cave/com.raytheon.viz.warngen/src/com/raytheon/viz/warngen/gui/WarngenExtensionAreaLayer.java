package com.raytheon.viz.warngen.gui;

import org.eclipse.core.databinding.observable.ChangeEvent;
import org.eclipse.core.databinding.observable.IChangeListener;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.JTSCompiler;
import com.raytheon.uf.viz.core.drawables.JTSCompiler.JTSGeometryData;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.rsc.AbstractMapResourceData;
import com.raytheon.uf.viz.core.maps.rsc.StyledMapResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ShadeableCapability;
import org.locationtech.jts.geom.Geometry;

/**
 * Displays the area the polygon is allowed to extend into beyond the hatched
 * area.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#    Engineer     Description
 * ------------- ---------- ------------ ---------------------------------------
 * Mar 10, 2015  DCS 18509  D. Friedman  Initial revision
 * Sep 14, 2016  3241       bsteffen     Update deprecated JTSCompiler method
 *                                       calls
 * May 31, 2018  6562       tgurney      Changed to use its own custom resource
 *                                       data class
 * Feb 06, 2019  7728       randerso     Code cleanup.
 *
 * </pre>
 *
 */
public class WarngenExtensionAreaLayer
        extends StyledMapResource<AbstractMapResourceData, MapDescriptor>
        implements IDisposeListener, IChangeListener, AddListener {

    private IShadedShape extensionAreaShadedShape = null;

    private Geometry extensionAreaVis = null;

    private boolean extensionAreaVisDirty = false;

    public WarngenExtensionAreaLayer(
            WarngenExtensionAreaLayerResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        super.resourceChanged(type, object);
        if ((type == ChangeType.CAPABILITY)
                && (object instanceof ColorableCapability)) {
            extensionAreaVisDirty = true;
            issueRefresh();
        }
    }

    @Override
    public void propertiesChanged(ResourceProperties updatedProps) {
        super.propertiesChanged(updatedProps);
        if (warngenLayer != null) {
            warngenLayer.realizeExtensionAreaVisibility();
        }
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        super.project(crs);

        if (extensionAreaShadedShape != null) {
            extensionAreaVisDirty = true;
            issueRefresh();
        }
    }

    @Override
    protected void disposeInternal() {
        getDescriptor().getResourceList().removePostAddListener(this);
        if (extensionAreaShadedShape != null) {
            extensionAreaShadedShape.dispose();
        }
        extensionAreaVis = null;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        ColorableCapability coloring = getCapability(ColorableCapability.class);
        ShadeableCapability shading = getCapability(ShadeableCapability.class);
        if (extensionAreaVisDirty) {
            extensionAreaShadedShape.reset();
            if (extensionAreaVis != null) {
                JTSCompiler comp = new JTSCompiler(extensionAreaShadedShape,
                        null, descriptor);
                Geometry g = extensionAreaVis;
                extensionAreaVisDirty = false;
                if (g != null) {
                    JTSGeometryData data = comp.createGeometryData();
                    data.setGeometryColor(coloring.getColor());
                    comp.handle(g, data);
                }
            }
        }
        target.drawShadedShape(extensionAreaShadedShape, shading.getOpacity());

    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        getDescriptor().getResourceList().addPostAddListener(this);
        extensionAreaShadedShape = target.createShadedShape(true,
                this.descriptor.getGridGeometry());
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                getWarngenLayer();
            }
        });
    }

    private WarngenLayer warngenLayer;

    protected WarngenLayer getWarngenLayer() {
        if (warngenLayer != null) {
            return warngenLayer;
        }

        synchronized (this) {
            if (getDescriptor() != null) {
                for (ResourcePair rp : getDescriptor().getResourceList()) {
                    if (rp.getResource() instanceof WarngenLayer) {
                        warngenLayer = (WarngenLayer) rp.getResource();
                        break;
                    }
                }
            }
            if (warngenLayer != null) {
                warngenLayer.registerListener(this);
                warngenLayer.getObservableExtensionAreaVis()
                        .addChangeListener(this);
                handleChange(null);
            }

            return warngenLayer;
        }
    }

    @Override
    public void disposed(AbstractVizResource<?, ?> rsc) {
        synchronized (this) {
            if (warngenLayer != null) {
                warngenLayer.unregisterListener(this);
                warngenLayer.getObservableExtensionAreaVis()
                        .removeChangeListener(this);
                warngenLayer = null;
                extensionAreaShadedShape.reset();
                extensionAreaVis = null;
            }
        }
    }

    @Override
    public void handleChange(ChangeEvent event) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (warngenLayer != null) {
                    extensionAreaVis = warngenLayer
                            .getObservableExtensionAreaVis().getValue();
                } else {
                    extensionAreaVis = null;
                }
                extensionAreaVisDirty = true;
                issueRefresh();
            }
        });
    }

    @Override
    public void notifyAdd(ResourcePair rp) throws VizException {
        if (warngenLayer == null && rp.getResource() instanceof WarngenLayer) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    getWarngenLayer();
                }
            });
        }
    }

}
