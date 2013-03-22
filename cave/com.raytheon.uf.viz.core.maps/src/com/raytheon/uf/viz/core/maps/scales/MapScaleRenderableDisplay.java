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
package com.raytheon.uf.viz.core.maps.scales;

import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.globals.VizGlobalsManager;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.display.PlainMapRenderableDisplay;
import com.raytheon.uf.viz.core.maps.scales.MapScales.MapScale;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.viz.ui.actions.LoadSerializedXml;

/**
 * MapRenderableDisplay associated with a {@link MapScale}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 22, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class MapScaleRenderableDisplay extends PlainMapRenderableDisplay
        implements IMapScaleDisplay {

    @XmlAttribute
    protected String scale = (String) VizGlobalsManager.getCurrentInstance()
            .getPropery(VizConstants.SCALE_ID);

    public MapScaleRenderableDisplay() {
        super();
    }

    public MapScaleRenderableDisplay(IMapDescriptor desc) {
        super(desc);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.maps.scales.IMapScaleDisplay#getScaleName()
     */
    @Override
    public String getScaleName() {
        return scale;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.maps.scales.IMapScaleDisplay#changeScale(com
     * .raytheon.uf.viz.core.maps.scales.MapScales.MapScale)
     */
    @Override
    public void changeScale(MapScale scale) {
        MapScale currentScale = MapScales.getInstance().getScaleByName(
                getScaleName());
        Bundle bundle = (Bundle) LoadSerializedXml.deserialize(currentScale
                .getFile());
        for (AbstractRenderableDisplay display : bundle.getDisplays()) {
            descriptor.getResourceList().removeAll(
                    display.getDescriptor().getResourceList());
        }
        loadScale(scale);
    }

    @Override
    public void clear() {
        MapScale scale = MapScales.getInstance().getScaleByName(getScaleName());
        if (scale == null) {
            scale = MapScales.getInstance().getScaleByName(
                    (String) VizGlobalsManager.getCurrentInstance().getPropery(
                            VizConstants.SCALE_ID));
        }
        if (scale != null) {
            descriptor.getResourceList().clear();
            loadScale(scale);
        }
    }

    protected void loadScale(MapScale scale) {
        Bundle bundle = (Bundle) LoadSerializedXml.deserialize(scale.getFile());
        for (AbstractRenderableDisplay ard : bundle.getDisplays()) {
            try {
                descriptor.setGridGeometry(ard.getDescriptor()
                        .getGridGeometry());
                descriptor.getResourceList().addAll(
                        ard.getDescriptor().getResourceList());
                ard.getDescriptor().getResourceList().clear();
                break;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        descriptor.getResourceList().instantiateResources(descriptor, true);
        this.scale = scale.getDisplayName();
        scaleToClientArea(getBounds());
    }

    @Override
    public Map<String, Object> getGlobalsMap() {
        Map<String, Object> globals = super.getGlobalsMap();
        globals.put(VizConstants.SCALE_ID, getScaleName());
        return globals;
    }

}
