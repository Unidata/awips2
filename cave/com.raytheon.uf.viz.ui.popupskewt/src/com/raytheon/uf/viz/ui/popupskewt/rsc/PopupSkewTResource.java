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
package com.raytheon.uf.viz.ui.popupskewt.rsc;

import java.util.List;
import java.util.Map;

import javax.measure.Measure;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.eclipse.jface.action.IMenuManager;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.ui.popupskewt.config.SoundingSource;
import com.raytheon.uf.viz.ui.popupskewt.ui.PopupSkewTConfigAction;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Resource that interacts with other resources to plot height and temperature
 * on a popup skewT dialog. Is enabled through inspecting/interrogating.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2013       2190 mschenke    Initial creation
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PopupSkewTResource extends
        AbstractVizResource<PopupSkewTResourceData, IDescriptor> implements
        IVerticalSoundingProvider, IContextMenuContributor {

    public static final String HEIGHT_INTERROGATE_ID = "height";

    public static final String DATA_VALUE_INTERROGATE_ID = "dataValue";

    private static class InterrogateResult {

        private final AbstractVizResource<?, ?> interrogatedResouce;

        /** Height in kft */
        private final Float height;

        /** Temperature in deg C */
        private final Float temperature;

        private final DataTime dataTime;

        public InterrogateResult(AbstractVizResource<?, ?> interrogatedResouce,
                DataTime dataTime, Float height, Float temperature) {
            this.interrogatedResouce = interrogatedResouce;
            this.height = height;
            this.temperature = temperature;
            this.dataTime = dataTime;
        }

        public AbstractVizResource<?, ?> getInterrogatedResource() {
            return interrogatedResouce;
        }

        public float getHeight() {
            return height;
        }

        public float getTemperature() {
            return temperature != null ? temperature : Float.NaN;
        }

        public DataTime getDataTime() {
            return dataTime;
        }

    }

    private PopupSkewTContainer container = PopupSkewTContainer
            .createContainer(this);

    /** Last calculated sounding */
    private VerticalSounding sounding;

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected PopupSkewTResource(PopupSkewTResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        if (resourceData.isSystem() == false) {
            setPopupSkewTOn(true);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        PopupSkewTContainer.disposeContainer(container);
        container = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (resourceData.isSystem() == false) {
            container.setPopupSkewTOn(true);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (container == null) {
            container = PopupSkewTContainer.createContainer(this);
            if (resourceData.isSystem() == false) {
                container.setPopupSkewTOn(true);
            }
        }
    }

    @Override
    public String getName() {
        String name = super.getName();
        DataTime cur = descriptor.getFramesInfo().getCurrentFrame();
        if (cur != null) {
            name += " " + cur.getLegendString();
        }
        return name;
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        if (isPopupSkewTOn()) {
            FramesInfo currInfo = descriptor.getFramesInfo();
            DataTime soundingTime = currInfo.getCurrentFrame();
            InterrogateResult result = getSkewTParameters(
                    descriptor.getResourceList(), coord);

            try {
                if (result != null) {
                    container.plotHeight(result.getHeight(),
                            result.getTemperature());
                    if (result.getDataTime() != null) {
                        soundingTime = result.getDataTime();
                    }
                }

                VerticalSounding sounding = getSounding(soundingTime,
                        coord.asLatLon());
                if (sounding != null && sounding.size() > 0) {
                    container.plotSounding(sounding);
                }
            } catch (Exception e) {
                throw new VizException(e);
            }
        }
        return resourceData.isSystem() ? super.inspect(coord) : "_";
    }

    /**
     * Gets the best interrogation result to plot.
     * 
     * @param resourceList
     * @param coord
     * @param currResult
     * @return
     */
    private InterrogateResult getSkewTParameters(ResourceList resourceList,
            ReferencedCoordinate coord) {
        FramesInfo currInfo = descriptor.getFramesInfo();
        for (ResourcePair rp : resourceList) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc != null && rsc != this && rp.getProperties().isVisible()) {
                try {
                    Map<String, Object> dataMap = rsc.interrogate(coord);
                    if (dataMap != null) {
                        // Look for height
                        Float height = getValue(dataMap, HEIGHT_INTERROGATE_ID,
                                SI.METER);
                        if (height != null) {
                            // Check for temperature
                            Float temperature = getValue(dataMap,
                                    DATA_VALUE_INTERROGATE_ID, SI.CELSIUS);

                            // Get the dataTime for the resource
                            DataTime dataTime = (DataTime) dataMap
                                    .get(DataTime.class.toString());
                            if (dataTime == null) {
                                dataTime = currInfo.getTimeForResource(rsc);
                            }

                            return new InterrogateResult(rsc, dataTime, height,
                                    temperature);
                        }
                    }
                } catch (VizException e) {
                    // Ignore exception and move to next resource
                }
            }
        }

        List<AbstractVizResource<?, ?>> groups = resourceList
                .getResourcesByType(IResourceGroup.class);
        for (AbstractVizResource<?, ?> group : groups) {
            if (group.getProperties().isVisible()) {
                InterrogateResult result = getSkewTParameters(
                        ((IResourceGroup) group).getResourceList(), coord);
                if (result != null) {
                    return result;
                }
            }
        }

        return null;
    }

    private Float getValue(Map<String, Object> dataMap, String ID,
            Unit<?> expectedUnit) {
        Float expectedValue = null;
        Object obj = dataMap.get(ID);
        if (obj instanceof Measure) {
            Measure<?, ?> measure = (Measure<?, ?>) obj;
            if (measure.getValue() instanceof Number
                    && measure.getUnit().isCompatible(expectedUnit)) {
                Number value = (Number) measure.getValue();
                expectedValue = (float) measure.getUnit()
                        .getConverterTo(expectedUnit)
                        .convert(value.doubleValue());
            }
        }
        return expectedValue;
    }

    public SoundingSource getSource() {
        return container.getSoundingSource();
    }

    public void setSource(SoundingSource soundingSource) {
        if (getSource() != soundingSource) {
            container.setSoundingSource(soundingSource);
        }
    }

    public boolean isPopupSkewTOn() {
        return container.isPopupSkewTOn();
    }

    public void setPopupSkewTOn(boolean popupSkewTOn) {
        if (resourceData.isSystem()) {
            container.setPopupSkewTOn(popupSkewTOn);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider#getSounding
     * (com.raytheon.uf.common.time.DataTime,
     * com.vividsolutions.jts.geom.Coordinate)
     */
    @Override
    public VerticalSounding getSounding(DataTime requestTime,
            Coordinate location) {
        SoundingSource soundingSource = container.getSoundingSource();
        IVerticalSoundingProvider soundingProvider = container
                .getSoundingProvider();
        if (soundingSource != null && soundingProvider != null
                && requestTime != null) {
            if (sounding == null
                    || sounding.getDataTime().equals(requestTime) == false
                    || sounding.getLongitude() != location.x
                    || sounding.getLatitude() != location.y) {
                VerticalSounding sounding = new VerticalSounding();
                sounding.setStationId(getName());
                DataTime[] times = soundingProvider.getSoundingTimes();
                if (times != null) {
                    long timeInterval = soundingSource.getValidTimeInterval()
                            * TimeUtil.MILLIS_PER_MINUTE;

                    DataTime timeToUse = null;
                    long minDiff = Long.MAX_VALUE;
                    for (DataTime dt : times) {
                        long diff = Math.abs(requestTime.getMatchValid()
                                - dt.getMatchValid());
                        if (diff < minDiff && diff <= timeInterval) {
                            minDiff = diff;
                            timeToUse = dt;
                        } else if (diff == minDiff
                                && dt.getMatchRef() > timeToUse.getMatchRef()) {
                            minDiff = diff;
                            timeToUse = dt;
                        }
                    }

                    if (timeToUse != null) {
                        VerticalSounding providerSounding = soundingProvider
                                .getSounding(timeToUse, location);
                        if (providerSounding != null) {
                            sounding = providerSounding;
                        }
                    }
                }
                sounding.setLongitude((float) location.x);
                sounding.setLatitude((float) location.y);
                sounding.setDataTime(requestTime);
                this.sounding = sounding;
            }
            return this.sounding;
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider#
     * getSoundingTimes()
     */
    @Override
    public DataTime[] getSoundingTimes() {
        IVerticalSoundingProvider soundingProvider = container
                .getSoundingProvider();
        if (soundingProvider != null) {
            return soundingProvider.getSoundingTimes();
        }
        return new DataTime[0];
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider#
     * getSoundingSource()
     */
    @Override
    public String getSoundingSource() {
        IVerticalSoundingProvider soundingProvider = container
                .getSoundingProvider();
        if (soundingProvider != null) {
            return soundingProvider.getSoundingSource();
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IContextMenuContributor#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager, int, int)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (resourceData.isSystem()) {
            // check for non-system resource on same descriptor
            for (PopupSkewTResource rsc : descriptor.getResourceList()
                    .getResourcesByTypeAsType(PopupSkewTResource.class)) {
                if (rsc.getResourceData().isSystem() == false) {
                    // A non-system resource lives on descriptor, don't add menu
                    return;
                }
            }
        }
        menuManager.add(new PopupSkewTConfigAction(resourceData
                .getContextMenuName(), this));
    }

}
