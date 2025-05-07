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
package com.raytheon.uf.viz.xy.timeseries.rsc;

import java.util.Map;
import java.util.Objects;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.adapter.CoordAdapter;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.alerts.DataCubeAlertMessageParser;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.groups.ICombinedResourceData;
import com.raytheon.uf.viz.d2d.core.procedures.IPointsToolContainer;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;

/**
 * Resource data for time series graphs
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 23, 2009            njensen     Initial creation
 * May 08, 2014 2060       njensen     Constructor sets alert parser
 * Jul 17, 2015 4220       mapeters    Added hashCode(), AxisParameter.equals()/hashCode()
 * Apr 15, 2019 7596       tgurney     Added xml adapter for Coordinate class.
 * Jul 24, 2024 2037624    mapeters    Replace isObjectsEqual() usage
 *
 * </pre>
 *
 * @author njensen
 */
@XmlAccessorType(XmlAccessType.NONE)
public class TimeSeriesResourceData extends AbstractRequestableResourceData
        implements IPointsToolContainer, ICombinedResourceData {

    private static final String TIME_SERIES_ADAPTER_EXTENSION = "com.raytheon.uf.viz.xy.timeseries.timeseriesadapter";

    static public class AxisParameter {
        /** short code (key) for the parameter */
        public String code;

        /** long name for the parameter */
        public String name;

        @Override
        public boolean equals(Object object) {
            if (this == object) {
                return true;
            }
            if (this.getClass() != object.getClass()) {
                return false;
            }
            AxisParameter otherAxisParameter = (AxisParameter) object;
            if (!Objects.equals(this.code, otherAxisParameter.code)) {
                return false;
            }
            if (!Objects.equals(this.name, otherAxisParameter.name)) {
                return false;
            }

            return true;
        }

        @Override
        public int hashCode() {
            return Objects.hash(code, name);
        }
    }

    /**
     * Parameter to use along the y axis.
     */
    @XmlElement
    private AxisParameter yParameter;

    /**
     * Parameter to use along the x axis.
     */
    @XmlElement
    private AxisParameter xParameter;

    @XmlAttribute
    private String source;

    @XmlElement
    @XmlJavaTypeAdapter(value = CoordAdapter.class)
    private Coordinate coordinate;

    @XmlAttribute
    private String pointLetter;

    @XmlAttribute
    private String levelKey;

    @XmlAttribute
    protected CombineOperation combineOperation;

    @XmlElement
    protected AbstractResourceData secondaryResourceData;

    private AbstractVizResource<?, ?> secondaryResource;

    public TimeSeriesResourceData() {
        this.setAlertParser(new DataCubeAlertMessageParser());
    }

    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        if (secondaryResourceData != null) {
            secondaryResource = secondaryResourceData.construct(loadProperties,
                    descriptor);
        }
        return super.construct(loadProperties, descriptor);
    }

    @Override
    public void update(Object updateData) {
        if (secondaryResourceData != null) {
            secondaryResourceData.update(updateData);
        }
        super.update(updateData);
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        if (objects.length > 0) {
            AbstractTimeSeriesAdapter<?> adapter = getAdapter(objects[0]);
            adapter.setResourceData(this);
            TimeSeriesResource resource = new TimeSeriesResource(this,
                    loadProperties, adapter);
            for (PluginDataObject object : objects) {
                resource.addRecord(object);
            }
            return resource;
        }
        throw new VizException(
                "No records retrieved, unable to determine resource type");

    }

    /**
     * @param pluginDataObject
     * @return
     */
    private AbstractTimeSeriesAdapter<?> getAdapter(PluginDataObject object)
            throws VizException {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        if (registry == null) {
            throw new VizException("Error loading ExtensionRegistry");
        }
        IExtensionPoint point = registry
                .getExtensionPoint(TIME_SERIES_ADAPTER_EXTENSION);
        if (point == null) {
            throw new VizException(
                    "Error loading Extension points for Time Series Adapters");
        }
        Map<String, Object> uriFields = RecordFactory.getInstance()
                .loadMapFromUri(object.getDataURI());
        IExtension[] extensions = point.getExtensions();

        for (IExtension ext : extensions) {
            IConfigurationElement[] config = ext.getConfigurationElements();

            for (IConfigurationElement cfg : config) {
                boolean useAdapter = false;
                String targetClass = cfg.getAttribute("class");
                for (Class<?> clazz : object.getClass().getInterfaces()) {
                    if (clazz.getName().equals(targetClass)) {
                        useAdapter = true;
                        break;
                    }
                }
                if (!useAdapter) {
                    for (Class<?> clazz = object
                            .getClass(); clazz != PluginDataObject.class; clazz = clazz
                                    .getSuperclass()) {
                        if (clazz.getName().equals(targetClass)) {
                            useAdapter = true;
                            break;
                        }
                    }
                }

                IConfigurationElement[] constraints = cfg
                        .getChildren("constraint");
                for (IConfigurationElement constraint : constraints) {
                    Object value = uriFields
                            .get(constraint.getAttribute("key"));
                    if (value == null) {
                        value = "null";
                    }
                    if (!value.toString()
                            .equals(constraint.getAttribute("value"))) {
                        useAdapter = false;
                        break;
                    }
                }
                if (useAdapter) {
                    try {
                        return (AbstractTimeSeriesAdapter<?>) cfg
                                .createExecutableExtension("adapter");
                    } catch (CoreException e) {
                        throw new VizException(
                                "Error constructing Time Series adapter", e);
                    }
                }

            }
        }

        throw new VizException("No Time Series adapter registered for: "
                + object.getClass().getSimpleName());
    }

    public void setCoordinate(Coordinate pointCoordinate) {
        this.coordinate = pointCoordinate;
    }

    public Coordinate getCoordinate() {
        return this.coordinate;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    @Override
    public String getPointLetter() {
        return pointLetter;
    }

    @Override
    public void setPointLetter(String pointLetter) {
        this.pointLetter = pointLetter;
    }

    public AxisParameter getYParameter() {
        return yParameter;
    }

    public void setYParameter(AxisParameter parameter) {
        yParameter = parameter;
    }

    public AxisParameter getXParameter() {
        return xParameter;
    }

    public void setXParameter(AxisParameter parameter) {
        xParameter = parameter;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        TimeSeriesResourceData other = (TimeSeriesResourceData) obj;
        if (!Objects.equals(coordinate, other.coordinate)) {
            return false;
        }
        if (!Objects.equals(pointLetter, other.pointLetter)) {
            return false;
        }
        if (!Objects.equals(source, other.source)) {
            return false;
        }
        if (!Objects.equals(yParameter, other.yParameter)) {
            return false;
        }

        if (!Objects.equals(secondaryResourceData,
                other.secondaryResourceData)) {
            return false;
        }

        return true;
    }

    @Override
    public int hashCode() {
        return Objects.hash(coordinate, pointLetter, source, yParameter,
                secondaryResourceData);
    }

    public void setLevelKey(String levelKey) {
        this.levelKey = levelKey;
    }

    public String getLevelKey() {
        return levelKey;
    }

    @Override
    public Coordinate getPointCoordinate() {
        return coordinate;
    }

    @Override
    public void setPointCoordinate(Coordinate pointCoordinate) {
        this.coordinate = pointCoordinate;
    }

    @Override
    public CombineOperation getCombineOperation() {
        return this.combineOperation;
    }

    @Override
    public AbstractResourceData getSecondaryData() {
        return this.secondaryResourceData;
    }

    @Override
    public AbstractVizResource<?, ?> getSecondaryResource() {
        return this.secondaryResource;
    }

    @Override
    public void setCombineOperation(CombineOperation operation) {
        this.combineOperation = operation;

    }

    @Override
    public void setSecondaryData(AbstractResourceData data) {
        this.secondaryResourceData = data;
        ((ICombinedResourceData) secondaryResourceData)
                .setCombineOperation(CombineOperation.NONE);
    }

}
