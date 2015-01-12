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
package com.raytheon.uf.viz.xy.varheight.rsc;

import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.alerts.DataCubeAlertMessageParser;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IPointsToolContainer;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.viz.core.rsc.ICombinedResourceData;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Resource data for var height displays
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2009            njensen     Initial creation
 * Aug 15, 2013 2258       bsteffen    Convert profiler sounding to var height
 *                                     with hodo.
 * May 08, 2014 2060       njensen     Constructor sets alert parser
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class VarHeightResourceData extends AbstractRequestableResourceData
        implements IPointsToolContainer, ICombinedResourceData {

    private static final String VAR_HEIGHT_ADAPTER_EXTENSION = "com.raytheon.uf.viz.xy.varheight.varheightadapter";

    @XmlElement
    protected Coordinate point;

    @XmlElement
    protected String source;

    @XmlAttribute
    private String parameter;

    @XmlAttribute
    private String parameterName;

    @XmlAttribute
    private String pointLetter;

    @XmlAttribute
    protected CombineOperation combineOperation;

    @XmlElement
    protected AbstractResourceData secondaryResourceData;

    private AbstractVizResource<?, ?> secondaryResource;

    public VarHeightResourceData() {
        this.setAlertParser(new DataCubeAlertMessageParser());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#construct
     * (com.raytheon.uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {

        if (secondaryResourceData != null) {
            secondaryResource = secondaryResourceData.construct(loadProperties,
                    descriptor);
        }

        return super.construct(loadProperties, descriptor);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#update(java
     * .lang.Object)
     */
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
        VarHeightResource rsc = null;
        if (objects.length > 0) {
            PluginDataObject pdo = objects[0];
            if (point == null && pdo instanceof ISpatialEnabled) {
                /*
                 * This is here to allow more flexibility in bundles, if a
                 * bundle has no point than use the point in the pdo.
                 */
                point = ((ISpatialEnabled) pdo).getSpatialObject()
                        .getGeometry().getCoordinate();
            }
            AbstractVarHeightAdapter<?> adapter = getAdapter(pdo);
            adapter.setResourceData(this);
            rsc = new VarHeightResource(this, loadProperties, adapter);
            if (rsc != null) {
                for (PluginDataObject rec : objects) {
                    rsc.addRecord(rec);
                }
                return rsc;
            }
        }
        throw new VizException(
                "No records retrieved, unable to determine resource type");
    }

    protected AbstractVarHeightAdapter<?> getAdapter(PluginDataObject object)
            throws VizException {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        if (registry == null) {
            throw new VizException("Error loading ExtensionRegistry");
        }
        IExtensionPoint point = registry
                .getExtensionPoint(VAR_HEIGHT_ADAPTER_EXTENSION);
        if (point == null) {
            throw new VizException(
                    "Error loading Extension points for Var Height Adapters");
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
                    for (Class<?> clazz = object.getClass(); clazz != PluginDataObject.class; clazz = clazz
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
                    if (!value.toString().equals(
                            constraint.getAttribute("value"))) {
                        useAdapter = false;
                        break;
                    }
                }
                if (useAdapter) {
                    try {
                        return (AbstractVarHeightAdapter<?>) cfg
                                .createExecutableExtension("adapter");
                    } catch (CoreException e) {
                        throw new VizException(
                                "Error constructing Var Height adapter", e);
                    }
                }

            }
        }

        throw new VizException("No Var Height adapter registered for: "
                + object.getClass().getSimpleName());
    }

    public Coordinate getPoint() {
        return point;
    }

    public void setPoint(Coordinate point) {
        this.point = point;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime
                * result
                + ((combineOperation == null) ? 0 : combineOperation.hashCode());
        result = prime * result
                + ((parameter == null) ? 0 : parameter.hashCode());
        result = prime * result
                + ((parameterName == null) ? 0 : parameterName.hashCode());
        result = prime * result + ((point == null) ? 0 : point.hashCode());
        result = prime * result
                + ((pointLetter == null) ? 0 : pointLetter.hashCode());
        result = prime
                * result
                + ((secondaryResource == null) ? 0 : secondaryResource
                        .hashCode());
        result = prime
                * result
                + ((secondaryResourceData == null) ? 0 : secondaryResourceData
                        .hashCode());
        result = prime * result + ((source == null) ? 0 : source.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        VarHeightResourceData other = (VarHeightResourceData) obj;
        if (combineOperation != other.combineOperation)
            return false;
        if (parameter == null) {
            if (other.parameter != null)
                return false;
        } else if (!parameter.equals(other.parameter))
            return false;
        if (parameterName == null) {
            if (other.parameterName != null)
                return false;
        } else if (!parameterName.equals(other.parameterName))
            return false;
        if (point == null) {
            if (other.point != null)
                return false;
        } else if (!point.equals(other.point))
            return false;
        if (pointLetter == null) {
            if (other.pointLetter != null)
                return false;
        } else if (!pointLetter.equals(other.pointLetter))
            return false;
        if (secondaryResource == null) {
            if (other.secondaryResource != null)
                return false;
        } else if (!secondaryResource.equals(other.secondaryResource))
            return false;
        if (secondaryResourceData == null) {
            if (other.secondaryResourceData != null)
                return false;
        } else if (!secondaryResourceData.equals(other.secondaryResourceData))
            return false;
        if (source == null) {
            if (other.source != null)
                return false;
        } else if (!source.equals(other.source))
            return false;
        return true;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getSource() {
        return source;
    }

    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    public String getParameter() {
        return parameter;
    }

    public void setParameterName(String parameterName) {
        this.parameterName = parameterName;
    }

    public String getParameterName() {
        return parameterName;
    }

    @Override
    public Coordinate getPointCoordinate() {
        return this.point;
    }

    @Override
    public void setPointCoordinate(Coordinate pointCoordinate) {
        this.point = pointCoordinate;
    }

    @Override
    public String getPointLetter() {
        return pointLetter;
    }

    @Override
    public void setPointLetter(String pointLetter) {
        this.pointLetter = pointLetter;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.ICombinedResourceData#getCombineOperation()
     */
    @Override
    public CombineOperation getCombineOperation() {
        return this.combineOperation;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.ICombinedResourceData#getSecondaryData()
     */
    @Override
    public AbstractResourceData getSecondaryData() {
        return secondaryResourceData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.ICombinedResourceData#getSecondaryResource()
     */
    @Override
    public AbstractVizResource<?, ?> getSecondaryResource() {
        return secondaryResource;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.ICombinedResourceData#setCombineOperation(com
     * .raytheon.viz.core.rsc.ICombinedResourceData.CombineOperation)
     */
    @Override
    public void setCombineOperation(CombineOperation operation) {
        this.combineOperation = operation;

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.ICombinedResourceData#setSecondaryData(com.
     * raytheon.uf.viz.core.rsc.AbstractResourceData)
     */
    @Override
    public void setSecondaryData(AbstractResourceData data) {
        this.secondaryResourceData = data;
        ((ICombinedResourceData) this.secondaryResourceData)
                .setCombineOperation(CombineOperation.NONE);
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        DataTime[] times = super.getAvailableTimes();
        for (DataTime time : times) {
            time.setLevelValue(null);
        }
        return times;
    }

}
