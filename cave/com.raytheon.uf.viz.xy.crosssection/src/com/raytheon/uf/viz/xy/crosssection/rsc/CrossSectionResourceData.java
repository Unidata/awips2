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
package com.raytheon.uf.viz.xy.crosssection.rsc;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlTransient;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.alerts.DataCubeAlertMessageParser;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;

/**
 * Resource data for cross sections
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009            njensen     Initial creation
 * May 08, 2014 2060       njensen     Constructor sets alert parser
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class CrossSectionResourceData extends AbstractRequestableResourceData {

    private static final String CROSS_SECTION_ADAPTER_EXTENSION = "com.raytheon.uf.viz.xy.crosssection.crosssectionadapter";

    @XmlAttribute
    protected String parameter;

    @XmlAttribute
    protected String parameterName;

    @XmlAttribute
    protected List<String> stationIDs;

    @XmlAttribute
    private String source;

    @XmlTransient
    private int numLines;

    private Set<DataTime> blackListedTimes = new HashSet<DataTime>();

    public CrossSectionResourceData() {
        this.setAlertParser(new DataCubeAlertMessageParser());
    }

    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        if (descriptor instanceof CrossSectionDescriptor) {
            numLines = ((CrossSectionDescriptor) descriptor).getLines().size();
        }
        return super.construct(loadProperties, descriptor);
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        AbstractCrossSectionAdapter<?> adapter = getAdapter(objects[0]);
        adapter.setResourceData(this);
        AbstractCrossSectionResource resource = null;
        DisplayType displayType = loadProperties.getCapabilities()
                .getCapability(this, DisplayTypeCapability.class)
                .getDisplayType();
        if (displayType.equals(DisplayType.IMAGE)) {
            resource = new CrossSectionImageResource(this, loadProperties,
                    adapter);
        } else if (displayType.equals(DisplayType.ARROW)
                || displayType.equals(DisplayType.BARB)) {
            resource = new CrossSectionVectorResource(this, loadProperties,
                    adapter);
        }
        if (resource == null) {
            resource = new CrossSectionContourResource(this, loadProperties,
                    adapter);
        }
        for (PluginDataObject pdo : objects) {
            resource.addRecord(pdo);
        }
        return resource;
    }

    public String getParameter() {
        return parameter;
    }

    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getParameterName() {
        return parameterName;
    }

    public void setParameterName(String parameterName) {
        this.parameterName = parameterName;
    }

    protected AbstractCrossSectionAdapter<?> getAdapter(PluginDataObject object)
            throws VizException {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        if (registry == null) {
            throw new VizException("Error loading ExtensionRegistry");
        }
        IExtensionPoint point = registry
                .getExtensionPoint(CROSS_SECTION_ADAPTER_EXTENSION);
        if (point == null) {
            throw new VizException(
                    "Error loading Extension points for Cross Section Adapters");
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
                        return (AbstractCrossSectionAdapter<?>) cfg
                                .createExecutableExtension("adapter");
                    } catch (CoreException e) {
                        throw new VizException(
                                "Error constructing Cross Section adapter", e);
                    }
                }

            }
        }

        throw new VizException("No Cross Section adapter registered for: "
                + object.getClass().getSimpleName());
    }

    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        DataTime[] times = super.getAvailableTimes();
        List<DataTime> newTimes = new ArrayList<DataTime>();
        for (int i = 0; i < times.length; ++i) {
            DataTime time = times[i];
            if (blackListedTimes.contains(time) == false) {
                for (int j = 0; j < numLines; ++j) {
                    DataTime newTime = time.clone();
                    newTime.setLevelValue(new Double(j));
                    newTimes.add(newTime);
                }
            }
        }
        return newTimes.toArray(new DataTime[] {});

    }

    @Override
    public PluginDataObject[] getLatestPluginDataObjects(DataTime[] desired,
            DataTime[] current) throws VizException {
        if (desired == null) {
            return new PluginDataObject[0];
        }
        DataTime[] stripped = new DataTime[desired.length];
        for (int i = 0; i < desired.length; ++i) {
            if (desired[i] != null) {
                stripped[i] = desired[i].clone();
                stripped[i].setLevelValue(null);
            }
        }

        DataTime[] sc = new DataTime[current.length];
        for (int i = 0; i < current.length; ++i) {
            if (current[i] != null) {
                sc[i] = current[i].clone();
                sc[i].setLevelValue(null);
            }
        }

        return super.getLatestPluginDataObjects(stripped, sc);
    }

    /**
     * @return the stationIDs
     */
    public List<String> getStationIDs() {
        return stationIDs;
    }

    /**
     * @param stationIDs
     *            the stationIDs to set
     */
    public void setStationIDs(List<String> stationIDs) {
        this.stationIDs = stationIDs;
    }

    public void blackListTime(DataTime time) {
        DataTime cloned = time.clone();
        cloned.setLevelValue(null);
        blackListedTimes.add(cloned);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result
                + ((parameter == null) ? 0 : parameter.hashCode());
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
        CrossSectionResourceData other = (CrossSectionResourceData) obj;
        if (parameter == null) {
            if (other.parameter != null)
                return false;
        } else if (!parameter.equals(other.parameter))
            return false;
        if (source == null) {
            if (other.source != null)
                return false;
        } else if (!source.equals(other.source))
            return false;
        return true;
    }

}
