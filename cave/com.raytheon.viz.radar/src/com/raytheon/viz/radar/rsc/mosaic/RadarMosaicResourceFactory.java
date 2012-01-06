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
package com.raytheon.viz.radar.rsc.mosaic;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.ResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.radar.Activator;

/**
 * Use a base resource, a substitution key and a comma separated list of
 * substitution values to generate a resource list. Each resource in the list
 * will be created be serializing the base resource and replacing all
 * occurrences of key with a value from the list and deserializing the result.
 * 
 * This is intended to be uses from a serialized bundle. Since the base resource
 * is deserialized and then serialized again the substitution can only work for
 * String values. Specifically this has been tested to work with Request
 * Constraint Values in a metadata map.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class RadarMosaicResourceFactory extends AbstractMosaicResourceFactory {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(RadarMosaicResourceFactory.class);

    @XmlAttribute(name = "key")
    protected String substitutionKey;

    @XmlAttribute(name = "values")
    protected String substitutionValues;

    @XmlElement
    protected ResourcePair resource;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.ResourceGroup#getResourceList()
     */
    @Override
    public ResourceList getResourceList() {
        try {
            ResourceList resourceList = new ResourceList();
            // Put the base resource in a group so it can be serialized
            ResourceGroup baseGroup = new ResourceGroup();
            baseGroup.getResourceList().add(resource);
            String baseXml = SerializationUtil.marshalToXml(baseGroup);
            resourceList.clear();
            for (String icao : substitutionValues.split(",")) {
                String xml = baseXml.replace(substitutionKey, icao);
                resourceList.add(((ResourceGroup) SerializationUtil
                        .unmarshalFromXml(xml)).getResourceList().get(0));
            }
            return resourceList;
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM, e
                            .getLocalizedMessage(), e);
        }
        return null;
    }

    /**
     * @return the icaoList
     */
    public String getIcaoList() {
        return substitutionValues;
    }

    /**
     * @param icaoList
     *            the icaoList to set
     */
    public void setIcaoList(String icaoList) {
        this.substitutionValues = icaoList;
    }

}
