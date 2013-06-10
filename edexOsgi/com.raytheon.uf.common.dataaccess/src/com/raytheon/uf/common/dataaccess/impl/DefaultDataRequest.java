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
package com.raytheon.uf.common.dataaccess.impl;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.serialization.XmlGenericMapAdapter;
import com.raytheon.uf.common.serialization.adapters.JTSEnvelopeAdapter;
import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * An default request for requesting data through the Data Access Framework.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 6, 2012            njensen     Initial creation
 * Feb 14, 2013 1614       bsteffen    Refactor data access framework to use
 *                                     single request.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class DefaultDataRequest implements IDataRequest {

    @XmlElement
    protected String datatype;

    @XmlJavaTypeAdapter(value = XmlGenericMapAdapter.class)
    protected Map<String, Object> identifiers;

    @XmlElement(name = "parameter")
    protected String[] parameters;

    @XmlElement(name = "level")
    protected Level[] levels;

    @XmlElement(name = "locationName")
    protected String[] locationNames;
    
    @XmlJavaTypeAdapter(value = JTSEnvelopeAdapter.class)
    protected Envelope envelope;

    public void setDatatype(String datatype) {
        this.datatype = datatype;
    }

    public void addIdentifier(String key, Object value) {
        if (identifiers == null) {
            identifiers = new HashMap<String, Object>();
        }
        identifiers.put(key, value);
    }

    public void setParameters(String... params) {
        this.parameters = params;
    }

    public void setLevels(Level... levels) {
        this.levels = levels;
    }
    
    public void setLocationNames(String... locationNames) {
        this.locationNames = locationNames;

    }

    public void setEnvelope(Envelope env) {
        this.envelope = env;
    }

    public String getDatatype() {
        return datatype;
    }

    public Map<String, Object> getIdentifiers() {
        return identifiers;
    }

    public String[] getParameters() {
        return parameters;
    }

    public Level[] getLevels() {
        return levels;
    }

    public String[] getLocationNames() {
        return locationNames;
    }
    
    public Envelope getEnvelope() {
        return envelope;
    }


}
