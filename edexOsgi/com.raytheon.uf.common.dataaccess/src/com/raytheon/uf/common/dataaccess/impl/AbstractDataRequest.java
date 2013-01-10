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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.serialization.XmlGenericMapAdapter;

/**
 * 
 * An abstract request for requesting data through the Data Access Framework.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 6, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractDataRequest {

    @XmlElement
    protected String datatype;

    @XmlJavaTypeAdapter(value = XmlGenericMapAdapter.class)
    protected Map<String, Object> identifiers;

    @XmlElement
    protected String[] parameters;

    @XmlElement
    protected Level[] levels;

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

}
