package com.raytheon.uf.common.datadelivery.retrieval.xml;

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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Service Config
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20 Oct, 2012   1163      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(name = "serviceConfig")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ServiceConfig implements ISerializableObject {

    private static final String ALTERNATE_NAMING_SCHEMA = "ALTERNATE_NAMING_SCHEMA";

    @XmlAttribute(name = "name")
    @DynamicSerializeElement
    private String name;

    @XmlElement(name = "dateConfig", type = DateConfig.class)
    @DynamicSerializeElement
    private DateConfig dateConfig;

    @XmlElement(name = "dataSetConfig", type = DataSetConfig.class)
    @DynamicSerializeElement
    private DataSetConfig dataSetConfig;

    @XmlElements({ @XmlElement(name = "constant", type = Constant.class) })
    @DynamicSerializeElement
    private List<Constant> constant;

    private Map<String, Constant> constants = null;

    private Map<String, Constant> namingSchemas = null;

    public ServiceConfig() {

    }

    /**
     * Creates the constants for speed
     */
    private void createConstantsMap() {
        if (constants == null) {
            constants = new HashMap<String, Constant>();
            for (Constant con : getConstant()) {
                constants.put(con.getName(), con);
            }
        }
    }

    public List<Constant> getConstant() {
        return constant;
    }

    /**
     * Gets the constant by name
     * 
     * @param name
     * @return
     */
    public Constant getConstantByName(String name) {

        if (constants == null) {
            createConstantsMap();
        }

        return constants.get(name);
    }

    /**
     * Gets the constants value
     * 
     * @param name
     * @return
     */
    public String getConstantValue(String name) {

        Constant cons = getConstantByName(name);

        if (cons != null) {
            return cons.getValue();
        }

        return null;
    }

    public DataSetConfig getDataSetConfig() {
        return dataSetConfig;
    }

    public DateConfig getDateConfig() {
        return dateConfig;
    }

    public String getName() {
        return name;
    }

    /**
     * Gets the constant for this naming schema
     * 
     * @param schemaName
     * @return
     */
    public Constant getNamingSchema(String schemaName) {
        return getNamingSchemas().get(schemaName);
    }

    /**
     * Speed the searching of naming schemas
     * 
     * @return
     */
    private Map<String, Constant> getNamingSchemas() {

        if (namingSchemas == null) {
            namingSchemas = new HashMap<String, Constant>();

            for (Constant const1 : getConstant()) {
                if (const1.getName().startsWith(ALTERNATE_NAMING_SCHEMA)) {
                    namingSchemas.put(const1.getValue(), const1);
                }
            }
        }

        return namingSchemas;

    }

    public void setConstant(List<Constant> constant) {
        this.constant = constant;
    }

    public void setDataSetConfig(DataSetConfig dataSetConfig) {
        this.dataSetConfig = dataSetConfig;
    }

    public void setDateConfig(DateConfig dateConfig) {
        this.dateConfig = dateConfig;
    }

    public void setName(String name) {
        this.name = name;
    }

}
