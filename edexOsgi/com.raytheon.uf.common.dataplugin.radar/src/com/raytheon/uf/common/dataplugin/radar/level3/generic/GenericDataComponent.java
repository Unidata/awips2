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
package com.raytheon.uf.common.dataplugin.radar.level3.generic;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataParameter.AttributeNames;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2009            askripsk     Initial creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

@DynamicSerialize
public class GenericDataComponent implements ISerializableObject {
    @DynamicSerialize
    public static enum ComponentType {
        RADIAL(1, "Radial Component",
                "com.raytheon.uf.common.dataplugin.radar.level3.generic.RadialComponent"), GRID(
                2, "Grid Component",
                "com.raytheon.uf.common.dataplugin.radar.level3.generic.GridComponent"), AREA(
                3, "Area Component",
                "com.raytheon.uf.common.dataplugin.radar.level3.generic.AreaComponent"), TEXT(
                4, "Text Component",
                "com.raytheon.uf.common.dataplugin.radar.level3.generic.TextComponent"), TABLE(
                5, "Table Component",
                "com.raytheon.uf.common.dataplugin.radar.level3.generic.TableComponent"), EVENT(
                6, "Event Component",
                "com.raytheon.uf.common.dataplugin.radar.level3.generic.EventComponent");

        @DynamicSerializeElement
        private int code;

        @DynamicSerializeElement
        private String description;

        @DynamicSerializeElement
        private String classType;

        private ComponentType(int code, String description, String classType) {
            this.code = code;
            this.description = description;
            this.classType = classType;
        }

        /**
         * @return the code
         */
        public int getCode() {
            return code;
        }

        /**
         * @param code
         *            the code to set
         */
        public void setCode(int code) {
            this.code = code;
        }

        /**
         * @return the description
         */
        public String getDescription() {
            return description;
        }

        /**
         * @param description
         *            the description to set
         */
        public void setDescription(String description) {
            this.description = description;
        }

        /**
         * @return the classType
         */
        public String getClassType() {
            return classType;
        }

        /**
         * @param classType
         *            the classType to set
         */
        public void setClassType(String classType) {
            this.classType = classType;
        }

        public static ComponentType valueOf(int code) {
            ComponentType rval = null;

            for (ComponentType currType : ComponentType.values()) {
                if (currType.getCode() == code) {
                    rval = currType;
                    break;
                }
            }

            return rval;
        }

        public GenericDataComponent getNewInstance() {
            GenericDataComponent rval = null;

            try {
                rval = (GenericDataComponent) Class.forName(this.classType)
                        .newInstance();
                rval.setComponentType(this);
            } catch (Exception e) {
                e.printStackTrace();
            }

            return rval;
        }

        @Override
        public String toString() {
            return this.description;
        }
    }

    @DynamicSerializeElement
    protected ComponentType componentType;

    @DynamicSerializeElement
    protected List<GenericDataParameter> parameters = new ArrayList<GenericDataParameter>();

    public GenericDataComponent() {

    }

    public void parseData(DataInputStream in) throws IOException {
    }

    public String getValue(String attributeID) {
        return getParameterAttributeDescription(attributeID,
                AttributeNames.VALUE.toString());
    }

    public int getValueAsInt(String attributeID) {
        String tmp = getValue(attributeID);
        return tmp.equals("") ? 0 : Integer.parseInt(tmp);
    }

    public float getValueAsFloat(String attributeID) {
        String tmp = getValue(attributeID);
        return tmp.equals("") ? 0 : Float.parseFloat(tmp);
    }

    public double getValueAsDouble(String attributeID) {
        String tmp = getValue(attributeID);
        return tmp.equals("") ? 0 : Double.parseDouble(tmp);
    }

    public String getUnits(String attributeID) {
        return getParameterAttributeDescription(attributeID,
                AttributeNames.UNITS.toString());
    }

    public String getParameterAttributeDescription(String attributeID,
            String attributeName) {
        String rval = "";

        for (GenericDataParameter currParameter : parameters) {
            if (currParameter.getId().equalsIgnoreCase(attributeID)) {
                rval = currParameter.getAttributeDescription(attributeName);
                break;
            }
        }

        return rval;
    }

    /**
     * @return the componentType
     */
    public ComponentType getComponentType() {
        return componentType;
    }

    /**
     * @param componentType
     *            the componentType to set
     */
    public void setComponentType(ComponentType componentType) {
        this.componentType = componentType;
    }

    /**
     * @return the parameters
     */
    public List<GenericDataParameter> getParameters() {
        return parameters;
    }

    /**
     * @param parameters
     *            the parameters to set
     */
    public void setParameters(List<GenericDataParameter> parameters) {
        this.parameters = parameters;
    }

    @Override
    public String toString() {
        StringBuffer rval = new StringBuffer("Generic Component - Type: "
                + componentType + "\n");

        for (GenericDataParameter currParam : parameters) {
            rval.append(currParam);
            rval.append("\n");
        }

        return rval.toString();
    }
}
