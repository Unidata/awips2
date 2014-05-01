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
package com.raytheon.uf.common.pointdata;

import java.io.File;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;

/**
 * A generic description for a type of point data
 * 
 * This consists of a set of parameters and additional metadata about the point
 * data type.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009             chammack    Initial creation
 * Oct 9, 2013  2361       njensen     Use JAXBManager for XML
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "pointDataDescription")
public class PointDataDescription {

    private static final int DEFAULT_LEVELSIZE = 64;

    @XmlType(name = "pointDataType")
    public static enum Type {
        FLOAT, BOOLEAN, STRING, INT, CHAR, DOUBLE, LONG;
    };

    public static final int FILL_VALUE_INT = -9999;

    private static final SingleTypeJAXBManager<PointDataDescription> jaxb = SingleTypeJAXBManager
            .createWithoutException(PointDataDescription.class);

    @XmlElement(name = "dimension")
    public Dimension[] dimensions;

    @XmlElement(name = "parameter")
    public ParameterDescription[] parameters;

    public PointDataDescription() {
        this.parameters = new ParameterDescription[0];
        this.dimensions = new Dimension[0];
    }

    /**
     * 
     * @param file
     * @return
     * @throws JAXBException
     */
    public static PointDataDescription fromFile(File file)
            throws SerializationException {
        PointDataDescription pdd = jaxb.unmarshalFromXmlFile(file);
        pdd.resolveDimensions();
        return pdd;
    }

    /**
     * 
     * @param is
     * @return
     * @throws JAXBException
     */
    public static PointDataDescription fromStream(InputStream is)
            throws SerializationException {
        PointDataDescription pdd = (PointDataDescription) jaxb
                .unmarshalFromInputStream(is);
        pdd.resolveDimensions();

        return pdd;
    }

    /**
     * Populates the dimensionAsInt in each {@link ParameterDescription} for
     * easy access. Looks up each dimension in the xml Dimension elements.
     * 
     */
    public void resolveDimensions() {
        Map<String, Integer> dimensionMap = new HashMap<String, Integer>();
        for (Dimension dim : dimensions) {
            String name = dim.getDimensionName();
            if (name != null) {
                dimensionMap.put(name, dim.getDimensionLength());
            }
        }

        for (ParameterDescription pd : parameters) {
            String key = pd.getDimension();
            if (key != null) {
                Integer value = dimensionMap.get(key);
                pd.setDimensionAsInt(value);
            } else if (pd.getNumDims() > 1) {
                // set to default value
                pd.setDimensionAsInt(DEFAULT_LEVELSIZE);
            }
        }
    }

    public String[] getParameterNames() {
        String[] parameters = new String[this.parameters.length];
        for (int i = 0; i < parameters.length; i++) {
            parameters[i] = this.parameters[i].getParameterName();
        }
        return parameters;
    }

}
