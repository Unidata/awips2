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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.pointdata.elements.AbstractPointDataObject;
import com.raytheon.uf.common.pointdata.elements.FloatPointDataObject;
import com.raytheon.uf.common.pointdata.elements.IntPointDataObject;
import com.raytheon.uf.common.pointdata.elements.LongPointDataObject;
import com.raytheon.uf.common.pointdata.elements.StringPointDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A Thrift-compliant representation of {@link PointDataContainer}
 * 
 * PointDataContainer uses inheritance in maps, which is not supported in
 * standard thrift. This datastructure flattens the map into its components.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 14, 2010           chammack    Initial creation
 * Oct 30, 2012  15448    Xiaochuan   Check if container != null first in from().
 * Dec 02, 2013  2537     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@DynamicSerialize
public class PointDataThriftContainer {

    @DynamicSerializeElement
    FloatPointDataObject[] floatData;

    @DynamicSerializeElement
    IntPointDataObject[] intData;

    @DynamicSerializeElement
    LongPointDataObject[] longData;

    @DynamicSerializeElement
    StringPointDataObject[] stringData;

    @DynamicSerializeElement
    protected int size;

    /**
     * @return the floatData
     */
    public FloatPointDataObject[] getFloatData() {
        return floatData;
    }

    /**
     * @param floatData
     *            the floatData to set
     */
    public void setFloatData(FloatPointDataObject[] floatData) {
        this.floatData = floatData;
    }

    /**
     * @return the intData
     */
    public IntPointDataObject[] getIntData() {
        return intData;
    }

    /**
     * @param intData
     *            the intData to set
     */
    public void setIntData(IntPointDataObject[] intData) {
        this.intData = intData;
    }

    /**
     * @return the longData
     */
    public LongPointDataObject[] getLongData() {
        return longData;
    }

    /**
     * @param longData
     *            the longData to set
     */
    public void setLongData(LongPointDataObject[] longData) {
        this.longData = longData;
    }

    /**
     * @return the stringData
     */
    public StringPointDataObject[] getStringData() {
        return stringData;
    }

    /**
     * @param stringData
     *            the stringData to set
     */
    public void setStringData(StringPointDataObject[] stringData) {
        this.stringData = stringData;
    }

    /**
     * @return the size
     */
    public int getSize() {
        return size;
    }

    /**
     * @param size
     *            the size to set
     */
    public void setSize(int size) {
        this.size = size;
    }

    /**
     * Transform from a {@link PointDataContainer} to a
     * {@link PointDataThriftContainer}
     * 
     * @param container
     * @return
     */
    public static PointDataThriftContainer from(PointDataContainer container) {
        PointDataThriftContainer pdtc = new PointDataThriftContainer();
        List<IntPointDataObject> intTypes = new ArrayList<IntPointDataObject>();
        List<FloatPointDataObject> floatTypes = new ArrayList<FloatPointDataObject>();
        List<StringPointDataObject> stringTypes = new ArrayList<StringPointDataObject>();
        List<LongPointDataObject> longTypes = new ArrayList<LongPointDataObject>();
        
        if( container != null )
        {
        	for (Map.Entry<String, AbstractPointDataObject<?>> v : container.pointDataTypes
        			.entrySet()) {
        		AbstractPointDataObject<?> o = v.getValue();
        		if (o instanceof IntPointDataObject) {
        			intTypes.add((IntPointDataObject) o);
        		} else if (o instanceof FloatPointDataObject) {
        			floatTypes.add((FloatPointDataObject) o);
        		} else if (o instanceof StringPointDataObject) {
        			stringTypes.add((StringPointDataObject) o);
        		} else if (o instanceof LongPointDataObject) {
        			longTypes.add((LongPointDataObject) o);
        		} else {
        			throw new UnsupportedOperationException("Got type: "
                        + o.getClass().getName()
                        + ".  Code must be updated to support new type");
        		}
        	
        	}

        	pdtc.intData = intTypes
                	.toArray(new IntPointDataObject[intTypes.size()]);
        	pdtc.floatData = floatTypes.toArray(new FloatPointDataObject[floatTypes
        	                                                             .size()]);
        	pdtc.stringData = stringTypes
                	.toArray(new StringPointDataObject[stringTypes.size()]);
        	pdtc.longData = longTypes.toArray(new LongPointDataObject[longTypes
        	                                                          .size()]);

        	pdtc.size = container.getAllocatedSz();
        }
        
        return pdtc;

    }

}
