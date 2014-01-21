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
package com.raytheon.uf.common.dataaccess.response;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * IGeometryData wrapper used as part of <code>GetGeometryDataResponse</code>.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 03, 2013           dgilling    Initial creation
 * Jan 06, 2014  2537     bsteffen    Store geometry index instead of WKT.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public class GeometryResponseData extends AbstractResponseData {

    @DynamicSerializeElement
    private Map<String, Object[]> dataMap;

    @DynamicSerializeElement
    private int geometryWKTindex;

    public GeometryResponseData() {
        // no-op, for serialization
    }

    public GeometryResponseData(final IGeometryData data, final int geometryWKTindex) {
        super(data);

        Set<String> parameters = data.getParameters();
        dataMap = new HashMap<String, Object[]>(parameters.size(), 1);
        for (String param : parameters) {
            Object[] dataTuple = new Object[3];
            if (IGeometryData.Type.STRING.equals(data.getType(param))) {
                dataTuple[0] = data.getString(param);
            } else {
                dataTuple[0] = data.getNumber(param);
            }
            dataTuple[1] = data.getType(param).toString();
            if (data.getUnit(param) != null) {
                dataTuple[2] = data.getUnit(param).toString();
            }
            dataMap.put(param, dataTuple);
        }
        this.geometryWKTindex = geometryWKTindex;
    }

    public Map<String, Object[]> getDataMap() {
        return dataMap;
    }

    public void setDataMap(Map<String, Object[]> dataMap) {
        this.dataMap = dataMap;
    }

    public int getGeometryWKTindex() {
        return geometryWKTindex;
    }

    public void setGeometryWKTindex(int geometryWKTindex) {
        this.geometryWKTindex = geometryWKTindex;
    }
}
