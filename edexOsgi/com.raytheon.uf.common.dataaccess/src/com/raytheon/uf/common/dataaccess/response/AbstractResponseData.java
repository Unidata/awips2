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

import com.raytheon.uf.common.dataaccess.IData;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;

/**
 * Base class for responses returned from the Data Access Framework. Wraps all
 * of the data for the <code>IData</code> interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2013            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public abstract class AbstractResponseData {

    @DynamicSerializeElement
    protected DataTime time;

    @DynamicSerializeElement
    protected String level;

    @DynamicSerializeElement
    protected String locationName;

    @DynamicSerializeElement
    protected Map<String, Object> attributes;

    protected AbstractResponseData() {
        // no-op, for serialization
    }

    protected AbstractResponseData(final IData data) {
        time = data.getDataTime();
        locationName = data.getLocationName();
        if (data.getLevel() != null) {
            level = data.getLevel().toString();
        }
        Set<String> attrNames = data.getAttributes();
        attributes = new HashMap<String, Object>(attrNames.size(), 1);
        for (String attr : attrNames) {
            attributes.put(attr, data.getAttribute(attr));
        }
    }

    public DataTime getTime() {
        return time;
    }

    public void setTime(DataTime time) {
        this.time = time;
    }

    public String getLevel() {
        return level;
    }

    public void setLevel(String level) {
        this.level = level;
    }

    public String getLocationName() {
        return locationName;
    }

    public void setLocationName(String locationName) {
        this.locationName = locationName;
    }

    public Map<String, Object> getAttributes() {
        return attributes;
    }

    public void setAttributes(Map<String, Object> attributes) {
        this.attributes = attributes;
    }
}
