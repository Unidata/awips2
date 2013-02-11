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
package com.raytheon.uf.common.stats;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Load event to track dialog/table/data load times.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 5, 2013    1584     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class LoadEvent extends StatisticsEvent {
    private static final long serialVersionUID = 1L;

    private static final Map<String, String> FIELD_UNIT_MAP;
    static {
        Map<String, String> m = new HashMap<String, String>();
        m.put("loadTime", "ms");
        FIELD_UNIT_MAP = Collections.unmodifiableMap(m);
    }

    /**
     * The plugin name
     */
    @DynamicSerializeElement
    private String pluginName;

    /**
     * The workstation id
     */
    @DynamicSerializeElement
    private String workstation;

    /**
     * The type of object that is having its load time tracked
     */
    @DynamicSerializeElement
    private String type;

    /**
     * The version of CAVE
     */
    @DynamicSerializeElement
    private String caveVersion;

    /**
     * The load time in ms
     */
    @DynamicSerializeElement
    private long loadTime;

    /**
     * Message
     */
    @DynamicSerializeElement
    private String message;

    @Override
    protected Map<String, String> getFieldUnitMap() {
        return FIELD_UNIT_MAP;
    }

    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * @param pluginName
     *            the pluginName to set
     */
    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * @return the workstation
     */
    public String getWorkstation() {
        return workstation;
    }

    /**
     * @param workstation
     *            the workstation to set
     */
    public void setWorkstation(String workstation) {
        this.workstation = workstation;
    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * @return the loadTime
     */
    public long getLoadTime() {
        return loadTime;
    }

    /**
     * @param loadTime
     *            the loadTime to set
     */
    public void setLoadTime(long loadTime) {
        this.loadTime = loadTime;
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * @param message
     *            the message to set
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * @return the caveVersion
     */
    public String getCaveVersion() {
        return caveVersion;
    }

    /**
     * @param caveVersion
     *            the caveVersion to set
     */
    public void setCaveVersion(String caveVersion) {
        this.caveVersion = caveVersion;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        return super.toString() + " : " + getMessage();
    }
}
