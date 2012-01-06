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
package com.raytheon.uf.common.activetable;

import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2010            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

@DynamicSerialize
public class UpdateActiveTableRequest implements IServerRequest {

    @DynamicSerializeElement
    private Map<String, Object>[] activeTable;

    @DynamicSerializeElement
    private String xmlSource;

    @DynamicSerializeElement
    private ActiveTableMode mode = ActiveTableMode.PRACTICE;

    @DynamicSerializeElement
    private float timeOffset = 0.0f;

    /**
     * @return the activeTable
     */
    public Map<String, Object>[] getActiveTable() {
        return activeTable;
    }

    /**
     * @param activeTable
     *            the activeTable to set
     */
    public void setActiveTable(Map<String, Object>[] activeTable) {
        this.activeTable = activeTable;
    }

    /**
     * @param xmlSource
     */
    public void setXmlSource(String xmlSource) {
        this.xmlSource = xmlSource;
    }

    /**
     * @return
     */
    public String getXmlSource() {
        return xmlSource;
    }

    /**
     * @param mode
     *            the mode to set
     */
    public void setMode(ActiveTableMode mode) {
        this.mode = mode;
    }

    /**
     * @return the mode
     */
    public ActiveTableMode getMode() {
        return mode;
    }

    /**
     * @param timeOffset
     *            the timeOffset to set
     */
    public void setTimeOffset(float timeOffset) {
        this.timeOffset = timeOffset;
    }

    /**
     * @return the timeOffset
     */
    public float getTimeOffset() {
        return timeOffset;
    }

}
