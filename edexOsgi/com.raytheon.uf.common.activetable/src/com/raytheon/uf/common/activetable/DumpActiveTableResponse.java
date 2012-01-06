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

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Send back the dumped active table. This is just a wrapper around a String,
 * plus another String for an error message in case something went wrong.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 4, 2010            wldougher     Initial creation
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */

@DynamicSerialize
public class DumpActiveTableResponse implements ISerializableObject {

    @DynamicSerializeElement
    private String dump;

    @DynamicSerializeElement
    private Integer unfilteredCount = Integer.valueOf(0);

    @DynamicSerializeElement
    private Integer filteredCount = Integer.valueOf(0);

    /**
     * @return the unfilteredCount
     */
    public Integer getUnfilteredCount() {
        return unfilteredCount;
    }

    /**
     * @return the filteredCount
     */
    public Integer getFilteredCount() {
        return filteredCount;
    }

    /**
     * @param unfilteredCount
     *            the unfilteredCount to set
     */
    public void setUnfilteredCount(Integer unfilteredCount) {
        this.unfilteredCount = unfilteredCount;
    }

    /**
     * @param filteredCount
     *            the filteredCount to set
     */
    public void setFilteredCount(Integer filteredCount) {
        this.filteredCount = filteredCount;
    }

    @DynamicSerializeElement
    private String message;

    /**
     * Get the dumped text.
     * 
     * @return the dump
     */
    public String getDump() {
        return dump;
    }

    /**
     * Get the error message.
     * 
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * The dumped string.
     * 
     * @param dump
     *            the dump to set
     */
    public void setDump(String dump) {
        this.dump = dump;
    }

    /**
     * Set the error message.
     * 
     * @param message
     *            the message to set
     */
    public void setMessage(String message) {
        this.message = message;
    }

}
