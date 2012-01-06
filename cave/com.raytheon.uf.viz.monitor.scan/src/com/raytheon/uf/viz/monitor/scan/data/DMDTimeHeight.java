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
package com.raytheon.uf.viz.monitor.scan.data;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Contains the data used for the time-height graph.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 10, 2010            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class DMDTimeHeight implements ISerializableObject{
    /**
     * The ident of the data.
     */
    private String ident;
    
    /**
     * Time offset in milliseconds.
     */
    public Long offset;
    
    /**
     * Value.
     */
    public Double value;
    
    /**
     * Height in kft.
     */
    public Double height;
    
    /**
     * Low level diameter.
     */
    public Double llDiam;
    
    /**
     * Constructor.
     */
    public DMDTimeHeight() {
        
    }

    /**
     * Get the time offset.
     * @return The time offset.
     */
    public Long getOffset() {
        return offset;
    }

    /**
     * Set the time offset.
     * @param offset The time offset.
     */
    public void setOffset(Long offset) {
        this.offset = offset;
    }

    /**
     * Get the value.
     * @return The value.
     */
    public Double getValue() {
        return value;
    }

    /**
     * Set the value.
     * @param value The value.
     */
    public void setValue(Double value) {
        this.value = value;
    }
    
    /**
     * Get the height in kft.
     * @return Height in kft.
     */
    public Double getHeight() {
        return height;
    }
    
    /**
     * Set the height in kft.
     * @param height Height in kft.
     */
    public void setHeight(Double height) {
        this.height = height;
    }

    /**
     * 
     * @return
     */
    public Double getLlDiam() {
        return llDiam;
    }

    public void setLlDiam(Double llDiam) {
        this.llDiam = llDiam;
    }

    /**
     * @param ident the ident to set
     */
    public void setIdent(String ident) {
        this.ident = ident;
    }

    /**
     * @return the ident
     */
    public String getIdent() {
        return ident;
    }
}
