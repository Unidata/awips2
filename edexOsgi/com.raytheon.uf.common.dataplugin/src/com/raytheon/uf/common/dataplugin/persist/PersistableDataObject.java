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

package com.raytheon.uf.common.dataplugin.persist;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * This is the root class for any object being persisted in the database using
 * Hibernate.
 * <p>
 * Only supporting data objects not directly associated with a plugin or
 * representing a spatial object should directly extend this class. The abstract
 * children of this class are as follows: 1) SpatialDataObject -- Classes
 * defining spatial components should extend this class.<br>
 * 2) PluginDataObject -- Classes representing data types (satellite, radar,
 * grib, etc.) should extend this class.<br>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/24/07      353         bphillip    Initial Check in    
 * 20080408           1039 jkorman     Added traceId for tracing data.
 * Oct 10, 2012 1261        djohnson     Add generic for identifier.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class PersistableDataObject<IDENTIFIER_TYPE> implements
        IPersistableDataObject<IDENTIFIER_TYPE>,
        Serializable, ISerializableObject {

    private static final long serialVersionUID = -6747395152869923909L;

    /**
     * Object used as the unique identifier. Intended to be used as the primary
     * key for the associated database table.
     */
    @XmlElement
    @DynamicSerializeElement
    protected IDENTIFIER_TYPE identifier;

    private String traceId = "";

    /**
     * Some records allow data to be updated instead of inserted. This flag
     * allows this behavior
     */
    private boolean overwriteAllowed = false;

    /**
     * @return the overwriteAllowed
     */
    public boolean isOverwriteAllowed() {
        return overwriteAllowed;
    }

    /**
     * @param overwriteAllowed
     *            the overwriteAllowed to set
     */
    public void setOverwriteAllowed(boolean overwriteAllowed) {
        this.overwriteAllowed = overwriteAllowed;
    }

    /**
     * Gets the identifier
     * 
     * @return The identifier
     */
    @Override
    public IDENTIFIER_TYPE getIdentifier() {
        return identifier;
    }

    /**
     * Sets the identifier
     * 
     * @param identifier
     *            The identifier
     */
    public void setIdentifier(IDENTIFIER_TYPE identifier) {
        this.identifier = identifier;
    }

    /**
     * Get the unique trace identifier for this objects source data.
     * 
     * @return The traceId
     */
    public String getTraceId() {
        return traceId;
    }

    /**
     * Set the unique trace identifier for this objects source data.
     * 
     * @param traceId
     *            The traceId to set
     */
    public void setTraceId(String traceId) {
        this.traceId = traceId;
    }

}
