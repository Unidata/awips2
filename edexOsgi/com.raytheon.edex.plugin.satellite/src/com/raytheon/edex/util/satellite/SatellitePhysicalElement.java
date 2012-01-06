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

package com.raytheon.edex.util.satellite;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * A satellite physical element
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Entity
@Table(name = "satellite_physical_elements")
public class SatellitePhysicalElement extends PersistableDataObject implements
        Serializable, ISerializableObject {

    private static final long serialVersionUID = 8429844485032687146L;

    /** The physical element id */
    @Id
    private int elementId;

    /** The physical element name */
    @Column(length = 64)
    private String elementName;

    /**
     * Constructs an empty SatellitePhysicalElement
     */
    public SatellitePhysicalElement() {
    }

    /**
     * Constructs a new SatellitePhysicalElement
     * 
     * @param elementId
     *            The element id
     * @param elementName
     *            The element name
     */
    public SatellitePhysicalElement(int elementId, String elementName) {
        this.elementId = elementId;
        this.elementName = elementName;
    }

    public int getElementId() {
        return elementId;
    }

    public void setElementId(int elementId) {
        this.elementId = elementId;
    }

    public String getElementName() {
        return elementName;
    }

    public void setElementName(String elementName) {
        this.elementName = elementName;
    }

    public String toString() {
        return elementName;
    }

}
