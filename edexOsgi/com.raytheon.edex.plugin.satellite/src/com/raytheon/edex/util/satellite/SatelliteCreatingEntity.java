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
 * A satellite creating entity
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
@Table(name = "satellite_creating_entities")
public class SatelliteCreatingEntity extends PersistableDataObject implements
        Serializable, ISerializableObject {

    private static final long serialVersionUID = -4678013903413236803L;

    /** The id of the creating entity */
    @Id
    private int entityId;

    /** The name of the creating entity */
    @Column(length = 64)
    private String entityName;

    /**
     * Constructs and empty SatelliteCreatingEntity
     */
    public SatelliteCreatingEntity() {

    }

    /**
     * Constructs a new creating entity
     * 
     * @param entityId
     *            The entity id
     * @param entityName
     *            The entity name
     */
    public SatelliteCreatingEntity(int entityId, String entityName) {
        this.entityId = entityId;
        this.entityName = entityName;
    }

    public int getEntityId() {
        return entityId;
    }

    public void setEntityId(int entityId) {
        this.entityId = entityId;
    }

    public String getEntityName() {
        return entityName;
    }

    public void setEntityName(String entityName) {
        this.entityName = entityName;
    }

    public String toString() {
        return entityName;
    }

}
