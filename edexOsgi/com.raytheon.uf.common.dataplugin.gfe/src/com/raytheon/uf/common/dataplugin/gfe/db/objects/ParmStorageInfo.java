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
package com.raytheon.uf.common.dataplugin.gfe.db.objects;

import java.awt.Point;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

/**
 * Derived class that consolidates storage info for a Parm.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/14/08     #1030      randerso    Initial port
 * 08/05/2013   #1571      randerso    Moved to com.raytheon.uf.common.dataplugin
 *                                     Added GridParmInfo as a field.
 *                                     Added hibernate annotations
 * 10/22/2013   #2361      njensen     Remove ISerializableObject
 * 01/15/2014   #1571      randerso    Added clone method and copy constructor
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@Entity
@Table(name = "gfe_parminfo", uniqueConstraints = { @UniqueConstraint(columnNames = { "parmId_id" }) })
public class ParmStorageInfo implements Cloneable {

    /**
     * Auto-generated surrogate key
     */
    @Id
    @SequenceGenerator(name = "GFE_PARMINFO_GENERATOR", sequenceName = "gfe_parminfo_seq")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GFE_PARMINFO_GENERATOR")
    private int id;

    @Embedded
    private GridParmInfo gridParmInfo;

    @Column(length = 8, nullable = false)
    private String storageType;

    @Column(nullable = false)
    private float dataOffset;

    @Column(nullable = false)
    private float dataMultiplier;

    @Column(length = 8, nullable = false)
    private String dataType;

    /**
     * Default constructor
     */
    public ParmStorageInfo() {

    }

    /**
     * Constructor
     * 
     * @param dataType
     * @param gridParmInfo
     * @param dataOffset
     * @param dataMultiplier
     * @param storageType
     */
    public ParmStorageInfo(final String dataType,
            final GridParmInfo gridParmInfo, float dataOffset,
            float dataMultiplier, final String storageType) {
        this.storageType = storageType;
        this.dataOffset = dataOffset;
        this.dataMultiplier = dataMultiplier;
        this.dataType = dataType;
        this.gridParmInfo = gridParmInfo;
    }

    /**
     * Copy constructor
     * 
     * @param orig
     */
    public ParmStorageInfo(final ParmStorageInfo orig) {
        this.storageType = orig.storageType;
        this.dataOffset = orig.dataOffset;
        this.dataMultiplier = orig.dataMultiplier;
        this.dataType = orig.dataType;
        this.gridParmInfo = orig.gridParmInfo.clone();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public ParmStorageInfo clone() {
        return new ParmStorageInfo(this);
    }

    /**
     * @return the id
     */
    public int getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(int id) {
        this.id = id;
    }

    /**
     * @return the storageType
     */
    public String getStorageType() {
        return storageType;
    }

    /**
     * @return the gridSize
     */
    public Point getGridSize() {
        return this.gridParmInfo.getGridLoc().gridSize();
    }

    /**
     * @return the parmID
     */
    public ParmID getParmID() {
        return gridParmInfo.getParmID();
    }

    /**
     * @return the parmName
     */
    public String getParmName() {
        return gridParmInfo.getParmID().getParmName();
    }

    /**
     * @return the parmLevel
     */
    public String getParmLevel() {
        return gridParmInfo.getParmID().getParmLevel();
    }

    /**
     * @return the level
     */
    public String getLevel() {
        return gridParmInfo.getParmID().getParmLevel();
    }

    /**
     * @return the dataOffset
     */
    public float getDataOffset() {
        return dataOffset;
    }

    /**
     * @return the dataMultiplier
     */
    public float getDataMultiplier() {
        return dataMultiplier;
    }

    /**
     * @return the dataType
     */
    public String getDataType() {
        return dataType;
    }

    /**
     * @return the gridParmInfo
     */
    public GridParmInfo getGridParmInfo() {
        return gridParmInfo;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result) + Float.floatToIntBits(dataMultiplier);
        result = (prime * result) + Float.floatToIntBits(dataOffset);
        result = (prime * result)
                + ((dataType == null) ? 0 : dataType.hashCode());
        result = (prime * result)
                + ((gridParmInfo == null) ? 0 : gridParmInfo.hashCode());
        result = (prime * result)
                + ((storageType == null) ? 0 : storageType.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof ParmStorageInfo)) {
            return false;
        }
        ParmStorageInfo other = (ParmStorageInfo) obj;
        if (Float.floatToIntBits(dataMultiplier) != Float
                .floatToIntBits(other.dataMultiplier)) {
            return false;
        }
        if (Float.floatToIntBits(dataOffset) != Float
                .floatToIntBits(other.dataOffset)) {
            return false;
        }
        if (dataType == null) {
            if (other.dataType != null) {
                return false;
            }
        } else if (!dataType.equals(other.dataType)) {
            return false;
        }
        if (gridParmInfo == null) {
            if (other.gridParmInfo != null) {
                return false;
            }
        } else if (!gridParmInfo.equals(other.gridParmInfo)) {
            return false;
        }
        if (storageType == null) {
            if (other.storageType != null) {
                return false;
            }
        } else if (!storageType.equals(other.storageType)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("ParmStorageInfo for parm: ").append(getParmName());
        sb.append(" Level: ").append(getLevel());
        sb.append(" DataType: ").append(getDataType());
        sb.append(" GridSize: ").append(getGridSize());
        sb.append(" DataOffset: ").append(getDataOffset());
        sb.append(" DataMultiplier: ").append(getDataMultiplier());
        sb.append(" StorageType: ").append(getStorageType());

        return sb.toString();
    }
}
