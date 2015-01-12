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

package com.raytheon.uf.common.dataplugin.taf;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class representing a sky coverage item contained in a taf message
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 6/21/2007    180         Phillippe   initial creation.
 * 4/16/2008    934         grichard    Added toString overridden method.
 * Nov 01, 2013 2361        njensen     Remove XML annotations
 * May 15, 2014 3002        bgonzale    Moved to com.raytheon.uf.common.dataplugin.taf.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "taf_sky_cover")
@DynamicSerialize
public class TafSkyCover extends PersistableDataObject {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private int id;

    /** The taf record this skycover object belongs to * */
    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private ChangeGroup parentID;

    /** The type of sky coverage * */
    @DynamicSerializeElement
    @Column(length = 3)
    private String type;

    /** The height of the cloud layer * */
    @DynamicSerializeElement
    @Column
    private Integer height;

    // For convective low level cloud - CB
    @DynamicSerializeElement
    @Column(length = 3)
    private String genus;

    /**
     * @return the parentID
     */
    public ChangeGroup getParentID() {
        return parentID;
    }

    /**
     * @param parentID
     *            the parentID to set
     */
    public void setParentID(ChangeGroup parentID) {
        this.parentID = parentID;
    }

    /**
     * No-Arg Constructor.
     */
    public TafSkyCover() {
        this.type = "";
        this.height = 0;
    }

    /**
     * Constructor
     * 
     * @param type
     * @param height
     */
    public TafSkyCover(String type, int height) {
        this.type = type;
        this.height = height;
    }

    /**
     * @return the serialVersionUID
     */
    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    /**
     * @return the height
     */
    public Integer getHeight() {
        return height;
    }

    /**
     * @param height
     *            the height to set
     */
    public void setHeight(Integer height) {
        this.height = height;
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
     * @return the genus
     */
    public String getGenus() {
        return genus;
    }

    /**
     * @param genus
     *            the genus to set
     */
    public void setGenus(String genus) {
        this.genus = genus;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof TafSkyCover) {
            TafSkyCover sky = (TafSkyCover) obj;

            if (parentID != sky.parentID) {
                return false;
            }

            if (!(this.height == null ? sky.getHeight() == null : this.height
                    .equals(sky.getHeight()))) {
                return false;
            }

            if (!(this.type == null ? sky.getType() == null : this.type
                    .equals(sky.getType()))) {
                return false;
            }

            return true;

        } else {
            return false;
        }

    }

    @Override
    public int hashCode() {

        return new HashCodeBuilder(17, 37).append(parentID).append(height)
                .append(type).toHashCode();
    }

    /**
     * @return
     */
    @Override
    public String toString() {
        StringBuilder retVal = new StringBuilder("_TAF SKY COVER_");

        if (type != null) {
            retVal.append(type);
        } else {
            retVal.append("---");
        }
        if (height != null) {
            retVal.append(String.format("%3d", height));
        } else {
            retVal.append("---");
        }
        if (genus != null) {
            retVal.append(genus);
        }

        return retVal.toString();
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }
}
