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

package com.raytheon.uf.common.dataplugin.obs.metar.util;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class representing a sky coverage item contained in a metar message
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 14, 2007  139      Phillippe   initial creation
 * Apr 14, 2008  996      jkorman     Added cloud genus field.    
 * May 11, 2009  2338     jsanchez    Implemented the Comparable interface.
 * Dec 03, 2013  2537     bsteffen    Remove ISerializableObject
 * 
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SkyCover implements Serializable,
        Comparable<SkyCover> {

    private static final long serialVersionUID = 1L;

    /** The primary key for the database table * */
    @Id
    @GeneratedValue
    private Integer key;

    /** The type of sky coverage * */
    @XmlElement
    @DynamicSerializeElement
    @Column
    private String type;

    /** The height of the cloud layer * */
    @XmlElement
    @DynamicSerializeElement
    @Column
    private Integer height;

    // For convective low level cloud - CB or TCU
    @XmlElement
    @DynamicSerializeElement
    @Column(length = 3)
    private String genus;

    /** The METAR this sky coverage belongs to */
    @ManyToOne
    @JoinColumn(name = "parentMetar", nullable = false)
    private MetarRecord parentMetar;

    /**
     * No-Arg Constructor.
     */
    public SkyCover() {
        this.type = "";
        this.height = null;
    }

    /**
     * Constructor
     * 
     * @param type
     * @param height
     */
    public SkyCover(String type, int height) {
        this.type = type;
        this.height = height;
    }

    /**
     * Constructor
     * 
     * @param type
     * @param height
     */
    public SkyCover(String type, Integer height) {
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

    /**
     * @return the key
     */
    public Integer getKey() {
        return key;
    }

    /**
     * Private since only Hibernate should be setting this generated key.
     * 
     * @param key
     *            the key to set
     */
    @SuppressWarnings("unused")
    private void setKey(Integer key) {
        this.key = key;
    }

    public MetarRecord getParentMetar() {
        return parentMetar;
    }

    public void setParentMetar(MetarRecord parentMetar) {
        this.parentMetar = parentMetar;
    }

    @Override
    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + ((height == null) ? 0 : height.hashCode());
        result = PRIME * result + ((key == null) ? 0 : key.hashCode());
        result = PRIME * result + ((type == null) ? 0 : type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final SkyCover other = (SkyCover) obj;
        if (height == null) {
            if (other.height != null)
                return false;
        } else if (!height.equals(other.height))
            return false;
        if (key == null) {
            if (other.key != null)
                return false;
        } else if (!key.equals(other.key))
            return false;
        if (type == null) {
            if (other.type != null)
                return false;
        } else if (!type.equals(other.type))
            return false;
        return true;
    }

    public int compareTo(SkyCover sc) {
        int rVal = 0;

        if (this.height != null && sc.getHeight() != null) {
            rVal = height.compareTo(sc.getHeight());
        } else if (this.height != null && sc.getHeight() == null) {
            rVal = -1;
        } else if (this.height == null && sc.getHeight() != null) {
            rVal = 1;
        }

        return rVal;
    }

}
