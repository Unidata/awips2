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
package com.raytheon.uf.edex.registry.ebxml.services.cataloger;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

/**
 * 
 * Object representing and entry into the index
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 29, 2012            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Entity
@Cache(region = "registryObjects", usage = CacheConcurrencyStrategy.TRANSACTIONAL, include = "all")
@Table(name = "registryIndex")
public class IndexEntry {

    /** The unique key identifying this entry */
    @Id
    protected Integer key;

    /** The parent object id for which this index entry belongs */
    private String parentId;

    /** The type of object this index entry refers to */
    private String parentObjectType;

    /** The name of the index to enter this value into */
    private String indexName;

    /** The indexed value */
    @Column(columnDefinition = "text")
    private String value;

    /**
     * Creates a new IndexEntry object
     */
    public IndexEntry() {

    }

    /**
     * Gets the key associated with this entry. The key is the hashCode
     * 
     * @return The key
     */
    public Integer getKey() {
        key = this.hashCode();
        return key;
    }

    /**
     * Creates a new IndexEntry with the given values
     * 
     * @param parentId
     *            The parent object id for which this index entry belongs
     * @param parentObjectType
     *            The type of object this index entry refers to
     * @param indexName
     *            The name of the index to enter this value into
     * @param value
     *            The indexed value
     */
    public IndexEntry(String parentId, String parentObjectType,
            String indexName, String value) {
        this.parentId = parentId;
        this.parentObjectType = parentObjectType;
        this.indexName = indexName;
        this.value = value;
    }

    public String getParentId() {
        return parentId;
    }

    public void setParentId(String parentId) {
        this.parentId = parentId;
    }

    public String getParentObjectType() {
        return parentObjectType;
    }

    public void setParentObjectType(String parentObjectType) {
        this.parentObjectType = parentObjectType;
    }

    public String getIndexName() {
        return indexName;
    }

    public void setIndexName(String indexName) {
        this.indexName = indexName;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((indexName == null) ? 0 : indexName.hashCode());
        result = prime * result
                + ((parentId == null) ? 0 : parentId.hashCode());
        result = prime
                * result
                + ((parentObjectType == null) ? 0 : parentObjectType.hashCode());
        result = prime * result + ((value == null) ? 0 : value.hashCode());
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
        IndexEntry other = (IndexEntry) obj;
        if (indexName == null) {
            if (other.indexName != null)
                return false;
        } else if (!indexName.equals(other.indexName))
            return false;
        if (parentId == null) {
            if (other.parentId != null)
                return false;
        } else if (!parentId.equals(other.parentId))
            return false;
        if (parentObjectType == null) {
            if (other.parentObjectType != null)
                return false;
        } else if (!parentObjectType.equals(other.parentObjectType))
            return false;
        if (value == null) {
            if (other.value != null)
                return false;
        } else if (!value.equals(other.value))
            return false;
        return true;
    }

    public void setKey(Integer key) {
        this.key = key;
    }

}
