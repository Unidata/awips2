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
package com.raytheon.uf.common.dataplugin.grid;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Contains all attributes of a grid record except time. This is done to save
 * space in the db since across time most grid data has the same
 * level/parameter/etc so having all other information in a separate table saves
 * space and improves theoretical performance.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@Entity
@Table(name = "grid_info")
@SequenceGenerator(name = "GRIDINFO_GENERATOR", sequenceName = "gridinfo_seq", allocationSize = 1)
@DynamicSerialize
public class GridInfoRecord extends PersistableDataObject<Integer> {

    private static final long serialVersionUID = 1L;

    /** The id */
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRIDINFO_GENERATOR")
    @DynamicSerializeElement
    private Integer id;

    @Column
    @DataURI(position = 0)
    @DynamicSerializeElement
    private String datasetId;

    /**
     * Any string which can differentiate this record from other records with
     * the same datasetId, examples of this would be for different versions of
     * the same grid or for different events from the same model.
     */
    @Column
    @DataURI(position = 1)
    @DynamicSerializeElement
    private String secondaryId;

    @Column
    @DynamicSerializeElement
    @DataURI(position = 2)
    private String ensembleId;

    /** The spatial information */
    @ManyToOne
    @PrimaryKeyJoinColumn
    @DataURI(position = 3)
    @DynamicSerializeElement
    private GridCoverage location;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @DataURI(position = 4, embedded = true)
    @DynamicSerializeElement
    private Parameter parameter;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @DynamicSerializeElement
    @DataURI(position = 5, embedded = true)
    private Level level;

    public GridInfoRecord() {

    }

    public GridInfoRecord(GridInfoRecord record) {
        this.datasetId = record.getDatasetId();
        this.level = record.getLevel();
        this.location = record.getLocation();
        this.parameter = record.getParameter();
        this.ensembleId = record.getEnsembleId();
        this.secondaryId = record.getSecondaryId();
        this.id = record.getId();
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getDatasetId() {
        return datasetId;
    }

    public void setDatasetId(String datasetId) {
        this.datasetId = datasetId;
    }

    public String getSecondaryId() {
        return secondaryId;
    }

    public void setSecondaryId(String secondaryId) {
        this.secondaryId = secondaryId;
    }

    public String getEnsembleId() {
        return ensembleId;
    }

    public void setEnsembleId(String ensembleId) {
        this.ensembleId = ensembleId;
    }

    public GridCoverage getLocation() {
        return location;
    }

    public void setLocation(GridCoverage location) {
        this.location = location;
    }

    public Parameter getParameter() {
        return parameter;
    }

    public void setParameter(Parameter parameter) {
        this.parameter = parameter;
    }

    public Level getLevel() {
        return level;
    }

    public void setLevel(Level level) {
        this.level = level;
    }

    @Override
    public String toString() {
        return "/" + datasetId + "/" + secondaryId + "/" + ensembleId + "/"
                + location + "/" + parameter + "/" + level;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((datasetId == null) ? 0 : datasetId.hashCode());
        result = prime * result
                + ((ensembleId == null) ? 0 : ensembleId.hashCode());
        result = prime * result + ((level == null) ? 0 : level.hashCode());
        result = prime * result
                + ((location == null) ? 0 : location.hashCode());
        result = prime * result
                + ((parameter == null) ? 0 : parameter.hashCode());
        result = prime * result
                + ((secondaryId == null) ? 0 : secondaryId.hashCode());
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
        GridInfoRecord other = (GridInfoRecord) obj;
        if (datasetId == null) {
            if (other.datasetId != null)
                return false;
        } else if (!datasetId.equals(other.datasetId))
            return false;
        if (ensembleId == null) {
            if (other.ensembleId != null)
                return false;
        } else if (!ensembleId.equals(other.ensembleId))
            return false;
        if (level == null) {
            if (other.level != null)
                return false;
        } else if (!level.equals(other.level))
            return false;
        if (location == null) {
            if (other.location != null)
                return false;
        } else if (!location.equals(other.location))
            return false;
        if (parameter == null) {
            if (other.parameter != null)
                return false;
        } else if (!parameter.equals(other.parameter))
            return false;
        if (secondaryId == null) {
            if (other.secondaryId != null)
                return false;
        } else if (!secondaryId.equals(other.secondaryId))
            return false;
        return true;
    }

}
