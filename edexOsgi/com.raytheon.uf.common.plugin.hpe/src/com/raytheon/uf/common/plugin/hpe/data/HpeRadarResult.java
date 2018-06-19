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
package com.raytheon.uf.common.plugin.hpe.data;

import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

import org.hibernate.annotations.Type;

import com.raytheon.uf.common.plugin.hpe.data.HpeEnums.HpeBiasSource;
import com.raytheon.uf.common.plugin.hpe.data.HpeEnums.HpeDataSource;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * HpeRadarResult table data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2014    3026    mpduff      Initial creation
 * Oct 12, 2016    5631    bkowal      Added JPA annotations.
 * 
 * </pre>
 * 
 * @author mpduff
 */

@Entity
@Table(name = "hperadarresult")
@DynamicSerialize
public class HpeRadarResult {

    @EmbeddedId
    @DynamicSerializeElement
    private HpeRadarResultId id;

    @DynamicSerializeElement
    @Column(name = "num_radar_avail", length = 5, nullable = true)
    private Short numRadarAvailable;

    @DynamicSerializeElement
    @Type(type = "com.raytheon.uf.common.plugin.hpe.data.HpeBiasSourceUserType")
    @Column(name = "bias_source", length = 20, nullable = true)
    private HpeBiasSource biasSource;

    @DynamicSerializeElement
    @Column(name = "radar_data_source", length = 1, nullable = true)
    @Enumerated(EnumType.STRING)
    private HpeDataSource radarDataSource;

    /**
     * Default constructor.
     */
    public HpeRadarResult() {
    }

    public HpeRadarResultId getId() {
        return id;
    }

    public void setId(HpeRadarResultId id) {
        this.id = id;
    }

    /**
     * @return the numRadarAvailable
     */
    public Short getNumRadarAvailable() {
        return numRadarAvailable;
    }

    /**
     * @param numRadarAvailable
     *            the numRadarAvailable to set
     */
    public void setNumRadarAvailable(Short numRadarAvailable) {
        this.numRadarAvailable = numRadarAvailable;
    }

    public HpeBiasSource getBiasSource() {
        return biasSource;
    }

    public void setBiasSource(HpeBiasSource biasSource) {
        this.biasSource = biasSource;
    }

    public HpeDataSource getRadarDataSource() {
        return radarDataSource;
    }

    public void setRadarDataSource(HpeDataSource radarDataSource) {
        this.radarDataSource = radarDataSource;
    }

    /**
     * Determine if this is an empty data object.
     * 
     * @return true if empty object, false if populated with data
     */
    public boolean isEmpty() {
        if (biasSource != null) {
            return false;
        }

        if ((id != null) && (id.getHpeProductName() != null
                && !id.getHpeProductName().trim().isEmpty())) {
            return false;
        }

        if (numRadarAvailable > 0) {
            return false;
        }

        if ((id != null) && (id.getProductTime() != null)) {
            return false;
        }

        if (radarDataSource != null) {
            return false;
        }

        // Empty file
        return true;
    }
}