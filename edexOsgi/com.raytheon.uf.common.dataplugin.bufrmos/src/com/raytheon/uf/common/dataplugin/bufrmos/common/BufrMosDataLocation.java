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
package com.raytheon.uf.common.dataplugin.bufrmos.common;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.annotations.NullFloat;
import com.raytheon.uf.common.dataplugin.annotations.NullString;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Common location objects for MOS Data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2011            rjpeter     Initial creation
 * Jun 28, 2012 827        dgilling    Annotate id field for serialization.
 * Jul 26, 2013 1051       bsteffen    Discard bufrmos data with invalid
 *                                     location.
 * Nov 04, 2013 2361       njensen     Remove XML annotations
 * Jul 22, 2015 4360       rferrel     Named unique constraint;
 *                                      stationid, latitude and longitude no longer nullable.
 * Jan 12, 2016 4677       tgurney     Make id a sequence-generated field.
 * Jan 19, 2016 4677       tgurney     Remove generateId method
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "bufrmos_locationseq", allocationSize = 1)
@Table(name = "bufrmos_location", uniqueConstraints = { @UniqueConstraint(name = "uk_bufrmos_location_datauri_fields", columnNames = {
        "stationId", "latitude", "longitude" }) })
@DynamicSerialize
public class BufrMosDataLocation extends PersistableDataObject {
    private static final long serialVersionUID = 1L;

    /** The id */
    @DynamicSerializeElement
    @GeneratedValue(generator = PluginDataObject.ID_GEN)
    @Id
    private Integer id;

    // Id of the station making this observation.
    @Column(length = 48, nullable = false)
    @Index(name = "mosLocationStationIndex")
    @DataURI(position = 0)
    @NullString
    @DynamicSerializeElement
    private String stationId;

    @DataURI(position = 1)
    @NullFloat
    @Column(nullable = false)
    @DynamicSerializeElement
    private Double latitude;

    @DataURI(position = 2)
    @NullFloat
    @Column(nullable = false)
    @DynamicSerializeElement
    private Double longitude;

    public Double getLatitude() {
        return latitude;
    }

    public void setLatitude(Double latitude) {
        this.latitude = latitude;
    }

    public Double getLongitude() {
        return longitude;
    }

    public void setLongitude(Double longitude) {
        this.longitude = longitude;
    }

    /**
     * @return the stationId
     */
    public String getStationId() {
        return stationId;
    }

    /**
     * @param stationId
     *            the stationId to set
     */
    public void setStationId(String stationId) {
        this.stationId = stationId;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public boolean isValid() {
        if (longitude == null || latitude == null) {
            return false;
        }
        if (latitude > 90.0 || latitude < -90.0) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((latitude == null) ? 0 : latitude.hashCode());
        result = prime * result
                + ((longitude == null) ? 0 : longitude.hashCode());
        result = prime * result
                + ((stationId == null) ? 0 : stationId.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        BufrMosDataLocation other = (BufrMosDataLocation) obj;
        if (latitude == null) {
            if (other.latitude != null) {
                return false;
            }
        } else if (!latitude.equals(other.latitude)) {
            return false;
        }
        if (longitude == null) {
            if (other.longitude != null) {
                return false;
            }
        } else if (!longitude.equals(other.longitude)) {
            return false;
        }
        if (stationId == null) {
            if (other.stationId != null) {
                return false;
            }
        } else if (!stationId.equals(other.stationId)) {
            return false;
        }
        return true;
    }
}
