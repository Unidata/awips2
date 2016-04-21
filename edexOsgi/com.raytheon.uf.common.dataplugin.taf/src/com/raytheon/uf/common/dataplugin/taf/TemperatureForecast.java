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
 * 
 * A temperature forecast found in a taf message.
 * 
 * <pre>
 * 
 *   SOFTWARE HISTORY
 *  
 *   Date           Ticket#     Engineer    Description
 *   ------------   ----------  ----------- --------------------------
 *   8/30/2006                  bphillip    Initial Creation
 *   6/21/2007      180         bphillip    Updated for use with plugin persistence pattern
 *   Nov 01, 2013   2361        njensen     Remove XML annotations
 *   May 15, 2014   3002        bgonzale    Moved to com.raytheon.uf.common.dataplugin.taf.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "taf_temperature_forecasts")
@DynamicSerialize
public class TemperatureForecast extends PersistableDataObject {

    @Id
    @GeneratedValue
    private int id;

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /** The taf record this weather condition object belongs to * */
    @ManyToOne
    @JoinColumn(name = "parentID", nullable = false)
    private ChangeGroup parentID;

    /** Temperature valid time */
    @DynamicSerializeElement
    @Column
    private Integer valid_time;

    /** Surface temperature */
    @DynamicSerializeElement
    @Column
    private Integer sfc_temp_c;

    public TemperatureForecast() {

    }

    /**
     * Constructor for new temperature forecast
     * 
     * @param negative
     *            "M" if temperature is negative
     * @param temp
     *            The temperature
     * @param time
     *            The valid time for the temperature
     */
    public TemperatureForecast(ChangeGroup parentid, String negative,
            String temp, String time) {
        this.parentID = parentid;
        sfc_temp_c = Integer.parseInt(temp);
        if (negative != null) {
            sfc_temp_c *= -1;
        }

        valid_time = Integer.parseInt(time);
    }

    /**
     * Converts Temperature Forecast object to a string
     */
    @Override
    public String toString() {
        return "\nTemperature Forecast-> Surface Temp: " + sfc_temp_c + " at "
                + valid_time + " Z";
    }

    public ChangeGroup getParentID() {
        return parentID;
    }

    public void setParentID(ChangeGroup parentID) {
        this.parentID = parentID;
    }

    public Integer getValid_time() {
        return valid_time;
    }

    public void setValid_time(Integer valid_time) {
        this.valid_time = valid_time;
    }

    public Integer getSfc_temp_c() {
        return sfc_temp_c;
    }

    public void setSfc_temp_c(Integer sfc_temp_c) {
        this.sfc_temp_c = sfc_temp_c;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof TemperatureForecast) {
            TemperatureForecast forecast = (TemperatureForecast) obj;

            if (this.parentID != forecast.parentID) {
                return false;
            }

            if (!(this.valid_time == null ? forecast.getValid_time() == null
                    : this.valid_time.equals(forecast.getValid_time()))) {
                return false;
            }

            if (!(this.sfc_temp_c == null ? forecast.getSfc_temp_c() == null
                    : this.sfc_temp_c.equals(forecast.getSfc_temp_c()))) {
                return false;
            }

            return true;

        } else {
            return false;
        }

    }

    @Override
    public int hashCode() {

        return new HashCodeBuilder(17, 37).append(parentID).append(
                this.valid_time).append(this.sfc_temp_c).toHashCode();
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

}
