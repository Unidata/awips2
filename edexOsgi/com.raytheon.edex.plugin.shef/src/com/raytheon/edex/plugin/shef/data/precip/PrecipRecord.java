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
package com.raytheon.edex.plugin.shef.data.precip;

import java.lang.reflect.Field;
import java.util.Date;

import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Duration;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Extremum;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElement;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.TypeSource;

/**
 * Precipitation Data record
 * <p>
 * Contains precipitation information collected and generated from various
 * sources. Sources of precipitation information include:
 * <ul>
 * <li>SHEF
 * <li>Gage Precipitation Processor (PP)
 * </ul>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2008 1548       jelkins     Initial creation
 * 10/16/2008   1548       jelkins     Integrated ParameterCode Types
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class PrecipRecord {

    /**
     * TODO Construct a precipitation record from METAR data
     */
    // public PrecipRecord(MetarData mt) {
    // }
    private String locationId;

    private PhysicalElement physicalElement;

    private Duration shefDuration;
    // Actual duration code - Needed for "V"
    private short timeDuration;
    
    private TypeSource typeSource;

    private Extremum extremum;

    private Date obsTime;
    
    private float value;

    private String qualifier;

    private long qualCode;

    public boolean revision;

    private String productId;

    private Date productTime;

    private Date postingTime;

    private int dataDuration;

    /**
     * Construct a precipitation record from SHEF data.
     * 
     * @param data
     */
    public PrecipRecord(ShefData data) {

        physicalElement = data.getPhysicalElement();
        shefDuration = data.getDuration();
        timeDuration = data.getDurationValue();
        
        extremum = data.getExtremum();
        
        obsTime = data.getObservationTimeObj();
        
        typeSource = data.getTypeSource();

        locationId = data.getLocationId();
        qualifier = data.getQualifier();
        value = (data.getValue() != null) ? data.getValue().floatValue() : 0;
        revision = data.isRevisedRecord();
    }
    
    /**
     * Reads the location id from the record.
     * <p>
     * The location id is usually used by the gage precipitation processor.
     * 
     * @return the locationId
     */
    public String getLocationId() {
        return locationId;
    }

    /**
     * Creates or updates the location id.
     * <p>
     * The location id is usually set within the SHEF decoder.
     * 
     * @param locationId
     *            the locationId to set
     */
    public void setLocationId(String locationId) {
        this.locationId = locationId;
    }

    /**
     * Reads the physical element from the record.
     * <p>
     * Set by the constructor when the record is first created.
     * 
     * @return the physicalElement
     */
    public PhysicalElement getPhysicalElement() {
        return physicalElement;
    }

    /**
     * Creates or updates the physical element.
     * <p>
     * Set by the constructor when the record is first created.
     * 
     * @param physicalElement
     *            the physicalElement to set
     */
    public void setPhysicalElement(PhysicalElement physicalElement) {
        this.physicalElement = physicalElement;
    }

    /**
     * Reads the SHEF duration from the record.
     * <p>
     * The SHEF duration along with the observation time can be used by the gage
     * precipitation processor to determine if a report has a duration of 1 hour
     * (PPH), 6 hours (PPQ), or 24 hours (PPP,PPH).
     * 
     * @return the shefDuration
     * 
     */
    public Duration getShefDuration() {
        return shefDuration;
    }

    /**
     * Creates or Updates the shef duration.
     * <p>
     * Set by constructor when the record is first created. Part of the PEDTSEP
     * string.
     * 
     * @param shefDuration
     *            the shefDuration to set
     */
    public void setShefDuration(Duration shefDuration) {
        this.shefDuration = shefDuration;
    }
    
    /**
     * @return the timeDuration
     */
    public short getTimeDuration() {
        return timeDuration;
    }

    /**
     * @param timeDuration the timeDuration to set
     */
    public void setTimeDuration(short timeDuration) {
        this.timeDuration = timeDuration;
    }

    /**
     * Reads the type source from the record.
     * <p>
     * 
     * @return the typeSource
     */
    public TypeSource getTypeSource() {
        return typeSource;
    }

    /**
     * Creates or updates the type source.
     * <p>
     * 
     * @param dataType
     *            the typeSource to set
     */
    public void setTypeSource(TypeSource dataType) {
        this.typeSource = dataType;
    }

    /**
     * Reads extremum from the record.
     * <p>
     * The extremum doesn't seem to be used by Gage_PP. It may be used by
     * something else.
     * 
     * @return the extremum
     */
    public Extremum getExtremum() {
        return extremum;
    }

    /**
     * Creates or Updates the extemum field.
     * 
     * @param extremum
     *            the extremum to set
     */
    public void setExtremum(Extremum extremum) {
        this.extremum = extremum;
    }

    /**
     * @return the obsTime
     */
    public Date getObsTime() {
        return obsTime;
    }

    /**
     * @param obsTime the obsTime to set
     */
    public void setObsTime(Date obsTime) {
        this.obsTime = obsTime;
    }

    /**
     * @return the value
     */
    public float getValue() {
        return value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(float value) {
        this.value = value;
    }

    /**
     * @return the qualifier
     */
    public String getQualifier() {
        return qualifier;
    }

    /**
     * @param qualifier
     *            the qualifier to set
     */
    public void setQualifier(String qualifier) {
        this.qualifier = qualifier;
    }

    /**
     * @return the qualCode
     */
    public long getQualCode() {
        return qualCode;
    }

    /**
     * @param qualityCode
     *            the qualCode to set
     */
    public void setQualCode(long qualityCode) {
        this.qualCode = qualityCode;
    }

    /**
     * @return the revision
     */
    public boolean isRevision() {
        return revision;
    }

    /**
     * @param b
     *            the revision to set
     */
    public void setRevision(boolean b) {
        this.revision = b;
    }

    /**
     * Reads
     * 
     * @return the productId
     */
    public String getProductId() {
        return productId;
    }

    /**
     * Creates or updates
     * 
     * @param productId
     *            the productId to set
     */
    public void setProductId(String productId) {
        this.productId = productId;
    }

    /**
     * @return the productTime
     */
    public Date getProductTime() {
        return productTime;
    }

    /**
     * @param productTime2
     *            the productTime to set
     */
    public void setProductTime(Date productTime2) {
        this.productTime = productTime2;
    }

    /**
     * Read the posting time field from the record.
     * 
     * @return the postingTime
     */
    public Date getPostingTime() {
        return postingTime;
    }

    /**
     * Create or update the posting time
     * 
     * @param postTime
     *            the postingTime to set
     */
    public void setPostingTime(Date postTime) {
        this.postingTime = postTime;
    }

    /**
     * @return the dataDuration
     */
    public int getDataDuration() {
        return dataDuration;
    }

    /**
     * Creates or updates the data duration.
     * <p>
     * The data duration is set within the gage precipitation processor.
     * 
     * @param dataDuration
     *            the dataDuration to set
     */
    public void setDataDuration(int dataDuration) {
        this.dataDuration = dataDuration;
    }

    public String toString() {
        return PrecipRecord.toString(this);
    }

    /**
     * Get a string representation of the object state
     * <p>
     * TODO move this method into a more generic location and class
     * 
     * @param o
     *            Class to perform the toString on
     * @return a string containing the class name and the names of all fields
     *         and associated field values. A null pointer exception will be
     *         thrown if the object has no fields.
     */
    public static String toString(Object o) {

        StringBuilder returnValue = new StringBuilder();

        returnValue.append(o.getClass().getSimpleName());
        returnValue.append(" :: [ ");

        for (Field field : o.getClass().getDeclaredFields()) {

            returnValue.append(field.getName());
            returnValue.append(" = \"");

            try {
                field.setAccessible(true);
                returnValue.append(field.get(o));
            } catch (IllegalArgumentException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (IllegalAccessException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            returnValue.append("\", ");

        }

        returnValue.setLength(returnValue.length() - 3);
        returnValue.append(" ]");

        return returnValue.toString();
    }

}