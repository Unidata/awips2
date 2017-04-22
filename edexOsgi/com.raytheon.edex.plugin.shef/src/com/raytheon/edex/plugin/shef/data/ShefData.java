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
package com.raytheon.edex.plugin.shef.data;

import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.plugin.shef.util.SHEFDate;
import com.raytheon.edex.plugin.shef.util.ShefParm;
import com.raytheon.uf.common.dataplugin.shef.tables.IngestfilterId;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Duration;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Extremum;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElement;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.Probability;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.TypeSource;
import com.raytheon.uf.common.dataplugin.shef.util.SHEFTimezone;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * SHEF Data parent object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/19/08     387         M. Duff     Initial creation.  
 * 10/16/2008   1548        jelkins     Integrated ParameterCode Types
 * 04/29/2014   3088        mpduff      cleanup.
 * 06/26/2014   3321        mpduff      Added ingestfilter primary key getter.
 * 
 * </pre>
 */
public class ShefData implements ISerializableObject {

    private String stringValue = null;

    private Double value = null;

    private String qualifier = "Z";

    private String locationId = null;

    // Only used for B records.
    private String dataSource = null;

    private PhysicalElement physicalElement = PhysicalElement.HEIGHT_RIVER_STAGE;

    private Duration duration = Duration.INSTANTENOUS;

    private Short durationValue = null;

    /**
     * contains a variable duration code in the case the a variable duration is
     * specified instead of the standard duration codes
     */
    private String durationCodeVariable = null;

    private TypeSource typeSource = TypeSource.READING_NONSPECIFIC;

    private String dataTypeCode = TypeSource.READING_NONSPECIFIC.getCode()
            .substring(0, 1);

    private String dataSourceCode = TypeSource.READING_NONSPECIFIC.getSource();

    private Extremum extremum = Extremum.NULL;

    private Probability probability = Probability.NULL;

    private String retainedComment = null;

    private String rawData = null;

    private String observationTime = null;

    private SHEFDate obsTime = null;

    private String unitsCode = null;

    private String creationDate = null;

    private SHEFDate createTime = null;

    private int timeSeriesId = ShefConstants.SHEF_NOT_SERIES;

    private String parameterCodeString = null;

    private boolean revisedRecord = false;

    /**
     * Empty constructor
     */
    public ShefData() {

    }

    /**
     * @return the stringValue
     */
    public String getStringValue() {
        return stringValue;
    }

    /**
     * @param stringValue
     *            the stringValue to set
     */
    public void setStringValue(String stringValue) {
        this.stringValue = stringValue;
        try {
            boolean neg = false;
            int negPos = stringValue.indexOf('-');
            if (negPos >= 0) {
                stringValue = stringValue.substring(negPos + 1);
                neg = true;
            }
            value = Double.parseDouble(stringValue);
            if (neg && Math.signum(value) != 0) {
                value *= -1.0;
            }
        } catch (NumberFormatException nfe) {
            value = null;
        } catch (NullPointerException npe) {
            value = null;
        }
    }

    /**
     * @return the value
     */
    public Double getValue() {
        return value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(Double value) {
        this.value = value;
    }

    /**
     * @return the qualifier
     */
    public String getQualifier() {
        return qualifier;
    }

    /**
     * @param qual
     *            the qualifier to set
     */
    public void setQualifier(String qual) {
        qualifier = (qual == null) ? "Z" : qual;
    }

    /**
     * @return the locationId
     */
    public String getLocationId() {
        return locationId;
    }

    /**
     * @param locationId
     *            the locationId to set
     */
    public void setLocationId(String locationId) {
        this.locationId = locationId;
    }

    /**
     * @return the dataSource
     */
    public String getDataSource() {
        return dataSource;
    }

    /**
     * @param dataSource
     *            the dataSource to set
     */
    public void setDataSource(String dataSource) {
        this.dataSource = dataSource;
    }

    /**
     * @return the timeSeriesId
     */
    public int getTimeSeriesId() {
        return timeSeriesId;
    }

    /**
     * @param timeSeriesId
     *            the timeSeriesId to set
     */
    public void setTimeSeriesId(int timeSeriesId) {
        this.timeSeriesId = timeSeriesId;
    }

    /**
     * Get the parameter code string
     * 
     * @return the parameterCode
     */
    public String getParameterCodeString() {
        return parameterCodeString;
    }

    /**
     * Set the parameter code string
     * 
     * @param peCode
     *            the parameterCode to set
     * @param variableDuration
     */
    public void setParameterCodeString(String peCode, String variableDuration) {
        if ((peCode != null) && (peCode.length() >= 2)) {
            parameterCodeString = peCode;
            PhysicalElement pe = PhysicalElement
                    .getEnum(peCode.substring(0, 2));
            if (!PhysicalElement.UNKNOWN.equals(pe)) {

                // Set up default values for PEDTSEP
                String paramProbability = Probability.NULL.getCode();
                String paramExtremum = Extremum.NULL.getCode();
                String paramType = TypeSource.READING_NONSPECIFIC.getCode()
                        .substring(0, 1);
                String paramSource = TypeSource.READING_NONSPECIFIC.getSource();
                String paramDuration = "Z";

                switch (peCode.length()) {
                case 7: {
                    paramProbability = peCode.substring(6, 7);
                }
                case 6: {
                    paramExtremum = peCode.substring(5, 6);
                }
                case 5: {
                    paramSource = peCode.substring(4, 5);
                }
                case 4: {
                    paramType = peCode.substring(3, 4);
                    if ("Z".equals(paramType)) {
                        paramType = "R";
                    }
                }
                case 3: {
                    paramDuration = peCode.substring(2, 3);
                }
                case 2: {
                    setProbability(Probability.getEnum(paramProbability));

                    setExtremum(Extremum.getEnum(paramExtremum));

                    // check to see if this is a valid typesource
                    String key = paramType + paramSource;

                    Integer n = ShefParm.getTypeSourceCode(key);
                    if ((n != null) && (n == 1)) {
                        TypeSource ts = TypeSource.getEnum(key);
                        dataTypeCode = paramType;
                        dataSourceCode = paramSource;

                        setTypeSource(ts);
                    } else {

                    }

                    Duration duration = Duration.INSTANTENOUS;
                    if ("Z".equals(paramDuration)) {
                        // Use the default duration code for this PE
                        duration = ParameterCode.Duration.getDefault(pe);
                    } else if ("V".equals(paramDuration)) {
                        duration = Duration.VARIABLE_PERIOD;
                    } else {
                        duration = Duration.getEnum(paramDuration);
                    }
                    // For now set the codes. We'll fix up the actual duration
                    // numeric code later.
                    setDuration(duration);
                    setDurationCodeVariable(paramDuration);

                    setPhysicalElement(pe);
                    break;
                }
                default: {
                    // This is an error condition!
                }
                }
            }
        }
    }

    /**
     * Get the retained comment
     * 
     * @return the retainedComment
     */
    public String getRetainedComment() {
        return retainedComment;
    }

    /**
     * Set the retained comment
     * 
     * @param comment
     *            the retainedComment to set
     */
    public void setRetainedComment(String comment) {
        if ((comment != null) && (comment.length() == 0)) {
            comment = null;
        }
        retainedComment = comment;
    }

    /**
     * Get the raw data
     * 
     * @return the rawData
     */
    public String getRawData() {
        return rawData;
    }

    /**
     * Set the raw data
     * 
     * @param rawData
     *            the rawData to set
     */
    public void setRawData(String rawData) {
        this.rawData = rawData;
    }

    /**
     * Get the physical element
     * 
     * @return the physicalElement
     */
    public PhysicalElement getPhysicalElement() {
        return physicalElement;
    }

    /**
     * Set the physical element
     * 
     * @param element
     *            the physicalElement to set
     */
    public void setPhysicalElement(PhysicalElement element) {
        physicalElement = element;
    }

    /**
     * Get the duration
     * 
     * @return the duration
     */
    public Duration getDuration() {
        return duration;
    }

    /**
     * Set the duration
     * 
     * @param duration
     *            the duration to set
     */
    public void setDuration(Duration duration) {
        this.duration = duration;
    }

    /**
     * @return the durationValue
     */
    public Short getDurationValue() {
        return durationValue;
    }

    /**
     * @param duration
     *            the durationValue to set
     */
    public void setDurationValue(Short duration) {
        durationValue = duration;
    }

    /**
     * Get the extremum
     * 
     * @return the extremum
     */
    public Extremum getExtremum() {
        return extremum;
    }

    /**
     * Set the extremum
     * 
     * @param extremum
     *            the extremum to set
     */
    public void setExtremum(Extremum extremum) {
        this.extremum = extremum;
    }

    /**
     * Get the probability
     * 
     * @return the probability
     */
    public Probability getProbability() {
        return probability;
    }

    /**
     * Set the probability
     * 
     * @param probability
     *            the probability to set
     */
    public void setProbability(Probability probability) {
        this.probability = probability;
    }

    /**
     * Get the observation time
     * 
     * @return the observationTime
     */
    public String getObservationTime() {
        return observationTime;
    }

    /**
     * Set the observation time
     * 
     * @param anObservationTime
     *            the observationTime to set
     */
    public void setObservationTime(String anObservationTime) {
        observationTime = anObservationTime;
    }

    /**
     * Get the units code
     * 
     * @return the unitsCode
     */
    public String getUnitsCode() {
        return unitsCode;
    }

    /**
     * Set the units code
     * 
     * @param unitsCode
     *            the unitsCode to set
     */
    public void setUnitsCode(String unitsCode) {
        this.unitsCode = unitsCode;
    }

    /**
     * Get the creation date
     * 
     * @return the creationDate
     */
    public String getCreationDate() {
        return creationDate;
    }

    /**
     * Set the creation date
     * 
     * @param creationDate
     *            the creationDate to set
     */
    public void setCreationDate(String creationDate) {
        this.creationDate = creationDate;
    }

    /**
     * Get the creation date Date object
     * 
     * @return the creationDateObj
     */
    public Date getCreationDateObj() {
        Date retDate = null;
        if (createTime != null) {
            retDate = createTime.toCalendar().getTime();
        }
        return retDate;
    }

    /**
     * Set the creation date Date obj
     * 
     * @param creationDate
     *            the creationDateObj to set
     */
    public void setCreationDateObj(Date creationDate) {
        SHEFDate d = new SHEFDate(creationDate, SHEFTimezone.GMT_TIMEZONE);
        if (d != null) {
            createTime = d;
        }
    }

    /**
     * @return the createTime
     */
    public SHEFDate getCreateTime() {
        return createTime;
    }

    /**
     * @param createTime
     *            the createTime to set
     */
    public void setCreateTime(SHEFDate createTime) {
        if (createTime != null) {
            this.createTime = new SHEFDate(createTime);
        }
    }

    /**
     * Get the duration code
     * 
     * @return the durationCode
     */
    public String getDurationCodeVariable() {
        return durationCodeVariable;
    }

    /**
     * Set the duration code
     * 
     * @param durationCode
     *            the durationCode to set
     */
    public void setDurationCodeVariable(String durationCode) {
        durationCodeVariable = durationCode;
    }

    /**
     * Get the observation time Date object
     * 
     * @return the observationTimeObj
     */
    public Date getObservationTimeObj() {
        Date retDate = null;
        if (obsTime != null) {
            retDate = obsTime.toCalendar().getTime();
        }
        return retDate;
    }

    /**
     * Set the observation time Date object
     * 
     * @param observationTime
     *            the observationTimeObj to set
     */
    public void setObservationTimeObj(Date observationTime) {
        SHEFDate d = new SHEFDate(observationTime, SHEFTimezone.GMT_TIMEZONE);
        if (d != null) {
            obsTime = d;
        }
    }

    public void setObsTime(SHEFDate date) {
        if (date != null) {
            obsTime = new SHEFDate(date);
        }
    }

    public SHEFDate getObsTime() {
        return obsTime;
    }

    /**
     * @return the typeSource
     */
    public ParameterCode.TypeSource getTypeSource() {
        return typeSource;
    }

    /**
     * @param typeSource
     *            the typeSource to set
     */
    public void setTypeSource(ParameterCode.TypeSource typeSource) {
        this.typeSource = typeSource;
    }

    /**
     * @return the revisedRecord
     */
    public boolean isRevisedRecord() {
        return revisedRecord;
    }

    /**
     * @param revisedRecord
     *            the revisedRecord to set
     */
    public void setRevisedRecord(boolean revisedRecord) {
        this.revisedRecord = revisedRecord;
    }

    /**
     * Perform duration fixup for variable period.
     * 
     * @param data
     *            ShefData instance to check.
     */
    public int fixupDuration(Short durationValue) {
        int errorCode = 0;
        if (duration != null) {
            if (Duration.VARIABLE_PERIOD.equals(duration)) {
                if (durationValue != null) {
                    setDurationValue(durationValue);
                } else {
                    errorCode = -1;
                }
            } else {
                setDurationValue((short) duration.getValue());
            }
        } else {
            errorCode = -2;
        }
        return errorCode;
    }

    /**
     * Processes all internal data so that it is ready for PostSHEF.
     * 
     * <pre>
     * 1. All dates converted to UTC. 
     * 2. All data values converted to their English equivalent. 
     * 3. Ensure that all "defaults" are set correctly for output.
     * </pre>
     */
    public void toPostData() {
        if ("S".equals(unitsCode)) {
            if (physicalElement != null) {
                String key = physicalElement.getCode();
                Double cf = ShefParm.getPhysicalElementConversionFactor(key);
                Double n = doConversion(physicalElement, unitsCode, value);
                if (n == null) {
                    if (cf != null) {
                        value *= cf;
                    }
                } else {
                    value = n;
                }
                stringValue = String.format("%f", value);
                unitsCode = "E";
            }
        }
        if (createTime != null) {
            createTime.toZuluDate();
        }
        if (obsTime != null) {
            obsTime.toZuluDate();
        }
        switch (getPhysicalElement()) {
        case PRECIPITATION_ACCUMULATOR:
        case PRECIPITATION_INCREMENT:
        case PRECIPITATION_INCREMENT_DAILY: {
            if (getValue() >= 0) {
                String val = getStringValue();
                // Is there a decimal point in the value?
                if (val.indexOf('.') < 0) {
                    double value = getValue() / 100.0;
                    setStringValue(String.format("%.3f", value));
                }
            }
            break;
        }
        }
    }

    /**
     * 
     * @param divisor
     * @param base
     * @param multiplier
     * @param adder
     */
    public void adjustValue(double divisor, double base, double multiplier,
            double adder) {
        double adjustedValue = Double.parseDouble(stringValue);
        adjustedValue = (adjustedValue / divisor + base) * multiplier + adder;
        value = adjustedValue;
        stringValue = String.valueOf(adjustedValue);
    }

    public StringBuilder toString(StringBuilder receiver) {
        if (receiver == null) {
            receiver = new StringBuilder();
        }
        receiver.append(String.format("%-8s", locationId));
        if (obsTime != null) {
            receiver.append(obsTime.toOutString());
        } else {
            receiver.append("   0 0 0 0 0 0");
        }
        receiver.append(" ");
        if (createTime != null) {
            receiver.append(createTime.toOutString());
        } else {
            receiver.append("   0 0 0 0 0 0");
        }
        receiver.append(" ");
        // PE
        receiver.append(physicalElement.getCode());
        receiver.append(" ");
        // Type Code
        if (TypeSource.UNKNOWN.equals(typeSource)) {
            receiver.append("  ");
        } else {
            receiver.append(dataTypeCode);
            // Source Code
            receiver.append(dataSourceCode);
        }
        // Extremnum
        receiver.append(extremum.getCode());
        // Data Value
        if (value != null) {
            receiver.append(String.format("%10.3f", value));
        } else {
            receiver.append(String.format("%10s", ShefConstants.SHEF_MISSING));
        }
        receiver.append(" ");
        // Data Qualifier
        receiver.append((qualifier != null) ? qualifier : " ");
        if (probability != null) {
            Double p = probability.getValue();
            receiver.append(String.format("%6.2f", p));
        } else {
            receiver.append("      ");
        }

        if (durationValue != null) {
            receiver.append(String.format("%5d", durationValue));
        } else {
            receiver.append(String.format("%5d", 0));
        }
        // Revision code
        receiver.append((revisedRecord) ? " 1" : " 0");
        receiver.append(" ");
        // Data source
        receiver.append(String.format("%-8s", (dataSource != null) ? dataSource
                : " "));
        receiver.append(" ");
        // Time series indicator
        receiver.append(String.format("%3d", timeSeriesId));
        receiver.append(" ");
        // Full Parameter code
        receiver.append(String.format("%-7s", parameterCodeString));
        receiver.append(" ");
        // Unused
        receiver.append(String.format("%8s", " "));
        receiver.append(" ");
        if (retainedComment != null) {
            receiver.append(retainedComment);
        }
        return receiver;
    }

    /**
     * Human readable output of data stored in this object
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        return toString(sb).toString();
    }

    /**
     * The data's PETSEP.
     * 
     * @return
     */
    public String getPeDTsE() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.getPhysicalElement().getCode());
        sb.append(this.getTypeSource().getCode());
        sb.append(this.getExtremum().getCode());
        sb.append(this.getDurationCodeVariable());
        return sb.toString();
    }

    /**
     * Get the ingest filter table primary key value for this data object.
     * 
     * @return The primary key object
     */
    public IngestfilterId getIngestFilterKey() {
        IngestfilterId id = new IngestfilterId();
        id.setLid(this.getLocationId());
        id.setDur(this.getDurationValue());
        id.setExtremum(this.getExtremum().getCode());
        id.setPe(this.getPhysicalElement().getCode());
        id.setTs(this.getTypeSource().getCode());
        return id;
    }

    /**
     * 
     * @param element
     * @param unitCode
     * @param dValue
     * @return The converted value or null to indicate no conversion took place.
     */
    private Double doConversion(PhysicalElement element, String unitCode,
            Double dValue) {
        if (dValue != null) {
            if (element != null) {
                switch (element) {
                case TEMPERATURE_AIR_DRY:
                case TEMPERATURE_COOLING:
                case TEMPERATURE_DEW:
                case TEMPERATURE_FREEZING:
                case TEMPERATURE_HEATING:
                case TEMPERATURE_AIR_WET:
                case TEMPERATURE_AIR_MINIMUM:
                case TEMPERATURE_PAN_WATER:
                case TEMPERATURE_ROAD_SURFACE:
                case TEMPERATURE_WATER:
                case TEMPERATURE_AIR_MAXIMUM:
                case TEMPERATURE_FREEZING_SURFACE: {
                    if ("S".equals(unitCode)) {
                        dValue = ((value * 9.0) / 5.0) + 32;
                    }
                    break;
                }
                default: {
                    dValue = null;
                }
                }
            }
        }
        return dValue;
    }

    /**
     * 
     * @param args
     */
    public static final void main(String[] args) {

        // ShefData d = new ShefData();
        //
        // d.setParameterCodeString("AD","Z");
        //
        // System.out.println(d);
        //
        // double dv = 0.04;
        //
        // System.out.println(String.format("[%.3f]",dv));
        //

        double adjustedValue = 10;
        double divisor = 1;
        double base = 0;
        double multiplier = 1000;
        double adder = 0;

        double n = (adjustedValue / divisor + base) * multiplier + adder;

        System.out.println(n);

        Pattern Q_CODES = Pattern.compile("Q[^BEF]");
        Matcher m = Q_CODES.matcher("QI");
        if (m.matches()) {
            System.out.println("found");
        }

    }
}
