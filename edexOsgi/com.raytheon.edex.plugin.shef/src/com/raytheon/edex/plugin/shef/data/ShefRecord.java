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

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.edex.plugin.shef.util.SHEFDate;
import com.raytheon.edex.plugin.shef.util.ShefUtil;
import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode;
import com.raytheon.uf.common.dataplugin.shef.util.SHEFTimezone;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.time.DataTime;

/**
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * June2006		3,14		Phillippe	Initial Creation.	
 * 20071129     472         jkorman     Added IDecoderGettable interface.
 * 19Mar2008    387         M. Duff     Modified to store SHEF data.
 * May 07, 2013	1869      	bsteffen   	Remove dataURI column from
 *                                      PluginDataObject.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class ShefRecord extends PluginDataObject {
    
    public static enum ShefType {
        A, B, E;
    }
    
    private static final long serialVersionUID = 2726928942130489733L;

    private static final int MILLIS_PER_MINUTE = 1000 * 60;

    /**
     * Collection of SHEF data values for this record
     */
    @Transient
    private List<ShefData> dataValues = null;
    
    @Transient
    protected String rawMessage = null;

    @Transient
    protected ShefType shefType = null;

    @Transient
    protected boolean revisedRecord = false;

    @Transient
    protected String recordDate = null;

    @Transient
    protected Date recordDateObj = null;

    // protected String timeZone = ShefConstants.Z; // Defaults to Z/GMT

    @Transient
    protected TimeZone timeZone = SHEFTimezone.getSysTimeZone(ShefConstants.Z);

    @Transient
    protected String creationDate = null;

    @Transient
    protected Date creationDateObj = null;

    @Transient
    protected String dataStringQualifier = "Z";

    @Transient
    protected String durationCode = null;

    @Transient
    protected ArrayList<String> commentArray = new ArrayList<String>();

    @Transient
    protected String wmoHeader = null;

    @Transient
    protected Date productTime = null;

    @Transient
    private String locationId = null;

    @Transient
    private String messageSource = null;

    @Transient
    private SHEFDate obsDate = null;
    
    @Transient
    private SHEFDate createDate = null;

    @Transient
    private int durationValue = ParameterCode.Duration.DEFAULT.getValue();

    /**
     * Empty constructor
     */
    public ShefRecord() {

    }

    /**
     * Get the data stored in this object
     * 
     * @return Vector of data objects
     */
    public List<ShefData> getDataValues() {
        return dataValues;
    }

    /**
     * Set the data values
     * 
     * @param dataValues
     *            array of data values
     */
    public void setDataValues(List<ShefData> values) {
        dataValues = values;
    }

    public void addDataValue(ShefData value) {
        if(dataValues == null) {
            dataValues = new ArrayList<ShefData>();
        }
        dataValues.add(value);
    }
    
    /**
     * Get the record type.
     * 
     * @return This record's type.
     */
    public ShefType getShefType() {
        return shefType;
    }

    /**
     * Set the SHEF type
     * 
     * @param shefType
     *            The type of this record.
     */
    public void setShefType(ShefType type) {
        shefType = type;
    }

    /**
     * Get the raw message
     * 
     * @return the rawMessage
     */
    public String getRawMessage() {
        return rawMessage;
    }

    /**
     * Set the raw message
     * 
     * @param message
     */
    public void setRawMessage(String message) {
        this.rawMessage = message;
    }

    /**
     * Get the record date
     * 
     * @return the recordDate
     */
    public String getRecordDate() {
        return recordDate;
    }

    /**
     * Get the record date as a Date object
     * 
     * @return record date as a date object
     */
    public Date getRecordDateObj() {
        return recordDateObj;
    }

    /**
     * Set the record date Date object
     * 
     * @param recordDateObj
     *            the recordDateObj to set
     */
    public void setRecordDateObj(Date recordDateObj) {
        this.recordDateObj = recordDateObj;
    }

    /**
     * Record date is in the format "mmdd" or "yymmdd" or “ccyymmdd” where
     * 
     * mm = Month, two digits (01-12) dd = Day, two digits (01-31) yy = Year,
     * two digits (00-99) cc = Century, two digits (17-21)
     * 
     * @param recordDate
     *            the recordDate to set
     * @throws ParseException
     */
    public void setRecordDate(String recordDate) throws ParseException {
        this.recordDate = recordDate;
        if (recordDate.length() == 4) {
            int year = ShefUtil.getFullYear(recordDate);
            synchronized(ShefConstants.YYYYMMDD_FORMAT){
                recordDateObj = ShefConstants.YYYYMMDD_FORMAT.parse(year
                        + recordDate);
            }
        } else if (recordDate.length() == 6) {
            int year = ShefUtil.getFullYear(recordDate);
            year = year / 100;
            synchronized(ShefConstants.YYYYMMDD_FORMAT){
                recordDateObj = ShefConstants.YYYYMMDD_FORMAT.parse(year
                        + recordDate);
            }
        } else { // recordDate length must be 8
            synchronized(ShefConstants.YYYYMMDD_FORMAT){
                recordDateObj = ShefConstants.YYYYMMDD_FORMAT.parse(recordDate);
            }
        }

        /* Set the time zone into the object */
        Calendar c = new GregorianCalendar();
        c.setTime(recordDateObj);
        c.setTimeZone(getTimeZoneObject());
        recordDateObj = c.getTime();
    }

    /**
     * 
     * @param date
     */
    public void setRecordDate(SHEFDate date) {
        obsDate = date;
        recordDate = date.toLocal();
        recordDateObj = date.toCalendar().getTime();
    }

    /**
     * 
     * @param date
     */
    public void setObsDate(SHEFDate date) {
        obsDate = date;
    }
    
    /**
     * 
     * @return
     */
    public SHEFDate getObsDate() {
        return obsDate;
    }
    
    /**
     * Get the time zone code
     * 
     * @return the timeZone
     */
    public String getTimeZoneCode() {
        return timeZone.getID();
    }

    /**
     * @return the commentArray
     */
    public ArrayList<String> getCommentArray() {
        return commentArray;
    }

    /**
     * @param commentArray
     *            the commentArray to set
     */
    public void setCommentArray(ArrayList<String> commentArray) {
        this.commentArray = commentArray;
    }

    /**
     * Get the TimeZone object
     * 
     * @return the TimeZone object
     */
    public TimeZone getTimeZoneObject() {
        return timeZone;
    }

    public void setTimeZone(TimeZone timezone) {
        timeZone = timezone;
    }

    /**
     * Set the time zone
     * 
     * @param timeZone
     *            the timeZone to set
     */
    public void setTimeZone(String timezone) {
        timeZone = SHEFTimezone.getSysTimeZone(timezone);
    }

    /**
     * @return the timeZone
     */
    public TimeZone getTimeZone() {
        return timeZone;
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
     * @throws ParseException
     */
    public void setCreationDate(String date) {
        creationDate = date;
    }

    /**
     * 
     * @param date
     */
    public void setCreationDate(SHEFDate date) {
        if(date != null) {
            createDate = date;
            creationDate = date.toLocal();
            creationDateObj = date.toCalendar().getTime();
        } else {
            createDate = null;
            creationDate = null;
            creationDateObj = null;
        }
    }
    

    /**
     * Get the creation date Date object
     * 
     * @return the creationDateObj
     */
    public Date getCreationDateObj() {
        return creationDateObj;
    }

    /**
     * Set the creation date Date object
     * 
     * @param creationDateObj
     *            the creationDateObj to set
     */
    public void setCreationDateObj(Date creationDateObj) {
        this.creationDateObj = creationDateObj;
    }

    /**
     * Get the data string qualifier
     * 
     * @return the dataStringQualifier
     */
    public String getDataStringQualifier() {
        return dataStringQualifier;
    }

    /**
     * Set the data string qualifier
     * 
     * @param dataStringQualifier
     *            the dataStringQualifier to set
     */
    public void setDataStringQualifier(String dataStringQualifier) {
        this.dataStringQualifier = dataStringQualifier;
    }

    /**
     * Get the duration code
     * 
     * @return the durationCode
     */
    public String getDurationCode() {
        return durationCode;
    }

    /**
     * Set the duration code
     * 
     * @param durationCode
     *            the durationCode to set
     */
    public void setDurationCode(String durationCode) {
        this.durationCode = durationCode;
    }

    /**
     * Get the comments
     * 
     * @return the commentArray
     */
    public String[] getComments() {
        if (commentArray != null) {
            return commentArray.toArray(new String[0]);
        }
        return null;
    }

    /**
     * Add a comment to the list of comments
     * 
     * @param comment
     */
    public void setComment(String comment) {
        commentArray.add(comment);
    }

    /**
     * Is this record a revised record
     * 
     * @return true if the record is revised
     */
    public boolean isRevisedRecord() {
        return revisedRecord;
    }

    /**
     * Set whether the record is revised or not
     * 
     * @param revisedRecord
     *            the revisedRecord to set
     */
    public void setRevisedRecord(boolean revisedRecord) {
        this.revisedRecord = revisedRecord;
    }

    /**
     * @return the wmoHeader
     */
    public String getWmoHeader() {
        return wmoHeader;
    }

    /**
     * @param wmoHeader
     *            the wmoHeader to set
     */
    public void setWmoHeader(String wmoHeader) {
        this.wmoHeader = wmoHeader;
    }

    /**
     * @return the productTime
     */
    public Date getProductTime() {
        return productTime;
    }

    /**
     * @param productTime
     *            the productTime to set
     */
    public void setProductTime(Date productTime) {
        this.productTime = productTime;
    }

    @Override
    public DataTime getDataTime() {
        return null;
    }

    /**
     * Get the location id
     * 
     * @return the locationId
     */
    public String getLocationId() {
        return locationId;
    }

    /**
     * Set the location id
     * 
     * @param locationId
     *            the locationId to set
     */
    public void setLocationId(String lid) {
        locationId = lid;
    }

    /**
     * @return the messageSource
     */
    public String getMessageSource() {
        return messageSource;
    }

    /**
     * @param messageSource
     *            the messageSource to set
     */
    public void setMessageSource(String msgSrc) {
        messageSource = msgSrc;
    }

    /**
     * Get the IDecoderGettable reference for this record.
     * 
     * @return The IDecoderGettable reference for this record. Null for this
     *         class.
     */
    @Override
    public IDecoderGettable getDecoderGettable() {
        return null;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Record type = ");
        sb.append(shefType);
        
//        if(obsDate != null) {
//            sb.append(obsDate.toOutString());
//        } else {
//            sb.append("   0  0  0  0  0  0");
//        }
//        sb.append(" ");
//        if(createDate != null) {
//            sb.append(createDate.toOutString());
//        } else {
//            sb.append("   0  0  0  0  0  0");
//        }
//        sb.append(" ");
//        // PE
//        sb.append("HG");
//        sb.append(" ");
//        sb.append(String.format("%4d",durationValue));
//        sb.append(" ");
//        // Type Code
//        sb.append("R");
//        sb.append(" ");
//        // Source Code
//        sb.append("G");
//        sb.append(" ");
//        // Extremnum
//        sb.append("Z");
//        sb.append(" ");
//        // Probability Code
//        sb.append(" ");
//        sb.append(" ");
//        // Data Value
//        sb.append("00000.000");
//        sb.append(" ");
//        // Data Qualifier
//        sb.append(" ");
//        sb.append(" ");
//        // Revision code
//        sb.append((revisedRecord) ? "0" : "1");
//        sb.append(ShefConstants.EOL);
//        sb.append("----------------------------------------");
        if(dataValues != null) {
            for(ShefData d : dataValues) {
                sb.append(ShefConstants.EOL);
                d.toString(sb);
            }
        }
        sb.append(ShefConstants.EOL);
        return sb.toString();
    }
    
    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
