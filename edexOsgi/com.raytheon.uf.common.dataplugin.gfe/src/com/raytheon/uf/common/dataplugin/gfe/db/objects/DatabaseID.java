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

package com.raytheon.uf.common.dataplugin.gfe.db.objects;

import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataplugin.gfe.serialize.DatabaseIDAdapter;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;

/**
 * 
 * Object used to identify an hdf5 grid "database".<br>
 * This is a port from original DatabaseID found in AWIPS I
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/6/08       875        bphillip    Initial Creation
 * 8/19/09     2899        njensen     Rewrote equals() for performance
 * 5/08/12     #600        dgilling    Implement clone().
 * 6/25/12     #766        dgilling    Fix isValid().
 * 01/18/13    #1504       randerso    Removed setters since class should be immutable
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@XmlJavaTypeAdapter(value = DatabaseIDAdapter.class)
@DynamicSerialize
@DynamicSerializeTypeAdapter(factory = DatabaseIDAdapter.class)
public class DatabaseID implements Serializable, Comparable<DatabaseID>,
        ISerializableObject, Cloneable {

    private static final long serialVersionUID = 5792890762609478694L;

    /**
     * String signifying this database does not use a model time (i.e. is a
     * singleton database)
     */
    public static final String NO_MODEL_TIME = "00000000_0000";

    public static final String MODEL_TIME_FORMAT = "yyyyMMdd_HHmm";

    public static final ThreadLocal<SimpleDateFormat> dateFormat = new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat df = new SimpleDateFormat(
                    DatabaseID.MODEL_TIME_FORMAT);
            df.setTimeZone(TimeZone.getTimeZone("GMT"));
            return df;
        }

    };

    /** Denotes what type of database */
    public enum DataType {
        NONE, GRID
    };

    /** The site identifier */
    private String siteId;

    /** The database format */
    private DataType format;

    /** Optional database type */
    private String dbType;

    /** The model name */
    private String modelName;

    /** Model Time yyyymmdd_hhmm */
    private String modelTime;

    /** The model identifier */
    private String modelId;

    /** The short model identifier */
    private String shortModelId;

    /**
     * Creates a new DatabaseID
     */
    public DatabaseID() {
        format = DataType.NONE;
    }

    /**
     * Constructor taking database identifier
     * 
     * @param dbIdentifier
     */
    public DatabaseID(String dbIdentifier) {
        if (decodeIdentifier(dbIdentifier)) {
            encodeIdentifier();
        } else {
            // set to default values
            format = DataType.NONE;
            dbType = "";
            siteId = "";
            modelName = "";
            modelTime = NO_MODEL_TIME;
            modelId = "";
            shortModelId = "";
        }
    }

    /**
     * Constructor taking the individual components of the db ID
     * 
     * @param siteId
     *            The site ID
     * @param format
     *            The format of the database
     * @param dbType
     *            The optional database type
     * @param modelName
     *            The model name
     * @param modelTime
     *            The model time
     */
    public DatabaseID(String siteId, DataType format, String dbType,
            String modelName, Date modelTime) {
        this(siteId, format, dbType, modelName, dateFormat.get().format(
                modelTime));
    }

    /**
     * Constructor taking the individual components of the db ID
     * 
     * @param siteId
     *            The site ID
     * @param format
     *            The format of the database
     * @param dbType
     *            The optional database type
     * @param modelName
     *            The model name
     * @param modelTime
     *            The model time
     */
    public DatabaseID(String siteId, DataType format, String dbType,
            String modelName, String modelTime) {
        this.siteId = siteId;
        this.format = format;
        this.dbType = dbType;
        this.modelName = modelName;
        this.modelTime = modelTime;
        encodeIdentifier();
    }

    /**
     * Constructor taking the individual components of the db ID. The model time
     * is not specified. (This is for a rolling database)
     * 
     * @param siteId
     *            The site ID
     * @param format
     *            The format of the database
     * @param dbType
     *            The optional database type
     * @param modelName
     *            The model name
     */
    public DatabaseID(String siteId, DataType format, String dbType,
            String modelName) {
        this(siteId, format, dbType, modelName, NO_MODEL_TIME);
    }

    /**
     * The less than operator. Ordering is by site, format, type, model, and
     * modeltime. <br>
     * Note that more current times are treated as less than.
     * 
     * @param dbId
     *            The database ID to compare to
     * @return True if "less than"
     */

    public boolean lessThan(DatabaseID dbId) {
        boolean retVal = true;
        try {
            if (this.siteId.compareTo(dbId.siteId) > 0) {
                retVal = false;
            } else if (this.format.compareTo(dbId.format) > 0) {
                retVal = false;
            } else if (this.dbType.compareTo(dbId.dbType) > 0) {
                retVal = false;
            } else if (this.modelName.compareTo(dbId.modelName) > 0) {
                retVal = false;
            } else if (this.modelTime.compareTo(dbId.modelTime) < 0) {
                retVal = false;
            }
        } catch (NullPointerException e) {
            // Catches npe to avoid excessive null checking
            retVal = false;
        }
        return retVal;
    }

    /**
     * Returns true if this is a valid database identifier
     * 
     * @return True if this is a valid database identifier
     */

    public boolean isValid() {
        return !this.format.equals(DataType.NONE);
    }

    /**
     * Returns the format type as a string. Returns NONE,GRID
     * 
     * @param dataType
     *            The datatype
     * @return The string representation of the datatype
     */

    public String dataTypeAsString(DataType dataType) {
        switch (dataType) {
        case NONE:
            return "NONE";
        case GRID:
            return "GRID";
        default:
            return "";
        }
    }

    /**
     * Strips the type off of the ID
     * 
     * @return A database id with type stripped
     */

    public DatabaseID stripType() {
        if (modelTime.equals(DatabaseID.NO_MODEL_TIME)) {
            return new DatabaseID(siteId, format, "", modelName);
        } else {
            return new DatabaseID(siteId, format, "", modelName, modelTime);
        }

    }

    /**
     * Strips off model time
     * 
     * @return A new database ID with no model time
     */

    public DatabaseID stripModelTime() {
        return new DatabaseID(siteId, format, dbType, modelName);
    }

    /**
     * Decodes a string representation of a database ID into its component parts
     * and assigns them accordingly
     * 
     * @param dbIdentifier
     *            The string representation of a databaseID
     */

    private boolean decodeIdentifier(String dbIdentifier) {

        // set to default values
        format = DataType.NONE;
        dbType = "";
        siteId = "";
        modelName = "";
        modelTime = NO_MODEL_TIME;

        // parse into '_' separated strings
        String[] strings = dbIdentifier.split("_");
        if (strings.length != 6) {
            return false;
        }

        // store the data
        if (strings[1].equals(dataTypeAsString(DataType.GRID))) {
            format = DataType.GRID;
        } else {
            return false;
        }

        siteId = strings[0];
        dbType = strings[2];
        modelName = strings[3];

        // date-time group
        if ((strings[4].length() != 8) || (strings[5].length() != 4)) {
            return false;
        }

        // make sure the digits are there
        String dtg = strings[4] + '_' + strings[5]; // back together
        if (!dtg.equals(NO_MODEL_TIME)) {
            if (!decodeDtg(dtg)) {
                return false;
            }
        }
        return true;
    }

    private boolean decodeDtg(String dtgString) {
        if ((dtgString == null)
                || (dtgString.length() != MODEL_TIME_FORMAT.length())) {
            return false;
        }
        try {
            dateFormat.get().parse(dtgString);
            modelTime = dtgString;
        } catch (ParseException e) {
            return false;
        }
        return true;
    }

    /**
     * Assigns identifier values based on values of private variables
     */

    private void encodeIdentifier() {
        if (dbType != null) {
            modelId = siteId + "_" + format + "_" + dbType + "_" + modelName;
        } else {
            modelId = siteId + "_" + format + "__" + modelName;
        }

        shortModelId = modelName;
        if ((dbType != null) && !dbType.isEmpty()) {
            shortModelId += "_" + dbType;
        }

        if (!modelTime.equals(NO_MODEL_TIME)) {
            modelId += "_" + modelTime;
            shortModelId += "_" + modelTime.substring(6, 8)
                    + modelTime.substring(9, 11);
        } else {
            modelId += "_" + NO_MODEL_TIME;
        }
        shortModelId += " (" + siteId + ")";
        modelId = modelId.intern();
    }

    @Override
    public String toString() {
        return modelId;
    }

    /**
     * @return the siteId
     */

    public String getSiteId() {
        return siteId;
    }

    /**
     * @return the format
     */

    public DataType getFormat() {
        return format;
    }

    /**
     * @return the dbType
     */

    public String getDbType() {
        return dbType;
    }

    /**
     * @return the modelName
     */
    public String getModelName() {
        return modelName;
    }

    /**
     * @return the modelTime
     */

    public String getModelTime() {
        return modelTime;
    }

    /**
     * @return the modelId
     */

    public String getModelId() {
        return modelId;
    }

    /**
     * @return the shortModelId
     */

    public String getShortModelId() {
        return shortModelId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (dbType == null ? 0 : dbType.hashCode());
        result = prime * result + (format == null ? 0 : format.hashCode());
        result = prime * result + (modelId == null ? 0 : modelId.hashCode());
        result = prime * result
                + (modelTime == null ? 0 : modelTime.hashCode());
        result = prime * result + (siteId == null ? 0 : siteId.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        // == is a safe comparison since we interned the modelId
        return this.toString() == obj.toString();
    }

    /**
     * @return the modelDate
     */

    public Date getModelDate() {
        Date date = null;
        if ((modelTime != null) && !NO_MODEL_TIME.equalsIgnoreCase(modelTime)) {
            try {
                date = dateFormat.get().parse(this.modelTime);
            } catch (ParseException e) {
            }
        }

        return date;
    }

    public Date getModelTimeAsDate() {
        if (this.modelTime.equals(NO_MODEL_TIME)) {
            return new Date(0);
        }

        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

        cal.set(Calendar.YEAR, Integer.parseInt(modelTime.substring(0, 4)));
        cal.set(Calendar.MONTH, Integer.parseInt(modelTime.substring(4, 6)) - 1);
        cal.set(Calendar.DAY_OF_MONTH,
                Integer.parseInt(modelTime.substring(6, 8)));
        cal.set(Calendar.HOUR_OF_DAY,
                Integer.parseInt(modelTime.substring(9, 11)));
        cal.set(Calendar.MINUTE, Integer.parseInt(modelTime.substring(11)));
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        return cal.getTime();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(DatabaseID o) {

        int site = this.siteId.compareTo(o.getSiteId());
        if (site != 0) {
            return site;
        }

        int format = this.format.compareTo(o.getFormat());
        if (format != 0) {
            return format;
        }

        int type = this.dbType.compareTo(o.getDbType());
        if (type != 0) {
            return type;
        }

        int model = this.modelName.compareTo(o.getModelName());
        if (model != 0) {
            return model;
        }

        int time = -this.getModelTimeAsDate().compareTo(o.getModelTimeAsDate());
        return time;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    protected DatabaseID clone() throws CloneNotSupportedException {
        return new DatabaseID(this.siteId, this.format, this.dbType,
                this.modelName, this.modelTime);
    }

}
