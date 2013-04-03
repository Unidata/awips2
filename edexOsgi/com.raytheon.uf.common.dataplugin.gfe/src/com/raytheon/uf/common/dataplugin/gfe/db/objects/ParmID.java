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
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.gfe.serialize.ParmIDAdapter;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;

/**
 * 
 * Object used to identify parm.<br>
 * This is a port from original DatabaseID found in AWIPS I
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/6/08       875        bphillip    Initial Creation
 * 5/8/12       #600       dgilling    Implement clone().
 * 01/18/13    #1504       randerso    Removed setters since class should be immutable
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@XmlJavaTypeAdapter(value = ParmIDAdapter.class)
@DynamicSerialize
@DynamicSerializeTypeAdapter(factory = ParmIDAdapter.class)
public class ParmID implements Comparable<ParmID>, Serializable,
        ISerializableObject, Cloneable {

    private static final long serialVersionUID = 6801523496768037356L;

    private static final String DEFAULT_LEVEL = "SFC";

    private static final SimpleDateFormat MODEL_TIME_FORMAT;
    static {
        MODEL_TIME_FORMAT = new SimpleDateFormat("MMMddHH");
        MODEL_TIME_FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /** The name of the parm (i.e. T for Temperature) */
    private String parmName;

    /** The level at which this parm applies */
    private String parmLevel;

    /** The database that this parm ID is associated with */
    private DatabaseID dbId;

    /**
     * The parameter name/level information <br>
     * Example: T_SFC (Temperature parameter and surface level)
     */
    private String compositeName;

    /** The parmID including the parameter, level and database ID */
    private String shortParmId;

    /** A more extended version of the parameter ID */
    private String parmId;

    @Override
    public String toString() {
        return this.parmId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public ParmID clone() throws CloneNotSupportedException {
        return new ParmID(this.parmName, this.dbId.clone(), this.parmLevel);
    }

    /**
     * Gets the default level for all parms. In this case, the default level is
     * the surface(SFC)
     * 
     * @return The default level
     */
    static public String defaultLevel() {
        return DEFAULT_LEVEL;
    }

    public ParmID() {

    }

    /**
     * Constructs a new ParmID. Defaults level to surface.
     * 
     * @param parmName
     *            The parm name
     * @param parmModel
     *            The model name
     */
    public ParmID(String parmName, String parmModel) {

        this.parmName = parmName;
        this.parmLevel = defaultLevel();
        this.dbId = new DatabaseID(parmModel);
        encodeIdentifier();
    }

    /**
     * Constructs a new ParmID
     * 
     * @param parmName
     *            The parm name
     * @param parmModel
     *            The model name
     * @param level
     *            The level value
     */
    public ParmID(String parmName, String parmModel, String level) {
        this.parmName = parmName;
        this.parmLevel = level;
        this.dbId = new DatabaseID(parmModel);
        encodeIdentifier();
    }

    /**
     * Constructs a new ParmID
     * 
     * @param parmIdentifier
     *            The string representation of a ParmID
     */
    public ParmID(String parmIdentifier) {
        decodeIdentifier(parmIdentifier);
        encodeIdentifier();
    }

    /**
     * Constructs a new parmID
     * 
     * @param parmName
     *            The parm name
     * @param dbId
     *            The database identifier
     */
    public ParmID(String parmName, DatabaseID dbId) {
        this.parmName = parmName;
        this.parmLevel = defaultLevel();
        this.dbId = dbId;
        encodeIdentifier();
    }

    /**
     * Constructs a new parmID
     * 
     * @param parmName
     *            The parm name
     * @param dbId
     *            The database identifier
     * @param level
     *            The level
     */
    public ParmID(String parmName, DatabaseID dbId, String level) {
        this.parmName = parmName;
        this.parmLevel = level;
        this.dbId = dbId;
        encodeIdentifier();
    }

    /**
     * Returns the composite name for this ParmID for UI purposes, which is the
     * parmName_level when the level isn't SFC, and parmName when the level is
     * SFC
     * 
     * @return Gets the parm ID name for display on UI components
     */

    public String compositeNameUI() {
        if (DEFAULT_LEVEL.equals(parmLevel)) {
            return parmName;
        }
        return this.compositeName;
    }

    /**
     * Utility that returns the parmName and level from a composite name
     * 
     * @param composite
     * @return The parmName is returned as [0] and the level as [1]
     */

    public static String[] parmNameAndLevel(String composite) {
        if (composite.indexOf("_") > -1) {
            return composite.split("_");
        } else {
            return new String[] { composite, DEFAULT_LEVEL };
        }
    }

    /**
     * Gets the expression name for this ParmID
     * 
     * @param topoID
     * @param mutableID
     * @param includeTime
     * @return The expression name
     */

    public String expressionName(ParmID topoID, DatabaseID mutableID,
            boolean includeTime) {

        String expressionName = "";
        if (this.equals(topoID)) {
            return new String("Topo");
        }

        DatabaseID dbID = this.dbId;
        if (dbID.equals(mutableID)) {
            if (DEFAULT_LEVEL.equals(this.parmLevel)) {
                expressionName = parmName;
            } else {
                expressionName = compositeName;
            }
        } else {
            expressionName = compositeName + "_" + dbID.getSiteId() + "_"
                    + dbID.getDbType() + "_" + dbID.getModelName();
            if (includeTime) {
                Date modelDate = dbID.getModelDate();
                if (modelDate == null) {
                    expressionName += "_00000000_0000";
                } else {
                    expressionName += "_"
                            + MODEL_TIME_FORMAT.format(dbID.getModelDate());
                }
            }
        }

        return expressionName;
    }

    /**
     * Ensures that _parmName and _level are at least one character and that the
     * database id is valid. Neither the parmname or level can contain any
     * non-alphanumeric characters.
     * 
     * @return True if this is a valid parmID
     */

    public boolean isValid() {
        if (parmName == null || parmLevel == null || dbId == null) {
            return false;
        }

        if (parmName.length() < 1 || parmLevel.length() < 1 || !dbId.isValid()) {
            return false;
        }

        if (!Util.isAlnum(parmName)) {
            return false;
        }

        if (!Util.isAlnum(parmLevel)) {
            return false;
        }

        return true;
    }

    /**
     * Decodes the String version of the parmID and populates the fields
     * accordingly
     * 
     * @param parmIdentifier
     *            The string parmID
     */

    private void decodeIdentifier(String parmIdentifier) {
        String[] parts = parmIdentifier.split(":");
        String[] nameLevel = parts[0].split("_");
        this.dbId = new DatabaseID(parts[1]);
        if (nameLevel.length == 2) {
            parmName = nameLevel[0];
            parmLevel = nameLevel[1];
        } else {
            parmName = nameLevel[0];
            parmLevel = defaultLevel();
        }
    }

    /**
     * populates the identifier fields
     */

    private void encodeIdentifier() {
        this.compositeName = this.parmName + "_" + this.parmLevel;
        shortParmId = this.compositeName + ":" + dbId.getShortModelId();
        parmId = this.compositeName + ":" + dbId.getModelId();
    }

    /**
     * @return the parmName
     */

    public String getParmName() {
        return parmName;
    }

    /**
     * @return the parmLevel
     */

    public String getParmLevel() {
        return parmLevel;
    }

    /**
     * @return the dbId
     */

    public DatabaseID getDbId() {
        return dbId;
    }

    /**
     * @return the compositeName
     */

    public String getCompositeName() {
        return compositeName;
    }

    /**
     * @return the shortParmId
     */

    public String getShortParmId() {
        if (shortParmId == null) {
            encodeIdentifier();
        }
        return shortParmId;
    }

    /**
     * @return the parmId
     */

    public String getParmId() {
        return parmId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return parmId.hashCode();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }

        if (getClass() != obj.getClass()) {
            return false;
        }

        final ParmID other = (ParmID) obj;
        if (dbId == null) {
            if (other.dbId != null) {
                return false;
            }
        } else if (!dbId.equals(other.dbId)) {
            return false;
        }

        if (parmLevel == null) {
            if (other.parmLevel != null) {
                return false;
            }
        } else if (!parmLevel.equals(other.parmLevel)) {
            return false;
        }
        if (parmName == null) {
            if (other.parmName != null) {
                return false;
            }
        } else if (!parmName.equals(other.parmName)) {
            return false;
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(ParmID o) {

        // Check the name
        int nameComparison = this.parmName.compareTo(o.getParmName());
        if (nameComparison != 0) {
            return nameComparison;
        }

        // Check the level
        int levelComparison = this.parmLevel.compareTo(o.getParmLevel());
        if (levelComparison != 0) {
            return levelComparison;
        }

        // Check the db
        int dbComparison = this.dbId.compareTo(o.getDbId());
        return dbComparison;

    }

    /**
     * Short format ParmID serializer
     * 
     * @param parmID
     * @return
     */
    public static String shortSerializer(ParmID parmID) {
        return parmID.toString();
    }

    /**
     * Short format ParmID deserializer
     * 
     * @param parmIDasString
     * @return
     */
    public static ParmID shortDeserializer(String parmIDasString) {
        return new ParmID(parmIDasString);
    }

    /**
     * Displays a formatted string of parmID: ex. ABR IFP ISC ----- MinT SFC
     * 
     * @return a formated string
     */

    public String getUIFormattedString() {
        String singleTime = "-------";
        String formattedStr = "";
        String typeStr = "";
        String timeStr = "";
        String modelStr = "";
        String parmStr = "";

        if (getDbId().getDbType() != null) {
            typeStr = getDbId().getDbType();
        } else {
            typeStr = "IFP";
        }

        if (getDbId().getModelTime() == null) {
            timeStr = singleTime;
        } else {
            if (getDbId().getModelTime().equalsIgnoreCase(
                    DatabaseID.NO_MODEL_TIME)) {
                timeStr = singleTime;
            } else {
                timeStr = getDbId().getModelTime();
            }
        }

        modelStr = getDbId().getModelName();
        parmStr = getParmName();

        formattedStr = getDbId().getSiteId() + " " + typeStr + "  " + modelStr
                + " " + timeStr + "  " + parmStr + "  " + getParmLevel();

        return formattedStr;

    }

}
