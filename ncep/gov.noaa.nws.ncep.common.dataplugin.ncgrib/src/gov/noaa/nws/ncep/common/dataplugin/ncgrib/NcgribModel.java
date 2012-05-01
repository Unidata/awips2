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

package gov.noaa.nws.ncep.common.dataplugin.ncgrib;

import java.text.ParsePosition;
import java.util.Calendar;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.NcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.util.NcgribModelLookup;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.util.NcgridModel;

/**
 * Class encapsulating parameter, level, and spatial information of the grib
 * record.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 1/31/11                  M. Li		Add eventName for dynamic model name
 * 9/08/11                  X. Guo		Check file size to create hash code
 * 11/17/11                 X. Guo      Fixed hash generator problem
 * 3/2012					T. Lee		Added grib file template 
 *
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "ncgrib_models")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcgribModel extends PersistableDataObject {

    private static final long serialVersionUID = 4417959632479870000L;

    /** The id */
    @Id
    private Integer id;

    /** The originating center ID - 7 for NCEP
     * information from ON388 - table 0
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int centerid;

    /** The national subcenter ID - 0 for now
     *  information from ON388 - table c for grib2 section 1,
     *  octets 8-9 */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int subcenterid;

    /** The generating process number : 96 for gfs pgrb files:
     *  114 for NAEFS, 84 for meso NAM 12KM, 86 for RUC, 81 for GFS analysis,
     *  82 for analysis GDAS, etc...
     *  information form ON388 - table A
     *  Generating Process or Model from originating center 7
     *  which is NCEP
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int genprocess;
    
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String template;

    /** The backgenprocess number (currently gfs is 0)
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int backGenprocess;

    /** The name of the grib parameter
     *   such as temperatioure, pressure, ...etc*/
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String parameterName;

    /**
     * The abbreviation for the grib parameter. For accumulation and probability
     * parameters, the duration is appended such as T, P, ...etc.
     */
    @Column
    @DataURI(position = 1)
    @XmlAttribute
    @DynamicSerializeElement
    private String parameterAbbreviation;

    /** The unit for this grib parameter */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String parameterUnit;

    /** The grid number : 4 for gfs pgrb 1.0, 2 for 2.5, ...etc */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String gridid;

    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int gridNumber;

    /**
     * The pds template number (grib 2) from which this information was
     * extracted
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int pdsTemplate;

    /** The type of ensemble forecast (See Code table 4.6) */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    @DataURI(position = 3)
    private Integer typeEnsemble;

    /** The perturbation number of the ensemble */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    @DataURI(position = 4)
    private String perturbationNumber;

    /** The number of forecasts in the ensemble */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer numForecasts;
    
    /** The unique model name (i.e.NAM212) */
    /** The short model name (i.e.NAM80) This should be interpreted from
     *  the generating process number and grid id : 96 for gfs,
     *  114 for NAEFS, 84 for meso NAM 12KM, 86 for RUC, 81 for GFS analysis,
     *  82 for analysis GDAS, etc...
     *  information form ON388 - table A
     *  Generating Process or Model from originating center 7
     *  which is NCEP
     */ 
    @Column
    @DataURI(position = 0)
    @XmlAttribute
    @DynamicSerializeElement
    private String modelName;
    
    /** The name of event
     *   such as hurricane name ...etc*/
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String eventName;

    /** The database insert time of the object */
    @Column(columnDefinition = "timestamp without time zone default now()", insertable = false, updatable = true)
    private Calendar insertTime;

    /** The spatial information */
    @ManyToOne
    @PrimaryKeyJoinColumn
    @XmlElement
    @DynamicSerializeElement
    private NcgridCoverage location;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @XmlElement
    @DynamicSerializeElement
    @DataURI(position = 2, embedded = true)
    private Level level;

    /**
     * Creates and empty GribModel object
     */
    public NcgribModel() {

    }

    /**
     * Copy constructor
     */
    public NcgribModel(NcgribModel copy) {
        this.backGenprocess = copy.backGenprocess;
        this.centerid = copy.centerid;
        this.genprocess = copy.genprocess;
        this.template = copy.template;
        this.gridid = copy.gridid;
        this.gridNumber = copy.gridNumber;
        this.id = copy.id;
        if (copy.insertTime != null) {
            this.insertTime = (Calendar) copy.insertTime.clone();
        }
        this.level = copy.level;
        this.location = copy.location;
        this.modelName = copy.modelName;
        this.eventName = copy.eventName;
        this.numForecasts = copy.numForecasts;
        this.parameterAbbreviation = copy.parameterAbbreviation;
        this.parameterName = copy.parameterName;
        this.parameterUnit = copy.parameterUnit;
        this.pdsTemplate = copy.pdsTemplate;
        this.perturbationNumber = copy.perturbationNumber;
        this.subcenterid = copy.subcenterid;
        this.typeEnsemble = copy.typeEnsemble;
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();

        buffer.append("                    Id: ").append(id).append("\n");
        buffer.append("          PDS Template: ").append(pdsTemplate).append(
                "\n");
        buffer.append("                Center: ").append(centerid).append("\n");
        buffer.append("             Subcenter: ").append(subcenterid).append(
                "\n");
        buffer.append("            Model Name: ").append(modelName)
                .append("\n");
        buffer.append("            Event Name: ").append(eventName)
        .append("\n");
        buffer.append("    Generating Process: ").append(genprocess).append(
                "\n");
        buffer.append("    Grib File Template: ").append(template).append(
        "\n");
        buffer.append("        Parameter Name: ").append(parameterName).append(
                "\n");
        buffer.append("Parameter Abbreviation: ").append(parameterAbbreviation)
                .append("\n");
        buffer.append("        Parameter Unit: ").append(
                getParamterUnitPrettyString()).append("\n");

        if (level != null) {
            buffer.append("            Level Name: ").append(
                    level.getMasterLevel().getName()).append("\n");
            buffer.append("       Level One Value: ").append(
                    level.getLevelonevalue()).append("\n");
            buffer.append("       Level Two Value: ").append(
                    level.getLeveltwovalue()).append("\n");
            buffer.append("            Level Unit: ").append(
                    getLevelUnitPrettyString()).append("\n");
        } else {

        }
        buffer.append("         Type Ensemble: ").append(typeEnsemble).append(
                "\n");
        buffer.append("   Perturbation Number: ").append(perturbationNumber)
                .append("\n");
        buffer.append("         Num Forecasts: ").append(numForecasts).append(
                "\n");
        if (location == null) {
            buffer.append("Location is NULL").append("\n");
        } else {
            buffer.append(location.toString()).append("\n");
        }

        return buffer.toString();
    }

    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(modelName);
        builder.append(eventName);
        builder.append(parameterName);
        builder.append(parameterAbbreviation);
        builder.append(level);
        builder.append(typeEnsemble);
        builder.append(perturbationNumber);
        return builder.toHashCode();
    }

    public int hashCode(String afileName) {
        HashCodeBuilder builder = new HashCodeBuilder();
//        String[] tokens = afileName.split("\\.");
        String tmp = afileName;
        if ( tmp.contains(".")) {
        	tmp = tmp.replace(".", "_");
        }
        builder.append(modelName + "_" + tmp);
        builder.append(eventName);
        builder.append(parameterName);
        builder.append(parameterAbbreviation);
        builder.append(level);
        builder.append(typeEnsemble);
        builder.append(perturbationNumber);
        return builder.toHashCode();
    }
    
    /**
     * Generates a unique id from the hashcode of this object
     */
    public void generateId(String afileName) {
        this.id = hashCode(afileName);
    }

    /**
     * Generates a unique id from the hashcode of this object
     */
    public void generateId() {
        this.id = hashCode();
    }
    
    /**
     * Gets the level unit as a javax.measure.Unit<?> object. If the level unit
     * string cannot be successfully converted to a javax.measure.Unit<?>
     * object, Unit.ONE is returned
     * 
     * @return The level unit as a javax.measure.Unit<?> object
     */
    public Unit<?> getLevelUnitObject() {
        Unit<?> retVal = Unit.ONE;

        if (level != null) {
            Unit<?> tmp = level.getMasterLevel().getUnit();
            if (tmp != null) {
                retVal = tmp;
            }
        }
        return retVal;
    }

    /**
     * Gets the level unit as a pretty string meaning superscripts and
     * subscripts are printed accordingly
     * 
     * @return The level unit as a pretty string
     */
    public String getLevelUnitPrettyString() {
        Unit<?> unitObj = getLevelUnitObject();
        if (unitObj.equals(Unit.ONE)) {
            return "";
        } else {
            return unitObj.toString();
        }
    }

    /**
     * Gets the parameter unit as a javax.measure.Unit<?> object. If the
     * parameter unit string cannot be successfully converted to a
     * javax.measure.Unit<?> object, Unit.ONE is returned
     * 
     * @return The parameter unit as a javax.measure.Unit<?> object
     */
    public Unit<?> getParameterUnitObject() {
        Unit<?> retVal = Unit.ONE;

        if (this.parameterUnit != null) {
            try {
                retVal = UnitFormat.getUCUMInstance().parseProductUnit(
                        this.parameterUnit, new ParsePosition(0));
            } catch (Exception e) {
                // Unable to parse
                retVal = Unit.ONE;
            }
        }
        return retVal;
    }

    /**
     * Gets the parameter unit as a pretty string meaning superscripts and
     * subscripts are printed accordingly
     * 
     * @return The parameter unit as a pretty string
     */
    public String getParamterUnitPrettyString() {
        Unit<?> unitObj = getParameterUnitObject();
        if (unitObj.equals(Unit.ONE)) {
            return this.parameterUnit;
        } else {
            return unitObj.toString();
        }
    }

    /**
     * Gets the id
     * 
     * @return The id
     */
    public Integer getId() {
        return id;
    }

    /**
     * Sets the id
     * 
     * @param id
     *            The id
     */
    public void setId(Integer id) {
        this.id = id;
    }

    /**
     * Gets the center id
     * 
     * @return The center id
     */
    public int getCenterid() {
        return centerid;
    }

    /**
     * Sets the subcenter id
     * 
     * @param centerid
     *            The subcenter id
     */
    public void setCenterid(int centerid) {
        this.centerid = centerid;
    }

    /**
     * Gets the subcenter id
     * 
     * @return The subcenter id
     */
    public int getSubcenterid() {
        return subcenterid;
    }

    /**
     * Sets the subcenter id
     * 
     * @param subcenterid
     *            The subcenter id
     */
    public void setSubcenterid(int subcenterid) {
        this.subcenterid = subcenterid;
    }

    /**
     * Gets the genprocess
     * 
     * @return The genprocess
     */
    public int getGenprocess() {
        return genprocess;
    }

    /**
     * Sets the file template
     * 
     * @param fileTemplate
     *            The fileTemplate
     */
    public void setTemplate(String template) {
        this.template = template;
    }

    /**
     * Gets the file template
     * 
     * @return The fileTemplate
     */
    public String getTemplate() {
        return template;
    }

    /**
     * Sets the genproces
     * 
     * @param genprocess
     *            The genprocess
     */
    public void setGenprocess(int genprocess) {
        this.genprocess = genprocess;
    }
    
    /**
     * Gets the parameter name
     * 
     * @return parameterName
     */
    public String getParameterName() {
        return parameterName;
    }

    /**
     * Sets the parameter name
     * 
     * @param parameterName
     *            The parameter nam
     */
    public void setParameterName(String parameterName) {
        this.parameterName = parameterName;
    }

    /**
     * Gets the parameter abbreviation
     * 
     * @return The parameter abbreviation
     */
    public String getParameterAbbreviation() {
        return parameterAbbreviation;
    }

    /**
     * Sets the parameter abbreviation
     * 
     * @param parameterAbbreviation
     *            The parameter abbreviation
     */
    public void setParameterAbbreviation(String parameterAbbreviation) {
        this.parameterAbbreviation = parameterAbbreviation;
    }

    /**
     * Gets the parameter unit
     * 
     * @return The parameter unit
     */
    public String getParameterUnit() {
        return parameterUnit;
    }

    /**
     * Sets the parameter unit
     * 
     * @param parameterUnit
     *            The parameter unit
     */
    public void setParameterUnit(String parameterUnit) {
        this.parameterUnit = parameterUnit;
    }

    /**
     * Gets the level name
     * 
     * @return The level name
     */
    public String getLevelName() {
        String rval = null;
        if (level != null) {
            rval = level.getMasterLevel().getName();
        }
        return rval;
    }

    /**
     * Gets the level one value
     * 
     * @return The level one value
     */
    public Double getLevelOneValue() {
        Double rval = null;
        if (level != null) {
            rval = level.getLevelonevalue();
        }
        return rval;
    }

    /**
     * Gets the level two value
     * 
     * @return The level two value
     */
    public Double getLevelTwoValue() {
        Double rval = null;
        if (level != null) {
            rval = level.getLeveltwovalue();
        }
        return rval;
    }

    /**
     * Gets the level unit
     * 
     * @return The level unit
     */
    public String getLevelUnit() {
        String rval = null;
        if (level != null) {
            rval = level.getMasterLevel().getUnitString();
        }
        return rval;
    }

    /**
     * Gets the level info
     * 
     * @return The level info
     */
    public String getLevelInfo() {
        String rval = null;
        if (level != null) {
            rval = level.getLevelInfo();
        }
        return rval;
    }

    /**
     * Gets the grid id
     * 
     * @return The grid id
     */
    public String getGridid() {
        return gridid;
    }

    /**
     * Sets the grid id
     * 
     * @param gridid
     *            The grid id
     */
    public void setGridid(String gridid) {
        this.gridid = gridid;
        try {
            gridNumber = Integer.parseInt(gridid);
        } catch (NumberFormatException e) {
            gridNumber = -1;
        }
    }

    /**
     * Gets the database insert time
     * 
     * @return The database insert time
     */
    public Calendar getInsertTime() {
        return insertTime;
    }

    /**
     * Sets the database insert time
     * 
     * @param insertTime
     *            The database insert time
     */
    public void setInsertTime(Calendar insertTime) {
        this.insertTime = insertTime;
    }

    /**
     * Gets the spatial information
     * 
     * @return The spatial information
     */
    public NcgridCoverage getLocation() {
        return location;
    }

    /**
     * Sets the spatial information
     * 
     * @param location
     *            The spatial information
     */
    public void setLocation(NcgridCoverage location) {
        this.location = location;
    }

    /**
     * Gets the level information
     * 
     * @return The level information
     */
    public Level getLevel() {
        return level;
    }

    /**
     * Sets the level information
     * 
     * @param level
     *            The level information
     */
    public void setLevel(Level level) {
        this.level = level;
    }

    /**
     * Gets the ensemble type
     * 
     * @return The ensemble type
     */
    public Integer getTypeEnsemble() {
        return typeEnsemble;
    }

    /**
     * Sets the ensemble type
     * 
     * @param typeEnsemble
     *            The ensemble type
     */
    public void setTypeEnsemble(Integer typeEnsemble) {
        this.typeEnsemble = typeEnsemble;
    }

    /**
     * Gets the perturbation number
     * 
     * @return The perturbation number
     */
    public String getPerturbationNumber() {
        return perturbationNumber;
    }

    /**
     * Sets the perturbation number
     * 
     * @param perturbationNumber
     *            The perturbation number
     */
    public void setPerturbationNumber(String perturbationNumber) {
        this.perturbationNumber = perturbationNumber;
    }

    /**
     * Gets the number of ensemble forecasts
     * 
     * @return The number of ensemble forecasts
     */
    public Integer getNumForecasts() {
        return numForecasts;
    }

    /**
     * Sets the number of ensemble forecasts
     * 
     * @param numForecasts
     *            The number of ensemble forecasts
     */
    public void setNumForecasts(Integer numForecasts) {
        this.numForecasts = numForecasts;
    }

    /**
     * Gets the background generating process
     * 
     * @return The background generating process
     */
    public int getBackGenprocess() {
        return backGenprocess;
    }

    /**
     * Sets the background generating process
     * 
     * @param backGenprocess
     *            The background generating process
     */
    public void setBackGenprocess(int backGenprocess) {
        this.backGenprocess = backGenprocess;
    }

    /**
     * Gets the pds template number
     * 
     * @return The pds template number
     */
    public int getPdsTemplate() {
        return pdsTemplate;
    }

    /**
     * Sets the pds template number
     * 
     * @param pdsTemplate
     *            The pds template number
     */
    public void setPdsTemplate(int pdsTemplate) {
        this.pdsTemplate = pdsTemplate;
    }

    public String getModelName() {
        return modelName;
    }

    public String getModelTitle() {
        String rval = null;

        if (modelName != null) {
            NcgridModel model = NcgribModelLookup.getInstance().getModelByName(
                    modelName);

            if (model != null) {
                rval = model.getTitle();
            } else {
                rval = modelName;
            }
        }

        return rval;
    }

    public void setModelName(String modelName) {
        this.modelName = modelName;
    }

    public int getGridNumber() {
        return gridNumber;
    }

    public void setGridNumber(int gridNumber) {
        this.gridNumber = gridNumber;
    }

	public String getEventName() {
		return eventName;
	}

	public void setEventName(String eventName) {
		this.eventName = eventName;
	}
    
    
}
