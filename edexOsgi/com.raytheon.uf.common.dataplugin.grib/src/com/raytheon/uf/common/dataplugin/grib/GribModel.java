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

package com.raytheon.uf.common.dataplugin.grib;

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
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Deprecated, use grid
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@Table(name = "grib_models")
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
@Deprecated
public class GribModel extends PersistableDataObject {

    private static final long serialVersionUID = 4417959632479879335L;

    /** The id */
    @Id
    @DynamicSerializeElement
    private Integer id;

    /** The originating center ID */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int centerid;

    /** The national subcenter ID */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int subcenterid;

    /** The generating process number */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int genprocess;

    /** The backgenprocess number */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private int backGenprocess;

    /** The name of the grib parameter */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private String parameterName;

    /**
     * The abbreviation for the grib parameter. For accumulation and probability
     * parameters, the duration is appended
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

    /** The grid number */
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
    private Integer perturbationNumber;

    /** The number of forecasts in the ensemble */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private Integer numForecasts;

    /** The unique model name (i.e.NAM212) */
    @Column
    @DataURI(position = 0)
    @XmlAttribute
    @DynamicSerializeElement
    private String modelName;

    /** The database insert time of the object */
    @Column(columnDefinition = "timestamp without time zone default now()", insertable = false, updatable = true)
    private Calendar insertTime;

    /** The spatial information */
    @ManyToOne
    @PrimaryKeyJoinColumn
    @XmlElement
    @DynamicSerializeElement
    private GridCoverage location;

    @ManyToOne
    @PrimaryKeyJoinColumn
    @XmlElement
    @DynamicSerializeElement
    @DataURI(position = 2, embedded = true)
    private Level level;

    /**
     * Creates and empty GribModel object
     */
    public GribModel() {

    }

    /**
     * Copy constructor
     */
    public GribModel(GribModel copy) {
        this.backGenprocess = copy.backGenprocess;
        this.centerid = copy.centerid;
        this.genprocess = copy.genprocess;
        this.gridid = copy.gridid;
        this.gridNumber = copy.gridNumber;
        this.id = copy.id;
        if (copy.insertTime != null) {
            this.insertTime = (Calendar) copy.insertTime.clone();
        }
        this.level = copy.level;
        this.location = copy.location;
        this.modelName = copy.modelName;
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
        buffer.append("          PDS Template: ").append(pdsTemplate)
                .append("\n");
        buffer.append("                Center: ").append(centerid).append("\n");
        buffer.append("             Subcenter: ").append(subcenterid)
                .append("\n");
        buffer.append("            Model Name: ").append(modelName)
                .append("\n");
        buffer.append("    Generating Process: ").append(genprocess)
                .append("\n");
        buffer.append("        Parameter Name: ").append(parameterName)
                .append("\n");
        buffer.append("Parameter Abbreviation: ").append(parameterAbbreviation)
                .append("\n");
        buffer.append("        Parameter Unit: ")
                .append(getParamterUnitPrettyString()).append("\n");

        if (level != null) {
            buffer.append("            Level Name: ")
                    .append(level.getMasterLevel().getName()).append("\n");
            buffer.append("       Level One Value: ")
                    .append(level.getLevelonevalue()).append("\n");
            buffer.append("       Level Two Value: ")
                    .append(level.getLeveltwovalue()).append("\n");
            buffer.append("            Level Unit: ")
                    .append(getLevelUnitPrettyString()).append("\n");
        } else {

        }
        buffer.append("         Type Ensemble: ").append(typeEnsemble)
                .append("\n");
        buffer.append("   Perturbation Number: ").append(perturbationNumber)
                .append("\n");
        buffer.append("         Num Forecasts: ").append(numForecasts)
                .append("\n");
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
    public GridCoverage getLocation() {
        return location;
    }

    /**
     * Sets the spatial information
     * 
     * @param location
     *            The spatial information
     */
    public void setLocation(GridCoverage location) {
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
    public Integer getPerturbationNumber() {
        return perturbationNumber;
    }

    /**
     * Sets the perturbation number
     * 
     * @param perturbationNumber
     *            The perturbation number
     */
    public void setPerturbationNumber(Integer perturbationNumber) {
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
            DatasetInfo model = DatasetInfoLookup.getInstance().getInfo(
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

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        } else if (!obj.getClass().equals(this.getClass())) {
            return false;
        }
        GribModel rhs = (GribModel) obj;
        if (centerid != rhs.centerid) {
            return false;
        } else if (genprocess != rhs.genprocess) {
            return false;
        } else if (gridNumber != rhs.gridNumber) {
            return false;
        } else if (pdsTemplate != rhs.pdsTemplate) {
            return false;
        } else if (subcenterid != rhs.subcenterid) {
            return false;
        } else if (backGenprocess != rhs.backGenprocess) {
            return false;
        }

        if (gridid == null) {
            if (rhs.gridid != null) {
                return false;
            }
        } else if (!gridid.equals(rhs.gridid)) {
            return false;
        }

        if (insertTime == null) {
            if (rhs.insertTime != null) {
                return false;
            }
        } else if (!insertTime.equals(rhs.insertTime)) {
            return false;
        }

        if (level == null) {
            if (rhs.level != null) {
                return false;
            }
        } else if (!level.equals(rhs.level)) {
            return false;
        }

        if (location == null) {
            if (rhs.location != null) {
                return false;
            }
        } else if (!location.equals(rhs.location)) {
            return false;
        }

        if (modelName == null) {
            if (rhs.modelName != null) {
                return false;
            }
        } else if (!modelName.equals(rhs.modelName)) {
            return false;
        }

        if (numForecasts == null) {
            if (rhs.numForecasts != null) {
                return false;
            }
        } else if (!numForecasts.equals(rhs.numForecasts)) {
            return false;
        }

        if (parameterAbbreviation == null) {
            if (rhs.parameterAbbreviation != null) {
                return false;
            }
        } else if (!parameterAbbreviation.equals(rhs.parameterAbbreviation)) {
            return false;
        }

        if (parameterName == null) {
            if (rhs.parameterName != null) {
                return false;
            }
        } else if (!parameterName.equals(rhs.parameterName)) {
            return false;
        }

        if (parameterUnit == null) {
            if (rhs.parameterUnit != null) {
                return false;
            }
        } else if (!parameterUnit.equals(rhs.parameterUnit)) {
            return false;
        }

        if (perturbationNumber == null) {
            if (rhs.perturbationNumber != null) {
                return false;
            }
        } else if (!perturbationNumber.equals(rhs.perturbationNumber)) {
            return false;
        }

        if (typeEnsemble == null) {
            if (rhs.typeEnsemble != null) {
                return false;
            }
        } else if (!typeEnsemble.equals(rhs.typeEnsemble)) {
            return false;
        }
        return true;
    }
}
