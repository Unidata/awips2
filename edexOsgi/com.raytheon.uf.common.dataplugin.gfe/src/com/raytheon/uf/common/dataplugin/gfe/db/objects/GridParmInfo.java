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

import java.text.ParseException;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.List;

import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * GridParmInfo
 * 
 * Contains static information about a grid parameter
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/05/2008              chammack    Separated static attributes from GFERecord
 * 02/27/2008   879        rbell       Added constructors and equals(Object)
 * 03/20/2013     #1774    randerso    Removed unnecessary XML annotations,
 *                                     added isValid method to match A1
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@DynamicSerialize
public class GridParmInfo implements Cloneable, ISerializableObject {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridParmInfo.class);
    static {
        // TODO: is this the right place for these?

        // TODO: move the definition of Reflectivity out of viz.radar
        UnitFormat.getUCUMInstance().label(Unit.ONE, "dbz");

        UnitFormat.getUCUMInstance().label(Unit.ONE, "cat");
        UnitFormat.getUCUMInstance().label(Unit.ONE, "index");
        UnitFormat.getUCUMInstance().label(Unit.ONE, "wx");
        UnitFormat.getUCUMInstance().label(Unit.ONE, "wwa");
        UnitFormat.getUCUMInstance().label(Unit.ONE, "yn");
        UnitFormat.getUCUMInstance().label(Unit.ONE, "unit");
        UnitFormat.getUCUMInstance().label(Unit.ONE, "count");

        UnitFormat.getUCUMInstance().label(SI.SECOND, "sec");
        UnitFormat.getUCUMInstance().label(SI.CELSIUS, "C");

        UnitFormat.getUCUMInstance().label(NonSI.KNOT, "kt");
        UnitFormat.getUCUMInstance().label(NonSI.KNOT, "kts");
        UnitFormat.getUCUMInstance().label(NonSI.HOUR, "hrs");
        UnitFormat.getUCUMInstance().label(NonSI.DEGREE_ANGLE, "deg");
        UnitFormat.getUCUMInstance().label(NonSI.MILE, "SM");
        UnitFormat.getUCUMInstance().label(NonSI.FAHRENHEIT, "F");
    }

    /** The parm id associated with this grid parm info */
    @DynamicSerializeElement
    private ParmID parmID;

    /** The grid location associated with this grid parm info */
    @DynamicSerializeElement
    private GridLocation gridLoc;

    /** The grid type */
    @DynamicSerializeElement
    private GridType gridType;

    /** The parameter descriptive name */
    @DynamicSerializeElement
    private String descriptiveName;

    /** The units associated with the parameter */
    @DynamicSerializeElement
    private String unitString;

    private Unit<?> unitObject;

    /** The minimum allowed value */
    @DynamicSerializeElement
    private float minValue;

    /** The maximum allowed value */
    @DynamicSerializeElement
    private float maxValue;

    /** The precision of the value */
    @DynamicSerializeElement
    private int precision;

    /** Is value a rate parameter */
    @DynamicSerializeElement
    private boolean rateParm;

    /** Time Constraints */
    @DynamicSerializeElement
    private TimeConstraints timeConstraints;

    @DynamicSerializeElement
    private boolean timeIndependentParm;

    private String errorMessage;

    public GridParmInfo() {
        gridType = GridType.NONE;
        timeIndependentParm = false;
        minValue = 0;
        maxValue = 0;
        precision = 0;
        rateParm = false;

        parmID = null;
        gridLoc = null;
        unitString = null;
        descriptiveName = "";
        timeConstraints = null;
    }

    public GridParmInfo(GridParmInfo orig) {
        this.parmID = orig.parmID;
        this.gridLoc = orig.gridLoc;
        this.gridType = orig.gridType;
        this.unitString = orig.unitString;
        this.descriptiveName = orig.descriptiveName;
        this.minValue = orig.minValue;
        this.maxValue = orig.maxValue;
        this.precision = orig.precision;
        this.timeIndependentParm = orig.timeIndependentParm;
        this.timeConstraints = orig.timeConstraints;
        this.rateParm = orig.rateParm;
    }

    /**
     * Constructor using all fields
     * 
     * @param id
     * @param gridLoc
     * @param gridType
     * @param unit
     * @param descriptiveName
     * @param minValue
     * @param maxValue
     * @param precision
     * @param timeIndependentParm
     * @param timeConstraints
     * @param rateParm
     */
    public GridParmInfo(ParmID id, GridLocation gridLoc, GridType gridType,
            String unit, String descriptiveName, float minValue,
            float maxValue, int precision, boolean timeIndependentParm,
            TimeConstraints timeConstraints, boolean rateParm) {
        super();
        this.parmID = id;
        this.gridLoc = gridLoc;
        this.gridType = gridType;
        this.unitString = unit;
        this.descriptiveName = descriptiveName;
        this.minValue = minValue;
        this.maxValue = maxValue;
        this.precision = precision;
        this.timeIndependentParm = timeIndependentParm;
        this.timeConstraints = timeConstraints;
        this.rateParm = rateParm;

        if (!validCheck()) {
            statusHandler.warn(this.errorMessage);
            setDefaultValues();
        }
    }

    public GridParmInfo(ParmID id, GridLocation gridLoc, GridType gridType,
            String unit, String descriptiveName, float minValue,
            float maxValue, int precision, boolean timeIndependentParm,
            TimeConstraints timeConstraints) {
        this(id, gridLoc, gridType, unit, descriptiveName, minValue, maxValue,
                precision, timeIndependentParm, timeConstraints, false);
    }

    /**
     * GridParmInfo::setDefaultValues() Sets default values in private data.
     * Sets values to 0 or their default construction. Grid type is set to NONE.
     * 
     */
    private void setDefaultValues() {
        this.parmID = new ParmID();
        this.timeConstraints = new TimeConstraints();
        this.gridLoc = new GridLocation();
        this.unitString = "";
        this.descriptiveName = "";
        this.minValue = this.maxValue = 0.0f;
        this.precision = 0;
        this.timeIndependentParm = false;
        this.gridType = GridType.NONE;
        this.rateParm = false;
        return;
    }

    private boolean validCheck() {
        StringBuilder sb = new StringBuilder();
        if (!parmID.isValid()) {
            sb.append("GridParmInfo.ParmID is not valid [");
            sb.append(parmID);
            sb.append("]\n");
        }

        if (!timeConstraints.isValid()) {
            sb.append("GridParmInfo.TimeConstraints are not valid [");
            sb.append(timeConstraints);
            sb.append("]\n");
        }

        if (!gridLoc.isValid()) {
            sb.append("GridParmInfo.GridLocation is not valid\n");
        }

        if (timeIndependentParm && timeConstraints.anyConstraints()) {
            sb.append("GridParmInfo is invalid. There are time constraints ");
            sb.append(" for a time independent parm.  Constraints: ");
            sb.append(timeConstraints);
            sb.append("\n");
        }

        // units defined
        if (unitString == null || unitString.isEmpty()) {
            sb.append("GridParmInfo.Units are not defined.\n");
        }

        // max/min/precision checks
        if (maxValue < minValue) {
            sb.append("GridParmInfo is invalid. Max<Min Max=");
            sb.append(maxValue);
            sb.append(" Min=");
            sb.append(minValue);
            sb.append("\n");
        }

        // precision check
        if (precision < -2 || precision > 5) {
            sb.append("GridParmInfo is invalid.  Precision out of limits. ");
            sb.append(" Precision is: ");
            sb.append(precision);
            sb.append(". Must be betwwen -2 and 5\n");
        }

        this.errorMessage = sb.toString();
        if (errorMessage.isEmpty()) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * @return true if valid
     */
    public boolean isValid() {
        return errorMessage.isEmpty();
    }

    /**
     * @return the gridType
     */
    public GridType getGridType() {
        return gridType;
    }

    /**
     * @return the descriptiveName
     */
    public String getDescriptiveName() {
        return descriptiveName;
    }

    /**
     * @return the unit
     */
    public String getUnitString() {
        return unitString;
    }

    public synchronized Unit<?> getUnitObject() {
        if (unitObject == null) {
            try {
                unitObject = UnitFormat.getUCUMInstance().parseProductUnit(
                        this.unitString, new ParsePosition(0));
            } catch (ParseException e) {
                statusHandler
                        .handle(Priority.EVENTB, "Error parsing unit string \""
                                + this.unitString + "\"", e);

                unitObject = Unit.ONE;
            }
        }
        return unitObject;
    }

    /**
     * @return the minValue
     */
    public float getMinValue() {
        return minValue;
    }

    /**
     * @return the maxValue
     */
    public float getMaxValue() {
        return maxValue;
    }

    /**
     * @return the precision
     */
    public int getPrecision() {
        return precision;
    }

    /**
     * @return the rateParm
     */
    public boolean isRateParm() {
        return rateParm;
    }

    /**
     * @return the timeConstraints
     */
    public TimeConstraints getTimeConstraints() {
        return timeConstraints;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public GridParmInfo clone() {
        return new GridParmInfo(this);
    }

    public ParmID getParmID() {
        return parmID;
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
        result = prime * result
                + ((descriptiveName == null) ? 0 : descriptiveName.hashCode());
        result = prime * result + ((gridLoc == null) ? 0 : gridLoc.hashCode());
        result = prime * result
                + ((gridType == null) ? 0 : gridType.hashCode());
        result = prime * result + Float.floatToIntBits(maxValue);
        result = prime * result + Float.floatToIntBits(minValue);
        result = prime * result + ((parmID == null) ? 0 : parmID.hashCode());
        result = prime * result + precision;
        result = prime * result + (rateParm ? 1231 : 1237);
        result = prime * result
                + ((timeConstraints == null) ? 0 : timeConstraints.hashCode());
        result = prime * result + (timeIndependentParm ? 1231 : 1237);
        result = prime * result
                + ((unitString == null) ? 0 : unitString.hashCode());
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
        if (getClass() != obj.getClass()) {
            return false;
        }
        final GridParmInfo other = (GridParmInfo) obj;
        if (descriptiveName == null) {
            if (other.descriptiveName != null) {
                return false;
            }
        } else if (!descriptiveName.equals(other.descriptiveName)) {
            return false;
        }
        if (gridLoc == null) {
            if (other.gridLoc != null) {
                return false;
            }
        } else if (!gridLoc.equals(other.gridLoc)) {
            return false;
        }
        if (gridType == null) {
            if (other.gridType != null) {
                return false;
            }
        } else if (!gridType.equals(other.gridType)) {
            return false;
        }
        if (Float.floatToIntBits(maxValue) != Float
                .floatToIntBits(other.maxValue)) {
            return false;
        }
        if (Float.floatToIntBits(minValue) != Float
                .floatToIntBits(other.minValue)) {
            return false;
        }
        if (parmID == null) {
            if (other.parmID != null) {
                return false;
            }
        } else if (!parmID.equals(other.parmID)) {
            return false;
        }
        if (precision != other.precision) {
            return false;
        }
        if (rateParm != other.rateParm) {
            return false;
        }
        if (timeConstraints == null) {
            if (other.timeConstraints != null) {
                return false;
            }
        } else if (!timeConstraints.equals(other.timeConstraints)) {
            return false;
        }
        if (timeIndependentParm != other.timeIndependentParm) {
            return false;
        }
        if (unitString == null) {
            if (other.unitString != null) {
                return false;
            }
        } else if (!unitString.equals(other.unitString)) {
            return false;
        }
        return true;
    }

    /**
     * @return the gridLoc
     */
    public GridLocation getGridLoc() {
        return gridLoc;
    }

    /**
     * @return the timeIndependentParm
     */
    public boolean isTimeIndependentParm() {
        return timeIndependentParm;
    }

    public void resetParmID(ParmID parmId) {
        this.parmID = parmId;
    }

    @Override
    public String toString() {
        return "ParmID: " + parmID + " TimeConstraints: " + timeConstraints
                + " GridLoc: " + gridLoc + " Units: " + unitString + " Name: "
                + descriptiveName + " Min/Max AllowedValues: " + minValue + ','
                + maxValue + " Precision: " + precision + " TimeIndependent: "
                + timeIndependentParm + " RateParm: " + rateParm
                + " GridType: " + gridType;
    }

    public void setParmID(ParmID parmID) {
        this.parmID = parmID;
    }

    public void setGridLoc(GridLocation gridLoc) {
        this.gridLoc = gridLoc;
    }

    public void setGridType(GridType gridType) {
        this.gridType = gridType;
    }

    public void setDescriptiveName(String descriptiveName) {
        this.descriptiveName = descriptiveName;
    }

    public void setUnitString(String unitString) {
        this.unitString = unitString;
    }

    public void setUnitObject(Unit<?> unitObject) {
        this.unitObject = unitObject;
    }

    public void setMinValue(float minValue) {
        this.minValue = minValue;
    }

    public void setMaxValue(float maxValue) {
        this.maxValue = maxValue;
    }

    public void setPrecision(int precision) {
        this.precision = precision;
    }

    public void setRateParm(boolean rateParm) {
        this.rateParm = rateParm;
    }

    public void setTimeConstraints(TimeConstraints timeConstraints) {
        this.timeConstraints = timeConstraints;
    }

    public void setTimeIndependentParm(boolean timeIndependentParm) {
        this.timeIndependentParm = timeIndependentParm;
    }

    /**
     * Returns the list of discrete keys for DISCRETE elements.
     * 
     * @return If a discrete parameter, gets the available keys from the
     *         discrete definitions.
     */
    public List<String> getDiscreteKeys() {
        if (gridType == GridType.DISCRETE) {
            String id = parmID.getCompositeName();
            String site = parmID.getDbId().getSiteId();
            return DiscreteKey.discreteDefinition(site).symbols(id);
        } else {
            return new ArrayList<String>(0);
        }
    }

}
