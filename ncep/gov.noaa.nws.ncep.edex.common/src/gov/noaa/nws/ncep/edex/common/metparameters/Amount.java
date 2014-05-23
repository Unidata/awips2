package gov.noaa.nws.ncep.edex.common.metparameters;

import java.text.ParseException;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class used to hold a value and its units.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  06/16/2011   #441       G Hull    Initial creation.
 *  10/16/2011              G Hull    make sure types of missing data and value are the same;
 *                                    make value private
 *  11/14/2011              B Hebbard Resolve unit serialization issues
 *  04/01/2014   #1040      B Hebbard In syncUnits(), map unitStr "count" to Unit.ONE
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Amount implements ISerializableObject {

    /**
	 * 
	 */
    private static final long serialVersionUID = -7742796238434954463L;

    // TODO : add capability to let user set their own missing data value.
    @DynamicSerializeElement
    private Number missing_data_value = new Double(-9999);

    @DynamicSerializeElement
    private Number value = missing_data_value;

    // Following should always be the string representation of below
    // "unit"; that is, these must be kept in sync. (We use the String
    // due to problems serializing the Unit<?>)
    @DynamicSerializeElement
    private String unitStr = "";

    private transient Unit<?> unit;

    // To simplify the
    public Amount(Number val, Unit<?> unit) {
        // System.out.println("Constructor 0 called -- val " + val + " unit " +
        // unit);
        initMissingDataSentinel();

        setValue(val, unit);
    }

    public Amount(Unit<?> u) {
        // System.out.println("Constructor 1 called -- u " + u);
        setValue(missing_data_value, u);
    }

    public Amount(String unitStr) {
        // System.out.println("Constructor 2 called -- unitStr " + unitStr);
        try {
            Unit<?> u = new UnitAdapter().unmarshal(unitStr);
            setValue(missing_data_value, u);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public Amount() {
        // System.out.println("Constructor 3 called");
        // TODO Auto-generated constructor stub
    }

    // TODO Should we allow access to the value without the units? Or
    // can we let the user assume the stored units for convienience?
    public Number getValue() {
        return value;
    }

    // call hasValidValue before calling this method.
    //
    public Number getValueAs(Unit<?> unitNeeded) {
        if (unitNeeded != unit && unitNeeded.isCompatible(unit)) {
            double newValue = unit.getConverterTo(unitNeeded).convert(
                    value.doubleValue());
            return newValue;
        } else {
            return value;
        }
    }

    // public Unit<?> getUnit() {
    // return unit;
    // }

    public Unit<?> getUnit() {
        if (this.unitStr == null)
            return Unit.ONE;
        if (this.unit == null) {
            try {
                this.unit = (Unit<?>) UnitFormat.getUCUMInstance().parseObject(
                        this.unitStr);
            } catch (ParseException e) {
                // logger.warn("ParseException while parsing unit string: "
                // + this.unit + " defaulting to unit: " + Unit.ONE);
                this.unit = Unit.ONE;
            }
        }
        return this.unit;
    }

    public void setValue(Amount v) {
        // System.out.println("Amount.setValue(Amount v) called with v " + v);
        setValue(v.value, v.unit);
    }

    public void setValue(Number n) {
        // System.out.println("Amount.setValue(Number n) called with n " + n +
        // " and unit " + unit);
        // setValue( n, unit );
        value = n;
    }

    /**
     * @return the missing_data_value
     */
    public final Number getMissing_data_value() {
        return missing_data_value;
    }

    /**
     * @param missing_data_value
     *            the missing_data_value to set
     */
    public final void setMissing_data_value(Number missing_data_value) {
        this.missing_data_value = missing_data_value;
    }

    public void setValueAs(Number n, String unitStr) {
        try {
            Unit<?> u = new UnitAdapter().unmarshal(unitStr);
            setValue(n, u);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //
    public void setValue(Number n, Unit<?> u) {
        value = n;
        // unit = u;
        setUnitPair(u);

        // to make the hasValidValue method simpler, ensure that the types
        // of the value and missing data value are the same.
        // (wonder if there is a nicer way to do this for all Number types?)
        if (!value.getClass().equals(missing_data_value.getClass())) {
            if (value instanceof Double) {
                missing_data_value = new Double(
                        missing_data_value.doubleValue());
            } else if (value instanceof Float) {
                missing_data_value = new Float(missing_data_value.floatValue());
            } else if (value instanceof Short) {
                missing_data_value = new Short(missing_data_value.shortValue());
            } else if (value instanceof Integer) {
                missing_data_value = new Integer(missing_data_value.intValue());
            } else if (value instanceof Long) {
                missing_data_value = new Long(missing_data_value.longValue());
            } else if (value instanceof Byte) {
                missing_data_value = new Byte(missing_data_value.byteValue());
            } else {
                System.out
                        .println("Amount: missing_data_value and value are of different types?");
            }
        }

        // if( hasValidValue() ) {
        // System.out.println( getClass().getName() +" has valid  value "+
        // n.toString() );
        // }
    }

    // make sure the missing data sentinal and the stored value are of the
    // same type so that the comparisons will work.
    //
    public void initMissingDataSentinel() { // Number mds ) {
        // limit the number of
        if (value instanceof Double) {
            missing_data_value = new Double(-9999);
        } else if (value instanceof Float) {
            missing_data_value = new Float(-9999);
        } else if (value instanceof Integer) {
            missing_data_value = new Integer(-9999);
        } else if (value instanceof Long) {
            missing_data_value = new Long(-9999);
        } else if (value instanceof Short) {
            missing_data_value = new Short((short) -9999);
        } else {
            System.out
                    .println("Error: Number object in Amount is not one of the supported types: "
                            + "Double, Float, Integer, Long or Short");
        }
    }

    public void setMissingDataSentinel(Number mds) {
        missing_data_value = mds;
    }

    public Number getMissingValueSentinel() {
        return missing_data_value;
    }

    // convenience method used by PRLibrary.
    // TODO : replace calls with getValue().doubleValue
    public double doubleValue() {
        return value.doubleValue();
    }

    public boolean hasValidValue() {
        if (unit == null) {
            return false;
        }
        if (!value.getClass().equals(missing_data_value.getClass())) {
            // TODO: Do we care? See a lot of Double/Float differences
            // System.out.println("value and missingDataValue are of different types.");
        }
        if (value == null || value.equals(missing_data_value)) {
            // || value.doubleValue() <= missing_data_value.doubleValue()) ) {
            return false;
        }
        return true;
    }

    public void setValueToMissing() {
        value = missing_data_value;
    }

    public void setUnit(Unit<?> u) {
        // Set the unit field only, but not the associated string.
        /*
         * DEBUG String oldUnitStr = (unitStr == null ? "NULL" :
         * UnitFormat.getUCUMInstance().format(unit)); String newUnitStr =
         * UnitFormat.getUCUMInstance().format(u); if (oldUnitStr.equals("Pa")
         * && newUnitStr.equals("hPa")) {
         * System.out.println("In setUnit for... " + this.getClass());
         * System.out.println("...changing from Pa to hPa..."); } if
         * (oldUnitStr.equals("hPa") && newUnitStr.equals("Pa")) {
         * System.out.println("In setUnit for... " + this.getClass());
         * System.out.println("...changing from hPa to Pa..."); } DEBUG
         */
        unit = u;
    }

    public void setUnitPair(Unit<?> u) {
        // Set the unit object and string, but do not convert existing value.
        unit = u;
        /*
         * DEBUG String newUnitStr = UnitFormat.getUCUMInstance().format(u); if
         * (unitStr.equals("Pa") && newUnitStr.equals("hPa")) {
         * System.out.println("In setUnitPair for... " + this.getClass());
         * System.out.println("...changing from Pa to hPa..."); } if
         * (unitStr.equals("hPa") && newUnitStr.equals("Pa")) {
         * System.out.println("In setUnitPair for... " + this.getClass());
         * System.out.println("...changing from hPa to Pa..."); } unitStr =
         * newUnitStr; DEBUG
         */
        unitStr = UnitFormat.getUCUMInstance().format(u);
    }

    public void setUnitPairAndConvertValue(Unit<?> u) {
        // Set the unit object and string, and convert an existing value to
        // the new unit
        if (hasValidValue()) {
            value = getValueAs(u);
        }

        unit = u;
        /*
         * DEBUG String newUnitStr = UnitFormat.getUCUMInstance().format(u); if
         * (unitStr.equals("Pa") && newUnitStr.equals("hPa")) {
         * System.out.println("In setUnitPairAndConvertValue for... " +
         * this.getClass());
         * System.out.println("...changing from Pa to hPa..."); } if
         * (unitStr.equals("hPa") && newUnitStr.equals("Pa")) {
         * System.out.println("In setUnitPairAndConvertValue for... " +
         * this.getClass());
         * System.out.println("...changing from hPa to Pa..."); } unitStr =
         * newUnitStr; DEBUG
         */
        unitStr = UnitFormat.getUCUMInstance().format(u);
    }

    public void syncUnits() {
        Unit<?> u;
        if (unitStr.equals("count")) {
            u = Unit.ONE;
        } else {
            try {
                u = new UnitAdapter().unmarshal(unitStr);
            } catch (Exception e) {
                e.printStackTrace();
                // System.out.println("Amount.syncUnits():  'unitStr' string '"
                // + unitStr + "' invalid!!");
                return;
            }
        }
        if (unit == null) {
            // System.out.println("Amount.syncUnits():  'unit' NULL, using unitStr value "
            // + unitStr);
            unit = u;
        } else if (!unit.equals(u)) {
            // System.out.println("Amount.syncUnits():  'unit' is " + u +
            // ", but unitStr is " + unitStr + " using the latter");
            unit = u;
        } else {
            // System.out.println("Amount.syncUnits():  Good!  'unit' is " + u +
            // ", and unitStr is " + unitStr);
        }
    }

    public String getUnitStr() {
        return unitStr;
    }

    public void setUnitStr(String unitStr) {
        this.unitStr = unitStr;
    }

    // // convert the current value to
    // public void changeUnits( Unit<?> u ) {
    // if( value != MISSING_DATA_VALUE ) {
    // unit = u;
    // }
    // else {
    //
    // }
    // }
    // TODO : Do we need to worry about rounding errors here?
    // public Boolean isMissingValue( ) {
    // return value.doubleValue() == MISSING_DATA_VALUE;
    // }
}
