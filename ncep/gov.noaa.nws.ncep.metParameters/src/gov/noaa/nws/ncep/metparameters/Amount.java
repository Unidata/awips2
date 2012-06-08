package gov.noaa.nws.ncep.metparameters;


import javax.measure.unit.Unit;
/**
 * Class used to hold a value and its units. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  06/16/2011   #441       G Hull    Initial creation.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */

public class Amount {
	
	// TODO : add capability to let user set their own missing data value. 
	private Number missing_data_value = new Double( -9999 );

	protected Number value = missing_data_value;

	protected Unit<?> unit;

	// To simplify the 
	public Amount( Number val, Unit<?> unit) {
		this.value = val;
		this.unit = unit;		
		
		setMissingDataSentinel();		
	}
	
	public Amount( Unit<?> u ) {		
		value = missing_data_value;
		unit = u;
	}

	// TODO Should we allow access to the value without the units? Or 
	// can we let the user assume the stored units for convienience?
	public Number getValue() {
		return value;
	}

	// call hasValidValue before calling this method.
	//
	public Number getValueAs( Unit<?> unitNeeded ) {
		if( unitNeeded != unit && unitNeeded.isCompatible( unit ) ) {
			double newValue = unit.getConverterTo(unitNeeded).convert( value.doubleValue() );
			return newValue;
		}
		else {
			return value;
		}
	}

	public Unit<?> getUnit() {
		return unit;
	}

	public void setValue( Amount v ) {
		setValue( v.value, v.unit );
	}

	public void setValue( Number n ) {
		setValue( n, unit );
	}

	public void setValue( Number n, Unit<?> u ) {
		value = n;
		unit  = u;		
	}

	// make sure the missing data sentinal and the stored value are of the 
	// same type so that the comparisons will work.
	//
	// TODO : create a method to let the user set the missing data sentinal to
	// whatever they want....
	//
	public void setMissingDataSentinel( ) { // Number mds ) {
		// limit the number of 
		if( value instanceof Double ) {
			missing_data_value = new Double( -9999 );	
		}
		else if( value instanceof Float ) {
			missing_data_value = new Float( -9999 );	
		}
		else if( value instanceof Integer ) {
			missing_data_value = new Integer( -9999 );	
		}
		else if( value instanceof Long ) {
			missing_data_value = new Long( -9999 );	
		}
		else {
			System.out.println("Error: Number object in Amount is not one of the supported types: "+
					"Double, Float, Integer or Long " );
		}
	}
	
	public void setMissingDataSentinel( Number mds ) {
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
		if( (value == null || value.equals( missing_data_value ) 
				|| value.doubleValue() == missing_data_value.doubleValue()) ) {
			return false;
		}
		return true;
	}
	
	public void setValueToMissing( ) {
		value = missing_data_value;
	}

	public void setUnit( Unit<?> u ) {
		if( hasValidValue() ) {
			value = getValueAs( u );
		}
		
		unit = u;
	}

	//        // convert the current value to
	//        public void setUnits( Unit<?> u ) {
	//        	if( value != MISSING_DATA_VALUE ) {
	//        		unit = u;
	//        	}
	//        	else {
	//        		
	//        	}
	//        }
	// TODO : Do we need to worry about rounding errors here? 
	//        public Boolean isMissingValue( ) {
	//        	return value.doubleValue() == MISSING_DATA_VALUE;
	//        }
}
