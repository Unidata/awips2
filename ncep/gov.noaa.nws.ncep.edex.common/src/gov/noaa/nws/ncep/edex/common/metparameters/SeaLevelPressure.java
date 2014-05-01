package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.unit.NonSI;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.UnitAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;

/**
 * Maps to either of the GEMPAK parameters ALTI or ALTM depending on the unit
 * used to measure the sea level pressure
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize


 public class SeaLevelPressure extends AbstractMetParameter implements 
								javax.measure.quantity.Pressure,  ISerializableObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1025585414782928040L;

	public SeaLevelPressure() throws Exception {
		 super( new UnitAdapter().marshal(UNIT) );
	}

	@Override
	public String getFormattedString(String formatStr) {
		
		if( formatStr == null || formatStr.isEmpty() ||
				formatStr.startsWith("%" ) ) {
				return super.getFormattedString( formatStr );
	    }
		
		else if ( ( formatStr.compareToIgnoreCase("RSLT") == 0 ) 
				     || (formatStr.compareToIgnoreCase("SALT") == 0 )){
			              double  newPresValInMb = Double.NaN;
			              if ( ( this.getUnit().toString().compareTo("mb") != 0 ) ){
					                  double  oldPresVal     = getValue().doubleValue();
					                  newPresValInMb = this.getUnit()
					                                           .getConverterTo( NcUnits.MILLIBAR )
					                                           .convert( oldPresVal ) ;
					                  //setValue(new Amount ( newPresValInMb, NcUnits.MILLIBAR ));
					       }
					      
							 double abbrevPressVal    = ( newPresValInMb % 100 ) * 10; 
							 Integer abbrevpressValAsInt = new Integer ( ( int ) abbrevPressVal );
							 String abbrevPressureString = abbrevpressValAsInt.toString();
							 return abbrevPressureString;
		}
		
		else if ( ( formatStr.compareToIgnoreCase("RSLI") == 0 ) 
			     || (formatStr.compareToIgnoreCase("SALI") == 0 )){
			
			          double  newPresValInMb = Double.NaN;
			          double  oldPresVal     = getValue().doubleValue();
			          if ( ( this.getUnit().toString().compareTo("inHg") != 0 ) ){
                             newPresValInMb = this.getUnit()
                                                      .getConverterTo( NonSI.INCH_OF_MERCURY )
                                                      .convert( oldPresVal ) ;
                             //setValue(new Amount ( newPresValInMb, NonSI.INCH_OF_MERCURY ));
                       } else {
                    	   newPresValInMb = oldPresVal;
                       }
		      
				 double abbrevPressVal       = ( newPresValInMb % 10 ) * 100; 
				 Integer abbrevpressValAsInt = new Integer ( ( int ) abbrevPressVal );
				 String abbrevPressureString = abbrevpressValAsInt.toString();
                 
				 if (  abbrevPressureString.length() == 1 ){
                	 abbrevPressureString = new String ( "00" + abbrevPressureString);
                 }
                 if (  abbrevPressureString.length() == 2 ){
                	 abbrevPressureString = new String ( "0" + abbrevPressureString);
                 }                 
                 
				 return abbrevPressureString;
		}
		
		else
			return super.getFormattedString( formatStr );
	}
	
 }
