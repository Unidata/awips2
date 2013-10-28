package gov.noaa.nws.ncep.edex.common.metparameters;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

import 
gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;

import gov.noaa.nws.ncep.edex.common.metparameters.DewPointTemp;
import gov.noaa.nws.ncep.edex.common.metparameters.PressureLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
//import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

/**
 * Maps to the GEMPAK parameter PMSL
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize

public class MeanSeaLevelPres extends AbstractMetParameter implements 
							javax.measure.quantity.Pressure, ISerializableObject {
	/**
	 * 
	 */
	private static final long serialVersionUID = -5651047733149575606L;

	public MeanSeaLevelPres() {
		 super( UNIT );
	}

	@DeriveMethod		
	public MeanSeaLevelPres derive( PressureLevel prs, AirTemperature t, 
						DewPointTemp dpt, HeightAboveSeaLevel hght ) throws InvalidValueException, NullPointerException  {
		if( prs.hasValidValue() && t.hasValidValue() && dpt.hasValidValue() &&
				hght.hasValidValue() ) {
			Amount pmsl = PRLibrary.prPmsl ( prs, t, dpt, hght );
			if( pmsl.hasValidValue() ) {
				setValue( pmsl );
			}
		}
		else { 
			setValueToMissing();
		}
		
		return this;
	}
 	
	@Override
	public String getFormattedString( String formatStr ) {
		if( formatStr == null || formatStr.isEmpty() ||
			formatStr.startsWith("%" ) ) {
			return super.getFormattedString( formatStr );
		}
		else if ( ( formatStr.compareToIgnoreCase("RMSL") == 0 ) 
			     || (formatStr.compareToIgnoreCase("SMSL") == 0 )){
		              double  newPresValInMb = Double.NaN;
		              if ( ( this.getUnit().toString().compareTo("mb") != 0 ) ){
				                  double  oldPresVal     = getValue().doubleValue();
				                  newPresValInMb = this.getUnit()
				                                           .getConverterTo( NcUnits.MILLIBAR )
				                                           .convert( oldPresVal ) ;
				                  //setValue(new Amount ( newPresValInMb, NcUnits.MILLIBAR ));
				       }
				         double temp = newPresValInMb * 10;
						 double abbrevPressVal    = temp % 1000; 
						 abbrevPressVal = Math.abs(abbrevPressVal);
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
//		String fmtValStr = super.getFormattedString( "%2.2f" );
//
//		return fmtValStr.substring( 1 );
	}

	
 	
 	
 	
//	@DeriveMethod		
//	public MeanSeaLevelPres derive( StationPressure prs, Temperature t, 
//						DewPointTemp dpt, StationElevation selv ) {
//		if( prs.hasValidValue() && t.hasValidValue() && dpt.hasValidValue() &&
//				selv.hasValidValue() ) {
////			Amount prPmsl ( Amount pres,  Amount tmpc,  Amount dwpc,  Amount selv );			
//		}
//		else { 
//			setValueToMissing();
//		}
//		
//		return this;
//	}		

}
