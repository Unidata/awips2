package gov.noaa.nws.ncep.metparameters;


import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.metParameters.parameterConversion.NcUnits;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidRangeException;
import gov.noaa.nws.ncep.metParameters.parameterConversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;


public class MeanSeaLevelPres extends AbstractMetParameter implements 
							javax.measure.quantity.Pressure {
	public MeanSeaLevelPres() {
		 super( UNIT );
	}

	@DeriveMethod		
	public MeanSeaLevelPres derive( PressureLevel prs, AirTemperature t, 
						DewPointTemp dpt, HeightAboveSeaLevel hght ) throws InvalidValueException, NullPointerException, InvalidRangeException {
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
				                  double  oldPresVal     = this.value.doubleValue();
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
