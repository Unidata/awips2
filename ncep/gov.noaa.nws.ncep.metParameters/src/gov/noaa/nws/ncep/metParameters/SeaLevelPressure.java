package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.NonSI;

import gov.noaa.nws.ncep.metParameters.parameterConversion.NcUnits;


 public class SeaLevelPressure extends AbstractMetParameter implements 
								javax.measure.quantity.Pressure {

	public SeaLevelPressure() {
		 super( UNIT );
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
					                  double  oldPresVal     = this.value.doubleValue();
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
			          if ( ( this.getUnit().toString().compareTo("inHg") != 0 ) ){
                             double  oldPresVal     = this.value.doubleValue();
                             newPresValInMb = this.getUnit()
                                                      .getConverterTo( NonSI.INCH_OF_MERCURY )
                                                      .convert( oldPresVal ) ;
                             //setValue(new Amount ( newPresValInMb, NonSI.INCH_OF_MERCURY ));
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
