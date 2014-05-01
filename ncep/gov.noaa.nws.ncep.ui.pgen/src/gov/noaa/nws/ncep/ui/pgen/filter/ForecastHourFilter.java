/*
 * ForecasthourFilter
 * 
 * Date created 24 MARCH 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.filter;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;

/**
 * Implementation of a filter class for elements' forecast hours.
 * If the hour of the element is in the range of the filter hours,
 * the element is accepted.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/10		#256		B. Yin   	Initial Creation.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class ForecastHourFilter implements ElementFilter {

	private String hours;
	
	/**
	 * @param category a PGEN category
	 */
	public ForecastHourFilter(String forecastHours) {
		this.hours = forecastHours;
	}

	@Override
	public boolean accept(AbstractDrawableComponent adc) {
		
		String elHours = adc.getForecastHours();
		
		//if elHours is in format "HH:MM", get HH and MM
		boolean singleHr = false;
		int elHH = 0;
		int elMM = 0;
		if ( elHours!= null && !elHours.isEmpty() && !elHours.contains("-") ){
			try {
				if ( elHours.contains(":")){
					//for HH:MM
					elHH = Integer.valueOf(elHours.substring(0, elHours.indexOf(":")));
					elMM = Integer.valueOf(elHours.substring(elHours.indexOf(":")+1));
				}
				else {
					//for HH
					elHH = Integer.valueOf(elHours);
					elMM = 0;
				}
			}
			catch ( NumberFormatException e ){
				return false;
			}
			
			singleHr = true;
		}
		
		if ( elHours == null || elHours.isEmpty() ){
			//element does not have forecast hours.
			return true;
		}
		else if ( elHours.equalsIgnoreCase(hours) ) {
			//exactly match.
			return true;
		}
		else if ( singleHr && elMM == 0 && hours.equalsIgnoreCase(Integer.toString(elHH))){
			//for 3:00 and 3, etc.
			return true;
		}
		else if ( hours.endsWith("+") ){
			//for filters with "+", such as 0+, 3+, etc..
			int startHour = 0;
			try {
				startHour = Integer.valueOf(hours.substring(0, hours.length()-1));
			}
			catch(NumberFormatException e ){
				return false;
			}

			int endHour = startHour + 3;

			if  (singleHr && elHH < endHour && elHH >= startHour ){
				return true;
			}
		}
		else if (hours.equalsIgnoreCase("AIRM") || hours.equalsIgnoreCase("OTLK")){
			if ( elHours.contains("-")){
				String endTm = elHours.substring(elHours.indexOf("-")+1);
				try{
					//get start hour and end hour of the input element 
					int endHr = 0;
					if ( endTm.contains(":")){
						endHr = Integer.valueOf(endTm.substring(0, endTm.indexOf(":")));
					}
					else {
						endHr = Integer.valueOf(endTm);
					}
					
					if ( hours.equalsIgnoreCase("OTLK") && endHr > 6 ){
						return true;
					}
					else if ( hours.equalsIgnoreCase("AIRM") && endHr <=6 ){
						return true;
					}
					
				}
				catch (NumberFormatException e ){
					return false;
				}
			}
		}
		
		return false;
		
	}

}
