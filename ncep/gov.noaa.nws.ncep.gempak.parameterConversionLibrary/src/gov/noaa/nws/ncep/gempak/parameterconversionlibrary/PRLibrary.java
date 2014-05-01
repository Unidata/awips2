/*
 * 
 * PRLibrary
 * 
 * Date created 28 January 2011
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system. 
 */
package gov.noaa.nws.ncep.gempak.parameterconversionlibrary;
import java.util.Arrays;

import gov.noaa.nws.ncep.gempak.parameterconversionlibrary.GempakConstants;
import gov.noaa.nws.ncep.gempak.parameterconversionlibrary.MissingValueTester;
/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 *     Date            Ticket#    Engineer        Description
 * ------------------ ---------- --------------- --------------------------
 * 28-Jan-2011        398       Archana       Initial creation.
 * 04 Apr 2011		  398		F. J. Yen	  After JUnit testing (not complete), fix prAltm, prAltp,
 * 											  prCfct, prClct, prCldb, prClhx, prCmbc?,
 * 											  Add comments to prologue in prClct (indexing),
 * 											  prCmbc, prCtcc, prCtcf (indexing and bounds check),
 * 											  prDrct (Change && to ||), add equation to
 * 											  prologue (prMSkn, prKnms, prKnmb, prMhkn,
 * 											  prHgfm, prHgsf, prHgnm, prHmtr, prVapr, prIgro,
 * 											  prInmm, prMmin, prLhvp, prMhgt),
 * 											  prHcdm (add check for string length), prHgmf
 * 											  (round result, add equation to prologue),
 * 											  prRzll (fix order of precedence in equation;
 * 											  change test from < to <=; and set xlat and xlon
 * 											  to missing if have missing values), prMhgt (check 
 * 											  for missing values for scale), prP0ec (fix 
 * 											  calculation of ptend),  (Not complete and not final. 
 * 											  Some original code are commented out for comparison, 
 * 											  but will to be deleted when finalized.)
 * 
 *</pre> 
 * @author Archana
 * @version 1.0
 */
public class PRLibrary {

	public static RZLL getRZLLInstance(){
		return RZLL.getInstance();
	}
	
	/**
	 * This function computes altimeterInInches from altimeterInMillibars
	 * @param altimeterInMillibars
	 * @return the altimeter in inches
	 */
	public static float prAlti(float altimeterInMillibars){
		float altimeterInInches =GempakConstants.RMISSD;  
		if (altimeterInMillibars != GempakConstants.RMISSD){   
		     altimeterInInches = ( float ) ( altimeterInMillibars * 29.921 / 1013.25 );
		}
	    return altimeterInInches;
	}
	
	/**
	 * This function computes altimeterInMillibars from altimeterInInches 
	 * @param altimeterInMillibars
	 * @return the altimeter in millibars
	 */
	public static float prAltm(float altimeterInInches){
		float altimeterInMillibars = GempakConstants.RMISSD;  
		if(altimeterInInches != GempakConstants.RMISSD){
		       altimeterInMillibars = ( float ) ( altimeterInInches * 1013.25 / 29.921 );
		}
		return altimeterInMillibars;
	}	
	
	/**
	 * Computes altimeter from the station pressure and elevation
	 * @param pres - pressure at the station
	 * @param selv - elevation of the station 
	 * @return the computed altimeter in Inches
	 */
	public static float prAltp(float pres, float selv){
		  if(pres != GempakConstants.RMISSD && selv != GempakConstants.RMISSD){
		       float seaLevelTempInKelvin = GempakConstants.TMCK + 15;
		       float hgtk = prHgmk(selv);
		       double exponent = - ( GempakConstants.GRAVTY / ( GempakConstants.GAMUSD * GempakConstants.RDGAS ) * 1000.0f);
//		       double base = pres * ( 1.0f - ( hgtk * GempakConstants.GAMUSD / seaLevelTempInKelvin ) );
//		       float altm = (float)  Math.pow( base, Math.exp( exponent ) );
//		       return prAlti( pres * altm );
		       double base = ( 1.0f - ( hgtk * GempakConstants.GAMUSD / seaLevelTempInKelvin ) );
		       float altm = (float)  Math.pow( base, exponent );
		       return prAlti( pres * altm );
		  }else{
			  return GempakConstants.RMISSD;
		  }
	}
	
/***
 * Computes TPWN, the numeric weather code for temporary/ probability/vicinity weather, 
 * with probability 30 weather taking  precedence over vicinity weather 
 * and vicinity weather taking precedence over temporary weather.
 * @param twnm  - Temporary weather code
 * @param vwnm - Vicinity weather code
 * @param pprb    - Probability for forecast change indicator 
 * @return the temporary/probability/vicinity numeric weather code
 */
	public static float prTpwn(float twnm, float vwnm, float pprb){
		float tpwn = GempakConstants.RMISSD;
		if ( Math.round(pprb) == 30
				&& !MissingValueTester.isDataValueMissing(twnm)){
			      tpwn =  twnm;
		}else if (!MissingValueTester.isDataValueMissing(vwnm)){
			      tpwn = vwnm;
		}else{
			      tpwn = twnm;
		}
	     return tpwn;
	}
	
	/***
     * Computes prAllWeatherNumericCode, the numeric weather code for 
     * prevailing temporary/ probability/vicinity weather, 
     * with probability 30 weather taking  precedence over vicinity weather 
     * and vicinity weather taking precedence over temporary weather 
     * and temporary weather taking precedence over prevailing weather.
	 * @param wnum - Prevailing numeric weather code
	 * @param twnm  - Temporary weather code
	 * @param vwnm - Vicinity weather code
	 * @param pprb    - Probability for forecast change indicator 
	 * @return the all weather numeric code
	 */
	public static float prAwnm(float wnum, float twnm, float vwnm, float pprb){
		float prAllWeatherNumericCode = prTpwn( twnm,vwnm, pprb);
		return (MissingValueTester.isDataValueMissing(prAllWeatherNumericCode)
				       ? wnum : prAllWeatherNumericCode);
	}
	
	/***
	 * Computes a standard abbreviated 3-digit display of
     * pressure containing the tens and units digits and the first digit
     * after the decimal point.  The input is multiplied by 10, truncated,
     * and the original thousand and hundred digits are dropped
	 * @param pmsl - the pressure in mb
	 * @return the standard abbreviated pressure 
	 */
	public static float prAmsl(float pmsl){
		float pramsl = GempakConstants.RMISSD;
		if (MissingValueTester.isDataValueMissing(pmsl)){
			return pramsl;
		}else{
			            float aalt =  pmsl % 100;
			            aalt *= 10;
			            pramsl = Math.round(aalt);
			            return pramsl;
		}
	}
	
	/***
	 * Computes the WMO fractional cloud cover from
	 * the input numeric total cloud cover
	 * @param cfrt - Numeric total cloud cover
	 * @return RMISSD if the numeric total cloud cover is equal to RMISSD
	 * and the WMO fractional cloud cover otherwise
	 */
	public static float prCfct(float cfrt){
		float prcfct = GempakConstants.RMISSD;
		int[] itable = {1, 6, 6, 2, 2, 7, 3, 8, 4, 5};
		if ( !MissingValueTester.isDataValueMissing(cfrt)){
		      if ( (cfrt >= 0) && (cfrt <= 9) ){
		    	  int index = Math.round(cfrt);
		    	  /*sanity check*/
		    	  if(itable != null && itable.length > 0
		    			  && itable.length >= (index +1) ){     
//		    	       prcfct = itable[index + 1];
		    		  prcfct = itable[index];
		    	  }
		      }
		}
		return prcfct;
	}
	
	/***
	 * Computes the maximum cloud cover from the low, mid and high cloud covers.
	 * 
	 * Coverage values are ordered as follows:
	 * 		0      no clouds
	 * 		1      clear 
	 * 		6      thin scattered
	 * 		2      scattered 
	 * 		7      thin broken
	 * 		3      broken
	 * 		8      thin overcast
	 * 		4      overcast9
	 * 		9      partially obscured
	 * 		5      obscured
	 * 		
	 * @param clcl - low-level cloud cover
	 * @param clcm - mid-level cloud cover
	 * @param clch - high-level cloud cover
	 * @return the maximum cloud cover
	 */
	public static float prClct(float clcl, float clcm, float clch){
		/*Sanity check*/
		if ( clcl == GempakConstants.RMISSD 
				|| clcm == GempakConstants.RMISSD
				|| clch == GempakConstants.RMISSD){
			return GempakConstants.RMISSD;
		}
		int[] list = {1, 2, 4, 6, 8, 10, 3, 5, 7, 9};
		int[] invert = {0, 1, 6, 2, 7,  3, 8, 4, 9, 5};
		int ilow = 1;
		int imed = 1;
		int ihigh = 1;
		int iclcl = (int) clcl;
		int iclcm = (int) clcm;
		int iclch = (int) clch;
		if ( iclcl >= 0 && iclcl < 10 ){
			ilow = list[iclcl];
		}
		if ( iclcm >= 0 && iclcm < 10 ){
			imed = list[iclcm];
		}		
		if ( iclch >= 0 && iclch < 10 ){
			ihigh = list[iclch];
		}
		int[] tempArray = {ilow,imed,ihigh}; 
		Arrays.sort(tempArray);
//		int maxVal = tempArray[3];
		int maxVal = tempArray[2];
		if(maxVal == 9){
			maxVal = (imed  > ihigh ? imed : ihigh);
			if(maxVal < 3){
				maxVal = 9;
			}
		}
//		return invert[maxVal];
		return invert[maxVal - 1];
	}
	
   /***
    * Computes the numeric cloud coverage from the combined cloud height and coverage
    * @param comx - the combined cloud height (either at the low/medium/high cloud level expressed in hundreds of feet) and coverage.
    * @return the numeric cloud coverage
    */
	public static float prClcx(float comx){
		/*Sanity check*/
		if(MissingValueTester.isDataValueMissing(comx)){
			     return GempakConstants.RMISSD;
		}
		else{
			      return ( ((int) comx) % 10 );
		}
	}
	
	/***
	 * This method computes the lowest ceiling from ceil, chc1, chc2 and
     * chc3. if there is no ceiling, get the lowest layer from chc1, chc2 and chc3.
	 * @param ceil  - Ceiling in hundreds of feet
	 * @param chc1 - Cloud height & coverage 1 
	 * @param chc2 - Cloud height & coverage 2 
	 * @param chc3 - Cloud height & coverage 3 
	 * @return The lowest ceiling combined with a numeric cloud coverage  
	 */
	public static float prCldb ( float ceil,float chc1,float chc2,float chc3){
		float prcldb = GempakConstants.RMISSD;
		float temp  = GempakConstants.RMISSD; 
		if(!MissingValueTester.isDataValueMissing(ceil)){
			if(!MissingValueTester.isDataValueMissing(chc1)
					&& Math.abs( ceil - ( ( int ) chc1/10) )  < GempakConstants.RDIFFD ){
				prcldb = chc1;
			}
			else if (!MissingValueTester.isDataValueMissing(chc2)
						&& Math.abs( ceil - ( ( int ) chc2/10) )  < GempakConstants.RDIFFD ){
					prcldb = chc2;
				}
			else if (!MissingValueTester.isDataValueMissing(chc3)
					&& Math.abs( ceil - ( ( int ) chc3/10) )  < GempakConstants.RDIFFD ){
				prcldb = chc3;
			}
		}
		else{
			 /*Check the first report*/
			     if(!MissingValueTester.isDataValueMissing(chc1)){
				        temp = chc1;
			     }

				 /*Check the second report*/
//			     if(!MissingValueTester.isDataValueMissing(chc2)
//			    		 && ! (MissingValueTester.isDataValueMissing(chc2)
//			    				 && (chc1 > chc2) ) ){
//			     	temp = chc2;
//				 }
//				To make like legacy (test chc1 missing, not chc2):
//			     if(!MissingValueTester.isDataValueMissing(chc2)
//			    		 && !MissingValueTester.isDataValueMissing(chc1)
//			    				 && (chc1 > chc2) ){
//			    Fix legacy:
			     if(!MissingValueTester.isDataValueMissing(chc2)) {
			    	 if (MissingValueTester.isDataValueMissing(chc1)){
			    		 temp = chc2;
			    	 }
			    	 else if (chc1 > chc2) {
			    		 temp = chc2;
			    	 }
			     }
			     
				 /*Check the third report*/
//			     if(!MissingValueTester.isDataValueMissing(chc3)
//			    		 && !(MissingValueTester.isDataValueMissing(temp))
//			    		 && (temp > chc3)){
//				        temp = chc3;
//			     }
			     if(!MissingValueTester.isDataValueMissing(chc3)) {
			    	 if (MissingValueTester.isDataValueMissing(temp)) {
			    		 temp = chc3;
			    	 }
			    	 else if (temp > chc3){
			    	 	 temp = chc3;
			     	}
			     }
			    
			     if( !MissingValueTester.isDataValueMissing(temp) ){
			              prcldb = temp;
			     }
		}
		return prcldb;
	}
	
	   /***
	    * <pre>
	    * Computes the numeric cloud coverage from the combined cloud height and coverage
	    * @param comx - the combined cloud height (either at the low/medium/high cloud level expressed in hundreds of feet) and 
	    *                            numeric cloud coverage.
	    * @return the cloud height in feet * 100
	    * </pre>
	    */
		public static float prClhx(float comx){
			/*Sanity check*/
	        if(MissingValueTester.isDataValueMissing(comx) || comx < 10){
				     return GempakConstants.RMISSD;
			}
//	        return ( comx > 10000 ?  (  ( (int) (comx) - 10000) /10 )  : (int) comx);
	        return ( comx > 10000 ?  (  ( (int) (comx) - 10000) /10 )  : (int) (comx / 10.));
	 	}
		
		/***
		 * Computes the fractional cloud coverage from the numeric cloud coverage 
		 * @param clcx - the numeric cloud coverage for clouds ( one of Low/Mid/High/Total cloud coverage)
		 * @return the fractional cloud coverage
		 */
		public static float prCloa(float clcx){
			/*Sanity check*/
			if(MissingValueTester.isDataValueMissing(clcx)){
				return GempakConstants.RMISSD;
			}
			float[] cloud = {
					0.00f, /*missing*/ 
					0.00f,  /*clear*/ 
					0.40f,  /*scattered*/
					0.75f,  /*broken*/
					1.00f,  /*overcast*/
					1.00f,  /*obscured*/
					0.25f, /*thin scattered*/
				    0.60f,  /*thin broken*/
				    0.90f,  /*thin overcast*/
				    0.00f  /*partially obscured*/	
			};
			
			int icld = (int) clcx;
			if ( icld < 0 || icld > 9){
				icld = 0;
			}
			return ( (cloud != null && cloud.length > 0 && cloud.length >= icld )  ? cloud[icld] : GempakConstants.RMISSD );
		}
		
		/***
		 * Computes the combined low, mid and high cloud coverage
		 * The following equation is used:  cmbc = clcl*100 + clcm*10 + clch
		 * @param clcl   - low cloud coverage
		 * @param clcm - mid cloud coverage 
		 * @param clch  -  high cloud coverage
		 * @return the combined cloud coverage
		 */
		public static float prCmbc(float clcl, float clcm, float clch){
			/*Sanity check*/
//			if ( MissingValueTester.isDataValueMissing(clcl) 
//					|| MissingValueTester.isDataValueMissing(clcm)
//					|| MissingValueTester.isDataValueMissing(clch)){
//				return GempakConstants.RMISSD;
//			}
//999999	Set missing dta values to 0.
					
			int iclcl = 0;
			int iclcm = 0;
			int iclch = 0;
			
			if ( clcl >= 0 && clcl <= 9 ){
				iclcl = (int) clcl;
			}
			
			if ( clcm >= 0 && clcm <= 9 ){
				iclcm = (int) clcm;
			}
			
			if ( clch >= 0 && clch <= 9 ){
				iclch = (int) clch;
			}
			
			return (( iclcl * 100 ) + ( iclcm * 10 ) + iclch);
		}
		
		/***
		 * Computes the Ceiling converted to Mean Sea Level in hundreds of feet
		 * @param ceil - Ceiling in hundreds of feet 
		 * @param selv - Station elevation in meters 
		 * @return The ceiling converted to Mean Sea Level in hundreds of feet
		 */
		public static float prCmsl(float ceil, float selv){
			/*Sanity check*/
			if ( MissingValueTester.isDataValueMissing(ceil) 
					|| MissingValueTester.isDataValueMissing(selv) ){
				return GempakConstants.RMISSD;
			}
			return ( (prHgmf(selv) / 100) + ceil);
		}
		

		
		/***
		 * Computes the combined height and numeric sky coverage for high clouds,
		 * whose height is greater than 17,900 feet 
		 * @param chc1 - Cloud height & coverage 1 
		 * @param chc2 - Cloud height & coverage 2
		 * @param chc3 - Cloud height & coverage 3
		 * @return the computed combined height and numeric sky coverage for high clouds
		 */
		public static float prComh ( float chc1, float chc2, float chc3 ){
			float prcomh = GempakConstants.RMISSD;
			if (MissingValueTester.isDataValueMissing(chc1)){
				return GempakConstants.RMISSD;
			}
			int ichc1 = -1;
		    int ihght = -1;
		    int icovr = -1;
			int icovx = -1;
			/*
			 * (Non-Javadoc)
			 * Check for thin obscured combined with cloud height and coverage and get height code.			
			 */
			ichc1 = ( ( chc1 >= 10000 ) ? ( ( int ) (chc1) - 10000 )   :    ( int ) (chc1)  );

		    ihght = ichc1 / 10;
		  
		   /*Check height against high level limits*/ 		   
		  if ( ihght > 178) {
			  prcomh = chc1 % 10000;
			  icovr =  (int)  prCtcf( (float)(ichc1 %10) );
		  }
		   
		  /*Check 2nd report*/
		  if( !MissingValueTester.isDataValueMissing(chc2) ){
			  ihght = (int) (chc2 / 10);
			  icovx =  (int) prCtcf( ( chc2 - (float) ( ihght * 10 ) ) );
			  if  ( ihght > 178 && icovx > icovr){
				  prcomh = chc2;
				  icovr = icovx;
			  }
		  }
		
		  /*Check 3rd report*/
		  if( !MissingValueTester.isDataValueMissing(chc3) ){
			  ihght = (int) (chc3 / 10);
			  icovx =  ( int ) prCtcf( ( chc3 - (float) ( ihght * 10 ) ) );
			  if  ( ihght > 178 && icovx > icovr){
				  prcomh = chc3;
			  }
		  }		  
		  return prcomh;
		}
		
		/***
		 * Computes the combined height and numeric sky coverage for  clouds,
		 * whose height is lesser than 63000 feet 
		 * @param chc1 - Cloud height & coverage 1 
		 * @param chc2 - Cloud height & coverage 2
		 * @param chc3 - Cloud height & coverage 3
		 * @return the combined height & coverage for low clouds
		 */
		public static float prComl(float chc1, float chc2, float chc3 ){
			float prcoml = GempakConstants.RMISSD;
			int icovr = -1;
			if( !MissingValueTester.isDataValueMissing(chc1) ){
				if ( chc1 == 10000){
					/*Partially obscured conditions reported.*/
					prcoml = 9.0f;
				}else{
					/* Check for thin obscured combined with cloud height and
                     * coverage and get height code
                     */
					int ichc1 = -1;
					int ihght = -1;
					int icovx = -1;
					ichc1 = ( chc1 > 10000 ?  (  ( int ) chc1 - 10000) : (int) chc1  );
					ihght = ichc1 / 10;
					
					/*Check height against low-level limit.*/
					if ( ihght <= 63){
						prcoml = chc1;
						icovr = (int) prCtcf ( ((float)  (ichc1%10) )  );
            		}
					else if ( chc1 > 10000 ){
						prcoml = 9.0f;
					}
					
					/*Check 2nd cloud report.*/
					if ( !MissingValueTester.isDataValueMissing(chc2)){
						ihght = ( int ) (chc2) / 10;
						icovx = ( int ) prCtcf ( chc2 - (float) (ihght * 10) );
						if (  ( ihght <= 63 ) && ( icovx > icovr ) ){
							prcoml = chc2;
							icovr = icovx;
						}
					}

					/*Check 3rd cloud report.*/
					if ( !MissingValueTester.isDataValueMissing( chc3 )){
						ihght = ( int ) (chc3) / 10;
						icovx = ( int ) prCtcf ( chc3 - (float) (ihght * 10) );
						if (  ( ihght <= 63 ) && ( icovx > icovr ) ){
							prcoml = chc3;
						}
					}					
					
					
				}
			}
			return prcoml;
		}
		
		/***
		 * Computes the combined height and numeric sky coverage for  mid-level clouds,
		 * whose height is greater than 63000 feet but lesser than 179,000 feet. 
		 * @param chc1 - Cloud height & coverage 1 
		 * @param chc2 - Cloud height & coverage 2
		 * @param chc3 - Cloud height & coverage 3
		 * @return the combined height & coverage for mid-level clouds
		 */
		public static float prComm(float chc1, float chc2, float chc3 ){
			float prcomm = GempakConstants.RMISSD;
			int icovr = -1;
			if( !MissingValueTester.isDataValueMissing(chc1) ){
				    int ichc1 = -1;
					int ihght = -1;
					int icovx = -1;
					
					/* Check for thin obscured combined with cloud height and
                     * coverage and get height code
                     */					
					ichc1 = ( chc1 >= 10000 ?  (  ( int ) chc1 - 10000) : (int) chc1  );
					ihght = ichc1 / 10;
					
					/*Check height against mid-level limits.*/
					if ( ( ihght > 63 ) && ( ihght < 179 ) ){
						prcomm = chc1 %  10000;
						icovr = (int) prCtcf ( ((float)  (ichc1%10) )  );
            		}
					
					/*Check 2nd cloud report.*/
					if ( !MissingValueTester.isDataValueMissing(chc2)){
						ihght = ( int ) (chc2) / 10;
						icovx = ( int ) prCtcf ( chc2 - (float) (ihght * 10) );
						if (  ( ihght > 63 ) && ( ihght < 179 )  &&  ( icovx > icovr ) ){
							prcomm = chc2;
							icovr = icovx;
						}
					}

					/*Check 3rd cloud report.*/
					if ( !MissingValueTester.isDataValueMissing( chc3 )){
						ihght = ( int ) (chc3) / 10;
						icovx = ( int ) prCtcf ( chc3 - (float) (ihght * 10) );
						if (  ( ihght > 63 ) && ( ihght < 179 )  && ( icovx > icovr ) ){
							prcomm = chc3;
						}
					}					

			}
			return prcomm;
		}
		
		/***
		 *  Computes the highest cloud height and coverage from the low, mid and high cloud height and coverage
		 * @param coml   - Low report height & coverage 
		 * @param comm - Mid report height & coverage 
		 * @param comh  - High report height & coverage
		 * @return the highest combined cloud height and coverage
		 */
		public static float prComt(float coml, float comm, float comh){
			/*Sanity  check*/
			if ( MissingValueTester.isDataValueMissing(coml) 
					|| MissingValueTester.isDataValueMissing(comm)
					|| MissingValueTester.isDataValueMissing(comh)){
				          return GempakConstants.RMISSD;
			} else{
				         float prcomt = GempakConstants.RMISSD;
				         /*Decode coverage values for each level*/
				         int clcl = (  (int) coml )   - (  ((int) coml ) / 10 * 10   );
				         int clcm = (  (int) comm )   - (  ((int) comm ) / 10 * 10   );
				         int clch = (  (int) comh )   - (  ((int) comh ) / 10 * 10   );
				         
				         /*Get maximum coverage value*/
				         float clct =  prClct( clcl, clcm, clch );
				         
				         /*Find maximum combined value*/
				         if ( clct == clcl )
				        	 prcomt = coml;
				         else if ( clct == clcm )
				        	 prcomt = comm;
				         else
				        	 prcomt = coml;
			             
				         return prcomt;
			}
		}

		/***
		 * Computes the combined cloud height and coverage from the
		 * cloud height (clhx) and the cloud coverage (clcx) 
		 * @param clhx - Cloud height
		 * @param clcx - Cloud coverage
		 * @return The combined cloud height and coverage
		 */
		public static float prComx( float clhx, float clcx){
			float prcomx = GempakConstants.RMISSD;
			/*Check for clear case*/
			if ( clcx == 1)
				prcomx = clcx;
           			/*Check for obscured clouds with missing height; return cloud - coverage only.*/
			else if  ( ( ( clcx == 5 ) || ( clcx == 9 ) )&& MissingValueTester.isDataValueMissing(clhx)  )  {
				prcomx = clcx;
			}
			/*Check for missing or invalid data*/
			else if (   ( clhx < 0 ) || MissingValueTester.isDataValueMissing(clhx) )
			        return GempakConstants.RMISSD;
			else {
				 /*
				  * Combine data by multiplying height by 10 and adding clouds.
                  * Missing cloud amounts are given the value 0 (clear).
				  */
				      int iclhx = (int) clhx;
				      int iclcx = (int)  clcx;
				      if (  ( iclcx < 0 ) || ( iclcx > 9 ) )
				    	  iclcx = 0;
				      prcomx = ( iclhx * 10 ) + iclcx;
			}
			
			return prcomx;
		}
		
		/***
		 * Computes the cloud type symbol number from the synoptic code cloud type number
		 * @param ctyh - Synoptic code for high cloud
		 * @return The cloud type symbol number after adding 20 to ctyh if
		 *               ctyh lies between 1 and 9 and 0.0 otherwise 
		 */
		public static float prCsyh ( float ctyh){
			return (  ( ( ctyh >= 1) && ( ctyh <= 9 ) ) ? ctyh + 20 : 0.0f  );
		}
		
		/***
		 * Computes the cloud type symbol number from the synoptic code cloud type number
		 * @param ctyl - Synoptic code for low cloud
		 * @return The cloud type symbol number as ctyl if
		 *               ctyl lies between 1 and 9 and 0.0 otherwise 
		 */
		public static float prCsyl ( float ctyl){
			return (  ( ( ctyl >= 1) && ( ctyl <= 9 ) ) ? ctyl : 0.0f  );
		}		
		
		/***
		 * Computes the cloud type symbol number from the synoptic code cloud type number
		 * @param ctym - Synoptic code for mid cloud
		 * @return The cloud type symbol number after adding 10 to ctym if
		 *               ctym lies between 1 and 9 and 0.0 otherwise 
		 */
		public static float prCsym ( float ctym){
			return (  ( ( ctym >= 1) && ( ctym <= 9 ) ) ? ctym + 10 : 0.0f  );
		}		
		
		/***
		 * Computes the cloud type symbol number for the first level at which clouds are reported
		 * @param ctyl   - Synoptic code for low cloud
		 * @param ctym - Synoptic code for mid cloud
		 * @param ctyh - Synoptic code for high cloud
		 * @return Cloud symbol number
		 */
		public static float prCsyt ( float ctyl, float ctym, float ctyh){
			float prcsyt = 0.0f;
			if ( ( ctyl >= 1) && ( ctyl <= 9 ) ){
				prcsyt = ctyl;
			}
			else if ( ( ctym >= 1) && ( ctym <= 9 ) ){
				prcsyt = ctym + 10;
			}
			else if ( ( ctyh >= 1) && ( ctyh <= 9 ) ){
				prcsyt = ctyh + 20;
			}
			return prcsyt;
		}		
		
		/***
		 * Computes the maximum cloud cover from the combined height/coverage for low, mid and high clouds
		 * @param chc1 - Combined height and coverage for low clouds
		 * @param chc2 - Combined height and coverage for mid clouds
		 * @param chc3 - Combined height and coverage for high clouds
		 * @return the maximum cloud cover
		 */
		public static float prCtcc( float chc1, float chc2, float chc3){
			float prctcc = GempakConstants.RMISSD;
			
			/*Sanity check*/
			if ( !MissingValueTester.isDataValueMissing(chc1) 
					&& !MissingValueTester.isDataValueMissing(chc2)
					&& !MissingValueTester.isDataValueMissing(chc3)) {	
			/*
			 * Compute ordered values for each cloud cover, with checks for
             *  out-of-bounds values.
			 */
			int[] list = {1, 2, 4, 6, 8, 10, 3, 5, 7, 9};
			int[] invert = {0, 1, 6, 2, 7,  3, 8, 4, 9, 5};
			int ilow = 1;
			int imed = 1;
			int ihi = 1;
			int iclcl = Math.round(chc1) % 10;
			if ( ( iclcl >= 0 ) && ( iclcl < 10 ) )
				ilow = list[iclcl];

			int iclcm = Math.round(chc2) % 10;
			if ( ( iclcm >= 0 ) && ( iclcm < 10 ) )
				imed = list[iclcm];
			
			int iclch = Math.round(chc3) % 10;
			if ( ( iclch >= 0 ) && ( iclch < 10 ) )
				ihi = list[iclch];
		
			int[] tempArray = {ilow, imed, ihi};
			Arrays.sort(tempArray);
			
			/*Compute maximum value*/
			if (tempArray != null && tempArray.length == 3 ){
				int maxVal = tempArray[2];
				if ( maxVal == 9 ){
					maxVal = (  imed > ihi ? imed : ihi  );
					if ( maxVal < 3 )
						maxVal = 9;
				}
			     /*Invert maximum value to get numeric cloud coverage*/
//			     prctcc = invert[maxVal];
			     prctcc = invert[maxVal - 1];
			}
			}
			return prctcc;
		}
		
		/***
		 * Computes the CFRT from CLCT. CFRT is the WMO fractional cloud cover table
		 * @param clct - is the numberic total cloud cover
		 * @return the WMO fractional cloud cover
		 */
		public static float prCtcf(float clct){
			int[] itable = {0, 3, 6, 8, 9, 2, 5, 7, 0};
			if ( MissingValueTester.isDataValueMissing(clct) ){
				return GempakConstants.RMISSD;
			}
			
			/*Get fractional cloud cover WMO code*/
			if ( ( clct > 0 ) && ( clct <= 9 )   ){
				return itable [ (int) (clct - 1.)  ];
			}
			
			return GempakConstants.RMISSD;
		}
		
		/**
		 * Divides a floating point number by 100
		 * @param hvalue the value to divide
		 * @return the input value divided by 100, if hvalue is not missing
		 * and  RMISSD (-9999.0)  otherwise
		 */
		public static float prD100( float hvalue){
			return ( !MissingValueTester.isDataValueMissing(hvalue) ? hvalue/100 :  GempakConstants.RMISSD );
		}
		
		/**
		 * Computes the density of dry air given the pressure (in mb)
		 * and temperature (in Celsius)
		 * @param pres - Pressure in millibars
		 * @param tmpc - Temperature in Celsius
		 * @return Density of dry air in kg/(m**3)
		 */
		public static float prDden ( float pres, float tmpc ){
			
			float prdden = GempakConstants.RMISSD;
			/*Check for bad data*/
			if ( !MissingValueTester.isDataValueMissing(pres) 
					 && !MissingValueTester.isDataValueMissing(tmpc)
					 && tmpc >= -GempakConstants.TMCK){
                              /* Convert temperature and compute*/
				               float tmpk = prTmck( tmpc );
                              prdden = 100 * pres / ( GempakConstants.RDGAS * tmpk );
			 }
			return prdden;
		}
		
		/***
		 * Computes the dewpoint depression, from tmpx and dwpx, both of which
		 * must be in the same units (one of Kelvin, Celsius or Farenheit)
		 * @param tmpx - Air temperature
		 * @param dwpx - Dewpoint temperature
		 * @return the dewpoint depresssion (in the same units are tmpx and dwpx) 
		 *   if both tmpx and dwpx are valid values and RMISSD ( - 9999.0) otherwise.
		 */
		public static float prDdep ( float tmpx, float dwpx ){
		      return (  ( !MissingValueTester.isDataValueMissing(tmpx)
		    		          && !MissingValueTester.isDataValueMissing(dwpx) 
		    		          && dwpx <= tmpx)  ?  ( tmpx - dwpx )  : GempakConstants.RMISSD );
		}
		
		/**
		 *  Computes DMAX, the maximum temperature obtained by 
		 *  comparing the 6-hour maximum at 00Z, 
		 *  the 6-hour maximum at 06Z and 
		 *  the local midnight maximum (reported at 00 LST)
		 *  if either of the 6-hour values is missing, the maximum is set to missing.  
		 *  The inputs are in Celsius, the output in degrees Farenheit
		 * @param t00x - 6-hour maximum temperature at 00Z, Celsius
		 * @param t06x - 6-hour maximum temperature at 06Z, Celsius
		 * @param tdxc - Local midnight max temperature at 00 LST, Celsius
		 * @return The maximum of the 3 temperature values (in Farenheit) 
		 */
		public static float prDmax ( float t00x, float t06x, float tdxc){
			if ( MissingValueTester.isDataValueMissing(t00x)  
					|| MissingValueTester.isDataValueMissing(t06x)){
				return GempakConstants.RMISSD;
			}
			else if ( !MissingValueTester.isDataValueMissing(tdxc) ){
			      float[] tempArray = { t00x, t06x, tdxc };
			   	  Arrays.sort(tempArray);
			   	  return prTmcf ( tempArray[2] ); 	
			}else{
				       return ( t00x > t06x ? prTmcf (t00x) : prTmcf(t06x) );
			}
		}
		
		/**
		 * Converts the temperature from Celsius to Farenheit
		 * @param tmpc - the temperature in Celsius
		 * @return the temperature in Farenheit
		 */
		public static float prTmcf( float tmpc){
			return ( !MissingValueTester.isDataValueMissing( tmpc ) ?  ( tmpc * GempakConstants.RPRM ) + 32 : GempakConstants.RMISSD );
		}

		/**
		 * Converts the temperature from Farenheit to Celsius
		 * @param tmpc - the temperature in Farenheit
		 * @return the temperature in Celsius
		 */
		public static float prTmfc( float tmpf){
			return ( !MissingValueTester.isDataValueMissing( tmpf ) ?  ( tmpf - 32 ) / GempakConstants.RPRM  : GempakConstants.RMISSD );
		}		
		
		/**
		 * Converts the input temperature from Farenheit to Kelvin
		 * @param tmpf - The input temperature in Farenheit
		 * @return the equivalent temperature in Kelvin if tmpf is valid or RMISSD ( -9999.0) otherwise
		 */
		public static float prTmfk ( float tmpf) {
			if ( !MissingValueTester.isDataValueMissing(tmpf)){
				   float tmpc = prTmfc( tmpf );
				   return prTmck ( tmpc);
			}else{
				return GempakConstants.RMISSD;
			}
		}
		
		/**
		 * Converts the input temperature from Kelvin to Farenheit
		 * @param tmpk - the input temperature in Kelvin
		 * @return the equivalent temperature in Farenheit if tmpk is valid or RMISSD ( -9999.0) otherwise
		 */
		public static float prTmkf( float tmpk){
			return ( !MissingValueTester.isDataValueMissing(tmpk) 
					           ? ( prTmkc( tmpk )  * GempakConstants.RPRM ) + 32 :  GempakConstants.RMISSD );
		}
		
		/**
		 * Converts the input temperature from Celsius to Kelvin
		 * @param tmpc - temperature in Celsius
		 * @return the equivalent temperature in Kelvin if tmpc is valid and
		 * RMISSD (-9999.0) otherwise
		 */
		public static float prTmck( float tmpc){
			return  ( !MissingValueTester.isDataValueMissing(tmpc)  
					          ?  (tmpc + GempakConstants.TMCK) : GempakConstants.RMISSD  );
		}

		/**
		 * Converts the input temperature from Kelvin to Celsius
		 * @param tmpk - temperature in Kelvin
		 * @return the equivalent temperature in Celsius if tmpk is valid and
		 * RMISSD (-9999.0) otherwise
		 */		
		
       public static float prTmkc ( float tmpk ){
    	   return  ( !MissingValueTester.isDataValueMissing(tmpk)  
    			             ? ( tmpk - GempakConstants.TMCK) : GempakConstants.RMISSD  );
       }
		
       /**
        * <pre>
        * Computes the minimum temperature obtained by
        * comparing the 6-hour minimum at 12Z and the 6-hour minimum at 18Z.
        * if either of the 6-hour values is missing, the minimum is set to missing.
        * The inputs are in degrees C, the output in degrees F.
        *  </pre>
        * @param t12n - 6-hour minimum temperature at 12Z, deg Celsius
        * @param t18n - 6-hour minimum temperature at 18Z, deg Celsius
        * @return the minimum temperature (in Farenheit) after comparing the two input values, if they exist
        * and RMISSD (-9999.0) otherwise.
        */
		public static float prDmin ( float t12n, float t18n ){
			if ( !MissingValueTester.isDataValueMissing(t12n)  
					&& !MissingValueTester.isDataValueMissing(t18n)){
				float minValue = ( t12n < t18n ? t12n : t18n);
				return prTmcf( minValue );
			}
				return GempakConstants.RMISSD;
		}
		
		/**
		 * Computes the wind direction in degrees from the input u and v components of velocity,
		 * both of which must be in the same units (either meters/second or knots)
		 * @param uX - U component of velocity
		 * @param vX- V component of velocity
		 * @return The wind direction in degrees
		 */
		public static float prDrct ( float uX, float vX){
		           if ( !MissingValueTester.isDataValueMissing(uX) 
		        		   && !MissingValueTester.isDataValueMissing(vX)){
		        	   float prDrct = 0.0f;
//		        	         if ( ( uX != 0 ) &&  ( vX != 0 ) ){
		        	         if ( ( uX != 0f ) ||  ( vX != 0f ) ){
		        	        	 prDrct = (float) Math.atan2(-uX, -vX) * GempakConstants.RTD;
		        	        	 if ( prDrct <= 0 )
		        	        		 prDrct += 360;
		        	         }
		        	        	 return prDrct;
                    }
		        return GempakConstants.RMISSD;
		}
		
		/**
		 * Computes the dewpoint as the difference between the input temperature and dewpoint depression
		 * @param tmpx - temperature (in Celsius or Farenheit or Kelvin)
		 * @param dpdx - the dewpoint depression ( in the same units as the temperature)
		 * @return the dewpoint in the same units as tmpx and dpdx if both input parameters are valid and RMISSD ( - 9999.0)
		 * otherwise
		 */
		public static float prDwdp( float tmpx, float dpdx ){
			return ( ( !MissingValueTester.isDataValueMissing(tmpx) 
					    && !MissingValueTester.isDataValueMissing(dpdx)) ? tmpx - dpdx : GempakConstants.RMISSD  );
	    }
		
		/**
		 * Computes the dewpoint ( in Celsius ) from the mixing ratio (in grams/kilogram)
		 * and the pressure (in mb)
		 * @param rmix - the mixing ratio (in grams/kilogram)
		 * @param pres - the pressure (in mb) 
		 * @return the dewpoint (in Celsius), if both the input values are valid and RMISSD (-9999.0)
		 * otherwise
		 */
		public static float prDwpt(float rmix, float pres){
			float prDwpt = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(rmix)  
					&& !MissingValueTester.isDataValueMissing(pres)
					&&  ( rmix > 0)
					&& ( pres > 0)){
				    /*Convert gram/kilogram to gram/gram*/
				float ratio = rmix / 1000;
				 
				/*Calculate vapor pressure from mixing ratio and pressure*/
				float vaporPressure = ( pres * ratio ) / ( 0.62197f + ratio );
				
				/*Correct vapor pressure*/
				vaporPressure = vaporPressure / (1.001f + ((pres - 100.0f) / 900.0f) * .0034f);
				
				/*Calculate dewpoint*/
				prDwpt = ( float ) ( Math.log ( vaporPressure / 6.112 ) * 243.5 / ( 17.67 - Math.log( vaporPressure / 6.112 )));
			}
			return prDwpt;
		}
		
		/***
		 * Computes the Fosberg index from the temperature, relative humidity and wind speed
		 * at the surface. 
		 * @param tmpc - Temperature in Celsius 
		 * @param relh   - Relative humidity in percent
		 * @param sped  - Wind speed in meters/second
		 * @return the Fosberg index
		 */
		public static float prFosb(float tmpc, float relh, float sped){
			float prfosb = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(tmpc)
					&& !MissingValueTester.isDataValueMissing(relh)
					&& !MissingValueTester.isDataValueMissing(sped)){
				/*Change temperature to degrees Fahrenheit*/
				float tf   = prTmcf ( tmpc );
				
				/*Convert wind speed from meters/second to knots*/
				float sknt = prMskn ( sped );
	            
				/*Convert wind speed from knots to miles/hour*/
				float smph = prKnmh ( sknt );
				
	            float A = 0.03229f;
	            float B = 0.281073f;
	            float C = 0.000578f;
	            float  D = 2.22749f; 
	            float E = 0.160107f; 
	            float F = 0.014784f;
	            float  G = 21.0606f; 
	            float H = 0.005565f; 
	            float P = 0.00035f;
	            float  Q = 0.483199f; 
	            float R = 0.3002f; 
//	            float T = 9.0f/5.0f;
//	            float  U = 1.9425f;  
//	            float V = 0.868976f;
	            float  fw = GempakConstants.RMISSD;
	            if ( relh <= 10 )  {
                      fw = A + B * relh - C * relh * tf;
	            }
	            else if ( relh <= 50 )  {
                fw = D + E * relh - F * tf;
              }
                else{
                fw = G + H *relh*relh  - P * relh * tf - Q * relh; 
              }

	           float sss  = ( float )  ( Math.sqrt ( 1. + ( smph * smph ) ));
	           float  fwd  = fw / 30.0f;
	           float fwd2 = fwd * fwd;
	           float fwd3 = fwd2 * fwd;
	           float fire = 1 - 2 * fwd + 1.5f * fwd2 - 0.5f * fwd3;
				
				/*Find the Fosberg Index*/
	           
				prfosb = ( fire * sss ) / R;
	
			}
			return prfosb;
		}
		
		/**
		 * Computes the equivalent speed in knots from the input speed in meters/second
		 * using the equation:  SKNT  =  SPED * 1.9425  
		 * @param sped - speed in meters/second
		 * @return the speed in knots if sped is not missing and RMISSD (-9999.0) otherwise.
		 */
		public static float prMskn ( float sped ){
			return ( !MissingValueTester.isDataValueMissing(sped)
					           ? ( sped * GempakConstants.METERS_PER_SEC_TO_KNOTS) : GempakConstants.RMISSD );
		}

		/**
		 * Computes the equivalent speed in meters/second from the input speed in knots 
		 * using the equation:  SPED = SKNT / 1.9425
		 * @param sknt - speed in knots
		 * @return the speed in knots if sknt is not missing and RMISSD (-9999.0) otherwise.
		 */
		public static float prKnms ( float sknt ){
			return ( !MissingValueTester.isDataValueMissing(sknt)
					           ? ( sknt * GempakConstants.KNOTS_TO_METERS_PER_SEC ) : GempakConstants.RMISSD );
		}		
		
		
		/**
		 * Computes the equivalent speed in miles/hour from the input speed in knots
		 * using the equation:  SMPH = SKNT / 0.868976
		 * @param sknt - speed in knots
		 * @return the speed in miles/hour if sknt is not missing and RMISSD (-9999.0) otherwise.
		 */
		public static float prKnmh ( float sknt ){
			return ( !MissingValueTester.isDataValueMissing(sknt)
					           ? ( sknt * GempakConstants.KNOTS_TO_MILES_PER_HOUR) : GempakConstants.RMISSD );
		}

		/**
		 * Computes the equivalent speed in knots from the input speed in miles/hour
		 * using the equiation:  SKNT = SMPH * 0.868976
		 * @param smph - speed in miles/hour
		 * @return the speed in knots if smph is not missing and RMISSD (-9999.0) otherwise.
		 */
		public static float prMhkn ( float smph ){
			return ( !MissingValueTester.isDataValueMissing(smph)
					           ? ( smph * GempakConstants.MILES_PER_HOUR_TO_KNOTS) : GempakConstants.RMISSD );
		}
		
       /**
        * Determines  the height in meters which corresponds to a 
        * given code figure from WMO Code Table 1677.  If the given code 
        * figure is found to be invalid, then RMISSD is returned.        
        * @param hhString - the code figure from WMO table 1677
        * @return the Height in meters if the input string is parsed correctly and 
        *    if the integer code lies between the correct ranges. Otherwise it
        *    returns RMISSD (-9999.0) .
        *   
        */
		public static float prHcdm( String hhString ){
			hhString.trim();			
			float prhcdm = GempakConstants.RMISSD;
			if (hhString.length() != 2) return prhcdm;
			try{
				           float hh = Float.parseFloat(hhString.substring(0, 2));
				           int ihh = (int) hh;
                           if ( ( ihh >= 0 ) && ( ihh <= 50 ) )
                            	prhcdm = ihh * 30f;
                           else if (  (ihh >= 56) && (ihh <= 80)  ) // what about numbers from 51-56?
                            	prhcdm = ( ihh - 50f ) * 300f;
                           else if (  ( ihh >= 81 ) && ( ihh <= 89 )  )
                            	prhcdm = ( ( ihh - 80f ) * 1500f )  + 9000f ;
                            
                            /* (Non-Javadoc) Code figures 90 through 99 will also be mapped to
                             * RMISSD, since there is no clear way to set a single output
                             * value for the ranges represented by such code figures.
                             * */

		}catch ( NumberFormatException e ){
			System.out.println(e.getMessage());
		}
			return prhcdm;
		}
		
		/**
		 * <pre>
		 * Computes the Southern Region/CPC Rothfusz heat index.
		 * 
		 * The Rothfusz regression is optimal for TMPF > ~80 and RELH > ~40%.   
		 * This code applies a simple heat index formula and then resorts to   
		 * the Rothfusz regression only if the simple heat index exceeds 80,   
		 * implying temperatures near, but slightly below 80.  To make the    
		 * simple calculation continuous with the values obtained from the   
		 * Rothfusz regression, the simple result is averaged with TMPF in    
		 * computing the simple heat index value.                               
		 * Source:  NWS Southern Region SSD Technical Attachment SR 90-23  7/1/90.  
		 * Heat Index was originally known as the apparent temperature index 
		 * (Steadman, JAM, July, 1979).                                                                  
		 * This code includes adjustments made by the CPC for low RELH at high  
		 * TMPF and high RELH for TMPF in the mid 80's.
		 * 
		 * 
		 * @param tmpf  - the input air temperature
		 * @param relh   - the relative humidity 
		 * @return the heat index (in deg Farenheit) if both 
		 * the input air temperature and relative humidity
		 * are valid values and RMISSD (-9999.0) otherwise
		 * </pre> 
		 */
		public static float prHeat( float tmpf, float relh ){
			float prheat = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(tmpf) 
					&& !MissingValueTester.isDataValueMissing(relh)  ){
				     /*If the temperature is less than 40 degrees, set the heat index to the temperature*/
				       if ( tmpf <= 40.0f )
				    	   prheat = tmpf;
				       else{
				    	   /*
				    	    * Compute a simple heat index. If the value is less than 80 deg F
				    	    * use it
				    	    */
				    	         prheat = (float) (61 + ( tmpf - 68 ) * 1.2 + relh * 0.094);
                                 prheat = (float) ( ( tmpf + prheat ) * 0.5);
                                 /*Else compute the full regression value*/
                                 if ( prheat >= 80.0 ){
                                	float  t2 = tmpf * tmpf;
                                    float r2 = relh * relh;
                                    prheat =  ( float )  ( -42.379
                                                 +  2.04901523 * tmpf
                                                 + 10.14333127 * relh
                                                 -  0.22475541 * tmpf * relh
                                                 -  0.00683783 * t2
                                                 -  0.05481717 * r2
                                                 +  0.00122874 * t2 * relh
                                                 +  0.00085282 * tmpf *r2
                                                 -  0.00000199 * t2 * r2 );
                                   /*
                                    * Adjust for high regression at low relative humidity for temperatures above 80 degrees F.
                                    */
                                   if ( ( relh <= 13.0 ) &&  ( ( tmpf >=  80.0 ) &&( tmpf <= 112.0 ) ) )  {
                                	   float  adj1 =    ( float) ( ( 13. - relh ) / 4 );
                                	   float  adj2 =  ( float ) ( Math.sqrt ( ( 17 - Math.abs (tmpf - 95) ) / 17 ));
                                	   float adj  = adj1 * adj2;
                                	   prheat -= adj;
                                   }
                                   /*
                                    * Adjust for low regression at high relative humidity and temperatures in the mid-80s
                                    */
                                   else if (( relh > 85 ) &&  ( ( tmpf >=  80.0 ) &&( tmpf <= 87.0 ) )){
                                	   float  adj1 =    ( float) ( ( relh - 85.0 ) / 10.0 );
                                	   float  adj2 =  ( float ) (  ( 87.0 - tmpf ) / 5);
                                	   float adj  = adj1 * adj2;
                                	   prheat += adj;  
                                   }
                                 }
                             }
			}
			return prheat;
		}
		
		/***
		 * Computes the height (rounded off to the nearest foot) in feet for
		 * the input height in meters.  The equation used is:
		 * 		HGFT  =  math.round ( HGHT * 3.28084 )
		 * @param hght - the height in meters
		 * @return the height in feet
		 */
		public static float prHgmf(float hght){
			/*Sanity check*/			
			if( MissingValueTester.isDataValueMissing(hght) ){
				return GempakConstants.RMISSD;
			}
//			return ( (int) ( hght * GempakConstants.METERS_TO_FEET ) );
			return ( Math.round ( hght * GempakConstants.METERS_TO_FEET ) );
		}
		
		/**
		 * Computes the height in meters from the input height in feet using 
		 * the equation:  HGHT  =  HGFT * .3048
		 * @param hgft - the height in feet
		 * @return the equivalent height in meters if the input hgft is valid and RMISSD ( -9999.0 ) otherwise
		 */
		public static float prHgfm( float hgft ){
        		return ( !MissingValueTester.isDataValueMissing(hgft) 
        				?  ( hgft * GempakConstants.FEET_TO_METERS ) : GempakConstants.RMISSD  );
		}

		/**
		 * Computes the height in miles from the input height in feet using
		 * the equation:   HGML = HGFT / 5280.
		 * @param hgft - the height in feet
		 * @return the equivalent height in miles if the input hgft is valid and RMISSD ( -9999.0 ) otherwise
		 */		
		public static float prHgfs ( float hgft){
    		return ( !MissingValueTester.isDataValueMissing(hgft) 
    				?  ( hgft * GempakConstants.FEET_TO_MILES ) : GempakConstants.RMISSD  );			
		}

		/**
		 * Computes the height in feet from the input height in miles using the 
		 * equation:   HGFT = HGML * 5280
		 * @param hgml - the height in miles
		 * @return the equivalent height in miles if the input hgml is valid and RMISSD ( -9999.0 ) otherwise
		 */		
		public static float prHgsf ( float hgml){
    		return ( !MissingValueTester.isDataValueMissing(hgml) 
    				?  ( hgml * GempakConstants.MILES_TO_FEET ) : GempakConstants.RMISSD  );			
		}		
		
		/**
		 * Computes the height in kilometers, from the input height in meters
		 * @param value - height in meters
		 * @return the equivalent height in kilometers if the input value is valid and RMISSD ( - 9999.0) 
		 *    otherwise.
		 */
		public static float prHgmk(float value){
			return ( ( value != GempakConstants.RMISSD ) 
					         ? ( value * GempakConstants.METERS_TO_KILOMETERS ) 
					        		 : GempakConstants.RMISSD );
		}
		
		
		/**
		 * Computes the height in meters, from the input height in kilometers
		 * @param value - height in kilometers
		 * @return the equivalent height in meters if the input value is valid and RMISSD ( - 9999.0) 
		 *    otherwise.
		 */
		public static float prHgkm(float value){
			return ( ( value != GempakConstants.RMISSD ) 
					         ? ( value * GempakConstants.KILOMETERS_TO_METERS ) 
					        		 : GempakConstants.RMISSD );
		}		
		
		/**
		 * Computes the height in decameters, from the input height in meters
		 * @param hght - height in meters
		 * @return the equivalent height in decameters if the input value is valid and RMISSD ( - 9999.0) 
		 *    otherwise.
		 */
		public static float prHgmd(float hght){
			return ( ( hght != GempakConstants.RMISSD ) 
					         ? ( hght * GempakConstants.METERS_TO_DECAMETERS ) 
					        		 : GempakConstants.RMISSD );
		}	
		
		
		/**
		 * Computes the height in meters, from the input height in nautical miles using
		 * the equation:   HGTM  =  1852. * HGTN 
		 * @param hgtn - height in nautical miles
		 * @return the equivalent height in meters if the input value is valid and RMISSD ( - 9999.0) 
		 *    otherwise.
		 */
		public static float prHgnm(float hgtn){
			return ( ( hgtn != GempakConstants.RMISSD ) 
					         ? ( hgtn * GempakConstants.NAUTICAL_MILES_TO_METERS ) 
					        		 : GempakConstants.RMISSD );
		}	
		
		/**
		 * Computes the humiture index from the air temperature and the dew point 
		 * temperature using the equation:   PR_HMTR = TMPF + ( PR_VAPR ( DWPC ) - 21 )
		 * @param tmpf - the air temperature (in Farenheit)
		 * @param dwpf - the dew point (in Farenheit)
		 * @return the humiture index if both the air temperature and the dewpoint temperature are
		 * valid values and RMISSD ( -9999.0) otherwise
		 */
		public static float prHmtr( float tmpf, float dwpf){
			float prhmtr = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(tmpf)
					&& ( !MissingValueTester.isDataValueMissing(dwpf))){
				float dwpc = prTmfc(dwpf);
				prhmtr = tmpf + ( prVapr( dwpc ) - 21 );
			}
			return prhmtr;
		}
		
		/**
		 * Computes the vapor pressure ( in mb) from the input
		 * dewpoint temperature in Celsius using the equation:
		 * 		VAPR = 6.112 * EXP [ (17.67 * DWPC) / (DWPC + 243.5) ]
		 * @param dwpc - the dewpoint temperature ( in Celsius )
		 * @return the vapor pressure ( in mb) from the dewpoint temperature if it is valid
		 * and RMISSD ( - 9999.0) otherwise
		 */
		public static float prVapr ( float dwpc){
			if ( !MissingValueTester.isDataValueMissing(dwpc)  
					&& ( dwpc >= -240.0f) ){
				return ( (float)  ( 6.112 * ( Math.exp ( ( 17.67 * dwpc )  / ( dwpc + 243.5) ) ) ) );
			}		
			return GempakConstants.RMISSD;
		}
		
		/**
		 * Computes the rate of ice accretion/growth of ice 
		 * on a vessel in salt water, in units of inches per 3 hours (the WMO standard)
		 * 		The formula used is
		 * 			IGRO = ( A*pr + B*pr*pr + C*pr*pr*pr ) * CVFAC
		 * 		  where
		 * 			A  = 2.73 * 10e-2
		 * 			B  = 2.91 * 10e-4
		 * 			C  = 1.84 * 10e-6
		 * 			pr = ( sped * ( -1.7 - tmpc ) ) / ( 1 + 0.4 * ( sstc + 1.7 ) )
		 * 					(priesendorfer regression)
		 * 		  and
		 * 			CVFAC = 1.1811, to convert cm/hr to in/3hr. 
		 * 			
		 * @param tmpc - the observed surface air temperature in Celsius
		 * @param sstc   - the observed surface sea temperature in Celsius
		 * @param sped  - the observed wind speed
		 * @return the rate of ice growth if all the input values are valid and lie between specific limits
		 *   and if the rate of ice growth that is computed is greater than or equal to 0,
		 *   or RMISSD (-9999.0)
		 */
		public static float prIgro ( float tmpc, float sstc, float sped){
			float prigro = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(tmpc)
					&& !MissingValueTester.isDataValueMissing(sstc)
					&& !MissingValueTester.isDataValueMissing(sped)){
				        
				         /* Check that these values are within the valid range */
				         if (  ( sped >= 0   && sped <= 50)
				        		 && ( tmpc >= -20  && tmpc <= 0  )
				        		 && ( sstc >= -1.7f  && sstc <= 12 )){
				        	 
				        	 float A = 0.0273f;
				        	 float B = 0.000291f;
				        	 float C = 0.00000184f;
				        	 float cvfac = 1.1811f; // to convert cm/hr to in per 3 hours
				        	 float pr =  (float) (( sped * ( -1.7 - tmpc ) ) / ( 1 + 0.4 * ( sstc + 1.7 ) ));  // Compute the Priesendorfer regression
				        	 float pr2 = pr * pr;
				        	 prigro = (float) ( A * pr + B * pr2 + C * pr * pr2 ) * cvfac;
				        	 if(prigro < 0)
				        		 prigro = GempakConstants.RMISSD;
				         }				
			}
			return prigro;
		}
		
		/**
		 * Converts the input value (in inches) to millimeters using the equation:
		 * 		 INMM = XINCH * 25.4
		 * @param xinch - input value in inches
		 * @return the equivalent value in millimeters if the input value is not missing
		 * and RMISSD otherwise.
		 */
		public static float prInmm( float xinch){
			return ( !MissingValueTester.isDataValueMissing(xinch) 
					          ? ( xinch * GempakConstants.INCHES_TO_MILLIMETERS) : GempakConstants.RMISSD );
		}
		
		/**
		 * Converts the input height in millimeters to inches using the equation:
		 * 		MMIN  =  .0393701 * XMILM
		 * @param xmilm - the input height in millimeters
		 * @return the equivalent height in inches if the input height is valid or 
		 *  RMISSD otherwise.
		 */
		public static float prMmin( float xmilm){
			return ( !MissingValueTester.isDataValueMissing(xmilm) 
			          ? ( xmilm * GempakConstants.MILLIMETERS_TO_INCHES) : GempakConstants.RMISSD );
		}
		
		/**
		 * Computes the mountain obscuration threshold met indicator 
		 * @param cmsl - Ceiling converted to MSL in 100's of ft
		 * @param otval - Mountain obscuration threshold in 100's of ft
		 * @return The mountain obscuration threshold met indicator if
		 * the input values are valid or RMISSD (-9999) otherwise 
		 */
		public static float prMobs ( float cmsl, float otval){
			if  ( !MissingValueTester.isDataValueMissing(cmsl)
					&& !MissingValueTester.isDataValueMissing(otval)){
				return (  (cmsl < otval) ? 1.0f : 0.0f   );
			}
			return GempakConstants.RMISSD;
		}
		
		/**
		 * Computes the actual latitude given the range, azimuth and station latitude, longitude and elevation.
		 * Equations developed for use in the AOIPS radar package are used.
		 * @param slat - the station latitude
		 * @param slon - the station longitude
		 * @param range - the range in kilometers
		 * @param azim - the geographic azimuth in radians
		 * @param selv - the station elevation
		 * @return the actual latitude if all the input parameters are valid and RMISSD ( -9999.0)
		 * otherwise
		 */
		public static float prLati ( float slat, float slon, float range, float azim, float selv ){
			getRZLLInstance().prRzll ( slat, slon, range, azim, selv);
			return getRZLLInstance().getXlat();
		}

		/**
		 * Computes the actual longitude given the range, azimuth and station latitude, longitude and elevation.
		 * Equations developed for use in the AOIPS radar package are used.
		 * @param slat - the station latitude
		 * @param slon - the station longitude
		 * @param range - the range in kilometers
		 * @param azim - the geographic azimuth in radians
		 * @param selv - the station elevation
		 * @return the actual longitude if all the input parameters are valid and RMISSD ( -9999.0)
		 * otherwise
		 */
		public static float prLoni ( float slat, float slon, float range, float azim, float selv ){
			getRZLLInstance().prRzll ( slat, slon, range, azim, selv);
			return getRZLLInstance().getXlon();
		}
		
		/**
		 * Computes the latent heat of vaporization at constant pressure
		 * from the input temperature (in Celsius) using the equation:
		 * 		LHVP = ( 2.500 - .00237 * TMPC ) * 10E6
		 * LHVP is in J/kg.
		 * @param tmpc - the input temperature (in Celsius)
		 * @return the  latent heat of vaporization at constant pressure if
		 *  the input temperature is valid and RMISSD ( -9999.0)
		 * otherwise
		 */
		public static float prLhvp( float tmpc){
			return ( !MissingValueTester.isDataValueMissing(tmpc)
					?   ( float) (  (2.500 - 0.00237 * tmpc) * 1000000)
							: GempakConstants.RMISSD);
		}
		
		/**
		 * Computes the temperature of a parcel lifted (or sunk) 
		 * adiabatically to a given pressure.
		 * @param thta  - Potential temperature in Kelvin 
		 * @param thte  - Equivalent potential temp in Kelvin
		 * @param pres  - Lifted pressure
		 * @return the lifted temperature in Celsius, if all the input parameters are  valid or
		 * RMISSD (-9999.0) otherwise
		 */
		public static float prLtmp ( float thta, float thte, float pres ){
			float prltmp = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(thta) 
					&& !MissingValueTester.isDataValueMissing(thte)
					&& !MissingValueTester.isDataValueMissing(pres)){
				
				/*Compute parcel temperatures on moist and dry adiabats*/
				 float tmpe = prTmst ( thte, pres, 0.0f);
				 float tmpd = prTmpk ( pres, thta);
				if ( !MissingValueTester.isDataValueMissing( tmpe ) 
						&& !MissingValueTester.isDataValueMissing( tmpd ) ){
					
					/*
					 * ( Non-Javadoc )
					 * The correct parcel temperature is the warmer of the
					 *  temperature on the dry adiabat and the temperature on the
					 *  moist adiabat.
					 */
					prltmp =  (  tmpe > tmpd ? prTmkc ( tmpe ) : prTmkc ( tmpd ) );
				}
			}
			return prltmp;
		}

		/**
		 * Multiplies the input value by 100
		 * @param value - the value to be multiplied
		 * @return the value multiplied by 100 if it is not missing or RMISSD ( -9999.0) otherwise
		 */
		public static float prM100 ( float value ){
                 return  (  !MissingValueTester.isDataValueMissing(value) ? ( value * 100.0f) : GempakConstants.RMISSD  ); 
		}
		
		/**
		 * <pre>
		 * Computes the moist hydrostatic height at pressure pt from a lower 
		 * height and pressure, and the scale height in the layer.
		 * The moist hydrostatic height is computed as an integrated quantity.
		 * Thus, the lower height should have been integrated from the surface.
		 * The equation used is:   PR_MHGT  =  HB + SCALE * ALOG ( PB / PT )
		 * @param hb          - Bottom height
		 * @param pb          - Bottom pressure 
		 * @param pt           - Top pressure 
		 * @param scale       - Scale height
		 * @return The moist hydrostatic height if all the input values are valid and if the 
		 * pressure and temperature are greater than 0. Or RMISSD (-9999.0) otherwise.
		 * </pre>
		 */
		public static float prMhgt ( float hb, float pb, float pt, float scale){
			    if ( !MissingValueTester.isDataValueMissing(hb) 
			    		&& !MissingValueTester.isDataValueMissing(pb)
			    		&& !MissingValueTester.isDataValueMissing(pt)
			    		&& !MissingValueTester.isDataValueMissing(scale)
			    		&& ( pt > 0) && ( pb > 0) ){
			    	return (  ( float ) ( hb + scale * Math.log ( pb / pt ) ) );
			    }
			    
			    return GempakConstants.RMISSD;
		}
		
		/**
		 * Extracts the pressure change ( in millibars ) from 
		 * the pressure tendency information
		 * @param p03d - Pressure tendency information 
		 * @return the pressure change ( in mb )
		 */
		public static float prP03c( float p03d){
			float prp03c = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(p03d)){
				float[] psign = {1, 1, 1, 1, 0, -1, -1, -1, -1}; 
				   int itendc   = ( int ) ( p03d / 1000);
//				   float ptend = ( float ) (  ( ( ( int ) p03d )  %  1000   )   / 10 );
				   float ptend = ( float ) ( ( ( int ) p03d )  %  1000 )   / 10f;
				//  TODO: compare tests with legacy
				   if ( itendc < psign.length )
				       prp03c       = psign[itendc] * ptend;
			}
			return prp03c;
		}
		
		/**
		 * Translates a WMO pressure tendency string into the pressure tendency 
		 * code * 1000 + the magnitude of the change in tenths of millibars.
		 * @param ctend - the WMO pressure tendency string
		 * @return the pressure change information 
		 */
		public static float prP03d( String ctend ){
			if (ctend != null && !ctend.isEmpty()) {
//				String cc = ctend.substring(0, 4);
				/* 88888888888888888888888888888888 */
				String cc = ctend.substring(0);
				if (cc.length() >= 4) {
					cc = ctend.substring(0, 4);
				}
				/* 888 */
//				if ( cc.charAt( 0 ) > '0' && cc.charAt( 0 ) > '8' && ( cc.compareTo( "999" ) != 0 ) ) {
				if ( cc.charAt( 0 ) >= '0' && cc.charAt( 0 ) <='8' && ( cc.compareTo( "999" ) != 0 ) ) {
//					return Float.parseFloat( cc );
					/* 8888888888888888888888888888888888 */
					float p03d = GempakConstants.RMISSD;
					try {
					p03d =Float.parseFloat( cc );
					} catch (Exception e) {
						p03d = GempakConstants.RMISSD;
					}
					return p03d;
					/* 888 */
				}
			}
			return GempakConstants.RMISSD;
		}
	
		/**
		 * Computes station pressure from altimeter and station elevation using
		 * the equation
		 * 		PALT = ALTM * ( 1 - ( SELK * GAMUSD / To ) ) ** expo
		 * 		   where
		 * 			SELK  =  SELV / 1000 
		 * 			To  =  US Std. Atmos. sea level temp in Kelvin
		 * 				=  TMCK + 15
		 * 			expo  =  GRAVTY / ( GAMUSD * RDGAS ) * 1000 
		 * 	Wallace and Hobbs.
		 * 
		 * @param altm - Altimeter in millibars 
		 * @param selv - Station elevation in meters 
		 * @return the pressure in millibars if nont of the input values are missing and 
		 * RMISSD ( otherwise )
		 */
		public static float prPalt ( float altm, float selv){
			
			float prpalt = GempakConstants.RMISSD;
			
			if ( !MissingValueTester.isDataValueMissing(altm)
					&& !MissingValueTester.isDataValueMissing(selv)){
				            
				             float hgtk = prHgmk ( selv );
				            
				             /*Calculate the exponent*/
				             float expo = ( GempakConstants.GRAVTY / ( GempakConstants.GAMUSD * GempakConstants.RDGAS ) * 1000.0f);
				             
				             /*Calculate pressure*/
				             prpalt = ( float ) ( altm * Math.pow(   (  1 - ( hgtk * GempakConstants.GAMUSD/ ( GempakConstants.TMCK + 15 )   )  )  , expo ) );
			}
			return prpalt;
		}
		
		/**
		 * Computes the equivalent potential temperature ( in Kelvin ) from 
		 * the pressure ( in mb ), the temperature ( in Celsius ) and the dewpoint ( in Celsius )
		 * using the equation:
		 * 		 THTE = THTAM * EXP [ ( 3.376/TLCL - .00254 ) *  
		 * 								( MIXR * ( 1 + .81*.001*MIXR ) ) ] 
		 * 			where
		 * 				 THTAM = potential temperature of moist air 
		 * 				 	   = TMPK * (1000 / PRES) ** E
		 * 					 E = RKAPPA * ( 1 - ( .28 * .001 * MIXR ) ) 
		 * Bolton. 
		 * 
		 * @param pres   - the pressure ( in mb )
		 * @param tmpc  - the temperature ( in Celsius )
		 * @param dwpc  - the dewpoint ( in Celsius )
		 * @return the the equivalent potential temperature ( in Kelvin ), if all the input values 
		 * are valid or RMISSD (-9999.0) otherwise
		 */
		public static float prThte ( float pres, float tmpc, float dwpc   ){
			float prthte = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(pres)  
					&& !MissingValueTester.isDataValueMissing(tmpc)
					&& !MissingValueTester.isDataValueMissing(dwpc)
					&& ( pres > 0) ){
				             
				             /*Find mixing ratio*/
				             float rmix = prMixr ( dwpc, pres);
				             
				             if ( !MissingValueTester.isDataValueMissing(rmix)){
					              /*Change degrees Celsius to Kelvin*/
				            	 float tmpk = prTmck ( tmpc );
				            	 
				            	 /*Calculate  theta for moist air (thtam) */
				                float e = ( float) ( GempakConstants.RKAPPA * (1 - (0.28 * 0.001 * rmix) ));	 
				                float thtam = ( float ) ( tmpk * Math.pow(1000 / pres, e));
				                
				                /*Find the temperature at the lifted condensation level */
				                float tlcl = prTlcl ( tmpc, dwpc );
//				                e =  ( float ) ( ( ( 3.376  / tlcl ) - 0.00254 ) * ( rmix * ( 1 + 0.81 * 0.001 * rmix ) ) );
				                e =  ( ( 3.376f  / tlcl ) - 0.00254f ) * ( rmix * ( 1 + 0.81f * 0.001f * rmix ));
				                prthte =  ( float ) ( thtam * Math.exp(e));
				            }
			}
			return prthte;
		}
		
		/**
		 * Computes the temperature at the lifted condensation level for a parcel of air
		 * given the temperature ( in Celsius ) and the dewpoint (in Celsius) using the
		 * equation:
		 * 		TLCL = [ 1 / ( 1 / (DWPK-56) + ALOG (TMPK/DWPK) / 800 ) ] + 56
		 * Boton.
		 * @param tmpc - the temperature ( in Celsius )
		 * @param dwpc - the dewpoint ( in Celsius )
		 * @return the lifted condensation level temperature In Kelvin, if both input values are valid 
		 * and RMISSD ( -9999.0) otherwise
		 */
		public static float prTlcl ( float tmpc, float dwpc ){
			float prtlcl = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(tmpc)  
					&& !MissingValueTester.isDataValueMissing(dwpc)
					&& tmpc >= -GempakConstants.TMCK
					&& dwpc >= -GempakConstants.TMCK){
          				float tmpk = prTmck ( tmpc );
			        	float dwpk = prTmck ( dwpc );
			        	prtlcl =  ( float ) ( ( 800 * ( dwpk - 56 ) / ( 800 + ( dwpk - 56 ) * Math.log ( tmpk / dwpk ) ) ) + 56 );
   		      
			}
			return prtlcl;
		}
		
		
		/**
		 * Computes the temperature ( in Kelvin ) from the pressure ( in mb ) and 
		 * the potential temperature ( in Kelvin ) using the Poisson equation:
		 * 		TMPK = THTA * ( PRES / 1000 ) ** RKAPPA
		 *  
		 * @param pres - the pressure ( in mb )
		 * @param thta - the potential temperature ( in Kelvin )
		 * @return the temperature ( in Kelvin )
		 */
		public static float prTmpk ( float pres, float thta ){
			float prtmpk = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(pres) 
					&& !MissingValueTester.isDataValueMissing(thta) 
					&& ( pres >= 0 ) ){
				           prtmpk = ( float ) (  thta * (  Math.pow( pres / 1000f, GempakConstants.RKAPPA )  )  );
			}
				
			return prtmpk;
		}
		
		/**
		 * <pre>
		 * Computes the parcel temperature ( in Kelvin ) from the equivalent potential temp ( in Kelvin ),
		 * pressure ( in millibars ) and the first guess temperature ( in Kelvin ). 
		 * The parcel temperature at level pres on a specified moist adiabat ( thte ). 
		 * The computation is an iterative Newton-Raphson technique of the form:
		 * <code>
		 * x = x(guess) + [ f( x ) - f( x(guess) ) ] / f'( x(guess) )
		 * f' is approximated with finite differences
		 * f' = [ f( x(guess) + 1 ) - f( x(guess) ) ] / 1
		 * </code>
		 * If tguess is 0, a reasonable first guess will be made.
		 * Convergence is not guaranteed for extreme input values.  If the
		 * computation does not converge after 100 iterations, the missing 
		 * data value will be returned. 
		 * @param thte      - Equivalent potential temp ( in Kelvin )
		 * @param pres      - Pressure ( in millibars )
		 * @param tguess   - First guess temperature ( in Kelvin )
		 * @return the Parcel temperature in Kelvin if all the input values are valid 
		 * (without being extreme) and if a convergence is obtained within 100 iterations 
		 * or RMISSD (-9999.0) otherwise.
		 * </pre>
		 */
		public static float prTmst ( float thte, float pres, float tguess){
			float prtmst = GempakConstants.RMISSD;
			
			if ( !MissingValueTester.isDataValueMissing(thte)
					&& !MissingValueTester.isDataValueMissing(pres)
					&& !MissingValueTester.isDataValueMissing(tguess) 
					&&  ( thte > 0 ) 
					&&  ( pres > 0 )
					&&  ( tguess >= 0 ) ){
				
				  float tg = tguess;
				  /*
				   * If tguess is passed as 0. it is computed from an MIT scheme
				   */
				  if ( tg == 0 ){
					                    float diffVar = thte - 270; 
					                    float mathFormula1 =   (float)  ( diffVar  > 0   ? diffVar  : 0.0  );
					                    tg =  (float) ( (  thte - 5.0f * ( Math.pow ( mathFormula1, 1.05f)     )   ) * (  Math.pow ( pres / 1000.0f, 0.2f )  ) );
					  }
				
				  /*Set convergence and initial guess in degrees Celsius*/
				  float epsi = 0.01f;
				  float tgnu = prTmkc ( tg);
				  
				  /*
				   * Set a limit of 100 iterations.  Compute tenu,tenup, the
				   * thte's at one degree above the guess temperature.
				   */
				  int index = 0;
				  while( index < 100 ){
					  float tgnup = tgnu + 1;
					  float tenu  = prThte ( pres, tgnu, tgnu );
					  float tenup = prThte ( pres, tgnup, tgnup);
					  /*Check that the THTE's exist.*/
					  if ( !MissingValueTester.isDataValueMissing(tenup) 
							  && !MissingValueTester.isDataValueMissing(tenu)){
						  
						  /*Compute the correction*/
						  float cor =  ( thte - tenu ) / ( tenup - tenu );
						  tgnu += cor;
						  
						  if ( ( cor < epsi )  && (-cor < epsi) ){
							  
							  /*return on convergence*/
							  prtmst = prTmck ( tgnu);
	                          break;					  
						  }
					  }
                      index++;
				  }
			}
			return prtmst; 
		}
		/**
		 * Computes the mixing ratio in grams/kilograms from the dewpoint ( in Celsius )
		 * and the pressure ( in mb) using the equation:
		 * 		MIXR = .62197 * ( e / ( PRES - e ) ) * 1000. 
		 * 			where
		 * 				e    =  VAPR * corr
		 * 				corr =  (1.001 + ( ( PRES - 100. ) / 900. ) * .0034)
		 * ( University of Wisconsin green sheet ).
		 * This method can also be used for the folloiwng computations:
		 * 		MIXS from TMPC and PRES
		 * 		SMXR from DWPC and PALT
		 * 		SMXS from TMPC and PALT
		 * 
		 * @param dwpc - the dewpoint ( in Celsius )
		 * @param pres   - the pressure ( in mb)
		 * @return the mising ratio ( in grams / kilograms ) if both the input
		 * parameters are valid and RMISSD (-9999.0) otherwise.
		 */
		public static float prMixr ( float dwpc, float pres ){
			float prmixr = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(dwpc) 
					&& !MissingValueTester.isDataValueMissing(pres)){
				     /*Calculate vapor pressure*/
				float vapr = prVapr ( dwpc );
				if ( !MissingValueTester.isDataValueMissing(vapr)){
					/*
					 * (Non-Javadoc)
					 * corr is a correction to the vapor pressure since the atmosphere is not an ideal gas.
				     */
					float corr =   ( float )  (1.001 + ( ( pres - 100 ) / 900 ) * 0.0034 );
                    float e = corr * vapr;
                           
                    /*
                     * Test for unphysical case of large E at low PRES
                     */
                    if ( e <=  ( 0.5 * pres ) ){
                    	  /*Calculate mixing ratio */
                          prmixr = ( float )  ( 0.62197 * ( e / ( pres - e ) ) * 1000 );	
                    }
				}
			}
			return prmixr;
		}
		
		/**
		 * Returns PMSL if available, otherwise will return ALTM if available.
		 * The purpose of this method is to provide greater  station availability
		 * for surface analysis.
		 * @param altm - Altimeter ( in mb )
		 * @param pmsl - Sea-level pressure ( in mb )
		 * @return Returns pmsl if available, otherwise returns altm. 
		 *    If neither are available it returns RMISSD ( - 9999.0)
		 */
		public static float prPany ( float altm, float pmsl){
			if ( !MissingValueTester.isDataValueMissing ( pmsl ) )
				        return pmsl;
			else if ( !MissingValueTester.isDataValueMissing ( altm ) )
		        return altm;
			else
				return GempakConstants.RMISSD;
		}
		
		/**
		 * Computes the wind direction in degrees from packed speed and direction
		 * given as input using the equation:
		 * 		DRCT = 5. *  INT ( PSPD / 500. ) 
		 * 
		 * @param pspd - packed speed and direction. It is in the form DDFFF
         * where DD is the wind direction in tens of degrees, and FFF is
         * either the wind speed or wind speed plus 500, depending on the
         * units digit of direction rounded to the nearest 5 degrees 
		 * @return Wind direction in degrees
		 */
		public static float prPkdd ( float pspd){
			float prpkdd = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(pspd) ){
				/*
				 * ( Non-Javadoc ) 
				 * The factor of 5 accounts for encoding the units digit of
                 * direction (to the nearest 5 degrees) in the hundreds digit of
                 * speed.
                 */
				prpkdd =  ( float) ( 5 * (int) ( pspd / 500 ) );
				if ( prpkdd >= 360)
					  prpkdd -= 360;
			}
			return prpkdd;
		}
		
		/**
		 * Computes the wind speed ( in knots ) given the packed speed and direction
		 * using the equation:
		 * 		SPED = MOD ( INT (PSPD) , 500 )
		 * @param pspd - The packed speed and direction. It is in the form DDFFF
         * where DD is the wind direction in tens of degrees, and FFF is
         * either the wind speed or wind speed plus 500, depending on the
         * units digit of direction rounded to the nearest 5 degrees 
		 * @return The wind speed ( in knots ) 
		 */
		public static float prPkss ( float pspd){
			float prpkss = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(pspd) ){
				  // Formally, SPED is MOD ( PSPD, 1000 ), with 500 subtracted off if
				  // needed, but 1000 is a multiple of 500, so it is done in 1 step.
			      prpkss = ( ( int ) pspd ) % 500 ;	
			}
			return prpkss;
		}
		
		/**
		 * Computes the lifted condensation level pressure ( in mb ) for a parcel of air
		 * from TMPC, PRES, adn TLCL.  TLCL may be computed using PR_TLCL.  The equation
		 * used is a modified Poisson equation:
		 * 		PLCL = PRES * ( TLCL / TMPK ) ** ( 1 / RKAPPA ) 
		 * @param tmpc - Temperature ( in Celsius ) before lifting the air parcel
		 * @param pres  - Pressure ( in mb )  before lifting the air parcel
		 * @param tlcl   - Temperature ( in Kelvin ) at the lifted condensation level 
		 * @return the pressure at the lifted condensation level, if all the inputs are valid and RMISSD ( -9999) otherwise
		 */
		public static float prPlcl ( float tmpc, float pres, float tlcl){
			float prplcl = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing( tmpc ) 
				&&	!MissingValueTester.isDataValueMissing( pres )	
//				&&	!MissingValueTester.isDataValueMissing( tmpc ) ){
				&&	!MissingValueTester.isDataValueMissing( tlcl ) ){
				float tmpk = prTmck ( tmpc);
				prplcl =   ( float )    (  pres *  Math.pow( ( tlcl / tmpk ) , ( 1 / GempakConstants.RKAPPA ) ) ); 
			}
			return prplcl;
		}
		
		/**
		 * <pre>
		 *  Computes the mean sea level pressure ( in mb ) from the station pressure ( in mb ),
		 *  the temperature ( in deg Celsius), the dewpoint ( in deg Celsius ) and 
		 *  the station elevation ( in meters ) using the equation:
		 *  	PMSL = PRES * EXP ( ( GRAVTY * SELV ) / ( RDGAS * TVAVE ) ) 
		 *     where
		 *     		 TVAVE = avg virtual temp between station and sea level 
		 *     		        = TVRK + ( DELTV / 2 )
		 *     		 DELTV = GAMUSD * SELV / 1000
		 *  Wallace and Hobbs.
		 * @param pres - the station pressure ( in mb )
		 * @param tmpc - the temperature ( in deg Celsius)
		 * @param dwpc - the dewpoint ( in deg Celsius )
		 * @param selv - the station elevation ( in meters )
		 * @return the mean sea level pressure ( in mb ) if all the inputs are valid and RMISSD ( -9999) otherwise
		 * 
		 * 
		 * </pre>
		 */
		public static float prPmsl ( float pres, float tmpc, float dwpc, float selv ){
			float prpmsl = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing( pres ) 
					&& !MissingValueTester.isDataValueMissing( tmpc ) 
					&& !MissingValueTester.isDataValueMissing( dwpc )
					&& !MissingValueTester.isDataValueMissing( selv ) ){
				      /*Calculate virtual temperature*/
				      float tv = prTvrk ( tmpc, dwpc, pres);
				      /* 8888888888888 */
				      System.out.println(" tv=" + tv);
				      /* 888 */
				      
				      /*deltaV and tVave*/
				      float deltaV = selv * GempakConstants.GAMUSD / 1000;
//				      float tVave  = tv + ( deltaV + 2);
				      float tVave  = tv + ( deltaV / 2);
				      float mathFormula = ( GempakConstants.GRAVTY * selv ) / ( GempakConstants.RDGAS * tVave );
				      /* 8888888888888 */
				      System.out.println(" selv=" + selv + " tVave=" + tVave + " mathForm=" + mathFormula);
				      /* 888 */ 
//				      prpmsl =  ( float) ( Math.exp(mathFormula) );
				      prpmsl =  (float) ( pres * Math.exp(mathFormula));
			}
			return prpmsl;
		}
		
		/**
		 * Computes the parcel pressure from THTE the equivalent potential temperature ( in Kelvin ) and
		 * and TMPC the parcel temperature  at PRES on a specified moist adiabat (THTE). The computation is an iterative Newton-Raphson 
		 * technique of the form:
		 *  	 x = x(guess) + [ f( x ) - f( x(guess) ) ] / f'( x(guess) )
		 *  
		 *  		f' is approximated with finite differences 
		 *  		f' = [ f( x(guess) + 1 ) - f( x(guess) ) ] / 1
		 * Convergence is not guaranteed for extreme input values.  If the
		 * computation does not converge after 100 iterations, the missing 
		 * data value will be returned.
		 * 
		 * @param thte   - Equivalent potential temperature ( in Kelvin )
		 * @param tmpk - Parcel temperature ( in Kelvin )
		 * @return the pressure ( in mb )
		 */
		public static float prPmst ( float thte, float tmpk ){
			float prpmst = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing( thte )
					&& !MissingValueTester.isDataValueMissing( tmpk )
					&& ( thte > 0 ) ){
				/*Set convergence and initial guess of pressure*/
				float epsi = 0.01f;
				float tmpc = prTmkc ( tmpk );
				float pgdn = ( float ) ( 1000 * Math.pow (  (  tmpk / thte ), GempakConstants.AKAPPA ) ); 
				/*
				 * (Non-Javadoc)
				 * Set a limit of 100 iterations.  Compute tedn and teup, the
				 * thte's at one degree above the guess temperature
				 */
				for ( int index = 0; index < 100 ; index++){
					float pgup = pgdn + 1;
					float tedn = prThte ( pgdn, tmpc, tmpc );
					float teup = prThte ( pgup, tmpc, tmpc );

					/*Check thte*/
					if ( MissingValueTester.isDataValueMissing(tedn) || MissingValueTester.isDataValueMissing(teup))
						break;
					else {
						/*Compute the correction; return on convergence*/
						float cor = ( thte - tedn ) / ( teup - tedn );
						pgdn = pgdn + cor;
						if ( Math.abs(cor) < epsi ){
							prpmst = pgdn;
							break;
						}		            	     	
					}
				}				   
			}
			return prpmst;
		}
		
		/**
		 * Computes PR24, the 24-hour precipitation calculated by 
		 * summing four 6-hour precipitation values
		 * @param p01 - First 6-hour precipitation amount 
		 * @param p02 - Second 6-hour precipitation amount
		 * @param p03 - Third 6-hour precipitation amount 
		 * @param p04 - Fourth 6-hour precipitation amount
		 * @return the total 24-hour precipitation amount
		 */
		public static float prPr24 ( float p01, float p02, float p03, float p04 ){
//			float p24 = GempakConstants.RMISSD;
			float[] tempArray = {p01, p02, p03, p04 };
			Arrays.sort(tempArray);
//			p24 = tempArray[3];
			float p24 = tempArray[3];
			if ( p24 > 0 ){
				p24 = 0;
		            if ( p01 > 0 ) 
		        	     p24 = p24 + p01;
		            
		            if ( p02 > 0 )
		            	 p24 = p24 + p02;
		            
		            if ( p03 > 0 ) 
		            	 p24 = p24 + p03;
		            
		            if ( p04 > 0 ) 
		            	 p24 = p24 + p04;

		    } else if (p24 < 0f){
		    	p24 = GempakConstants.RMISSD;
		    }
			return p24;
		}
		
		/**
		 * Computes the maximum precipitation amount for upto 4 preciptiation values in inches
		 * @param p01 - First precipitation amount 
		 * @param p02 - Second precipitation amount
		 * @param p03 - Third precipitation amount 
		 * @param p04 - Fourth precipitation amount
		 * @return the maximum precipitation
		 */
		public static float prPr6x ( float p01, float p02, float p03, float p04 ){
			float[] tempArray = {p01, p02, p03, p04 };
			Arrays.sort(tempArray);
			return tempArray[3];
		}
		
       /**
        * Computes the station pressure ( in mb ) from the temperature ( in deg Celsius ) 
        * and the potential temperature ( in Kelvin ) using Poisson's equation:
        * 		PRES = 1000. * ( PR_TMCK (TMPC) / THTA ) ** (1 / RKAPPA)
        * @param tmpc - temperature (in deg Celsius) 
        * @param thta   - potential temperature ( in Kelvin ) 
        * @return the station pressure ( in mb ) if both the inputs are valid and RMISSD  (-9999) otherwise
        */
		public static float prPres ( float tmpc, float thta ){
			float prpres = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(tmpc)
					&& !MissingValueTester.isDataValueMissing(thta)
					&& ( tmpc > -GempakConstants.TMCK )
					&& ( thta > 0 ) ){
				float tmpk = prTmck ( tmpc );
				prpres = ( float )  ( 1000 * Math.pow(tmpk / thta, 1 / GempakConstants.RKAPPA ) );
			}
			return prpres;
		}
		
		/**
		 * Computes the packed wind speed and direction from DRCT and SPED using
		 * the equation:   PSPD  =  JDRCT * 500 + JSPED
		 * 					where
		 * 						JDRCT = NINT ( DRCT / 5 )
		 * 						JSPED = NINT ( SPED )
		 * 
		 * @param drct - Wind direction in degrees
		 * @param sped - Wind speed 
		 * @return the packed wind speed and direction if both inputs are valid and RMISSD ( -9999 ) otherwise.
		 * It is in the form DDFFF, where DD is the wind direction in tens of degrees,
         * and FFF is either the wind speed or wind speed plus 500, 
         * depending on the unit digit of direction rounded to the nearest 
         * 5 degrees.
         * 
		 */
		public static float prPspd ( float drct, float sped ) {
			float prpspd = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing( drct )
					&& !MissingValueTester.isDataValueMissing(sped)){
				float jdrct = Math.round ( drct / 5);
//				float jsped = Math.round ( sped / 5);
				float jsped = Math.round ( sped );
				prpspd = jdrct * 500 + jsped;
			}
			return prpspd;
		}
		
		/**
		 * Extracts the symbol code from the pressure tendency information.  
		 * The code number is returned follow by 999 so that the output is a 4-digit number. 
		 * @param p03d - the pressure tendency information 
		 * @return the pressure tendency symbol code if the input is valid or RMISSD ( -9999.0 ) otherwise.
		 */
		public static float prPtsy ( float p03d ){
//			 return  ( !MissingValueTester.isDataValueMissing(p03d)  ?    ( ( int ) ( p03d / 1000 ) ) * 1000 + 999 : GempakConstants.RMISSD     );
			 return  ( !MissingValueTester.isDataValueMissing(p03d) & !(p03d<0) & !(p03d>=9000)  ?    ( ( int ) ( p03d / 1000 ) ) * 1000 + 999 : GempakConstants.RMISSD     );
		}
		
		/**
		 * Converts the numeric WMO weather code for past weather reported from
		 * an automatic station (WMO code table 4531) to the corresponding numeric WMO weather code 
		 * for past weather reported from a manned station (WMO code table 4561).
		 * @param pwwa - the auto station past weather code
		 * @return the manned station past code if the input is valid and RMISSD ( -9999 )  otherwise. 
		 */
		public static float prPwao ( float pwwa ){
			float prpwao = GempakConstants.RMISSD;
			int[] man =  { -1, -1,  3,  4,  -1,  5,  6,  7,  8,  9 };
			int iwx = Math.round(pwwa);
			if ( iwx >= 0 && iwx <= 9){
				if ( man[ iwx ] >= 0 )
					prpwao = man[ iwx ];
			}
			return prpwao;
		}
		
		/**
		 * Computes the quotient for parameter ratios using the equation:   PR_QUOT = X / Y
		 * (For computing QPF/Watch Threshold)
		 * @param x - Numerator
		 * @param y - Denominator
		 * @return the quotient if both inputs are valid and the denominator is non zero. Returns RMISSD ( -9999 )
		 * otherwise
		 */
		public static float prQuot ( float x, float y){
			if (  !MissingValueTester.isDataValueMissing( x )
					&& !MissingValueTester.isDataValueMissing( y )
					&&  ( y != 0) ){
				return (  x / y );
			}
			return GempakConstants.RMISSD;
		}
		
		/**
		 * Computes the relative humidity ( in percent ) from the input temperature and dewpoint using
		 * the equation:
		 * 			 RELH  =  VAPR / VAPS * 100
		 * 				where
		 * 					VAPR = vapor pressure
		 * 						 = PR_VAPR ( DWPC )
		 * 					VAPS = saturation vapor pressure
		 * 						 = PR_VAPR ( TMPC )
		 * 
		 * @param tmpc - temperature ( in Celsius )
		 * @param dwpc - dewpoint ( in Celsius)
		 * @return the relative humidity ( in percent ) if both inputs are valid and RMISSD ( -9999.0 ) otherwise
		 */
		public static float prRelh ( float tmpc, float dwpc ){
			float prrelh = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing( tmpc ) 
					&& !MissingValueTester.isDataValueMissing( dwpc )){
				/* Find the vapor pressure */
				float e = prVapr ( dwpc );
				
				/*Find the saturated vapor pressure*/
				float es = prVapr ( tmpc );
				
				/*Calculate humidity*/
				prrelh = ( e/es) * 100;
			}
			return prrelh;
		}
		
		/**
		 * Computes the dewpoint (in Celsius) from the temperature ( in Celsius )
		 * and the relative humidity ( in percent ). 
		 * @param tmpc - the temperature ( in deg Celsius ) 
		 * @param relh   - the relative humidity ( in percent )
		 * @return the dewpoint in ( deg Celsius), if both inputs are valid and the value of the vapor
		 * pressure computed is > ( 1* e^(-30)), or RMISSD (-9999) otherwise
		 */
		public static float prRhdp ( float tmpc, float relh){
			float prrhdp = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing( tmpc )
					&& !MissingValueTester.isDataValueMissing( relh ) ){
				
				/*Calculate saturation vapor pressure; test for existence*/
				float vaps = prVapr ( tmpc);
				if( !MissingValueTester.isDataValueMissing(vaps)){
					
	                       /*Calculate vapor pressure*/				
				           float vapr = relh * vaps / 100;
				           
				           /*Calculate dewpoint.  The VAPR test prevents LOG blowups*/
//				           if ( vapr >  ( Math.pow(Math.E, -30) ) ){//legacy checks for 1.E-30
				           if ( vapr >=  ( Math.pow(Math.E, -30) ) ){//legacy checks for 1.E-30
				        	   prrhdp =  ( float) ( 243.5 * ( Math.log(6.112) - Math.log(vapr) )  / (  Math.log(vapr)  - Math.log(6.112) - 17.67 ) );
					           
				        	   /* If the dew-point is less than -190 degrees C, it is treated as missing data
				        	    * Note: Legacy documents it but does not implement it.
				        	    * However, in CAVE, it was decided to implement it.
				        	    * */

				        	   if ( prrhdp < -190 )
					        	       prrhdp = GempakConstants.RMISSD;
				           }
				}
			}
			return prrhdp;
		}
		
		/**
		 * Computes the abbreviated standard altimeter code SALI from the altimeter setting in
		 * inches ALTI.  SALI is an abbreviated  altimeter code in inches which contains
		 * the unit digit and the first two digits after the decimal points. ALTI is
		 * multiplied by 100 truncated, and the original tens digit dropped.  The following
		 * equation is used:  SALI = NINT ( MOD ( ALTI, 10 ) * 100 ) 
		 *
		 * @param alti - the altimeter code in inches
		 * @return the abbreviated standard altimeter code from the input altimeter code
		 * (if it exists) and RMISSD (-9999) otherwise. 
		 */
		public static float prSali ( float alti){
			float prsali = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(alti) ){
				
				/*Drop the leading tens digits*/
				float aalt = alti % 10;
				
				/*
				 * Include the tenths and hundredths digit:
				 * Multiply by 100 and round it off to the nearest integer
				 */
				aalt *= 100;
				prsali = ( float ) Math.round(aalt);
			}
			
			return prsali;
		}
		
		/**
		 * Computes the standard abreviated altimeter code from the altimeter setting in inches,
		 * after converting it to millibars ALTM.  Then ALTM is multiplied by 10, truncated, and
		 * the original thousand and hundred digits are dropped.  The following equation
		 * is used:  SALT = NINT ( MOD ( ALTM, 100 ) * 10 ) 
		 * @param alti - the altimeter setting in inches
		 * @return the abbreviated standard altimeter code from the input altimeter setting
		 * (if it exists) and RMISSD (-9999) otherwise. 
		 */
		public static float prSalt ( float alti ){
			float prsalt = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(alti)){
				
				/*Convert the altimeter to millibars*/
				float altm = prAltm(alti);
				
				/*Drop the leading thousand and/or hundreds digits*/
				float aalt = altm % 100;
			    
				/* Include the tenths digit*/
				aalt *= 10;
//				prsalt = Math.round(aalt);
				prsalt = (int)aalt;
			}
			return prsalt;
		}
		
		/**
		 * Computes the scale height in a layer which can then be used to compute the
		 * moist hydrostatic height.  The folloiwng equation is used:
		 * 		SCLH  =  ( RDGAS / GRAVTY ) * TAV
		 * 				TAV    = average virtual temperature in layer
		 * 					   = ( TVIRTB + TVIRTT ) / 2
		 * 				TVIRTB = virtual temperature at bottom
		 * 				TVIRTT = virtual temperature at top
		 * @param tb - Bottom temperature ( in Celsius )
		 * @param tt - Top temperature ( in Celsius )
		 * @param tdb - Bottom dewpoint ( in Celsius )
		 * @param tdt - Top dewpoint ( in Celsius )
		 * @param pb - Bottom pressure ( in millibars )
		 * @param pt - Top pressure ( in millibars )
		 * @return the scale height ( in meters ) if all the input data exist and RMISSD (-9999) otherwise.
		 */
		public static float prSclh ( float tb, float tt, float tdb, float tdt, float pb, float pt ){
			float prsclh = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(tb) 
					&& !MissingValueTester.isDataValueMissing(tt)
					&& !MissingValueTester.isDataValueMissing(pb)
					&& !MissingValueTester.isDataValueMissing(pt)){
				
				         float tvb = prTvrk(tb, tdb, pb);
				         float tvt = prTvrk(tt, tdt, pt);
				         
				         if ( !MissingValueTester.isDataValueMissing(tvt)
				        		 && !MissingValueTester.isDataValueMissing(tvb)){
				        	 float tav = ( tvb + tvt ) / 2;
				        	 prsclh = tav * GempakConstants.RKAP;
				         }
			}
			return prsclh;
		}
		
		/**
		 * Combines the sky coverage symbol number with
		 * the wind speed and direction
		 * @param skyc - Sky coverage
		 * @param drct  - Wind direction in degrees
		 * @param sped - Wind speed (m/s or knots)
		 * @return The packed speed and direction
		 *  if none of the input values are missing and RMISSD (-9999) otherwise.
		 */
		public static float prSkyx ( float skyc, float drct, float sped ){
			float prskyx = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(skyc)  
					&& !MissingValueTester.isDataValueMissing(drct)
					&& !MissingValueTester.isDataValueMissing(sped)){
				         int jdrct = Math.round(drct);
				         int jsped = Math.round(sped);
				         int jskyc = Math.round(skyc);
				         prskyx = jdrct * 10 + jsped * 10000 + jskyc;
			}
			return prskyx;
		}
		
		
		/**
		 * Computes the wind speed from the 'U' and 'V' components of 
		 * the wind velocity. The formula is the square root of ( u^2 + v^2 )  
		 * 
		 * @param uWnd - U component of velocity
		 * @param vWnd - V component of velocity
		 * @return the computed windspeed if both inputs are valid and RMISSD ( -9999) otherwise  
		 */
		public static float prSped ( float uWnd, float vWnd){
			float prsped = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing(uWnd)
					&& !MissingValueTester.isDataValueMissing(vWnd)){
				        prsped =  ( float ) ( Math.sqrt(   ( Math.pow(uWnd, 2)   + Math.pow(vWnd, 2) )  ) ); 
			}
			return prsped;
		}
		
		/**
		 * Computes a standard height used on upper-air charts. 
		 * For data below 500 mb, the standard height is the last three digits of the height. 
		 *  For data at and above 500 mb, the height is the last three digits of the height in decameters.
		 * @param pres - Pressure in millibars
		 * @param hght - Height in meters
		 * @return the abbreviated height, if both inputs are valid and RMISSD ( -9999) otherwise.  
		 */
		public static float prStdz ( float pres, float hght){
			float prstdz = GempakConstants.RMISSD;
			if (  !MissingValueTester.isDataValueMissing(pres) 
					&& !MissingValueTester.isDataValueMissing(hght)){
				/*
				 * ( Non-Javadoc )
				 * This computation matches the method's description, as provided in the Javadoc.
				 * The method's description needs to be interpreted from a meteorologist's point of view
				 * as opposed to plain English.
				 * 
				 */
				        int ihhh = (  ( pres > 500 ) ? Math.round( hght ) : Math.round( hght / 10  ));
				        prstdz = ihhh % 1000;
			}
			return prstdz;
		}
		
		/**
		 * Computes the potential temperature ( in Kelvin ) from the
		 * temperature (in Celsius ) and the pressure ( in mb ).
		 * @param tmpc - The temperature ( in Celsius )
		 * @param pres  - The pressure ( in mb )
		 * @return the potential temperature ( in Kelvin ), if both inputs are valid 
		 * and RMISSD ( -9999) otherwise.  
		 */
		public static float prThta ( float tmpc, float pres){
			float prthta = GempakConstants.RMISSD;
			if ( !MissingValueTester.isDataValueMissing( tmpc )
					&& !MissingValueTester.isDataValueMissing( pres )
					&& ( pres > 0 )){
				
                    /*Change temperature in degrees Celsius to Kelvin.*/				
				    float tmpk = prTmck ( tmpc );
			
				    /*Calculate theta using Poisson's equation*/
				    prthta =  ( float ) ( tmpk *  Math.pow( ( 1000 / pres) , GempakConstants.RKAPPA) );
			}
			return prthta;
		}
		
		/**
		 * Computes wet bulb potential temperature ( in Celsius ) from the
		 * pressure, temperature and dewpoint. The result is obtained by first 
		 * computing the equivalent potential temperature (thte) of the the air parcel at level 
		 * pres. Then the air parcel is brought to 1000 mb moist adiabatically to get the 
		 * wet bulb potential temperature.  
		 * @param pres   - Pressure ( in millibars )
		 * @param tmpc  - Temperature ( in Celsius )
		 * @param dwpc  - Dewpoint ( in Celsius )
		 * @return The wet bulb potential temperature ( in Celsius ) if all inputs are valid 
		 *  and RMISSD ( -9999) otherwise.  
		 */
		public static float prThwc ( float pres, float tmpc, float dwpc) {
			float prthwc = GempakConstants.RMISSD;
			
			/*Check for missing and invalid data*/
			if ( !MissingValueTester.isDataValueMissing(tmpc)
					&& !MissingValueTester.isDataValueMissing(pres)
				    &&  !MissingValueTester.isDataValueMissing(dwpc)
					&& ( pres > 0 )){
				
				        /*Compute the thte*/
				         float thte = prThte(pres, tmpc, dwpc);

				         /*Check for missing 'thte' and compute wet bulb temperature.*/
				         if (!MissingValueTester.isDataValueMissing(thte) ){
				        	 float tg = 0;
				        	 float p1000 = 1000;
				        	 
				        	 /*Compute the parcel temperature (in Kelvin)*/
				        	 prthwc = prTmst ( thte, p1000, tg);
				        	 
				        	 if ( !MissingValueTester.isDataValueMissing(prthwc))
				        	     
				        		 /*Convert the parcel temperature to Celsius*/	  
				        		 prthwc = prTmkc( prthwc ); 
				         }
			}
			return prthwc;
		}
		
		/**
		 * <pre>
		 * Computes wet bulb temperature from the temperature, mixing ratio, and pressure.
		 * The result is obtained by solving for the temperature at which saturation occurs,
		 *  when the latent heat required to vaporize the water is provided by a cooling of the air.
		 *  The equation representing the process is:
		 *  <code> ( tmpk - tmwb ) * cp - ( Rsat (tmwb) - rmix ) * lvap = 0 </code> 
		 *  This implicit equation is solved by Newton's method, since the 
		 *  saturation mixing ratio Rsat is a transcendental function of tmwb.
		 *  The expressions for the heat of vaporization (LVAP) and saturation 
		 *   vapor pressure are equations (2) and (10) from Bolton (MWR, 1980).
		 *   </pre> 
		 * @param tmpk - Temperature (K)  
		 * @param rmix - Mixing ratio (g/kg) 
		 * @param pres - Pressure (mb) 
		 * @return Wet bulb temperature (K)  if all inputs are valid 
		 *  and RMISSD ( -9999) otherwise.  
		 */
		public static float prTmwb ( float tmpk, float rmix, float pres){
		 float prtmwb = GempakConstants.RMISSD;
			/*Check for missing and invalid data*/
			if ( !MissingValueTester.isDataValueMissing(tmpk)
					&& !MissingValueTester.isDataValueMissing(rmix)
				    &&  !MissingValueTester.isDataValueMissing(pres)
					&& ( pres > 0 )){
				
				/*Change temperature to degrees Celsius.*/
				float tmp = prTmkc ( tmpk );
				
				/*Compute the latent heat of vaporization.*/
				float lvap = prLhvp ( tmp );
                
				/*Compute the specific heat of moist air*/				
				rmix /= 1000;
				float cp =  ( float ) ( 1005.7 * ( 1.0 + 0.887 * rmix ) );
				
				float rlocp = lvap / cp;
				
				/*Do Newton iteration*/
				int iter = 0;
				float twb = tmp;
				boolean isConvrg = false;
				
				float A = 6.112f;
				float  B = 17.67f;
				float  C = 243.5f;
				float  EPSI = 0.622f;
				float G = B * C;
				float ERRMAX = 0.001f;
				
				while ( iter <= 50 && !isConvrg ){
				   iter++;
	               float bt = B * twb;
	               float  tpc = twb + C;
	               float d = ( float ) ( ( pres / A ) * Math.exp( (-bt) / tpc ) );
	                float dm1 = d - 1;
	                float f = ( tmp - twb ) - rlocp * ( EPSI / dm1 - rmix );
	                float df = (-G)  / ( tpc * tpc );
	                        df = d * df * rlocp * EPSI / ( dm1 * dm1 ) - 1;
	                float cor = f / df;
	                        twb = twb - cor;
					if ( Math.abs(cor)  <= ERRMAX )
						       isConvrg = true;
				}
				
				if ( isConvrg ){
					float twk = prTmck ( twb );
					if ( twk > tmpk )
						   twk = tmpk;
					prtmwb = twk;
				}
			}
		 return prtmwb;
		}

		/**
		 * Compute the coded value for the minimum temperature /maximum temperature/ probability of precipitation  
		 * @param tmin - Minimum temperature
		 * @param tmax - Maximum temperature
		 * @param pp24 - Probability of precipitation
		 * @return the coded value PPPYYYNNN 
		 * ( Where PPP is the Probability of precipitation, YYY is the maximum temperature and 
		 * NNN is the minimum temperature ), 
		 * if atleast one input is valid or RMISSD otherwise
		 */
		public static float prTpfr ( float tmin, float tmax, float pp24){
			float prtpfr = GempakConstants.RMISSD;
			/*
			 * ( Non- Javadoc)
			 * Construct the coded integer. The value is 9 digits as in
			 * PPPYYYNNN. 
 		     *  Where PPP is the Probability of precipitation, YYY  is the maximum temperature and 
		     * NNN is the minimum temperature 
			 * All values are adjusted by 200 to 
			 * account for negative and missing values.
			 */
			int ival = 0;
			if ( !MissingValueTester.isDataValueMissing(tmin))
				    ival = ival + ( Math.round(tmin) + 200  ) ;

			if ( !MissingValueTester.isDataValueMissing(tmax))
			       ival = ival + ( Math.round(tmax) + 200  )  * 1000 ;			
			
			if ( !MissingValueTester.isDataValueMissing(pp24))
			       ival = ival + ( Math.round(pp24) + 200  )  * 1000000 ;
			/*
			 * If all three values are missing (ival is still 0), then the answer is missing.
			 */
			if ( ival != 0)
				  prtpfr = ival;
			
			return prtpfr;
		}
	
		/**
		 * Computes the virtual temperature ( in Kelvin ) from the temperature ( in Celsius ),
		 * dewpoint ( in Celsius ) and pressure ( in mb ) where DWPC and PRES are used to compute
		 * MIXR.  The following equation is used:
		 * 		TVRK = TMPK * (1 + .001 * MIXR / .62197) / (1 + .001 * MIXR)
		 * If DWPC is missing, dry air is assumed and TMPK is returned.
		 * 
		 * @param tmpc  - Temperature ( in Celsius )
		 * @param dwpc  - Dewpoint ( in Celsius )
		 * @param pres    - Pressure ( in mb )
		 * @return the virtual temperature ( in Kelvin ) or RMISSD ( -9999.0 ) if there  
		 * are inputs missing for the computations.
		 */
		public static float prTvrk( float tmpc, float dwpc, float pres ){
			float prtvrk = GempakConstants.RMISSD;
			if ( MissingValueTester.isDataValueMissing( tmpc ) 
					|| MissingValueTester.isDataValueMissing( pres ) ){
                          return prtvrk;
			}
		    
			/*If dewpoint is missing, return temperature*/
//			else if ( !MissingValueTester.isDataValueMissing( dwpc ) ) {
			else if (MissingValueTester.isDataValueMissing( dwpc ) ) {
			       prtvrk = prTmck ( tmpc );
		     }
			
			else {
				        /*Change temperature to Kelvin.*/
				        float tmpk = prTmck ( tmpc ) ;
		    	        
				        /* Find mixing ratio in g/kg; if missing, return temperature */
				        float rmix = prMixr  ( dwpc, pres );
                        
				        if ( MissingValueTester.isDataValueMissing(rmix) ){
                        	          prtvrk = prTmck ( tmpc );
                        }else {
                        	          prtvrk =  ( float ) ( tmpk * ( 1 + 0.001 * rmix / 0.62197 ) / (  1 + 0.001 * rmix ) );
                        }
				
		     }			
			return prtvrk;
		}		
		
		/**
		 * Computes the 'U' component of the wind from its speed and direction
		 * @param sped - wind speed
		 * @param drct - wind direction
		 * @return The 'U' component of the wind if both inputs are valid and RMISSD ( -9999) 
		 * otherwise.
		 */
		public static float prUwnd ( float sped, float drct) {
			   float pruwnd = GempakConstants.RMISSD;
			    if ( !MissingValueTester.isDataValueMissing(drct) 
			    		&& !MissingValueTester.isDataValueMissing(sped)){
			    	           pruwnd =  ( float ) ( (-Math.sin( drct * GempakConstants.DTR ) ) * sped); 
			    }
			   return pruwnd;
		}
		
		/**
		 * Computes the 'V' component of the wind from its speed and direction
		 * @param sped - wind speed
		 * @param drct - wind direction
		 * @return The 'V' component of the wind if both inputs are valid and RMISSD ( -9999) 
		 * otherwise.
		 */
		public static float prVwnd ( float sped, float drct) {
			   float pruwnd = GempakConstants.RMISSD;
			    if ( !MissingValueTester.isDataValueMissing(drct) 
			    		&& !MissingValueTester.isDataValueMissing(sped)){
			    	           pruwnd =  ( float ) ( (-Math.cos( drct * GempakConstants.DTR) ) * sped ); 
			    }
			   return pruwnd;
		}		
		
		/**
		 * Computes the visibility ( in nautical miles ) from the input visibility ( in kilometers ) 
		 * @param vsbk - visibility ( in kilometers )
		 * @return visibility ( in nautical miles ) if the input is valid and RMISSD (-9999) otherwise
		 */
		public static float prVskn ( float vsbk) {
			return (  !MissingValueTester.isDataValueMissing(vsbk)  ?   (float) (vsbk * 0.54 ) : GempakConstants.RMISSD ) ;
		}
		
		/**
		 *  Converts the WMO cloud cover fraction code to the decimal cloud cover fraction
		 * @param wmoccv - WMO cloud cover code
		 * @return Decimal cloud cover fraction if the input is valid and RMISSD (-9999) otherwise
		 */
       public static float prWccv ( float wmoccv) {
    	   float prwccv = GempakConstants.RMISSD;
    	   float[] cover = {0, 0.1f, 0.2f, 0.4f, 0.5f, 0.6f, 0.7f, 0.9f, 1, 1};
    	   if (!MissingValueTester.isDataValueMissing(wmoccv)
    			   && (  wmoccv >= 0) && ( wmoccv <= 9)){
    		             int iccv = Math.round(wmoccv) + 1;
    		             if (iccv > 9){
    		            	    iccv = 9;
    		             }
    		             prwccv = cover[iccv];
    	   }
    	   return prwccv;
       }
		
       /**
        * Computes the wind chill equivalent temperature ( the temperature with calm winds that produces the same 
        * cooling effect as the given temperature with the given wind speed)
        * @param tmpf - Air temperature ( in Farenheit )
        * @param sknt  - Wind speed ( in knots ) 
        * @return the wind chill equivalent temperature ( in Farenheit ),
        *  if the inputs are valid and RMISSD (-9999) otherwise
        */
       public static float prWceq ( float tmpf, float sknt ){
    	   float prwceq = GempakConstants.RMISSD;
    	   /*Convert input variables to Celsius and meters/second.*/
    	   float tmpc = prTmfc ( tmpf );
    	   float sped = prKnms ( sknt );
    	   if ( !MissingValueTester.isDataValueMissing(tmpc) 
    			   && !MissingValueTester.isDataValueMissing(sped)){
    		     
    		          if ( sped <= 1.34)
    		        	         /*If the wind speed does not exceed 1.34 m/s ( not much wind to contribute to the wind chill),
    		        	          *  return the input temperature as the wind chill temperature*/
    		        	         prwceq = prTmcf(tmpc);
    		          else{
    		        	  /*
    		        	   * Compute the wind chill temp if the inputs are not missing and 
    		        	   * and the wind speed is greater than 1.34 m/s.
    		        	   * Equations for wind chill computation  
    		        	   * from R. Falconer, "Windchill, A Useful Wintertime Weather Variable", Weatherwise, Dec 1968.
    		        	   */
    		        	         float windChill  =  ( float )  ( 33.0 - ( ( 33.0 - tmpc ) * wci( sped ) / wci( 1.34f ) )  );
    		        	         prwceq = prTmcf(windChill);
    		          }
    	   }
    	   
    	   return prwceq;
       }
       
       /**
        * Computes the wind chill temperature from the air temperature and the wind speed
        * @param tmpf - Air temperature ( in degree Farenheit )
        * @param sknt  - Wind speed ( in knots ) 
        * @return the wind chill temperature ( in Farenheit ) if none of the inputs are missing and
        * RMISSD (-9999) otherwise
        */
       public static float prWcht ( float tmpf, float sknt){
    	   float prwrcht = GempakConstants.RMISSD;
    	   
    	   /*Convert the speed to miles per hour*/
    	   float smph = prKnmh ( sknt );
    	   
    	   if ( !MissingValueTester.isDataValueMissing(tmpf) 
    			   && !MissingValueTester.isDataValueMissing(smph)){
    		   /*If the inputs are not missing , check if the wind speed is <= 3 miles per hour*/
    		   if( smph <= 3 )
    			           prwrcht = tmpf;
    		   else{
    			          /*Compute the wind-chill temperature for wind speeds that exceed 3 miles per hour*/
    			           float wcht = ( float ) ( 35.74 + 0.6215 * tmpf -35.75 * Math.pow(smph, 0.16)
    			                                             + 0.4275 * tmpf *  Math.pow(smph, 0.16) );
    			           prwrcht = ( wcht > tmpf ? tmpf : wcht);
    			                              
    		   }
    	   }
    	   return prwrcht;
       }
       
       /**
        * Computes the wind component towards a specific direction from the
        * wind direction, wind speed and direction of desired component.
        * @param drct - the wind direction in degrees
        * @param sped - the wind speed in m/s
        * @param dcmp - the direction of the desired component
        * @return the component of the wind (in m/s)  if none of the input parameters are missing and
        * RMISSD (-9999) otherwise
        */
       public static float prWcmp ( float drct, float sped, float dcmp ){
    	   float prwcmp = GempakConstants.RMISSD;
    	   /*Check for missing input parameters*/
    	   if ( !MissingValueTester.isDataValueMissing(drct) 
    			   && !MissingValueTester.isDataValueMissing(sped)
    			   && !MissingValueTester.isDataValueMissing(dcmp)){
    		         /*Calculate wind speed toward specified direction*/     
    		         prwcmp = (float) (  sped * ( -Math.cos(  ( drct - dcmp)  *GempakConstants.DTR  )    )    );
    	   }
    	   return prwcmp;
       }
       
       /**
        * Computes the hierarchical value of the ceiling converted to MSL (mean sea level) in hundreds of feet, 
        * with a temporary/probability value taking precedence over a prevailing one. 
        * @param cmsl - Prevailing ceiling (MSL) ( in 100's of ft )
        * @param tcms - Temporary /probability ceiling (MSL) ( in 100's of ft )
        * @return Hierarchical ceiling (MSL) ( in 100's of ft )
        */
       public static float prWcms ( float cmsl, float tcms ){
    	   return  ( !MissingValueTester.isDataValueMissing(tcms)  ?   tcms : cmsl  );
       }
       
       /**
        * Computes the packed wind speed and direction from the input wind speed and wind direction 
        * @param drct - wind direction ( in degrees )
        * @param sped - wind speed ( in knots or m/s )
        * @return the packed speed and direction
        */
       public static float prWind ( float drct, float sped ){
    	   float prwind = GempakConstants.RMISSD;
    	   if ( !MissingValueTester.isDataValueMissing(drct)
    			   && !MissingValueTester.isDataValueMissing(sped)){
    		   /*
    		    * (Non-Javadoc)
    		    * The packed wind speed and direction are of the form:
    		    * SSSDDD, where SSS - wind speed ( in knots or m/s ) and 
    		    * DDD - wind direction in degrees
    		    * 
    		    */
    		   int jdrct = Math.round(drct);
    		   int jsped = Math.round(sped);
    		   prwind = jdrct + jsped * 1000;
    	   }
    	   return prwind;
       }
       
       /**
        * Converts the numeric WMO weather code for present weather,
        * reported from an automatic station ( WMO code table 4680 ) to
        * the corresponding numeric WMO weather code for present weather
        * reported from a manned station (WMO code table 4677).
        * @param wwma - Automatic station weather code
        * @return the manned station weather code if the input weather code ( after being rounded to the nearest integer )
        * lies between 0 and 99 (both inclusive) and the value returned from the array, by indexing this number is > 0. 
        * Otherwise, it returns RMISSD ( -9999 ). 
        */
       public static float prWmao ( float wwma ){
    	   float prwmao = GempakConstants.RMISSD;
    	   int[] man = { -1,   1,   2,   3,   5,   5,  -1,  -1,  -1,
                   -1,  10,  76,  13,  -1,  -1,  -1,  -1,  -1,
                   18,  -1,  28,  21,  20,  21,  22,  24,  29,
                   -1,  -1,  -1,  42,  41,  42,  44,  46,  47,
                   -1,  -1,  -1,  -1, 203, 203, 203,  63,  65,
                   73,  75,  67,  67,  -1,  53,  51,  53,  55,
                   56,  57,  57,  58,  59,  -1,  63,  61,  63,
                   65,  66,  67,  67,  68,  69,  -1,  73,  71,
                   73,  75,  79,  79,  79,  77,  78,  -1,  81,
                   80,  81,  81,  82,  85,  86,  86,  -1,  90,
                   95,  17,  95,  96,  17,  97,  99,  -1,  -1,
                   19 };
    	   /*
    	    * (Non-Javadoc)
            * Convert automatic station weather number to manual station
            * weather number.  Reserved locations in table 4680 and those
            * values not having an analogue in table 4677 are mapped to
            * RMISSD.
    	    * 
    	    */
    	   int iwx = Math.round(wwma);
    	   if ( iwx >= 0 && iwx <= 99){
    		   if ( man[iwx] >= 0)
    			   prwmao = man[iwx];
    	   }
    	   return prwmao;
       }
       
       /**
        * Computes the wind component toward a direction 90 degrees counterclockwise of a specified direction.
        * If no direction is specified, the component toward north is returned.
        * @param drct    - wind direction ( in degrees )
        * @param sped   - wind speed ( in knots or m/s )
        * @param dcmp  - specified wind direction ( in degrees ) 
        * @return a component of the wind in m/s if the input wind speed and direction are valid and if 
        * the specified wind direction is between 0 degrees and 360 degrees. Otherwise, it returns RMISSD ( -9999 ).
        * 
        */
       public static float prWnml ( float drct, float sped, float dcmp){
    	   float prwnml = GempakConstants.RMISSD;
    	   if ( !MissingValueTester.isDataValueMissing(sped)
    			   && !MissingValueTester.isDataValueMissing(drct)
    			   && ( dcmp >= 0 )  && ( dcmp <= 360 )  ){
    		   /*
    		    * Calculate wind speed 90 degrees to left of given direction. 
    		    */
    		   prwnml = ( float ) ( sped * ( -Math.cos( ( drct - dcmp - 90 ) * GempakConstants.DTR    )    )  );
    	   }
    	   return prwnml;
       }
       
       /**
        * Computes the combined wave period and height from the combined wave period and 
        * wave height. The group number distinguishes the different wind waves.
        * @param powv - Wave period ( in seconds )
        * @param howv - Wave height ( in meters )
        * @param group -  Wave group number
        *                                  0 = no group  
        *                                  1 = instrument waves
        *                                  2 = wind waves 
        *                                  4 = predominant swell waves 
        *                                  5 = secondary swell waves
        * @return the combined wave period and height if none of the input parameters are missing and
        * RMISSD ( -9999 ) otherwise.
        */
       public static float prWphf( float powv, float howv, float group){
    	   float prwphf = GempakConstants.RMISSD;
    	   
    	   if ( !MissingValueTester.isDataValueMissing(powv)
    			   && !MissingValueTester.isDataValueMissing(howv)
    			   && !MissingValueTester.isDataValueMissing(group)){
    		   float prhgmf = prHgmf(howv);
    		   prwphf = prWvph ( powv, prhgmf, group);
    	   }
    	   
    	   return prwphf;
       }
      
       /**
        * Computes the combined wave period and height from the combined wave period and 
        * wave height. The group number distinguishes the different wind waves.
        * @param powv - Wave period ( in seconds )
        * @param howv - Wave height ( in the same units as the calling method )
        * @param group -  Wave group number
        *                                  0 = no group  
        *                                  1 = instrument waves
        *                                  2 = wind waves 
        *                                  4 = predominant swell waves 
        *                                  5 = secondary swell waves
        * @return The combined wave period and height if none of the inputs are missing
        *  and RMISSD (-9999) otherwise. 
        */
       public static float prWvph ( float powv, float howv, float group ){
    	   float prwvph = GempakConstants.RMISSD;
    	   if ( !MissingValueTester.isDataValueMissing(powv)
    			   && !MissingValueTester.isDataValueMissing(howv)
    			   && !MissingValueTester.isDataValueMissing(group)){
    		   int ipowv = Math.round(powv);
    		   int ihowv = Math.round(howv);
    		   int igroup = Math.round(group);
    		   if ( (ipowv >= 0) && (ipowv <= 99) 
    				   && ( (ihowv >= 0) && (ihowv <= 99) ) 
    				   && ( (igroup >= 0)  && (igroup <= 9) ) ){
    			   prwvph = igroup * 10000 + ipowv * 100 + ihowv; 
    		   }
    	   }
    	   return prwvph;
       }
       
       /**
        * Computes the combined wave period and wave height ( in half meters ) from
        * either wper and whgt ( instrument wave data) or poww and howw (wind wave data), 
        * since either instrument wave data or wind wave data, but not both, can be reported.
        * A group number is prefixed to the combined value to distinguish between 
        * instrument waves and wind waves.
        * @param wper   -  Instrument wave period (seconds)
        * @param whgt   - Instrument wave height (meters)
        * @param poww  - Wind wave period (seconds)
        * @param howw  - Wind wave height (meters)
        * @return The combined wave period and wave height ( in half meters ) if either the 
        * instrument wave data or the wind wave data are valid and RMISSD  (-9999) otherwise.
        */
       public static float prWphm ( float wper, float whgt, float poww, float howw ){
    	   
    	   float prwphm = GempakConstants.RMISSD;
    	   if ( !MissingValueTester.isDataValueMissing(wper)
    			   && !MissingValueTester.isDataValueMissing(whgt)){
    		            prwphm = prWvph ( wper, 2 * whgt, 1); 
    	   }
    	   else if ( !MissingValueTester.isDataValueMissing(poww)
    			   && !MissingValueTester.isDataValueMissing(howw)){
	            prwphm = prWvph ( poww, 2 * howw, 2);
           }
    	   return prwphm;
       }
      
       /**
        * Translates a WMO pressure tendency code into a pressure change ( in mb )
        * @param ctend - WMO pressure tendency code
        * @return A pressure change ( in mb ) if the input is valid and RMISSD (-9999)
        *  otherwise.
        */
       public static float prWtnd ( String ctend ){
    	   float prwtnd = GempakConstants.RMISSD;
    	   float[] psign = {1,1,1,1,0,-1,-1,-1,-1  };
    	   if ( ctend != null && !ctend.isEmpty() ){
    		   Character firstChar = ctend.charAt( 0 );
    		     if ( firstChar >= '0'  && firstChar <= '8' ){
    		    	 int itendc = Integer.parseInt( firstChar.toString() );
    		    	 String substr = ctend.substring(1, 4);
    		    	 if (substr.compareTo("999") != 0){
    		    		float ptend = Float.parseFloat(substr);
    		    		prwtnd =  psign[ itendc ] *  ptend / 10 ;
    		    	 }
    		     }
    	   }
    	   return prwtnd;
       }
       
       /**
        * Computes the combined predominant swell wave direction and secondary
        * swell wave direction from the input parameters. 
        * A group number 3 is prefixed to the combined value.
        *  
        * @param dosw - Predominant swell wave direction ( in degrees) .
        * @param dos2  - Secondary swell wave direction ( in degrees).
        * @return The combined swell wave directions ( in tens of degrees )  if the input parameters
        * are not missing and RMISSD (-9999) otherwise. 
        */
       public static float prWvdd ( float dosw, float dos2){
    	   float prwvdd = GempakConstants.RMISSD;
    	  
    	   /* If the secondary direction is missing, a value of 99 is used */
    	   int idos2 =  ( !MissingValueTester.isDataValueMissing(dos2) ? Math.round( dos2 / 10 ) :  99 );
    	   if ( !MissingValueTester.isDataValueMissing(dosw)){
    		     int idosw = Math.round(dosw / 10);
    		     if (  ( idosw >= 0 && idosw <= 99 ) 
    		    		 && ( idos2 >= 0 && idos2 <= 99 ) ){
    		    	       prwvdd = 30000 + idosw *100 + idos2;
    		     }
    	   }
    	   return prwvdd;
       }
       
       /**
        * Translates a WMO visibility code into visibility in kilometers.
        * @param wmovis - WMO visibility code 
        * @return the visibility ( in km )
        */
       public static float prWvis ( float wmovis ){
    	   float prwvis = GempakConstants.RMISSD;
    	   float[] vis90 = { 0, 0.1f, 0.2f, 0.5f, 1, 2, 4, 10, 20, 50 };
    	   if ( !MissingValueTester.isDataValueMissing(wmovis)){
    		   
    		   /*Values between 0 - 50 translate into tenths of kilometers.*/
    		   if  ( wmovis >= 0 && wmovis <= 50 )
    			   prwvis = wmovis / 10;
    		   
    		   /*Values between 56 and 80 are 50 + whole kilometers.*/
    		   else if ( wmovis >= 56 && wmovis <= 80 )
    			   prwvis = wmovis - 50;
    		   
    		   /*Values from 81 - 89 are in increments of 5.*/
    		   else if ( wmovis >= 81 && wmovis <= 89 )
    			   prwvis = ( wmovis - 80 ) * 5 + 30 ;
    		   
    		   /*The values from 90 - 99 are in the array vis90*/
    		   else if  ( wmovis >= 90 && wmovis <= 99 ){
    			   int iknt = (int) (wmovis - 89);
    			   if (  vis90 != null && vis90.length > 0  ){
    				   if (  iknt >= vis90.length )
    				       iknt = vis90.length - 1;
    			       prwvis = vis90[iknt];
    			   }
    		   }
    	   }
    	   return prwvis;
       }
       
       /**
        * Computes the combined direction, period and height of the swell waves from the 
        * swell wave direction ( in tens of degrees ), the swell wave period ( in seconds )
        * and the swell wave height ( in half meters )
        * @param dosw - Swell wave direction in degrees
        * @param posw - Swell wave period in seconds
        * @param hosw - Swell wave height in meters 
        * @return The combined direction, period and height of the swell waves if none of the inputs are missing
        * and RMISSD ( -9999)  otherwise.
        */
       public static float prWvsw ( float dosw, float posw, float hosw){
    	   float prwvsw = GempakConstants.RMISSD;
    	   if ( !MissingValueTester.isDataValueMissing(dosw)
    			   && !MissingValueTester.isDataValueMissing(posw)
    			   && !MissingValueTester.isDataValueMissing(hosw)){
    		             float pphh = prWvph ( posw, 2 * hosw, 0 );
    		             if ( !MissingValueTester.isDataValueMissing(pphh))
    		            	      prwvsw =  Math.round( dosw / 10 ) * 10000 + Math.round(pphh) ; 
    	   }
    	   return prwvsw;
       }
       
       /**
        * Computes the worst case categorical identification of flight rules for prevailing 
        * and temporary / probability conditions.
        * @param xvfr - Prevailing categorical id of flight rules 
        * @param txvf - Temporary / probability categorical id of flight rules
        * @return The worst case categorical id of flight rules
        */
       public static float prWxvf ( float xvfr, float txvf ){
    	   float prwxvf = GempakConstants.RMISSD;
    	   
    	   if ( !MissingValueTester.isDataValueMissing(txvf)
    			   && !MissingValueTester.isDataValueMissing(xvfr)){
    		   prwxvf = ( xvfr < txvf ? xvfr : txvf );
    	   } 
    	   else if ( MissingValueTester.isDataValueMissing( txvf ) )
    		   prwxvf = xvfr;
    	   else if ( MissingValueTester.isDataValueMissing( xvfr ) )
    		   prwxvf = txvf;
    	 
    	   return prwxvf;
       }
       
 
       
       /**
        * Computes the numeric total cloud cover for the worst case aviation flight condition, 
        * based on the categorical identification of flight rules for prevailing and temporary / probability conditions.
        * @param xvfr - Prevailing categorical id of flight rules 
        * @param txvf - Temporary / Probability categorical id of flight rules
        * @param cfrt - Prevailing numeric total cloud cover
        * @param tcfr - Temporary / Probability numeric total cloud cover
        * @return Worst case numeric total cloud cover or RMISSD (-9999) if the computation does not fall through
        */
       public static float prWcfr ( float xvfr, float txvf, float cfrt, float tcfr) {
    	   float prwcfr = GempakConstants.RMISSD; 
    	   if ( MissingValueTester.isDataValueMissing(xvfr)
    			   || MissingValueTester.isDataValueMissing(txvf)){
    		              prwcfr = (  cfrt > tcfr   ?  cfrt  : tcfr  );
    	   }
    	   else if ( txvf < xvfr )
    		          prwcfr = tcfr;
    	   else if ( txvf == xvfr)
    		          prwcfr = (  cfrt > tcfr   ?  cfrt  : tcfr  );
    	   else
    		   prwcfr = cfrt;
    	   return prwcfr;
       }
       
       
       /**
        * Computes the windchill from the wind velocity ( part of  the Falconer equation  - refer method prWceq)
        * @param velocity - wind velocity  ( in meters per second )
        * @return the windchill temperature
        */
       private static float wci ( float velocity){
    	   if ( !MissingValueTester.isDataValueMissing(velocity) ){
           /* from R. Falconer, "Windchill, A Useful Wintertime Weather Variable", Weatherwise, Dec 1968.*/
    		   return (  ( float )  (10 * Math.sqrt(velocity) + 10.45 - velocity )); 
    	   }
    		   return GempakConstants.RMISSD;
       }
       
       /**
        * <pre>
        * Computes LIFR/IFR/MVFR/VFR flight conditions based on ceiling and visibility.
        * @param ceil - Ceiling in hundreds of feet
        * @param vsby - Visibility in statute miles
        * @return Flight conditions index value: 
        *     0 - LIFR
        *     1 - IFR
        *     2 - MVFR
        *     3 - VFR 
        *  or RMISSD (-9999), if both ceiling and visibility are missing
        *  </pre>
        */
       public static float prXvfr ( float ceil, float vsby ){
    	   float prxvfr = GempakConstants.RMISSD;
		   float vc = GempakConstants.RMISSD;
		   float vs = GempakConstants.RMISSD;
		   
		   if ( MissingValueTester.isDataValueMissing(ceil) && MissingValueTester.isDataValueMissing(vsby))
			   return prxvfr;
		   
		   /*Compute categorical flight rules*/
		   //Check the ceiling value
		   if ( !MissingValueTester.isDataValueMissing(ceil)){
		   if ( ceil < 0){
			   //no-op. So vc retains its RMISSD value
		   }
			   else if ( ceil < 5 )
    				    vc = 0;
    			   else if ( ceil < 10 )
    				     vc = 1;
    			   else if ( ceil <= 30 )
    				     vc = 2;
    			   else if ( ( vsby > 5 ) 
    					        || ( vsby < 0 )  
    					        ||  MissingValueTester.isDataValueMissing(vsby) ){
    				     prxvfr = 3; 
    			   }
    	   }

		   /*Check the visibility value.*/
           if (  !MissingValueTester.isDataValueMissing(vsby) ){
        	   if ( vsby < 0 ){
        		   //no-op. So vs retains it RMISSD value
        	   }
           else if ( vsby < 1 )
        		vs = 0;
        	else if  ( vsby < 3)
        		 vs = 1;
        	else if ( vsby <= 5)
        		vs = 2;
        	else 
        		vs = 3;
           }
           
           /*Determine the more restrictive of the two values.*/
           if ( MissingValueTester.isDataValueMissing ( vc ) )
        	   prxvfr = vs;
           else if ( MissingValueTester.isDataValueMissing( vs ) )
        	   prxvfr = vc;
           else
              prxvfr = ( vc < vs ? vc : vs );
           
           return prxvfr;
    	   
       }       
    
       /**
        * Computes station elevation from altimeter and station pressure.
        * It is also used to estimate height at various pressure levels from the altimeter in millibars.  
        * The PC library computes zmsl, Z000, Z950, Z850, Z800 by calling this function with pres 
        * equal to PMSL, 1000, 950, 850 and 800 respectively.
        * @param altm - Altimeter in millibars
        * @param pres - Pressure in millibars
        * @return the height ( in meters ) if neither input value is missing and both input values
        * are greater than zero. Otherwise, it returns RMISSD ( -9999 ).
        */
       public static float prZalt ( float altm, float pres ){
    	   float przalt = GempakConstants.RMISSD;
     	   if ( !MissingValueTester.isDataValueMissing( altm ) 
     			   && !MissingValueTester.isDataValueMissing(pres)
     			   && ( altm > 0 )
     			   &&( pres > 0 ) ) {
     		   
     		   float to = GempakConstants.TMCK + 15;
     		   float gamma = GempakConstants.GAMUSD / 1000 ;
     		   
     		   /*Calculate the exponent and pressure ratio.*/
     		   float expo =  ( gamma * GempakConstants.RDGAS ) / GempakConstants.GRAVTY;
     		   float prat  = pres / altm ;
     		   przalt = ( float ) ( ( to * (  1 - Math.pow ( prat, expo) ) ) / gamma );
     	   }
    	   return przalt;
       }
       
       /**
        * Converts the GEMPAK weather code to the WMO weather code,
        * which is used to plot weather symbols. 
        * @param wnum - GEMPAK weather code
        * @return the weather symbol number
        */
       public static float prNsym ( float wnum){
    	   
    	   float prnsym = GempakConstants.RMISSD;
//TODO : uncomment these 2 lines, once the PT library is implemented 
//    	   float wcod = ptWcod ( wnum ); /*Convert weather number to character weather code*/
//    	   prnsym = ptWsym ( wcod ); /*Convert weather character code to weather symbol number*/ 
    	   return prnsym;
       }
       
		public static class RZLL{
			private static RZLL rzll;
			
			/**Station latitude in degrees*/
			float stltdg = GempakConstants.RMISSD; 
			
			/**Station longitude in degrees*/
			float stlndg = GempakConstants.RMISSD;
			
			/**Range in kilometers*/
			float range = GempakConstants.RMISSD;
			
			/**Geographic azimuth in radians*/
			float azim = GempakConstants.RMISSD;
			
			/**Height above the ground in kilometers*/
			float hght = GempakConstants.RMISSD;

			/**Latitude in degrees*/
			float xlat = GempakConstants.RMISSD;
		    
			/**Longitude in degrees*/
			float xlon = GempakConstants.RMISSD;
		    
		     /**
			 * @return the xlat
			 */
			public float getXlat() {
				return xlat;
			}
			/**
			 * @return the xlon
			 */
			public float getXlon() {
				return xlon;
			}
			

			
		    private static RZLL getInstance(){
				if (rzll == null)
					{rzll = new RZLL();}
				return rzll;
			}
		    
		    /**
		     * Computes the actual latitude/longitude given the station latitude/longitude, elevation
		     * and azimuth.
		     * It uses equations developed for use in the AOIPS radar.
		     * @param instltdg - Station latitude in degrees
		     * @param instlndg - Station longitude in degrees
		     * @param inrange - Range in kilometers 
		     * @param inazim - Geographic azimuth in radians  
		     * @param inhght - Height above ground in km
		     */
			public void prRzll ( float instltdg,float instlndg,float inrange,float inazim,float inhght){
						if ( !MissingValueTester.isDataValueMissing(instltdg)
								&& !MissingValueTester.isDataValueMissing(instlndg)
								&& !MissingValueTester.isDataValueMissing(inrange)
								&& !MissingValueTester.isDataValueMissing(inazim)
								&& !MissingValueTester.isDataValueMissing(inhght)){
           							
							        this.stltdg  = instltdg;
			           				this.stlndg = instlndg;
						         	this.range  = inrange;
							        this.azim   = inazim;
							        this.hght   = inhght;
							       
							           float hdr   = GempakConstants.RMISSD;
							           float elev  = GempakConstants.RMISSD;   
							           float rad = GempakConstants.RMISSD;
							           float radp= GempakConstants.RMISSD;
							           
							        /*Convert the station lat/lon to radians*/
						           float stlat = stltdg * GempakConstants.DTR;
						           float stlon = stlndg * GempakConstants.DTR;
						           
						           
						           /*Get the elevation angle*/
						           hdr =  ( range == 0.0f ? 0.0f : hght / range  );
//						           elev = (float) ( Math.abs(hdr) < 1.0f ? Math.asin(hdr) : 0.0f );
						           elev = (float) ( Math.abs(hdr) <= 1.0f ? Math.asin(hdr) : 0.0f );	
						           
						           float temp =  (float)  (Math.pow( Math.sin(stlat), 2 ) ) ;
						           
						           /*Get the earth's corrected radius*/
						           rad =  (float) (6378.4 / Math.sqrt( 1 + ( 0.00677 * temp  )  ));
						           radp = 4* (rad/3);
						           					           
						           float dist = GempakConstants.RMISSD;
						           float cx =  GempakConstants.RMISSD;
						           float cy =  GempakConstants.RMISSD;
						           float mathFormula1 = GempakConstants.RMISSD;
						           float mathFormula2 = GempakConstants.RMISSD;
						           
						           /*Calculate the distance*/
						           
						           if ( elev > 0.2618f )
						                   dist =  ( float ) ( range * Math.cos( elev ));
						           else{
						        	           mathFormula1 =  ( float ) ( (1 -  ( Math.pow( elev, 2 ) / 2 ) ) - range * elev / radp);
						        	           dist = range * mathFormula1;
						           }
						        	  
						          /*Calculate the latitude and longitude*/
						          cx = ( float )  ( dist * Math.sin( azim ) ); 
						          cy = ( float )  ( dist * Math.cos( azim ) );
						           
//						           mathFormula2 = ( float )  ( ( ( 2 * Math.pow( rad, 2 ) ) * Math.tan( stlat ) ));
//						           xlat = ( float )  ( stlat + ( cy / rad ) - (   Math.pow(cx, 2)  / mathFormula2  ) );
						           mathFormula2 = (float) ( ( Math.pow(cx, 2) / ( 2 * Math.pow( rad, 2 ) ) * Math.tan( stlat ) ));
						           xlat = ( float )  ( stlat + ( cy / rad ) -  mathFormula2 );
						           xlon = ( float ) ( stlon + ( cx / ( rad * Math.cos( xlat ) ) ) ) ;
						           
						           /*Change lat/lon to degrees*/
						           xlat *= GempakConstants.RTD;
						           xlon *= GempakConstants.RTD;
						} else {
							xlat = GempakConstants.RMISSD;
							xlon = GempakConstants.RMISSD;
						}
						
			}
		}
}





























