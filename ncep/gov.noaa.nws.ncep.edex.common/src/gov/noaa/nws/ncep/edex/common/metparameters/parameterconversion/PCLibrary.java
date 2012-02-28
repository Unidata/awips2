/**
 * 
 */
package gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion;

import java.util.ArrayList;
import java.util.List;

import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.edex.common.metparameters.AirTemperature;
import gov.noaa.nws.ncep.edex.common.metparameters.Amount;
import gov.noaa.nws.ncep.edex.common.metparameters.DewPointTemp;
import gov.noaa.nws.ncep.edex.common.metparameters.HeightAboveSeaLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.Omega;
import gov.noaa.nws.ncep.edex.common.metparameters.PressureLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.SpecificHumidity;
import gov.noaa.nws.ncep.edex.common.metparameters.WindDirection;
import gov.noaa.nws.ncep.edex.common.metparameters.WindDirectionUComp;
import gov.noaa.nws.ncep.edex.common.metparameters.WindDirectionVComp;
import gov.noaa.nws.ncep.edex.common.metparameters.WindSpeed;
////import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidRangeException; 
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer2;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 *     Date            Ticket#    Engineer        Description
 * ------------------ ---------- --------------- --------------------------
 * 18-Feb-2011        398       Archana       Initial creation.
 * 
 *</pre> 
 * @author Archana
 * @version 1.0
 */
public final class PCLibrary {
      
	public enum VerticalCoordinate { NONE, PRESSURE, TEMPERATURE, HEIGHT };
	public enum LevelType { AT_SURFACE, AT_TOP, BETWEEN_LEVELS, 
		                                   BETWEEN_SURFACE_AND_TOP, EXACT_MATCH,
		                                   BELOW_SURFACE, ABOVE_TOP,  NONE };
	public enum SearchOrder { BOTTOM_UP, TOP_DOWN };
	private static LevelType locationOfLevel  = LevelType.NONE;
/**
	 * @return the locationOfLevel
	 */
	public final static LevelType getLocationOfLevel() {
		return locationOfLevel;
	}

     public final static class PCDHGT {


		private static PCDHGT instance;
    	  
		  /**The height increment*/
		  private static float delh;
    	  
		  /**The dry hydrostatic height*/
		  private static Amount pcdhgt;
    	  
		 public final static PCDHGT getInstance(){
    		  if ( instance == null ){
    			  instance = new PCDHGT();
    		  }
    		  return instance;
    	  }
    	  
    	  private PCDHGT(){
    	  }
    	  
    	  /**
    	   * Computes the dry hydrostatic height
    	   * @param tkb - bottom temperature
    	   * @param tkt  - top temperature
    	   * @param pb  - bottom pressure
    	   * @param pt   - top pressure
    	   * @param hb  - bottom height
    	 * @throws InvalidValueException 
    	 * @throws NullPointerException 
    	   */
    	 public final static void pcDhgt ( Amount tkb, Amount tkt, Amount pb, Amount pt, Amount hb) throws NullPointerException, InvalidValueException{
    		            pcdhgt = new Amount();
             		    delh = GempakConstants.RMISSD;        	  
    		            PRLibrary.checkNullOrInvalidValue( tkb );
    		            PRLibrary.checkNullOrInvalidValue( tkt );
    		            PRLibrary.checkNullOrInvalidValue( pb );
    		            PRLibrary.checkNullOrInvalidValue( pt );
    		            PRLibrary.checkNullOrInvalidValue( hb );
                       
    		             if ( pt.getValue().floatValue() > 0 ){
                  		 delh     = ( float ) ( GempakConstants.RKAP * 0.5 * (tkb.getValue().floatValue() 
                  				          + tkt.getValue().floatValue()) * Math.log ( pb.getValue().floatValue() / pt.getValue().floatValue() ));
        	             pcdhgt = new Amount ( hb.getValue().floatValue() + delh, hb.getUnit() );
    		             }
//        	  }
          }  
    	  
    	  /**
  		 * @return the delh
  		 */
  		public final static float getHeightIncrement() {
  			return delh;
  		}

  		/**
  		 * @return the pcdhgt
  		 */
  		public final static Amount getDryHydrostaticHeight() {
  			return pcdhgt;
  		}
	
      }
      
      /**
       * Computes the moist hydrostatic height
       * @param tb    - Bottom temperature
       * @param tt     - Top temperature
       * @param tdb  - Bottom dewpoint
       * @param tdt   - Top dewpoint
       * @param pb   - Bottom pressure
       * @param pt    - Top pressure
       * @param hb   - Bottom height
       * @return the moist hydrostatic height if the input values are not missing and
       * RMISSD (-9999) otherwise 
      * @throws InvalidValueException 
     * @throws NullPointerException 
       * 
       */
		public final static Amount pcMhgt ( Amount tb, Amount tt, Amount tdb, 
				                                       Amount tdt, Amount pb, Amount pt, Amount hb ) throws NullPointerException, InvalidValueException{
			Amount  pcmght = new Amount();  

       if  (   !PRLibrary.checkNullOrInvalidValue( tb )
            || !PRLibrary.checkNullOrInvalidValue( tt )
            || !PRLibrary.checkNullOrInvalidValue( tdb )
            || !PRLibrary.checkNullOrInvalidValue( tdt )
            || !PRLibrary.checkNullOrInvalidValue( pb )
            || !PRLibrary.checkNullOrInvalidValue( pt )
            || !PRLibrary.checkNullOrInvalidValue( hb ) )		
    	         return new Amount ( SI.METER );
       
             if (  pt.getValue().floatValue() != 0 ){
            Amount tvb = PRLibrary.prTvrk ( tb, tdb, pb );
				        Amount tvt = PRLibrary.prTvrk ( tt, tdt, pt );
				        float tav = ( tvb.getValue().floatValue() + tvt.getValue().floatValue() ) / 2;
				        pcmght =  new Amount (  ( float ) ( hb.getValue().floatValue()
				        		          + GempakConstants.RKAP * tav 
				        		          * Math.log ( pb.getValue().floatValue() / pt.getValue().floatValue() ) ), 
				        		           hb.getUnit() ); 
             }
			      return pcmght;
	  		}      
      
		/**
		 * Computes precipitable water
		 * @param tdb    - Bottom dewpoint 
		 * @param tdt     - Top dewpoint
		 * @param pb     - Bottom Pressure
		 * @param pt      - Top Pressure
		 * @param pwb  - Bottom precipitable water
		 * @return the computed precipitable water, if none of the inputs are missing and RMISSD (-9999) otherwise
		 * @throws InvalidRangeException 
		 * @throws InvalidValueException 
		 * @throws NullPointerException 
		 */
		public final static Amount pcPwtr ( Amount tdb, Amount tdt, Amount pb, Amount pt, Amount pwb) throws NullPointerException, InvalidValueException{
			Amount pcpwtr = new Amount();
			if ( tdb != null && tdt != null 
					&& pb != null && pt != null 
					&& tdb.hasValidValue()
					&& tdt.hasValidValue()
					&& pb.hasValidValue()
					&& pt.hasValidValue() ){
                      
				      /*Compute the average mixing ratio.  Note that the mixing
                       *ratio is divided by 1000. since PR_MIXR computes g/kg.
                       */
 
					    Amount  rmb  = PRLibrary.prMixr ( tdb, pb );
					    Amount rmt    = PRLibrary.prMixr ( tdt, pt );
					    float rmav  = ( ( rmb.getValue().floatValue() 
					    		                   + rmt.getValue().floatValue() ) / 2) / 1000;
					    pcpwtr = new Amount ( 100 * rmav 
					    		* ( pb.getValue().floatValue() - pt.getValue().floatValue() ) 
					    		/  (GempakConstants.GRAVTY + pwb.getValue().floatValue() ), 
					    		SI.MILLIMETER );				
			}
			else{
				       pcpwtr = pwb; 
			}
			return pcpwtr;
		}
		
		/**
		 * Computes the Montgomery stream function
		 * @param tb     - Bottom temperature
		 * @param tt      - Top temperature
		 * @param delh  - Incremental dry hydrostatic height
		 * @param psb   - Bottom psi
		 * @return the computed Montgomery Stream function if none of the inputs are missing 
		 * and RMISSD (-999) otherwise
		 * @throws InvalidValueException 
		 * @throws NullPointerException 
		 */
		public final static Amount pcPsym ( Amount tb, Amount tt, Amount delh, Amount psb) throws NullPointerException, InvalidValueException {
			float pcpsym = GempakConstants.RMISSD;
//			if ( !MissingValueTester.isDataValueMissing( tb ) 
//					&& !MissingValueTester.isDataValueMissing( tt )
//					&& !MissingValueTester.isDataValueMissing( delh )
//					&& !MissingValueTester.isDataValueMissing( psb ) ){
			PRLibrary.checkNullOrInvalidValue(tt);
			PRLibrary.checkNullOrInvalidValue(tb);
			PRLibrary.checkNullOrInvalidValue(delh);
			PRLibrary.checkNullOrInvalidValue(psb);
				/*
				 * (Non-Javadoc) 
				 * The values of 'cp' and 'g' are divided by 100 to make the 
				 * Montgomery stream function smaller than 10**6
				 * 
				 */
				          float cp = GempakConstants.RDGAS / GempakConstants.RKAPPA / 100;
				          float g = GempakConstants.GRAVTY/100;
				         pcpsym = ( cp * ( tt.getValue().floatValue() - tb.getValue().floatValue() ) + g * delh.getValue().floatValue() );
//			}
			return new Amount ( pcpsym, Unit.ONE);
		}
		
		/**
		 * Interpolates between two levels of data with respect to the log of pressure
		 * 
		 * @param vlev      - Pressure to be interpolated
		 * @param adata    - Data at first level 
		 * @param bdata    - Data at second level
		 * @param nparms - Size of data arrays
		 * @param intflg    - Interpolation flags
		 * @param angflg   - Angle interpolation flags
		 * @param outdat   - The data interpolated to vlev
		 * @return The data array interpolated to the pressure vlev, if the computations succeed.
		 * Else individual members of the data array are set to RMISSD (-9999).
		 */
		public final static float[] pcIntp ( float vlev,float[]  adata,float[]  bdata,int  nparms,boolean[]  intflg,boolean[]  angflg){
			float[]  outdat = new float[0];
			if ( adata != null && bdata != null 
					&& adata.length > 0 && bdata.length > 0){
			        
				    float pres1 = adata[0];
			        float pres2 = bdata[0];
			        if ( ( ( (pres1 < pres2) && (pres1 < vlev) && (vlev < pres2) ) 
			        		|| ( (pres2 < pres1) && (pres2 < vlev) && (vlev < pres1) ) ) 
			        		&& ( pres1 > 0 ) && ( pres2 > 0 ) ) {
			        	/*
			        	 * Set up interpolation. Check for interpolation of angles through 360 degrees.
			        	 */
			        	outdat = new float[nparms];
			        	float rmult =  ( float ) ( Math.log( vlev / adata[0] ) / Math.log( bdata[0] / adata[0]) ); 
			        	outdat[0] = vlev;
			        	for ( int i = 1; i < nparms; i++){
			        		outdat[i] = GempakConstants.RMISSD;
			        		if ( adata[i] !=GempakConstants.RMISSD
			        		        && bdata[i] != GempakConstants.RMISSD ) {
								if (intflg[i]) {
									if (angflg[i]) {
										float ang1 = adata[i] % 360;
										float ang2 = bdata[i] % 360;
										if (Math.abs(ang1 - ang2) > 180) {
											if (ang1 < ang2)
												ang1 = ang1 + 360;
											else
												ang2 = ang2 + 360;
										}
										float ang = ang1 + (ang2 - ang1)
												* rmult;
										outdat[i] = ang % 360;
									} else {
										outdat[i] = adata[i]
												+ (bdata[i] - adata[i]) * rmult;
									}
								}
							}
			        	}
			        }
			}
			return outdat;
		}
		
		/**
		 * Interpolates between two levels of data with respect to height
		 * @param hght      - Height to be interpolated
		 * @param adata     - Data at first level
		 * @param bdata     - Data at second level
		 * @param nparms  - Size of the data arrays
		 * @param intflg     - Interpolation flags
		 * @param angflg    - Angle interpolation flags
		 * @param jhght      - index of the height data in the input data arrays
		 * @return The data array interpolated to the height 'hght', if the computations succeed.
		 * Else individual members of the data array are set to RMISSD (-9999).
		 */
		public final static float[] pcInth( float hght,float[] adata,float[] bdata,int nparms,boolean[] intflg,boolean[] angflg,int jhght ){
			float[] outdat = new float[0];
			if ( adata != null && bdata != null && adata.length > 0 && bdata.length > 0){
                float hght1 = adata[jhght];
                float hght2 = bdata[jhght];
                if ( ( ( ( hght1 < hght2 ) && ( hght1 < hght ) &&  ( hght < hght2 ) )  
                	 ||  ( ( hght2 < hght1 ) && ( hght2 < hght ) && ( hght < hght1 ) ) ) ){
               
                	   float rmult = ( hght - adata [jhght] ) /  ( bdata [jhght] - adata [jhght] );
                       outdat = new float[nparms];
                       outdat[jhght] = hght;
                       for ( int i = 0; i < nparms ; i++ ){
                    	     if ( i != jhght){
 					        	if (adata[i] != GempakConstants.RMISSD
 				        		          && bdata[i] != GempakConstants.RMISSD ){
											if (intflg[i]) {
												if (angflg[i]) {
													float ang1 = adata[i] % 360;
													float ang2 = bdata[i] % 360;
													if (Math.abs(ang1 - ang2) > 180) {
														if (ang1 < ang2)
															ang1 += 360;
														else
															ang2 += 360;
													}

													float ang = ang1 + (ang2 - ang1) * rmult;
													outdat[i] = ang % 360;
												} else {
													        outdat[i] = adata[i] + (bdata[i] - adata[i]) * rmult;
												}
											}
										}else{
											outdat[i] = GempakConstants.RMISSD;
										}
                    	    }
                      }
                }
			}
			return outdat;
		}
		
		/**
		 * Interpolates between two levels of data with respect to temperature
		 * @param temp     - Temperature to be interpolated
		 * @param adata     - Data at first level
		 * @param bdata     - Data at second level
		 * @param nparms  - Size of the data arrays
		 * @param intflg     - Interpolation flags
		 * @param angflg    - Angle interpolation flags
		 * @param jtemp     - index of the temperature data in the input data arrays
		 * @return The data array interpolated to the temperature 'temp', if the computations succeed.
		 * Else individual members of the data array are set to RMISSD (-9999).
		 */
		public final static float[] pcIntt ( float temp,float[] adata,float[] bdata,
				     int nparms,boolean[] intflg,boolean[] angflg,int jtemp){
			float[] outdat = new float[0];
			if ( adata != null && bdata != null && adata.length > 0 && bdata.length > 0 ){
				
				float temp1 = adata[jtemp];
				float temp2 = bdata[jtemp];
				if ( (     ( ( temp1 < temp2 ) && ( temp1 < temp ) && ( temp  < temp2 ) )  
						||  ( ( temp2 < temp1 ) && ( temp2 < temp ) && ( temp  < temp1 ) ) )){
					outdat = new float[nparms];
					float rmult = ( temp - adata[jtemp] ) /  ( bdata[jtemp] - adata[jtemp] ) ;
					outdat[jtemp] = temp; 
					for ( int i = 0 ; i < nparms ; i++ ){
					        if ( i != jtemp ){
					        	if (adata[i] != GempakConstants.RMISSD
			        		          && bdata[i] != GempakConstants.RMISSD) {
					        	          	if (intflg[i]) {
					        			        if (angflg[i]) {
                                                    float ang1 = adata[i] % 360;
                                                    float ang2 = bdata[i] % 360;
                                                    if ( Math.abs( ang1 - ang2) > 180 ){
                                                    	if ( ang1 < ang2 )
                                                    		    ang1 += 360;
                                                    	else
                                                    		    ang2 += 360;
                                                    }
                                                   float ang = ang1 + (ang2 - ang1) * rmult;
                                                   outdat[i] = ang % 360; 
					        			        }
					        			        else{
					        			                    if ( i == 1 ){
					        			                    	float plog =  ( float ) ( Math.log( bdata[i] ) - Math.log ( adata[i] ) ) ;
					        			                    	outdat[i]  =   ( float ) ( adata[i] * Math.exp ( rmult * plog ) ); 
					        			                    }else{
					        			                    	 outdat[i] = adata[i] +  ( bdata[i] - adata[i] ) * rmult;
					        			                    }
					        			        }
									        }
								}else{
									outdat[i] = GempakConstants.RMISSD;
								}
					        }
					}
				}
				
			}
			return outdat;
		}
		
		/**
		 * Pulls out a single sounding (  <code>NcSoundingLayer2</code> ) from a list of sounding data
		 * @param listOfSoundingData - the list of  <code>NcSoundingLayer2</code> to be searched
		 * @param levelToFind - the index of the <code>NcSoundingLayer2</code> to be found in the list
		 *    
		 * @return the <code>NcSoundingLayer2</code> object using levelToFind as an index to listOfSounding.
		 * If the list is null or empty or if levelToFind is greater than the size of the list or it is a negative number, it returns null 
		 */
		public final static NcSoundingLayer2 pcGlev ( List <NcSoundingLayer2>  listOfSoundingData, int levelToFind  ){
			NcSoundingLayer2 thisSounding = null;
			if ( listOfSoundingData != null && !listOfSoundingData.isEmpty()  ){
				if (  levelToFind>= 0 && levelToFind < listOfSoundingData.size() ) {
					thisSounding = listOfSoundingData.get( levelToFind );
				}
			}
			return thisSounding;
		}
		
		
		/**
		 * Finds the appropriate sounding data for a given vertical level. 
		 * @param inputSoundingLayerList   - the list of NcSoundingLayer2 to search
		 * @param levelToFind                     - the vertical level for which a match is needed
		 * @param verticalCoordinateToFind - the type of vertical level - pressure / temperature / height
		 * @param searchOrder                     - the order in which the list is to be searched (bottom up or top down) 
		 * @return An empty list if levelToFind exceeds the bounds of the dataset or if no match is found. 
		 * If an exact match is found, a list the corresponding sounding and the sounding above it are returned.
		 * If levelToFind lies between 2 soundings in the data-set, both soundings are returned as a part of the list. 
		 *   
		 */
	   public final static List< NcSoundingLayer2 > pcFndl( List<NcSoundingLayer2> inputSoundingLayerList, 
	    		                                                               float levelToFind, 
                                                                               VerticalCoordinate verticalCoordinateToFind,
                                                                               SearchOrder searchOrder ) {
	    	List < NcSoundingLayer2 > listOfNearestSoundingLayers = new ArrayList<NcSoundingLayer2>( 0 );
	    	NcSoundingLayer2 firstNcSounding = null;
	    	NcSoundingLayer2 secondNcSounding = null;
	    	if ( inputSoundingLayerList != null && !inputSoundingLayerList.isEmpty()  && levelToFind != GempakConstants.RMISSD) {
				
	    		int beginIndex = 0;
				int endIndex = 0;
				int searchIndex = 0;
				
				/*Depending on the order of the search, change the order of the beginning and ending indices*/
				if ( searchOrder == SearchOrder.BOTTOM_UP ) {
					endIndex = inputSoundingLayerList.size() -1 ;
				} else {
					beginIndex = inputSoundingLayerList.size() - 1;
				}
				
				/*Set the searchIndex to the beginningIndex*/
				searchIndex = beginIndex;
				boolean done = false;
				
				while ( !done ) {
				
					float levelInSounding = GempakConstants.RMISSD;
					float nextLevelInSounding = GempakConstants.RMISSD; 
					NcSoundingLayer2 thisNcSounding = inputSoundingLayerList.get( searchIndex );
			        firstNcSounding = thisNcSounding;
			        int nextElementIndex = ( ( searchOrder == SearchOrder.BOTTOM_UP) ? searchIndex + 1 : searchIndex - 1);
			        secondNcSounding = pcGlev( inputSoundingLayerList, nextElementIndex );
			        if ( secondNcSounding == null )
			        	secondNcSounding = firstNcSounding;
			        
					switch (verticalCoordinateToFind) {

					case PRESSURE:
						
						levelInSounding         = thisNcSounding.getPressure().getValue().floatValue();
						nextLevelInSounding  = secondNcSounding.getPressure().getValue().floatValue();
						break;

					case HEIGHT:
						
						levelInSounding = thisNcSounding.getGeoHeight().getValue().floatValue();
						nextLevelInSounding  = secondNcSounding.getGeoHeight().getValue().floatValue();
						break;

					case TEMPERATURE:
						
						levelInSounding = thisNcSounding.getTemperature().getValue().floatValue();
						nextLevelInSounding  = secondNcSounding.getTemperature().getValue().floatValue();
						break;

					default:
						levelInSounding = GempakConstants.RMISSD;
						nextLevelInSounding = GempakConstants.RMISSD;
						break;
					}
		            
					/* Check if the levelToFind is out of bounds */
					if (levelInSounding != GempakConstants.RMISSD && nextLevelInSounding != GempakConstants.RMISSD) {
						if ( searchIndex == 0 ){
							done = checkDataBelowSurface(levelInSounding, levelToFind,verticalCoordinateToFind, searchOrder );
							if ( done )
							   locationOfLevel = LevelType.BELOW_SURFACE;
						}
						else if ( searchIndex == endIndex){
							done =  checkDataAboveTop(levelInSounding, levelToFind,verticalCoordinateToFind, searchOrder );
							if ( done )
								locationOfLevel = LevelType.ABOVE_TOP;
						}
					}
					
					/*Only if levelToFind is not out-of bounds, proceed with further checks...*/
					if ( !done) {
						/*Check if levelToFind has an exact match in the data-set*/
						done = ( levelInSounding == levelToFind );
						if ( done )
							locationOfLevel = LevelType.EXACT_MATCH;
						else{
							    /*Else, check if the levelToFind is between 2 levels in the data-set*/
						          done = checkDataBetweenLevels( levelInSounding, nextLevelInSounding, levelToFind, verticalCoordinateToFind, searchOrder);
			         	          if ( done )
			         	              locationOfLevel = LevelType.BETWEEN_LEVELS;
						}
						
						/*In either case, if a match is found, add the appropriate soundings  to the output list*/
					    if ( firstNcSounding != null && secondNcSounding != null && done ) {
		
					    	listOfNearestSoundingLayers.add ( firstNcSounding );
							listOfNearestSoundingLayers.add ( secondNcSounding );
                        
						}
					    
					    /*If the end of the sounding data-set is reached without a match, exit the loop*/
						if (searchIndex == endIndex)
							done = true;
						else
							searchIndex = ( ( searchOrder == SearchOrder.BOTTOM_UP ) ? searchIndex + 1 : searchIndex - 1 );
					}
				
				}
			}
			return listOfNearestSoundingLayers;
	    }
		
private static boolean checkDataAboveTop(float surfaceLevel,
				float levelToCheck, VerticalCoordinate verticalCoordinate,
				SearchOrder searchOrder) {
	boolean isDataAboveTop = false;
	if ( surfaceLevel != GempakConstants.RMISSD && levelToCheck != GempakConstants.RMISSD ){
		switch ( verticalCoordinate ){
		case PRESSURE:
			if ( searchOrder == SearchOrder.TOP_DOWN ){
				isDataAboveTop = ( levelToCheck < surfaceLevel );
			}else{
				isDataAboveTop = ( levelToCheck > surfaceLevel );
			}
			break;
		case TEMPERATURE:
			if ( searchOrder == SearchOrder.TOP_DOWN ){
				isDataAboveTop = ( levelToCheck > surfaceLevel );
			}else{
				isDataAboveTop = ( levelToCheck < surfaceLevel );
			}	    			
			break;
		case HEIGHT:
			if ( searchOrder == SearchOrder.TOP_DOWN ){
				isDataAboveTop = ( levelToCheck > surfaceLevel );
			}else{
				isDataAboveTop = ( levelToCheck < surfaceLevel );
			}	  	    			
			break;
		default:
			break;
		}
	}
	return isDataAboveTop;
		}

/**
 * 
 * @param firstLevel
 * @param secondLevel
 * @param levelToCheck
 * @param verticalCoordinate
 * @param searchOrder
 * @return
 */
	    private static boolean checkDataBetweenLevels ( float firstLevel, float secondLevel, float levelToCheck, VerticalCoordinate verticalCoordinate, SearchOrder searchOrder ){
	    	boolean isDataBetweenLevels = false;
	    	if ( levelToCheck != GempakConstants.RMISSD && firstLevel != GempakConstants.RMISSD && secondLevel != GempakConstants.RMISSD ){
	    	if ( searchOrder == SearchOrder.BOTTOM_UP ){
	    	   if ( verticalCoordinate == VerticalCoordinate.PRESSURE ){
	    			if ( firstLevel > levelToCheck   && levelToCheck > secondLevel && firstLevel > secondLevel )
	    				isDataBetweenLevels = true;
	    		}else{
	    			if ( firstLevel < levelToCheck   && levelToCheck< secondLevel && firstLevel < secondLevel )
	    				isDataBetweenLevels = true;
	    		}
	    		
	    	}else{
		    	   if ( verticalCoordinate == VerticalCoordinate.PRESSURE ){
		    			if ( firstLevel < levelToCheck   && levelToCheck < secondLevel && firstLevel < secondLevel )
		    				isDataBetweenLevels = true;
		    		}else{
		    			if ( firstLevel > levelToCheck   && levelToCheck > secondLevel && firstLevel > secondLevel )
		    				isDataBetweenLevels = true;
		    		}	    		
	    	}
	    	
	    }
	    	return isDataBetweenLevels;
	    }
	    
	    private static boolean checkDataBelowSurface ( float surfaceLevel, float levelToCheck, VerticalCoordinate verticalCoordinate, SearchOrder searchOrder ){
	    	boolean isDataBelowSurface = false;
	    	if ( surfaceLevel != GempakConstants.RMISSD && levelToCheck != GempakConstants.RMISSD ){
	    		switch ( verticalCoordinate ){
	    		case PRESSURE:
	    			if ( searchOrder == SearchOrder.BOTTOM_UP ){
	    				isDataBelowSurface = ( levelToCheck > surfaceLevel );
	    			}else{
	    				isDataBelowSurface = ( levelToCheck < surfaceLevel );
	    			}
	    			break;
	    		case TEMPERATURE:
	    			if ( searchOrder == SearchOrder.BOTTOM_UP ){
	    				isDataBelowSurface = ( levelToCheck < surfaceLevel );
	    			}else{
	    				isDataBelowSurface = ( levelToCheck > surfaceLevel );
	    			}	    			
	    			break;
	    		case HEIGHT:
	    			if ( searchOrder == SearchOrder.BOTTOM_UP ){
	    				isDataBelowSurface = ( levelToCheck < surfaceLevel );
	    			}else{
	    				isDataBelowSurface = ( levelToCheck > surfaceLevel );
	    			}	  	    			
	    			break;
	    			default:
	    				break;
	    		}
	    	}
	    	return isDataBelowSurface;
	    }
	    
	    /**
	     * 
	     * @param inputSoundingLayerList
	     * @return
	     * @throws Exception 
	     */

	   public final static List< NcSoundingLayer2 > findSoundingLayersClosestToVerticalCoordinate( List<NcSoundingLayer2> inputSoundingLayerList, 
	    		                                                                                     float levelToFind, 
	    		                                                                                    VerticalCoordinate verticalCoordinateToFind ) throws Exception{
            //Note: Combines the functionality of PC_FNDL and PC_FLVL
	    	List < NcSoundingLayer2 > listOfNearestSoundingLayers = new ArrayList<NcSoundingLayer2>( 0 );
	    	if (inputSoundingLayerList != null && !inputSoundingLayerList.isEmpty() ) {
				NcSoundingLayer2 prevNcSoundingLayer2 = new NcSoundingLayer2();
				NcSoundingLayer2 nextNcSoundingLayer2 = new NcSoundingLayer2();
				boolean found = false;
				int nextLevelIndex = 0;
				for (NcSoundingLayer2 thisNcSounding : inputSoundingLayerList) {

					if ( !found ) {
						float levelInSounding = GempakConstants.RMISSD;
						nextLevelIndex++;
						switch (verticalCoordinateToFind) {

						case PRESSURE:
							levelInSounding = thisNcSounding.getPressure().getValue().floatValue();
							break;

						case HEIGHT:
							levelInSounding = thisNcSounding.getGeoHeight().getValue().floatValue();
							break;

						case TEMPERATURE:
							levelInSounding = thisNcSounding.getTemperature().getValue().floatValue();
							break;

						default:
							levelInSounding = GempakConstants.RMISSD;
							break;
						}
						if ( ( levelInSounding != GempakConstants.RMISSD ) && ( levelInSounding > levelToFind ) )
							prevNcSoundingLayer2 = thisNcSounding;

						if ( ( levelInSounding != GempakConstants.RMISSD ) && ( levelInSounding < levelToFind ) ) {
							nextNcSoundingLayer2 = thisNcSounding;
							found = true;
						}

						if ( levelInSounding == levelToFind ) {
							prevNcSoundingLayer2 = thisNcSounding;
							if ( nextLevelIndex < inputSoundingLayerList.size() )
								nextNcSoundingLayer2 = PCLibrary.pcGlev( inputSoundingLayerList, nextLevelIndex );
							else
								nextNcSoundingLayer2 = prevNcSoundingLayer2;
						}

					} else {
						break;
					}
				}
				if (prevNcSoundingLayer2 != null && nextNcSoundingLayer2 != null) {
					listOfNearestSoundingLayers.add(prevNcSoundingLayer2);
					listOfNearestSoundingLayers.add(nextNcSoundingLayer2);
				}
			}
			return listOfNearestSoundingLayers;
	    }
		
	    /**
	     * 
	     * @param listOfSoundingLayersToInterpolate
	     * @param levelToInterpolate
	     * @param typeOfVerticalCoordinate
	     * @return the interpolated sounding data 
	     * if the input list of sounding data is not null or empty and contains exactly 2 NcSoundingLayer2 objects.
	     * @throws Exception 
	     */

	   public final static NcSoundingLayer2 interpolateBetweenTwoSoundingLayers( List<NcSoundingLayer2>  listOfSoundingLayersToInterpolate, 
	    		                                                                       float levelToInterpolate, 
	    		                                                                       VerticalCoordinate typeOfVerticalCoordinate ) throws Exception{
	    	NcSoundingLayer2 thisSounding = new NcSoundingLayer2();

			if (listOfSoundingLayersToInterpolate != null && 
					!listOfSoundingLayersToInterpolate.isEmpty() 
					&& listOfSoundingLayersToInterpolate.size() == 2) {
				NcSoundingLayer2 firstSounding = listOfSoundingLayersToInterpolate . get(0);
				float[] adata = new float[10];
				adata[0] = firstSounding.getPressure().getValue().floatValue();
				adata[1] = firstSounding.getTemperature().getValue().floatValue();
				adata[2] = firstSounding.getDewpoint().getValue().floatValue();
				adata[3] = firstSounding.getWindSpeed().getValue().floatValue();
				adata[4] = firstSounding.getWindDirection().getValue().floatValue();
				adata[5] = firstSounding.getGeoHeight().getValue().floatValue();
				adata[6] = firstSounding.getOmega().getValue().floatValue();
				adata[7] = firstSounding.getSpecificHumidity().getValue().floatValue();
				adata[8] = firstSounding.getWindU().getValue().floatValue();
				adata[9] = firstSounding.getWindV().getValue().floatValue();
				NcSoundingLayer2 secondSounding = listOfSoundingLayersToInterpolate . get(1);
				float[] bdata = new float[10];
				bdata[0] = secondSounding.getPressure().getValue().floatValue();
				bdata[1] = secondSounding.getTemperature().getValue().floatValue();
				bdata[2] = secondSounding.getDewpoint().getValue().floatValue();
				bdata[3] = secondSounding.getWindSpeed().getValue().floatValue();
				bdata[4] = secondSounding.getWindDirection().getValue().floatValue();
				bdata[5] = secondSounding.getGeoHeight().getValue().floatValue();
				bdata[6] = secondSounding.getOmega().getValue().floatValue();
				bdata[7] = secondSounding.getSpecificHumidity().getValue().floatValue();
				bdata[8] = secondSounding.getWindU().getValue().floatValue();
				bdata[9] = secondSounding.getWindV().getValue().floatValue();
				boolean[] intflg = new boolean[10];
				boolean[] angflg = new boolean[10];
				for (int i = 0; i < 10; i++) {
					if (i > 0) {
						intflg[i] = true;
					} else {
						intflg[i] = false;
					}

					if (i == 4) //wind direction alone needs to be interpolated by angle..
						angflg[i] = true;
					else
						angflg[i] = false;
				}
				int nparms = 10;
				int jhght = 5;
				int jtemp = 1;
				float[] interpolatedValues = new float[nparms];
				if (typeOfVerticalCoordinate
						.compareTo(PCLibrary.VerticalCoordinate.PRESSURE) == 0) {
					interpolatedValues = PCLibrary.pcIntp(levelToInterpolate,
							adata, bdata, nparms, intflg, angflg);
				} else if (typeOfVerticalCoordinate
						.compareTo(PCLibrary.VerticalCoordinate.HEIGHT) == 0) {
					//TODO:update the method signature of pcInth() to remove the jhght parameter?
					interpolatedValues = PCLibrary.pcInth(levelToInterpolate,
							adata, bdata, nparms, intflg, angflg, jhght);
				} else if (typeOfVerticalCoordinate
						.compareTo(PCLibrary.VerticalCoordinate.TEMPERATURE) == 0) {
					//TODO:update the method signature of pcIntt() to remove the jtemp parameter?
					interpolatedValues = PCLibrary.pcIntt(levelToInterpolate,
							adata, bdata, nparms, intflg, angflg, jtemp);
				}
				//  TODO might need to tweak this condition, if nparms is not 10
				if (interpolatedValues != null
						&& interpolatedValues.length == nparms && nparms == 10) {
					
					PressureLevel p = new PressureLevel();
					p.setValue(new Amount ( interpolatedValues[0], NcUnits.MILLIBAR));
					thisSounding.setPressure( p);
                    
					AirTemperature t = new AirTemperature();
                    t.setValue( new Amount ( interpolatedValues[1],
                    		           firstSounding.getTemperature().getUnit() ));
					thisSounding.setTemperature( t );
					
					DewPointTemp d = new DewPointTemp();
                    d.setValue( new Amount ( interpolatedValues[2],
         		           firstSounding.getDewpoint().getUnit() ));
					thisSounding.setDewpoint( d );

					WindSpeed ws = new WindSpeed();
					ws.setValue( new Amount ( interpolatedValues[3],
	         		           firstSounding.getWindSpeed().getUnit() ));
					thisSounding.setWindSpeed(ws);
					
					WindDirection wd = new WindDirection();
					wd.setValue( new Amount ( interpolatedValues[4],
	         		           firstSounding.getWindDirection().getUnit() ));
					thisSounding.setWindDirection( wd );
					
					HeightAboveSeaLevel h = new HeightAboveSeaLevel();
					h.setValue( new Amount ( interpolatedValues[5],
	         		           firstSounding.getGeoHeight().getUnit() ));
					thisSounding.setGeoHeight( h );
					
					Omega o = new Omega();
					o.setValue( new Amount ( interpolatedValues[6],
	         		   firstSounding.getOmega().getUnit() ));
					thisSounding.setOmega( o );
					
					SpecificHumidity sh = new SpecificHumidity();
					sh.setValue( new Amount ( interpolatedValues[7],
	         		     firstSounding.getSpecificHumidity().getUnit() ));
					thisSounding.setSpecificHumidity( sh );
					
					WindDirectionUComp uwnd = new WindDirectionUComp();
					uwnd.setValue( new Amount ( interpolatedValues[8],
	         		          firstSounding.getWindU().getUnit() ));
					thisSounding.setWindU(   uwnd );
					
					WindDirectionVComp vwnd = new WindDirectionVComp();
					vwnd.setValue( new Amount ( interpolatedValues[9],
	         		          firstSounding.getWindV().getUnit() ));
					thisSounding.setWindV(   vwnd );
				}
			}
			return thisSounding;
	    }
	    
	    /**
	     * <pre>
	     * Finds the sounding data at the requested level.
	     * If the requested level is AT_SURFACE, then the first element of the input list of sounding data is
	     * returned. 
         * If the requested level is AT_TOP, then the last element of the input list of sounding data is
	     * returned. 
	     * If the requested level is BETWEEN_SURFACE_AND_TOP, then the method 
	     * findSoundingLayersClosestToVerticalCoordinate(List NcSoundingLayer2, float, VerticalCoordinate)
	     * is called to find the sounding data for levels on either side of the input level.
	     *  
	     * </pre>
	     * @param inputSoundingLayerList    - the list of NcSoundingLayer2 to search
	     * @param levelToFind                     - the vertical level to be found
	     * @param verticalCoordinateToFind - the type of vertical level ( pressure / temperature / height )
	     * @param levelType                        - decides the location of the search - whether it is at the beginning of the list
	     *                                                         or at its end or in between the list.
	     * @return a list of NcSoundingLayer2 containing the desired sounding data. 
	     * If the search level requested is BETWEEN_SURFACE_AND_TOP, then 2 elements are present in the list.
	     * If the search level requested is either AT_SURFACE or AT_TOP, then only 1 element is present in the list.
	     * Otherwise, an empty list is returned.
	     */
	   public final static List < NcSoundingLayer2 > pcFlvl( List < NcSoundingLayer2 > inputSoundingLayerList, float levelToFind, 
	    		                                                            VerticalCoordinate verticalCoordinateToFind, LevelType levelType ){
	    	List < NcSoundingLayer2 > listOfSoundingLevelsToFind = new ArrayList < NcSoundingLayer2 >( 0 );
	    	if( inputSoundingLayerList != null && !inputSoundingLayerList.isEmpty() ){
	    		switch ( levelType ){
	    		      case AT_SURFACE:
	    			          NcSoundingLayer2 firstSoundingData = inputSoundingLayerList.get( 0 );
	    			          listOfSoundingLevelsToFind.add( firstSoundingData );
	    		      break;
	    			
	    		      case AT_TOP:
	    		    	  int listSize = inputSoundingLayerList.size();
    			          NcSoundingLayer2 lastSoundingData = inputSoundingLayerList.get( listSize - 1 );
    			          listOfSoundingLevelsToFind.add( lastSoundingData );	    		      
	    		      break;
	    		      
	    		      case BETWEEN_SURFACE_AND_TOP:
	    		    	  listOfSoundingLevelsToFind = pcFndl( inputSoundingLayerList, levelToFind, verticalCoordinateToFind,SearchOrder.BOTTOM_UP );
	    		      break;
	    		      
	    		      default:
     		          break;
	    		}
	    	}
	    	return listOfSoundingLevelsToFind;
	    }

	    /**
	     * Returns the sounding data at the top level of the list of sounding data
	     * @param listOfNcSounding - the list of sounding data
	     * @return the sounding at the top of the list that contains both valid pressure and valid temperature data
	     * @throws Exception 
	     */
	   public final static NcSoundingLayer2 pcFtop ( List<NcSoundingLayer2> listOfNcSounding ) throws Exception{
	    	NcSoundingLayer2 soundingAtTopLevel = new NcSoundingLayer2();
	    	if ( listOfNcSounding != null && !listOfNcSounding.isEmpty() ){
	    		     int sizeOfList = listOfNcSounding.size();
	    		     int startIndex = sizeOfList - 1;
	    		     boolean done = false;
	    		    while ( !done ){
	    		    	soundingAtTopLevel = listOfNcSounding.get(startIndex);
	    		    	PressureLevel p = soundingAtTopLevel.getPressure();
	    		    	AirTemperature t = soundingAtTopLevel.getTemperature();
	    		    	if ( soundingAtTopLevel != null  ){
	    		    		   if ( p != null && p.hasValidValue() && t != null && t.hasValidValue() )
	    		    		                       done = true;
	    		    	
	    		    	       startIndex--;
	    		    	       if ( startIndex < 0 )
	    		    		                      done = true;
	    		    	}
	    		    }
	    	}
	    	return soundingAtTopLevel;
	    }

	   public final static List<NcSoundingLayer2> pcLyrd ( List<NcSoundingLayer2> listOfNcSoundingLayer2, float levelToFind, VerticalCoordinate verticalCoordinate ){
	    	return pcFndl( listOfNcSoundingLayer2, levelToFind, verticalCoordinate, SearchOrder.BOTTOM_UP );
	    }
	    
//	    /**
//	     * Computes the value of pressure given the potential temperature
//	     * @param thta - the potential temperature 
//	     * @param firstLevelVerticalData      - VerticalData  comprising of (pressure/temperature/dew-point/geoHeight/thta)
//	     * @param secondLevelVerticalData -   VerticalData  comprising of (pressure/temperature/dew-point/geoHeight/thta)
//	     * @return the computed pressure or RMISSD (-9999)
//	     */
//	   public final static float pcPval ( float thta, VerticalData firstLevelVerticalData, VerticalData secondLevelVerticalData ){
//	    	float pressure = GempakConstants.RMISSD;
//	    	float p1;
//	    	float p2;
//	    	float th1;
//	    	float th2;
//	    	float t1;
//	    	float t2;	    	
//	    	if ( firstLevelVerticalData.getThta() <  secondLevelVerticalData.getThta() ){
//	    	           p1  = firstLevelVerticalData.getPressure();
//	    	           p2  = secondLevelVerticalData.getPressure();
//	    	           th1 = firstLevelVerticalData.getThta();
//	    	           th2 = secondLevelVerticalData.getThta();
//	    	           t1   = firstLevelVerticalData.getTemperature();
//	    	           t2 = secondLevelVerticalData.getTemperature();
//	    	}else{
//	    		        p1  = secondLevelVerticalData.getPressure();
//	    		        p2  = firstLevelVerticalData.getPressure();
//	    		        th1 = secondLevelVerticalData.getThta();
//	    		        th2 = firstLevelVerticalData.getThta();
//	    		        t1   = secondLevelVerticalData.getTemperature();
//	    		        t2   = firstLevelVerticalData.getTemperature();	    		
//	    	}
//
//	    	if (! MissingValueTester.isDataValueMissing( p1 ) 
//	    			&& ! MissingValueTester.isDataValueMissing( p2 )
//	    			&& ! MissingValueTester.isDataValueMissing( t1 )
//	    			&& ! MissingValueTester.isDataValueMissing( t2)
//	    			&& ! MissingValueTester.isDataValueMissing( th1 )
//	    			&& ! MissingValueTester.isDataValueMissing( th2) ){
//
//	    		float pg      =   ( float ) ( Math.exp (  Math.log ( p1 ) + ( thta - th1) * Math.log( p2 / p1 ) / ( th2 / th1 ) ) );
//	    		float pgold = GempakConstants.RMISSD;
//	    		float[] adata = new float[2];
//	    		float[] bdata = new float[2];
//	    		adata[0] = p1;
//	    		adata[1] = t1;
//	    		adata[0] = p2;
//	    		bdata[1] = t2;
//	    		float tg = GempakConstants.RMISSD;
//	    		float thg = GempakConstants.RMISSD;
//	    		boolean done = false;
//	    		int kiter = 1;
//	    		boolean[] intflg = new boolean[] {true, true};
//	    		boolean[] angflg = new boolean[] {false, false};
//	    		while ( !done ){
//	                      float[] cdata = new float[]{GempakConstants.RMISSD, GempakConstants.RMISSD }; 
//	                      cdata = pcIntp ( pg, adata, bdata, 2,intflg, angflg);
//	                      tg = cdata[1];
//	                      thg = PRLibrary.prThta ( tg, pg );
//	                      if  ( Math.abs ( thg - thta ) < 0.001 || ( pg == pgold )){
//	                    	  pressure  = pg;
//	                    	  done       = true;
//	                      }
//	                      else if ( checkIfInbetween(thta, th2, thg ) ){
//	                    	  p2 = pg;
//	                    	  th2 = thg;
//	                      }
//	                      else if ( checkIfInbetween(th1, thta, thg ) ){
//	                    	  p1 = pg;
//	                    	  th1 = thg;
//	                      }
//	                      else
//	                    	  done = true;
//	                      
//	                    pgold = pg;
//	                    pg     = ( float ) ( Math.exp (  Math.log ( p1 ) + ( thta - th1) * Math.log( p2 / p1 ) / ( th2 / th1 ) ) );
//	                    
//	                    if ( kiter > 100 )
//	                    	done = true;
//	                    else 
//	                    	kiter++;
//	    		}
//	    	}
//	    		return pressure;
//	   }
	    
	    private static boolean checkIfInbetween (  Amount value1, Amount value2, Amount valueToCheck ){
	    	boolean isInBetween = false;
	    	isInBetween = (  ( value1.hasValidValue() && value2.hasValidValue()  && valueToCheck.hasValidValue())
	    			                    && ( value1.getValue().floatValue() <= valueToCheck.getValue().floatValue() )  
	    			                    && ( valueToCheck.getValue().floatValue() <= value2.getValue().floatValue() ) );
	    	return isInBetween;
	    }
	    
//	    public VerticalData getVerticalDataFromSoundingData( NcSoundingLayer2 soundingData ){
//	    	return ( new VerticalData( soundingData ) );
//	    }
	    
//	    /**
//	     * 
//	     * @param listOfSoundingData
//	     * @param clev
//	     * @param verticaalCoordinate
//	     * @param dfdpth
//	     * @return
//	     */
//	   public final static List<NcSoundingLayer2> pcDpth ( List<NcSoundingLayer2> listOfSoundingData, float clev, VerticalCoordinate verticalCoordinate, float dfdpth){
//	    	List<NcSoundingLayer2> depthSoundingDataList = new ArrayList<NcSoundingLayer2>(0); 
//	    	if ( listOfSoundingData != null && !listOfSoundingData.isEmpty() ){
//	    		float depth = dfdpth;
//	    		
//	    		/*TODO: replace pcFndl with pcCmdt*/
//	    		NcSoundingLayer2 stndl = pcFndl(listOfSoundingData, clev, verticalCoordinate, SearchOrder.BOTTOM_UP ).get( 0 );
//	    		if ( MissingValueTester.isDataValueMissing(depth) ) {
//	    			List<NcSoundingLayer2> tempSoundingList = pcLyrd(listOfSoundingData, clev, verticalCoordinate);
//	    			if ( tempSoundingList != null && !tempSoundingList.isEmpty() &&  tempSoundingList.size() == 2 ) {
//	    			                NcSoundingLayer2 stndt = tempSoundingList.get(0);
//	    			                NcSoundingLayer2 stndb = tempSoundingList.get(1);
//	    			                switch ( verticalCoordinate ){
//	    			                case PRESSURE:
//	    			                	depth = stndb.getPressure() - stndt.getPressure();
//	    			                break;
//	    			                case TEMPERATURE:
//	    			                	depth =     PRLibrary.prThta( stndt.getTemperature(), stndt.getPressure() )   
//	    			                	              - PRLibrary.prThta( stndb.getTemperature(), stndb.getPressure() );
//	    			                break;
//	    			                case HEIGHT:
//	    			                	depth  = stndt.getGeoHeight() - stndb.getGeoHeight();
//	    			                break;
//	    			                default:
//	    			                break;
//	    			                }
//	    			}
//	    		}
//	    	}
//	    	return depthSoundingDataList;
//	    }
	    
}
