package gov.noaa.nws.ncep.edex.uengine.tasks.profile;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.measure.unit.SI;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

//import edu.emory.mathcs.backport.java.util.Collections;
import gov.noaa.nws.ncep.edex.common.metparameters.AirTemperature;
import gov.noaa.nws.ncep.edex.common.metparameters.Amount;
import gov.noaa.nws.ncep.edex.common.metparameters.DewPointTemp;
import gov.noaa.nws.ncep.edex.common.metparameters.HeightAboveSeaLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.Omega;
import gov.noaa.nws.ncep.edex.common.metparameters.PressureLevel;
import gov.noaa.nws.ncep.edex.common.metparameters.RelativeHumidity;
import gov.noaa.nws.ncep.edex.common.metparameters.SpecificHumidity;
import gov.noaa.nws.ncep.edex.common.metparameters.WindDirection;
import gov.noaa.nws.ncep.edex.common.metparameters.WindSpeed;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer2;

/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.rsc.NsharpMapResource This java class performs
 * the NSHARP Resource functions. This code has been developed by the SIB for
 * use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 08/21/2010	301			T. Lee		Initial coding
 * 09/15/2010	301			C. Chen		Added DB retrieval
 * 09/22/2010	301			T. Lee		Added UAIR merging algorithm
 * 11/05/2010   301         C. Chen     Minor changes to fix index out of bound issue
 * 11/15/2010   301         C. Chen     fix a index out of bound bug
 * 12/2010		   301			T. Lee/NCEP	Re-factored for BUFRUA
 * 5/10/2011     301         C. Chen     added rhToDewpoint(), tempToVapr()
 * 10/06/2011   465        Archana      redesigned the merge sounding to use 
 *                                                     the met parameters
 * 10/24/2011    465       Archana      Removed several debug statements. 
 *                                                    Used the Amount class to set values for the Met parameters   
 *  02/22/2012             C Chen       fixed minor bugs                                                                                                    
 *  02/28/2012               Chin Chen   modify several sounding query algorithms for better performance
 *	8/2012					T. Lee/NCEP	Removed missing wind interpolation
 *	8/2012					T. Lee/NCEP	Fixed max wind merging; May fix NSHARP EL calculation
 * </pre>
 * 
 * @author T. Lee
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MergeSounding2 implements ISerializableObject {
	/**
	 * 
	 */
	@DynamicSerializeElement
	private static final long serialVersionUID = 143076880949062764L;

	@DynamicSerializeElement
	final float RMISSD = -9999f;
	
	@DynamicSerializeElement
	final int IMISSD = -9999;

	/**
	 * Default constructor
	 */
	public MergeSounding2() {
	}

	/*
	 * Process native sounding data. Convert specific humidity to dew point
	 * temperature then compute the moist height.
	 */
	public List<NcSoundingLayer2> nativeModelSounding(List<NcSoundingLayer2> sls, 
			float elevation) {
		spfhToDewpoint(sls);
		constructHeight(sls, elevation);
		return sls;
	}

	/*
	 * Process upper air sounding data. Note that TTAA is the original/sorted
	 * data, while MAN is the TTAA without underground data and MAN_D is the 
	 * TTAA for display, i.e., the first level is the surface level, any under
	 * -ground levels will be above the surface level.
	 */
	public List<NcSoundingLayer2> mergeUairSounding(String level,
			List<NcSoundingLayer2> ttaa, List<NcSoundingLayer2>ttbb, 
			List<NcSoundingLayer2> ttcc, List<NcSoundingLayer2>ttdd,
			List<NcSoundingLayer2> ppaa, List<NcSoundingLayer2>ppbb,
			List<NcSoundingLayer2> ppcc, List<NcSoundingLayer2>ppdd,
			List<NcSoundingLayer2> trop_a, List<NcSoundingLayer2> trop_c, 
			List<NcSoundingLayer2> wmax_a, List<NcSoundingLayer2> wmax_c,
			float elevation ) {
		//System.out.println ( "From mergeUairSounding " );
		List<NcSoundingLayer2> sndata = new ArrayList<NcSoundingLayer2>();
		List<NcSoundingLayer2> man = null;
		
		/*///for debug
		//System.out.println("TTAA:");
		for (NcSoundingLayer2 l: ttaa){
			//System.out.println("P="+l.getPressure()+"H="+l.getGeoHeight()+"T="+l.getTemperature()+"D="+l.getDewpoint()+"Wd="+l.getWindDirection()+"Ws="+l.getWindSpeed());
		}
		//System.out.println("TTBB:");
		for (NcSoundingLayer2 l: ttbb){
			//System.out.println("P="+l.getPressure()+"H="+l.getGeoHeight()+"T="+l.getTemperature()+"D="+l.getDewpoint()+"Wd="+l.getWindDirection()+"Ws="+l.getWindSpeed());
		}
		//System.out.println("PPAA:");
		for (NcSoundingLayer2 l: ppaa){
			//System.out.println("P="+l.getPressure()+"H="+l.getGeoHeight()+"T="+l.getTemperature()+"D="+l.getDewpoint()+"Wd="+l.getWindDirection()+"Ws="+l.getWindSpeed());
		}
		//System.out.println("PPBB:");
		for (NcSoundingLayer2 l: ppbb){
			//System.out.println("P="+l.getPressure()+"H="+l.getGeoHeight()+"T="+l.getTemperature()+"D="+l.getDewpoint()+"Wd="+l.getWindDirection()+"Ws="+l.getWindSpeed());
		}*/
		// Return the specific levels requested by users
		if ( ttaa.size() > 0 ) {
			Collections.sort(ttaa, new reverseSortByPressure());
            //System.out.println("TTAA sounding: ");
//			for ( NcSoundingLayer2 soundLy2 : ttaa ){
//			   System.out.print(soundLy2.getPressure().getValue().doubleValue() + " , ");
//	        }
			//System.out.println();
			if (level.toUpperCase().equalsIgnoreCase("MAN")) {
				return ttaa;
			}	
			man = removeUnderGround(ttaa);

		} else {
			man=ttaa;
			if (level.toUpperCase().equalsIgnoreCase("MAN")) {
				return setMissing();
			}
			
		}


		// Sorting the data
		
		if ( ttbb.size() > 0) {
			Collections.sort(ttbb, new reverseSortByPressure());
            //System.out.println("TTBB sounding: ");
//			for ( NcSoundingLayer2 soundLy2 : ttbb ){
//			   System.out.print(soundLy2.getPressure().getValue().doubleValue() + " , ");
//	        }
			//System.out.println();
		}

		if ( ttcc.size() > 0) {
			Collections.sort(ttcc, new reverseSortByPressure());
            //System.out.println("TTCC sounding: ");
//			for ( NcSoundingLayer2 soundLy2 : ttcc ){
//			   System.out.print(soundLy2.getPressure().getValue().doubleValue() + " , ");
//	        }
		}

		if ( ttdd.size() > 0) {
			Collections.sort(ttdd, new reverseSortByPressure());
            //System.out.println("TTDD sounding: ");
//			for ( NcSoundingLayer2 soundLy2 : ttdd ){
//			   System.out.print(soundLy2.getPressure().getValue().doubleValue() + " , ");
//	        }
			//System.out.println();
		}
		
		if ( ppaa.size() > 0 ) {
			if (checkWindData(ppaa)) {
				Collections.sort(ppaa, new MergeSounding2.sortByHeight());
	            //System.out.println("PPAA sounding: ");
//				for ( NcSoundingLayer2 soundLy2 : ppaa ){
//				   System.out.print(soundLy2.getPressure().getValue().doubleValue() + " , ");
//		        }
			} else {
				Collections.sort(ppaa, new MergeSounding2.reverseSortByPressure());
	            //System.out.println("PPAA sounding: ");
//				for ( NcSoundingLayer2 soundLy2 : ppaa ){
//				   System.out.print(soundLy2.getPressure().getValue().doubleValue() + " , ");
//		        }
			}
			//System.out.println();
		} 

		
		if ( ppcc.size() > 0 ) {
			if (checkWindData(ppcc)) {
				Collections.sort(ppcc, new MergeSounding2.sortByHeight());
			} else {
				Collections.sort(ppcc, new MergeSounding2.reverseSortByPressure());
			}
            //System.out.println("PPCC sounding: ");
//			for ( NcSoundingLayer2 soundLy2 : ppcc ){
//			   System.out.print(soundLy2.getPressure().getValue().doubleValue() + " , ");
//	        }
			//System.out.println();
		} 	
		
		if ( ppbb.size() > 0 ) {
			if (checkWindData(ppbb)) {
				Collections.sort(ppbb, new MergeSounding2.sortByHeight());
			} else {
				Collections.sort(ppbb, new MergeSounding2.reverseSortByPressure());
			}
            //System.out.println("PPBB sounding: ");
//			for ( NcSoundingLayer2 soundLy2 : ppbb ){
//			   System.out.print(soundLy2.getPressure().getValue().doubleValue() + " , ");
//	        }
			//System.out.println();
		} 

		if ( ppdd.size() > 0 ) {

			if (checkWindData(ppdd)) {
				Collections.sort(ppdd, new MergeSounding2.sortByHeight());
			} else {
				Collections.sort(ppdd, new MergeSounding2.reverseSortByPressure());
			}

            //System.out.println("PPDD sounding: ");
//			for ( NcSoundingLayer2 soundLy2 : ppdd ){
//			   System.out.print(soundLy2.getPressure().getValue().doubleValue() + " , ");
//	        }
			//System.out.println();
		} 
		
		// Find surface data, return if users request surface data only.
		try{
		NcSoundingLayer2 sl = new NcSoundingLayer2();
		
		sl = getSurfaceData(man, ttbb, ppbb, elevation);	
		sndata.add(0, sl);
		//System.out.println ( "sndataSize in mergeUairSounding(), after calling getSurfaceData(): " + sndata.size() );		
		if ( isNumber(level) >= 0 ) {			
			if ( equal(0.f, Float.valueOf(level.trim()).floatValue()) ||
					equal(sl.getPressure().getValue().floatValue(), Float.valueOf(level.trim()).floatValue())) {
				//System.out.println("Returning surface data....");
				return sndata;
			}
		}

		// Merge mandatory data
		//System.out.println ( "Just before calling mergeMandatory() " );
		mergeMandatory(man, ttcc, sndata);
		int sndataSize = sndata.size();
		//System.out.println ( "sndataSize in mergeUairSounding(), after calling mergeMandatory(): " + sndata.size() );
		// Check if the single level is mandatory or not
		if ( isNumber(level) >= 0 ) {		
			
			for ( int kk = 0; kk < sndataSize; kk++) {
				if ( equal(Float.valueOf(level.trim()).floatValue(), sndata.get(kk)
						.getPressure().getValue().floatValue())) {
					sl.setPressure(sndata.get(kk).getPressure());
					sl.setTemperature(sndata.get(kk).getTemperature());
					sl.setDewpoint(sndata.get(kk).getDewpoint());
					sl.setWindDirection(sndata.get(kk).getWindDirection());
					sl.setWindSpeed(sndata.get(kk).getWindSpeed());
					sl.setGeoHeight(sndata.get(kk).getGeoHeight());
					sndata.clear();
					sndata.add(sl);	
					//System.out.println( "single layer of sounding added to sndata"  );
					return sndata;
				}
			}
		}

		// Merge mandatory winds
		mergeMandatoryWinds (ppaa, ppcc, sndata);
		//System.out.println ( "sndataSize in mergeUairSounding(), after calling mergeMandatoryWinds(): " + sndata.size() );
		// Merge tropopause
		mergeTropSigTemp (trop_a, trop_c, sndata);
		//System.out.println ( "sndataSize in mergeUairSounding(), after calling" +
//				" mergeTropSigTemp() for merging trop_a and trop_c: " + sndata.size() );
		// Merge TTBB
		mergeTropSigTemp (ttbb, ttdd, sndata);
		//System.out.println ( "sndataSize in mergeUairSounding(), after calling" +
//				" mergeTropSigTemp() for merging ttbb and ttdd: " + sndata.size() );
		// Construct height for ttbb
		constructTtbbHeight(sndata);
		//System.out.println ( "sndataSize in mergeUairSounding(), after calling" +
//				" constructTtbbHeight(): " + sndata.size() );
		// Merge significant winds on pressure surfaces
		if (!checkWindData(ppbb)) {
			mergeSigMaxWindOnPressure(ppbb,ppdd,sndata);
			//System.out.println ( "sndataSize in mergeUairSounding(), after calling" +
//			" mergeSigMaxWindOnPressure() for merging ppbb and ppdd: " + sndata.size() );
		}

		mergeSigMaxWindOnPressure(wmax_a,wmax_c,sndata);
		//System.out.println ( "sndataSize in mergeUairSounding(), after calling" +
//		" mergeSigMaxWindOnPressure() for merging wmax_a and wmax_c: " + sndata.size() );
		constructPpbbHeight(sndata);
		//System.out.println ( "sndataSize in mergeUairSounding(), after calling" +
//				" constructPpbbHeight()" + sndata.size() );
		if (checkWindData(ppbb)) {
			mergeSigWindOnHeight(ppbb,ppdd,sndata);
			//System.out.println ( "sndataSize in mergeUairSounding(), after calling" +
//					" mergeSigWindOnHeight() for merging ppbb and ppdd: " + sndata.size() );
			constructPpbbPressure(sndata);	
			//System.out.println ( "sndataSize in mergeUairSounding(), after calling" +
//					" constructPpbbPressure()" + sndata.size() );
		}

		// Reorder sounding profile so the first level is the surface data.
		// No need to reorder now becuz surface data is always the 1st level.
		//reOrderSounding(sndata);

		// Interpolate missing temperature, dew point and winds.
		constructMissing(1,sndata);
		constructMissing(2,sndata);
		//constructMissing(3,sndata);

		// Return single level or add underground mandatory data to the sounding profile
		if ( isNumber (level) == 0 ) {
			float rlev = new Integer(Integer.parseInt(level.trim())).floatValue();
//			System.out.println("Level is 0");
			return getSingLevel(rlev, sndata);
		} else if ( isNumber (level) == 1 ) {
//			System.out.println("Level is 1");
			float rlev = new Float(Float.parseFloat(level.trim()));
			return getSingLevel(rlev, sndata);
		} else {
//			System.out.println("Level is neither 0 nor 1");
			return addUnderGround(ttaa,sndata);
		}
		}
		catch ( Exception e ){
			e.printStackTrace();
		}
		
		return null;
	}

	/*
	 * Check an alpha-numerical string is a number or characters. 
	 */
	public int isNumber (String level) {
		try {
			if (Integer.parseInt(level) >=0 ) {
				return 0;
			} else {
				return -1;
			}
		} catch (NumberFormatException nfe1){
			try { 
				if (Float.parseFloat(level) >= 0.f) {
					return 1;
				} else {
					return -1;
				}				
			} catch (NumberFormatException nfe2) {
				try {
					if (Double.parseDouble(level) >= 0.) {
						return 2;						
					} else {
						return -1;
					}
				} catch (NumberFormatException nfe3) {
					return -1;
				}	
			}		
		}
	}

	/*
	 * convert specific humidity to dew point temperature.
	 */
	@DynamicSerializeElement
	float elevation;

	public List<NcSoundingLayer2> spfhToDewpoint(List<NcSoundingLayer2> sndata) {
		float spfh, pres;
		float dwpc = RMISSD;
		
		for ( NcSoundingLayer2 layer: sndata) {
			if (  ( layer.getDewpoint() != null ) 
					&& layer.getDewpoint().getValue().floatValue() == RMISSD) {
				SpecificHumidity specificHumidity = layer.getSpecificHumidity();
				PressureLevel pressureLevel = layer.getPressure();

				if ( specificHumidity != null && pressureLevel != null ){
					spfh = specificHumidity.getValue().floatValue();
					pres = pressureLevel.getValue().floatValue();
				if (spfh == RMISSD || pres == RMISSD || spfh <= 0.f
						|| pres <= 0.f) {
					continue;
				} else {

					float rmix = spfh / (1.f - spfh);
					//TODO : replace this piece of logic with a call to PRLibrary.prDwpt()?? 
					//might lead to minor differences in values
					// expected, since prDwpt() uses double instead of float.
					float e = (pres * rmix) / (.62197f + rmix);
					e = e / (1.001f + ((pres - 100.f) / 900.f) * .0034f);
					dwpc = (float) (Math.log(e / 6.112) * 243.5 / (17.67 - Math
							.log((e / 6.112))));
					DewPointTemp dewPoint;
					try {
						dewPoint = new DewPointTemp();
						dewPoint.setValue( new Amount ( dwpc, SI.CELSIUS ) );
						layer.setDewpoint( dewPoint);
					} catch (Exception e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
//					dewPoint.setValue( dwpc, "℃" );

					////System.out.println("spfhToDewpoint dwpc: " + dwpc);
				}
				}
			}
		}
		return sndata;
	}
	/*
	 *  computes DWPC from TMPC and RELH
	 *  Note: If DWPC is less than -190 degrees C, it is treated as		
	 *  missing data
	 *  Code is based on GEMPAK's prrhdp.f 
	 */
	public List<NcSoundingLayer2> rhToDewpoint(List<NcSoundingLayer2> sndata) {
		float rh, vapr,vaps, temp;
		float dwpc = RMISSD;
		
		for ( NcSoundingLayer2 layer: sndata ) {
			AirTemperature temperature            = layer.getTemperature();
			RelativeHumidity relativeHumidity = layer.getRelativeHumidity(); 
			DewPointTemp dewpoint                  = layer.getDewpoint(); 
			if ( temperature != null && relativeHumidity != null && dewpoint != null  ){
				
			if ( ! dewpoint.hasValidValue() ) {
				rh      = relativeHumidity.getValue().floatValue();
				temp = temperature.getValue().floatValue();

				if (rh == RMISSD || temp == RMISSD ) {
					continue;
				} else {
					vaps = tempToVapr(temp);
					vapr = rh * vaps /100;
					if( vapr < Math.exp ( -30 ) )
						continue;
					else {
						dwpc = (float) (243.5 * ( Math.log(6.112) - Math.log(vapr)) / 
								(Math.log(vapr) - Math.log(6.112)-17.67));
						dewpoint.setValue( new Amount ( dwpc, SI.CELSIUS ));
						layer.setDewpoint( dewpoint );
						////System.out.println("rhToDewpoint dwpc: " + dwpc);
					}
				}
			}
		}
		
	}
		return sndata;
	}
	/*
	 *  computes VAPR from TMPC 
	 *  Code is based on GEMPAK's prvapr.f 
	 */
	private float tempToVapr(float temp){
		return(float)(6.112 * Math.exp((17.67* temp)/(temp+243.5)));
	}

	private void constructHeight(List<NcSoundingLayer2> sndata, float elev) {

		/*
		 * For native model sounding, using hypsometric equation to build height
		 */
		elevation = elev;
		int lev = sndata.size();
		float tb = RMISSD, tdb = RMISSD, pb = RMISSD;
		float tt = RMISSD, tdt = RMISSD, pt = RMISSD;
		float dwptsf, psfc, tmpcsf, scaleh, mhgt = RMISSD;

		for (int k = 0; k < lev; k++) {

			AirTemperature temperature     =  sndata.get(k).getTemperature();
			DewPointTemp dewpoint          =  sndata.get(k).getDewpoint();
			PressureLevel    pressureLevel  =  sndata.get(k).getPressure();
			if ( temperature != null && dewpoint != null && pressureLevel != null ) {
			if (k == 0) {
				tmpcsf = temperature.getValue().floatValue();
				dwptsf = dewpoint.getValue().floatValue();
				psfc     = pressureLevel.getValue().floatValue();
				tb = tmpcsf;
				tt = tmpcsf;
				tdb = dwptsf;
				tdt = dwptsf;
				pb = psfc;
				pt = psfc;

				scaleh = scaleHeight(tb, tt, tdb, tdt, pb, pt);
				mhgt = moistHeight(elevation, pb, pt, scaleh);
			} else {
//				tt = sndata.get(k).getTemperature();
//				tdt = sndata.get(k).getDewpoint();
//				pt = sndata.get(k).getPressure();
				tt   = temperature.getValue().floatValue();
				tdt = dewpoint.getValue().floatValue();
				pt  = pressureLevel.getValue().floatValue();
				scaleh = scaleHeight(tb, tt, tdb, tdt, pb, pt);

				mhgt = moistHeight(mhgt, pb, pt, scaleh);
				tb = tt;
				tdb = tdt;
				pb = pt;

			}
			HeightAboveSeaLevel height;
			try {
				height = new HeightAboveSeaLevel();
				height.setValueAs( mhgt, "m" );
				sndata.get( k ).setGeoHeight( height );
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			
		}
		}
	}

	/*
	 * Compute moist height.
	 */
	private float moistHeight(float zb, float pb, float pt, float scale) {
		//System.out.println("From moistHeight: ");
		if (zb == RMISSD || pb == RMISSD || pt == RMISSD || scale == RMISSD) {
			return RMISSD;
		} else {
			//System.out.println("the computed moistHeight is  " + (float) (zb + scale * Math.log(pb / pt)));
			return (float) (zb + scale * Math.log(pb / pt));
		}
	}

	/*
	 * Compute scale height.
	 */
	private float scaleHeight(float tb, float tt, float tdb, float tdt,
			float pb, float pt) {
		//System.out.println("From scaleHeight: " );		
		final float RDGAS = 287.04f, GRAVTY = 9.80616f, RKAP = RDGAS / GRAVTY;
		if (tb == RMISSD || tt == RMISSD || pb == RMISSD || pt == RMISSD) {
			return RMISSD;
		} else {
			float tvb = virtualTemperature(tb, tdb, pb);
			float tvt = virtualTemperature(tt, tdt, pt);
			//System.out.println("tvb = " + tvb);
			//System.out.println("tvt = " + tvt);
			if (tvb == RMISSD || tvt == RMISSD) {
				return RMISSD;
			} else {
				float tav = (tvb + tvt) / 2.0f;

				//System.out.println("tav = " + tav);
				//System.out.println("RKAP * tav = " + RKAP * tav);
				return (RKAP * tav);
			}
		}
	}

	/*
	 * Compute virtual temperature
	 */
	private float virtualTemperature(float tt, float td, float pres) {
		if (tt == RMISSD || pres == RMISSD) {
			return RMISSD;
		} else if (td == RMISSD) {
			return celciusToKevin(tt);
		} else {
			float tmpk = celciusToKevin(tt);
			float rmix = mixingRatio(td, pres);
			if (rmix == RMISSD) {
				return celciusToKevin(tt);
			} else {
				return tmpk * (1.f + .001f * rmix / .62197f)
				/ (1.f + .001f * rmix);
			}
		}
	}

	/*
	 * Convert Celcius to Kelvin.
	 */
	@DynamicSerializeElement
	float TMCK = 273.15f;

	private float celciusToKevin(float tc) {
		if (tc == RMISSD) {
			return RMISSD;
		} else {
			return (tc + TMCK);
		}
	}

	/*
	 * Compute mixing ratio from DWPC and PRES.
	 */
	private float mixingRatio(float td, float pres) {
		if (td == RMISSD || pres == RMISSD) {
			return RMISSD;
		} else {
			float vapr = vaporPressure(td);
			if (vapr == RMISSD) {
				return RMISSD;
			}

			float corr = (1.001f + ((pres - 100.f) / 900.f) * .0034f);

			float e = corr * vapr;
			if ( e > ( .5f * pres ) ) {
				return RMISSD;
			} else {
				return .62197f * ( e / ( pres - e ) ) * 1000.f;
			}
		}
	}

	/*
	 * Compute vapor pressure from DWPC.
	 */
	private float vaporPressure(float td) {
		if (td == RMISSD) {
			return RMISSD;
		} else {
			return ( 6.112f * ( float ) Math.exp( ( 17.67 * td ) / ( td + 243.5 ) ) );
		}
	}

	/*
	 * Merge observed sounding data
	 */

	/*
	 * Check significant wind data (PPBB/PPDD) to see if the data is reported on
	 * pressure or height surfaces. Return TRUE if reported on height. The
	 * input can be PPBB or PPDD winds. (MR_CHKW)
	 * 
	 * Note that this is coded different from MR_CHKW, in that it will set zwind 
	 * to false only if pressure is less than 0.  An odd logic.
	 */
	public boolean checkWindData(List<NcSoundingLayer2> sndata) {
		boolean zwind = true;
		int sndataSize = sndata.size();
		for (int kk = 0; kk < sndataSize; kk++) {
			PressureLevel pressure = sndata.get(kk).getPressure();
			if ( pressure != null && pressure.hasValidValue() ) {	
				zwind = false;		
			}
		}
		return zwind;
	}

	/*
	 * Find surface data. (MR_SRFC)
	 */
	@DynamicSerializeElement
	final int NPARMS = 7;


	public NcSoundingLayer2 getSurfaceData(List<NcSoundingLayer2> man, 
			List<NcSoundingLayer2> ttbb, List<NcSoundingLayer2> ppbb, float elevation ) 
	    {
		float pres = RMISSD;
		NcSoundingLayer2 sl_sfc = null;
		try {
		 sl_sfc = new NcSoundingLayer2();

		/*
		 * Check for surface information in mandatory data.
		 */
		if (man.size() < 1) {
			for (int k = 0; k < NPARMS; k++) {
				/*
				 * ( Non- Javadoc )
				 * Storing a new object sets the corresponding member 
				 * in sl_sfc to the default missing value
				 */
				sl_sfc.setPressure(new PressureLevel() ); 
				sl_sfc.setTemperature( new AirTemperature() );
				sl_sfc.setDewpoint( new DewPointTemp() );
				sl_sfc.setWindDirection( new WindDirection() );
				sl_sfc.setWindSpeed( new WindSpeed() );
				sl_sfc.setGeoHeight( new HeightAboveSeaLevel() );
				sl_sfc.setOmega( new Omega() );
				return sl_sfc;
			}
		} else {
			
			// If surface pressure is higher than 1080mb, set to missing.
			// Note that GEMPAK sets it to 1060mb.
			PressureLevel pressure = man.get(0).getPressure();
			if ( pressure != null ){   
				pres = pressure.getValueAs ( NcUnits.MILLIBAR ).floatValue();
				PressureLevel newPressure = new PressureLevel();
			   if (pres > 1080.) {
				    sl_sfc.setPressure( newPressure );
			    } else {
			    	newPressure.setValue( new Amount (pres, NcUnits.MILLIBAR) );
				    sl_sfc.setPressure( newPressure );
			    }	
			}
			sl_sfc.setTemperature( man.get(0).getTemperature() );
			sl_sfc.setDewpoint( man.get(0).getDewpoint() );
			sl_sfc.setWindDirection( man.get(0).getWindDirection() );
			sl_sfc.setWindSpeed( man.get(0).getWindSpeed() );
			HeightAboveSeaLevel height = new HeightAboveSeaLevel();
            height.setValue( elevation, SI.METER );
			sl_sfc.setGeoHeight( height );
			sl_sfc.setOmega( man.get( 0 ).getOmega() );
		}

		/*
		 * Find the first reporting mandatory level above the surface.
		 */
		float pman = RMISSD;
		int iman = 1;
		while (pman == RMISSD && iman < man.size()) {
			PressureLevel presLevel          = man.get(iman).getPressure();
			AirTemperature temperature   = man.get(iman).getTemperature();
			HeightAboveSeaLevel  height = man.get(iman).getGeoHeight(); 
			if (  ( presLevel != null && presLevel.hasValidValue() ) 
					&& ( temperature != null &&  temperature.hasValidValue() )
					&&  ( height != null && height.hasValidValue() ) ) {
				PressureLevel pressureMandatory = man.get( 0 ).getPressure();
				if ( pressureMandatory != null )
				  pman =  pressureMandatory.getValue().floatValue() ;
			}
			iman++;
		}

		/*
		 * If surface pressure is missing or is less than first reporting
		 * mandatory level, set all surface data to missing
		 */
		if (pres == RMISSD || (pres < pman && pman != RMISSD)) {
			sl_sfc = setMissing().get(0);
		}

		/*
		 * If the surface significant temperature data is not missing, use it to
		 * replace the surface pressure, temperature and dewpoint. The check for
		 * significant level pressure to be less than or equal to pman eliminates
		 * using wildly erroneous data.
		 */
        if ( ttbb != null && ttbb.size() >= 1 ){
               PressureLevel  ttbbPressure =  ttbb.get( 0 ).getPressure();
               AirTemperature  ttbbTemperature =  ttbb.get( 0 ).getTemperature();
               if ( ( ttbbPressure != null && ttbbPressure.hasValidValue() ) 
            		   && ( ttbbTemperature != null && ttbbTemperature.hasValidValue())){
            	            PressureLevel slSfcPressure = sl_sfc.getPressure();
            	            if ( slSfcPressure != null )
            	                pman = slSfcPressure.getValue().floatValue();
               }
        }
        if ( ttbb.size() >= 1 ){
		PressureLevel ttbbPressure = ttbb.get( 0 ).getPressure();
		AirTemperature tempVal = ttbb.get( 0 ).getTemperature();
        
		if ( ( ttbbPressure != null && ttbbPressure.hasValidValue()) 
				&& ( tempVal != null) && tempVal.hasValidValue())  {

			
			pman = sl_sfc.getPressure().getValue().floatValue();
			float psql = ttbbPressure.getValue().floatValue();
			if (pman == RMISSD || equal(pman, psql)) {
				sl_sfc.setPressure(ttbb.get(0).getPressure());
				sl_sfc.setTemperature(ttbb.get(0).getTemperature());
				sl_sfc.setDewpoint(ttbb.get(0).getDewpoint());
			}
		}
		}

		/*
		 * If the first significant level wind data is surface information, use
		 * it to replace the surface data if the pressure is at surface.
		 */

		// PPBB reported on P-surfaces.
		if ( ppbb.size() > 0 ){
	    	HeightAboveSeaLevel ppbbGeoHeight =  ppbb.get(0).getGeoHeight();
	      	WindDirection  ppbbWindDir =  ppbb.get(0).getWindDirection();
		    PressureLevel ppbbPressure =  ppbb.get(0).getPressure();
			if ( checkWindData( ppbb ) ) {

				if ( ( ppbbGeoHeight != null 
						&& ppbbGeoHeight.getValue().floatValue() == 0 )
						&& ( ppbbWindDir != null && ppbbWindDir.hasValidValue()) ) {
					sl_sfc.setWindDirection(ppbb.get(0).getWindDirection());
					sl_sfc.setWindSpeed(ppbb.get(0).getWindSpeed());
				}

			} else {
				if ( ( ppbbPressure != null && ppbbPressure.hasValidValue() )
						&& ( ppbbWindDir != null && ppbbWindDir.hasValidValue()) )  {
					pman = sl_sfc.getPressure().getValue().floatValue();
					float psgl = Math.abs( ppbbPressure.getValue().floatValue() );
					if (pman == RMISSD || equal(pman, psgl)) {
						PressureLevel newPressure2 = new PressureLevel();
						newPressure2.setValue( new Amount ( psgl, NcUnits.MILLIBAR) );
						sl_sfc.setPressure( newPressure2 );
						sl_sfc.setWindDirection( ppbbWindDir );
						sl_sfc.setWindSpeed( ppbb.get(0).getWindSpeed() );
					}
				}
			}
		}

		}
		catch(Exception e){
			e.printStackTrace();
		}
		/*
		 * Add surface data to station data.
		 */
		return sl_sfc;
	}


	private boolean equal(float x, float y) {
		final float RDIFFD = .0001f;
		if ( (x + y) == 0. ) {
			return Math.abs(x-y) < RDIFFD;
		} else {			
			return Math.abs(x - y) / Math.abs( (x+y) / 2.) < RDIFFD;
		}
	}

	/*
	 * Merge the mandatory below 100 mb data and the mandatory above data.  sndata
	 * has surface observation ONLY.
	 * (MR_MAND)
	 */

	public void mergeMandatory(
			List<NcSoundingLayer2> man_a, List<NcSoundingLayer2> man_c,
			List<NcSoundingLayer2> sndata  ) {
         //System.out.println("From mergeMandatory \n Size of sndata: " + sndata.size());
		float plast;
		if ( man_a.size() < 1 && man_c.size() < 1 ) {
			return;
		}
		
		PressureLevel sndataPressure =  sndata.get(0).getPressure();
		if ( sndataPressure != null && (! sndataPressure.hasValidValue())) {
			plast = 2000.f;
		} else {
			plast = sndataPressure.getValue().floatValue();
		}

		/*
		 * Move the mandatory data below 100mb to the output array, sndata.
		 * Check that pressure is not missing and is decreasing.
		 */
		float pres = RMISSD;
		if ( man_a.size() > 0 ) {
			for (int kk = 0; kk < man_a.size(); kk++) {
				PressureLevel  mandatoryPressure = man_a . get( kk ) . getPressure();
				AirTemperature mandatoryTemp   = man_a.get(kk).getTemperature();
				WindDirection mandatoryWinDir  = man_a.get(kk).getWindDirection();
				if ( mandatoryPressure != null )
				       pres = mandatoryPressure.getValue( ).floatValue();
				if (   pres < plast && pres != RMISSD
						&& (  ( mandatoryTemp != null && mandatoryTemp.hasValidValue() )  
								   || ( mandatoryWinDir != null && mandatoryWinDir.hasValidValue() ) ) ) {
					addDataToList(kk, man_a, sndata);
					plast = pres;
				}
			}
		}

		/*
		 * Move the mandatory data above 100 mb to the output array.
		 */
		if ( man_c.size() > 0 ) {

			for (int kk = 0; kk < man_c.size(); kk++) {
				PressureLevel manPressure      =  man_c.get(kk).getPressure();
				AirTemperature manAirTemp =  man_c.get(kk).getTemperature();
				
				if ( manPressure != null  )
  				        pres = manPressure.getValue().floatValue() ;
				
				if ( pres < plast && pres != RMISSD
						&& ( manAirTemp != null && manAirTemp.hasValidValue() ) ) {
					             addDataToList(kk, man_c, sndata);
					             plast = manPressure . getValue().floatValue() ;
				}
			}
		}
		//System.out.println("size of sndata at the end of mergeMandatory: " + sndata.size());
	}

	/*
	 * Merge the mandatory below 100 mb wind data and the mandatory above wind
	 * data. (MR_MANW)
	 */
	public void mergeMandatoryWinds( List<NcSoundingLayer2> man_wa, 
			List<NcSoundingLayer2> man_wc, List<NcSoundingLayer2> sndata ) {
		int sndataSize = sndata.size();		
        int manscSize = man_wc.size();
		if ( man_wa.size() < 1 && man_wc.size() < 1 ) {
			return;
		}

		/*
		 * Append data.
		 */
		if (man_wc.size() > 0) {
			for (int kk = 0; kk < man_wc.size(); kk++) {
				man_wa.add(man_wc.get(kk));
			}
		}
		/*
		 * Loop through mandatory wind data.
		 */
		for (int lev = 0; lev < man_wa.size(); lev++) {

			/*
			 * If this is the correct level, add wind data.
			 */
			boolean found = false;
			PressureLevel manPressure      =  man_wa.get( lev ).getPressure();
			AirTemperature manAirTemp =  man_wa.get( lev ).getTemperature();
			float ppp = RMISSD; 
			if ( manPressure != null )
			     ppp = manPressure.getValue().floatValue();
			
			for (int kk = 0; kk < sndataSize && !found; kk++ ) {
				  PressureLevel sndataPressure =  sndata.get(kk).getPressure();
				  float pres = RMISSD;
				  if ( sndataPressure != null )
				     pres = sndataPressure.getValue().floatValue();
				
				  if (  (  ppp != RMISSD && pres != RMISSD ) &&  equal ( ppp, pres ) ) {
					  WindDirection sndataWindDir = sndata.get(kk).getWindDirection();
					   if (  ( sndataWindDir != null  &&  sndataWindDir.hasValidValue() ) ) {
						   sndata.get(kk).setWindDirection (man_wa.get(lev).getWindDirection());
						   sndata.get(kk).setWindSpeed(man_wa.get(lev).getWindSpeed());
					   }
				 	       found = true;
				  }
			}

			/*
			 * If not found, add to the list
			 */
			if ( !found ) {
				 WindDirection manwaWindDir  =  man_wa.get(lev).getWindDirection();
				 float ddd = RMISSD;

				 if ( manwaWindDir != null )
				      ddd = manwaWindDir.getValue().floatValue();
				
				if ( ppp != RMISSD && ddd != RMISSD) {
					addDataToList(lev,man_wa,sndata);
				}
			}
		}
	}

	/*
	 * Merge tropopause, max wind and significant temperature data (TTBB) to the
	 * station data array. The input parameter could be tropopause data or 
	 * significant temperature data. MR_TROP & MR_SIGT
	 */
	public List<NcSoundingLayer2> mergeTropSigTemp( List<NcSoundingLayer2> trop_a, 
			List<NcSoundingLayer2> trop_c, List<NcSoundingLayer2> sndata) {
		if ( trop_a.size() < 1 && trop_c.size() < 1 ) {
			return sndata;
		}

		/*
		 * Append two lists of wind data.
		 */
		if ( trop_c.size() > 0 ) {
			for (int kk = 0; kk < trop_c.size(); kk++) {
				trop_a.add(trop_c.get(kk));
			}
		}

		for (int lev = 0; lev < trop_a.size(); lev++) {
			boolean found = false;
			PressureLevel tropaPressure = trop_a.get(lev).getPressure();
//			float ppp = RMISSD;
			float ppp = RMISSD;
			if ( tropaPressure != null )
			       ppp =  tropaPressure.getValue( ).floatValue();
//			//System.out.println("ppp = " + ppp);
			int sndataSize = sndata.size();
			for (int kk = 0; kk < sndataSize && !found; kk++) {
			    PressureLevel  sndatakkPressure = sndata.get(kk).getPressure(); 
				
			    float pres = RMISSD; 
			    	if ( sndatakkPressure != null ){
//			    		if ( NcUnits.MILLIBAR == null ){
//			    	         //System.out.println( "<<<<Unit of pressure is null");  
//			    		}
			    			pres = sndatakkPressure. getValue(  ).floatValue();
//			    			//System.out.println("pres = " + pres);
			    	}else{
			    	     //System.out.println( ">>>>sndatakkpressure is null");
			    	}
				if ( equal( ppp , pres ) ) {
					      AirTemperature sndataTemp =  sndata.get( kk ).getTemperature() ;
					// add data to missing
					if ( sndataTemp != null &&  ( ! sndataTemp.hasValidValue() ) ) {
						sndata.get(kk).setTemperature(trop_a.get(lev).getTemperature());
						sndata.get(kk).setDewpoint(trop_a.get(lev).getDewpoint());
//						//System.out.println("The temperature is =  " + sndata.get(kk).getTemperature()
//								 .getValue().floatValue());
					}


					
					WindDirection sndataWindDir =  sndata.get(kk).getWindDirection();
					if ( sndataWindDir != null && ( ! sndataWindDir.hasValidValue() ) ) {
						sndata.get(kk).setWindDirection(
								trop_a.get(lev).getWindDirection());
						sndata.get(kk).setWindSpeed(
								trop_a.get(lev).getWindSpeed());
						//System.out.println("The wind direction is =  " + sndata.get(kk).getWindDirection()
//                                .getValue().floatValue());
					}
					

					found = true;
				}
			}

			/*
			 * if not found, add to the list
			 */
			if (!found) {
				AirTemperature tropaTemp =  trop_a.get(lev).getTemperature();
				float ttt = RMISSD;
				if ( tropaTemp != null )
					   ttt = tropaTemp.getValue().floatValue();
				if (ppp != RMISSD && ttt != RMISSD) {
					addDataToList(lev,trop_a,sndata);
				}
			}

		}

		/*
		 * Sort the sounding data in descending order.
		 */
		Collections.sort(sndata, new reverseSortByPressure());
		return sndata;
	}

	/**
	 * Compute height at significant temperature levels (TTBB) using a moist
	 * hydrostatic computation. (MR_SCMZ)
	 */
	public void constructTtbbHeight(List<NcSoundingLayer2> sndata) {
		boolean mand = false;
		boolean newblock = true;
		int blev = 0, tlev = 0;
		float[] scale = new float[200];
		float pb = RMISSD; 
		float zb = RMISSD; 
		float tb = RMISSD;  
		float tdb = RMISSD;  
		float zlev = RMISSD; 
		float  plev = RMISSD;
		float pt  = RMISSD; 
		float  zt = 0.f; 
		float tt  = RMISSD; 
		float tdt = RMISSD; 
		float znew = 0.f;
		
		int sndataSize = sndata.size();
		if ( sndataSize <= 2) return;
		
		for (int nlev = 0; nlev < sndata.size(); nlev++) {
			HeightAboveSeaLevel sndataGeoHeight = sndata.get(nlev).getGeoHeight();
			PressureLevel sndataPressure                   = sndata.get(nlev).getPressure();
			AirTemperature sndataTemperature         = sndata.get(nlev).getTemperature();
//           //System.out.println("blev = " + blev );
           //System.out.println("nlev = " + nlev );
//           //System.out.println("tlev = " + tlev );
			if ( newblock ) {

				if (  ( sndataGeoHeight != null && sndataGeoHeight.hasValidValue() )
						&& ( sndataGeoHeight != null && sndataGeoHeight.getValue().floatValue() == RMISSD ) ){
          					//System.out.println("Houston...we've had a problem");
				}
				 if ( ( sndataGeoHeight != null && sndataGeoHeight.getValue().floatValue() != RMISSD )
						&& ( sndataPressure != null && sndataPressure.hasValidValue() )
						&& ( sndataTemperature != null && sndataTemperature.hasValidValue() ) ) {
					                           blev = nlev;
					                           newblock = false;				
					                           //double h = sndataGeoHeight.getValue().doubleValue();
//					                           if ( h < 0 && h != -9999 )
					                        	   //System.out.println("if newblock  - height is negative: " + h );
				}
			} else {
				if (( sndataGeoHeight != null && sndataGeoHeight.hasValidValue() )
						&& ( sndataTemperature != null && sndataTemperature.hasValidValue() )) {
                                   					tlev = nlev;
					                               mand = true;
					                               //System.out.println("tlev is now set to nlev and its value is " + tlev );
						                           //double h = sndataGeoHeight.getValue().doubleValue();
//						                           if ( h < 0 && h != -9999 )
						                        	   //System.out.println("if not newblock  - height is negative: " + h );					                               
				}
			}

			/*
			 * Compute scale height to this level
			 */
			if (mand) {
				PressureLevel snPressure                    = sndata.get(blev).getPressure();
				HeightAboveSeaLevel snGeoHeight  = sndata .get(blev).getGeoHeight();
				AirTemperature  snTemperature         = sndata.get(blev).getTemperature();
				DewPointTemp  snDewpoint              = sndata.get(blev).getDewpoint();
				
				if ( snPressure != null ){
					pb    = snPressure.getValue().floatValue();
					plev =  snPressure.getValue().floatValue();
				}
				if ( snGeoHeight != null ){
					zb    = snGeoHeight.getValue().floatValue();
					zlev =  snGeoHeight.getValue().floatValue();
				}
				if  ( snTemperature !=  null ) 
				    tb = snTemperature.getValue().floatValue();
				if ( snDewpoint != null )
					tdb =snDewpoint.getValue().floatValue();
				for (int kk = blev + 1; kk <= tlev; kk++) {
                     PressureLevel sndPressure = sndata.get(kk).getPressure();  
				    AirTemperature sndTemperature = sndata.get(kk).getTemperature();
				  	HeightAboveSeaLevel sndGeoHeight = sndata.get(kk).getGeoHeight(); 
				    DewPointTemp sndDewpoint = sndata.get(kk).getDewpoint();
				    if ( sndPressure != null )
				                pt = sndPressure.getValue().floatValue();
				    if ( sndGeoHeight != null )            
				                zt = sndGeoHeight.getValue( ).floatValue();
				    if ( sndTemperature != null )                        
				                tt = sndTemperature.getValue().floatValue();
				    if ( sndDewpoint != null )            					            
				                tdt = sndDewpoint.getValue().floatValue();
					scale[kk] = scaleHeight(tb, tt, tdb, tdt, pb, pt);
					//System.out.println("scale[" + kk + "]  = " + scale[kk]);
					znew = moistHeight(zb, pb, pt, scale[kk]);
					
					if (znew != RMISSD) {
						pb = pt;
						tb = tt;
						tdb = tdt;
						zb = znew;
//						if ( znew < 0 )
					        //System.out.println("negative moist height = " + znew );
					}
				}

				/*
				 * Compute the scaling factor so the computed moist height is
				 * consistent at the mandatory level. Then recompute the height.
				 */
				float s = (zt - zlev) / (znew - zlev);
				//System.out.println("scaling factor s = " + s);
				float zbb = zlev;
				float pbb = plev;
				for (int kk = blev + 1; kk < tlev; kk++) {
                    PressureLevel sndPressure = sndata.get(kk).getPressure();  
				    HeightAboveSeaLevel sndGeoHeight = sndata.get(kk).getGeoHeight(); 
					if ( sndPressure != null )
				    pt = sndPressure.getValue().floatValue();
					zt = sndGeoHeight.getValue().floatValue();
					
					scale[kk] = scale[kk] * s;
					//System.out.println("Now, scale[" + kk + "] = " + scale[kk]);
					znew = moistHeight(zbb, pbb, pt, scale[kk]);
					if (znew != RMISSD) {
						pbb = pt;
						zbb = znew;
						HeightAboveSeaLevel newGeoHeight;
						try {
							newGeoHeight= new HeightAboveSeaLevel();
							newGeoHeight.setValueAs(znew, "m"); //TODO: explicitly set the units? Default is meter 
							sndata.get(kk).setGeoHeight(newGeoHeight);
//							double h = sndata.get(kk).getGeoHeight().getValue().doubleValue();
//							if ( h < 0 && h != RMISSD  )
							       //System.out.println("newly computed moist height is negative " +  h );
						} catch (Exception e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}

					}
				}
				mand = false;
				newblock = true;

				if ( (tlev+1)  != sndata.size() ) {
					HeightAboveSeaLevel  sndTlevGeoHeight =  sndata.get(tlev+1).getGeoHeight();
					PressureLevel sndTlevPressure = sndata.get(tlev+1).getPressure();
					AirTemperature sndTlevTemperature = sndata.get(tlev+1).getTemperature();
					if ( ( sndTlevGeoHeight != null && ( ! sndTlevGeoHeight.hasValidValue()) )
							&& ( sndTlevPressure != null && ( sndTlevPressure.hasValidValue()) )
							&& ( sndTlevTemperature != null && ( sndTlevTemperature.hasValidValue()) )) {
						         nlev--;
						         //System.out.println("after subtracting nlev, its value is :  " + nlev );
					}			
				}
			}
		}		

		// Compute height at the missing top levels

		if ( ( tlev + 1 ) <  sndata.size() ) {
			blev = tlev;
			PressureLevel                            sndataBlevPressure       = sndata.get(blev).getPressure();
			HeightAboveSeaLevel              sndataBlevHeight          = sndata.get(blev).getGeoHeight();
			AirTemperature                        sndataBlevTemperature = sndata.get(blev).getTemperature();
			DewPointTemp                         sndataBlevDewpoint      = sndata.get(blev).getDewpoint();
			
			if ( sndataBlevPressure != null ){
				pb    = sndataBlevPressure.getValue().floatValue();
				plev =  sndataBlevPressure.getValue().floatValue();
			}
			if ( sndataBlevHeight != null ){
				zb    = sndataBlevHeight.getValue().floatValue();
				zlev =  sndataBlevHeight.getValue().floatValue();
			}
			
			if ( sndataBlevTemperature != null )
			       tb = sndataBlevTemperature.getValue().floatValue();
			
			if ( sndataBlevDewpoint != null )
			       tdb = sndataBlevDewpoint.getValue().floatValue();
			
			
			for ( int kk = tlev+1; kk <  sndata.size(); kk++ ) {
				PressureLevel                            sndatakkPressure       = sndata.get(kk).getPressure();
				HeightAboveSeaLevel              sndatakkHeight          = sndata.get(kk).getGeoHeight();
				AirTemperature                        sndatakkTemperature = sndata.get(kk).getTemperature();
				DewPointTemp                         sndatakkDewpoint      = sndata.get(kk).getDewpoint();
				
				if ( sndatakkPressure != null ){
						pt = sndatakkPressure.getValue().floatValue();
				}
				
				if ( sndatakkHeight != null )
				     zt = sndatakkHeight.getValue().floatValue();
				
				if ( sndatakkTemperature != null )
				   tt = sndatakkTemperature.getValue().floatValue();
				
				if ( sndatakkDewpoint != null )
				     tdt = sndatakkDewpoint.getValue().floatValue();
				
				float xxx = scaleHeight(tb, tt, tdb, tdt, pb, pt);
				znew = moistHeight(zb, pb, pt, xxx);
				if (znew != RMISSD) {
					HeightAboveSeaLevel znewHeight; 
					 try {
						znewHeight = new HeightAboveSeaLevel();
						znewHeight.setValue(znew); //default unit for the height is meter
						sndata.get(kk).setGeoHeight(znewHeight);
						pb = pt;
						tb = tt;
						tdb = tdt;
						zb = znew;
					 } catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				}
			}
		} 

		return;
	}

	/*
	 * Merge the significant and maximum wind data on pressure surfaces. MR_PWND
	 */
	public void mergeSigMaxWindOnPressure(List<NcSoundingLayer2> sig_wa,
			List<NcSoundingLayer2> sig_wc,List<NcSoundingLayer2> sndata) {
		NcSoundingLayer2 sl;
		/*
		 * Do nothing if wind report is reported on height surfaces.
		 */

		if ( sig_wa.size() < 1 && sig_wc.size() < 1 ) {
			return;
		}

		/*
		 * Append two lists of wind data.
		 */
		if ( sig_wc.size() > 0) {
			for (int kk = 0; kk < sig_wc.size(); kk++) {
				sig_wa.add(sig_wc.get(kk));
			}
		}

		/*
		 * Merging
		 */
		int nlevel = sndata.size();
		try {
		for (int kk = 0; kk < sig_wa.size(); kk++) {
			boolean found = false;
			
			PressureLevel sigwaKKPressure  = sig_wa.get(kk).getPressure();
			WindDirection sigwaKKWindDir = sig_wa.get(kk).getWindDirection();
			
			for (int lev = 0; lev < nlevel; lev++) {
				PressureLevel sndataLevPressure = sndata.get(lev).getPressure();

				if ( sndataLevPressure != null && sigwaKKPressure != null ){
					
				       if ( equal ( sndataLevPressure.getValue().floatValue(), 
				    		             sigwaKKPressure.getValue().floatValue())) {

					// add data to missing
				    	   WindDirection sndataLevWindDir = sndata.get(lev).getWindDirection();
					           if (sndataLevWindDir  != null && ( !sndataLevWindDir.hasValidValue())) {
						                    sndata.get(lev).setWindDirection(
								            sig_wa.get(kk).getWindDirection());
						                    sndata.get(lev).setWindSpeed(
								            sig_wa.get(kk).getWindSpeed());
					             }
					             found = true;
				   }
				}
			}

			/*
			 * if not found, add to the list.
			 */
			if (!found) {
				if ( ( sigwaKKWindDir != null && sigwaKKWindDir.hasValidValue() 
						&& sigwaKKPressure != null && sigwaKKPressure.hasValidValue() )) {

					
					sl = new NcSoundingLayer2();
					sl.setPressure(sig_wa.get(kk).getPressure());
					sl.setTemperature(sig_wa.get(kk).getTemperature());
					sl.setDewpoint(sig_wa.get(kk).getDewpoint());
					sl.setWindDirection(sig_wa.get(kk).getWindDirection());
					sl.setWindSpeed(sig_wa.get(kk).getWindSpeed());
					sl.setGeoHeight(sig_wa.get(kk).getGeoHeight());
					sl.setOmega(sig_wa.get(kk).getOmega());
					sndata.add(sl);
				}
			}
			Collections.sort(sndata, new reverseSortByPressure());
		}
		
		}
		catch ( Exception e ){
			    e.printStackTrace();
		}
		return;
	}

	/*
	 * Construct height at significant wind levels (PPBB) if reported on
	 * pressure levels. Using moist hydrostatic computation for missing height
	 * at top levels. MR_INTZ
	 */
	public void constructPpbbHeight(List<NcSoundingLayer2> sndata) {
		int tlev = 0;
		//System.out.println("From constructPpbbHeight(): " );		
		if ( sndata != null ){
		int sndataSize = sndata.size();
		for (int kk = sndataSize - 1; kk >= 0; kk--) {
			HeightAboveSeaLevel sndataKKGeoHeight =  sndata.get(kk).getGeoHeight();
			if ( sndataKKGeoHeight != null && sndataKKGeoHeight.hasValidValue() ) {
				tlev = kk;
				//System.out.println("tlev is set to " + tlev );
				break;
			}
		}

		float pb = RMISSD, pt = RMISSD, zt = RMISSD, zb = RMISSD, pres = RMISSD, hght = RMISSD;
		int next;
		
		if ( sndataSize <= 2) return;
		
		for (int kk = 0; kk < tlev; kk++) {
			
			PressureLevel                  sndataKKPressure = sndata.get(kk).getPressure(); 
			HeightAboveSeaLevel sndataKKGeoHeight =  sndata.get(kk).getGeoHeight();
			if ( sndataKKPressure != null )
				    pres = sndataKKPressure.getValue().floatValue();
			
			if ( sndataKKGeoHeight != null )
			        hght = sndataKKGeoHeight.getValue().floatValue();
			if (pres == RMISSD) {
				// DO NOTHING
			} else if (hght != RMISSD) {
				pb = pres;
				zb = hght;
				pt = 2000.f;
			} else if (pb == RMISSD) {
				// DO NOTHING
			} else {

				/*
				 * Find next level with height and then interpolate the data.
				 */
				next = kk + 1;
				while (pres <= pt) {
					HeightAboveSeaLevel sndataNextGeoHeight =  sndata.get(next).getGeoHeight(); 
					if (sndataNextGeoHeight != null &&  sndataNextGeoHeight.hasValidValue()) {
						      PressureLevel sndataNextPressureLevel          =  sndata.get(next).getPressure(); 
						      if (sndataNextPressureLevel!= null )
						                pt = sndataNextPressureLevel.getValue().floatValue();

						      zt = sndataNextGeoHeight.getValue().floatValue();
					} else {
						next++;
					}
				}
				float hhh = (float) (zb + (zt - zb)
						* (Math.log(pres / pb) / Math.log(pt / pb)));
				HeightAboveSeaLevel newGeoHeight;
				try {
					       newGeoHeight  = new HeightAboveSeaLevel();
					       newGeoHeight.setValue( hhh ); 
					       sndata.get(kk).setGeoHeight( newGeoHeight );
				      } catch (Exception e) {
					          // TODO Auto-generated catch block
					          e.printStackTrace();
				      }

			}
		}

		if (tlev == (sndataSize-1) ) {
			return;
		} else {

			/*
			 * Compute moist hydrostatic height for missing height at top
			 * levels.
			 */
			float scale = RMISSD;
			float tb = RMISSD, tdb = RMISSD;
			float tt = RMISSD, tdt = RMISSD, mhght = 0.f;
			PressureLevel sndataTlevPressure                 = sndata.get(tlev).getPressure();
			HeightAboveSeaLevel sndataTlevHeight      = sndata.get(tlev).getGeoHeight();
			AirTemperature sndataTlevTemperature       = sndata.get(tlev).getTemperature();
			DewPointTemp sndataTlevDewpoint             =  sndata.get(tlev).getDewpoint();
			
			if ( sndataTlevPressure != null ) 
			      pb = sndataTlevPressure.getValue().floatValue();
			
			if ( sndataTlevHeight != null )
			      zb = sndataTlevHeight.getValue().floatValue();
			
			if ( sndataTlevTemperature != null )
			      tb = sndataTlevTemperature.getValue().floatValue();
			
			if( sndataTlevDewpoint != null )
			     tdb = sndataTlevDewpoint.getValue().floatValue();


			for (int kk = tlev+1; kk < sndataSize; kk++) {

				HeightAboveSeaLevel              sndatakkHeight          = sndata.get(kk).getGeoHeight();

				if (sndatakkHeight != null && ( !sndatakkHeight.hasValidValue() ) ) {
					PressureLevel                            sndatakkPressure       = sndata.get(kk).getPressure();
					AirTemperature                        sndatakkTemperature = sndata.get(kk).getTemperature();
					DewPointTemp                         sndatakkDewpoint      = sndata.get(kk).getDewpoint();
					
					if ( sndatakkPressure != null )
					       pt = sndatakkPressure.getValue().floatValue();
					
					zt = sndatakkHeight.getValue().floatValue();
					
					if( sndatakkTemperature != null )
					     tt = sndatakkTemperature.getValue().floatValue();
					
					if( sndatakkDewpoint != null )
					      tdt = sndatakkDewpoint.getValue().floatValue();
					
					scale = scaleHeight(tb, tt, tdb, tdt, pb, pt);
					mhght = moistHeight(zb, pb, pt, scale);
					
					HeightAboveSeaLevel newMoistHeight ;
					try {
						newMoistHeight = new HeightAboveSeaLevel();
						newMoistHeight.setValue(mhght);
						sndata.get(kk).setGeoHeight(newMoistHeight);
						
						
						if (mhght != RMISSD) {
							pb = pt;
							zb = zt;
							tb = tt;
							tdb = tdt;
						}
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				}
			}
			return;
		}
		
		}
	}

	/*
	 * Merge significant wind on height surfaces. The argument is sndata.
	 */
	public void mergeSigWindOnHeight(List<NcSoundingLayer2> sig_wa, 
			List<NcSoundingLayer2> sig_wc,List<NcSoundingLayer2> sndata ) {

		if ( sig_wa != null && sig_wc != null && sndata != null ){
		/*
		 * The following code needs to be replaced by significant wind data from
		 * database.
		 */

		/*
		 * Do nothing if wind report is not on height surfaces.
		 */
		if ( sig_wa.size() < 1 && sig_wc.size() < 1 ) {
			return;
		}

		/*
		 * Add two lists of wind data.
		 */
		if ( sig_wc.size() > 0 ) {
			for (int kk = 0; kk < sig_wc.size(); kk++) {
				sig_wa.add(sig_wc.get(kk));
			}
		}

		int nlevel = sndata.size();
		NcSoundingLayer2 sl;
		try{
		for (int kk = 0; kk < sig_wa.size(); kk++) {
			boolean found = false;
			HeightAboveSeaLevel sigwaHeight = sig_wa.get(kk).getGeoHeight();
			float zzz = RMISSD;
			if ( sigwaHeight != null )
			       zzz = sigwaHeight.getValue().floatValue();

			// Check surface level independently because sometimes station
			// report wind data twice at surface.  We don't want the surface
			// pressure to be missing.
			if ( zzz == 0 ) {
				WindDirection  sndataWindDir =  sndata.get(0).getWindDirection();
				if ( sndataWindDir != null &&  (! sndataWindDir.hasValidValue( ) ) ) {
					sndata.get(0).setWindDirection(
							sig_wa.get(0).getWindDirection());
					sndata.get(0).setWindSpeed(
							sig_wa.get(kk).getWindSpeed());
				}
				found = true;
			} else {
				for (int lev = 0; lev < nlevel; lev++) {
					HeightAboveSeaLevel sndataLevHeight = sndata.get(lev).getGeoHeight();
					WindDirection sndataLevWindDir = sndata.get(lev).getWindDirection(); 
					float hght = RMISSD;
					if ( sndataLevHeight != null  ) 
					hght = sndataLevHeight.getValue().floatValue();
					if ( equal ( zzz, hght ) || (zzz == 0 && lev == 0 && kk == 0)) {
						// add data to missing
						if ( sndataLevWindDir != null && ( ! sndataLevWindDir.hasValidValue() ) ) {
							sndata.get(lev).setWindDirection(
									sig_wa.get(kk).getWindDirection());
							sndata.get(lev).setWindSpeed(
									sig_wa.get(kk).getWindSpeed());
						}
						found = true;
					}
				}
			}

			/*
			 * if not found, add to the list.
			 */
			
			if ( !found ) {
				WindDirection sigwaWindDir                = sig_wa.get(kk).getWindDirection();
				HeightAboveSeaLevel sigwaGeoHeight = sig_wa.get(kk).getGeoHeight();
				
				if ( sigwaWindDir != null && sigwaWindDir.hasValidValue()
						&& sigwaGeoHeight != null && sigwaGeoHeight.hasValidValue()) {
					sl = new NcSoundingLayer2();
					sl.setPressure(sig_wa.get(kk).getPressure());
					sl.setTemperature(sig_wa.get(kk).getTemperature());
					sl.setDewpoint(sig_wa.get(kk).getDewpoint());
					sl.setWindDirection(sig_wa.get(kk).getWindDirection());
					sl.setWindSpeed(sig_wa.get(kk).getWindSpeed());
					sl.setGeoHeight(sig_wa.get(kk).getGeoHeight());
					sl.setOmega(sig_wa.get(kk).getOmega());
					sndata.add(sl);
				}
			}
		}

		         /*
 		          * Sorting the combined temperature and wind data based on Geopotential
 		          * height.
		          */
		           Collections.sort(sndata, new sortByHeight());	
		}catch ( Exception e ){
			      e.printStackTrace();
		}
		           
		           return;
		}
	}

	// Sort by height
	@XmlRootElement
	@XmlAccessorType(XmlAccessType.NONE)
	@DynamicSerialize
	public static class sortByHeight implements Comparator<NcSoundingLayer2>, ISerializableObject {
		/**
		 * 
		 */
		public sortByHeight(){
			
		}
		
		@DynamicSerializeElement
		private static final long serialVersionUID = 6079913485592639332L;

		public int compare(NcSoundingLayer2 l1, NcSoundingLayer2 l2) {
			if ( l1 != null && l2 != null ){
			       HeightAboveSeaLevel h1 =  l1.getGeoHeight();
			       HeightAboveSeaLevel h2 =  l2.getGeoHeight();
			       if ( h1 != null && h2 != null ){
				        return Float.compare( h1.getValue().floatValue(), h2.getValue().floatValue() );
			       }
			      else
			             return 0;
			}else
				return 0;
		}

	}

	// Reverse sort by pressure
	@XmlRootElement
	@XmlAccessorType(XmlAccessType.NONE)
	@DynamicSerialize
	public static class reverseSortByPressure implements Comparator<NcSoundingLayer2>,ISerializableObject {
		/**
		 * 
		 */
		
		public reverseSortByPressure(){
			
		}
		
		@DynamicSerializeElement
		private static final long serialVersionUID = 6451235617638144626L;

		public int compare(NcSoundingLayer2 l1, NcSoundingLayer2 l2) {
			if ( l1 != null && l2 != null ){
				PressureLevel p1 = l1.getPressure();
				PressureLevel p2 = l2.getPressure();
				if ( p1 != null && p2 != null ){
			      return Float.compare( p2.getValue(  ).floatValue(), 
					           p1.getValue(  ).floatValue() );
				}
				else
					return 0;
			}
			else
				return 0;
		}

	}

	/*
	 * Construct pressure at significant wind levels (PPBB) that are reported on
	 * height levels. MR_INTP
	 */
	public List<NcSoundingLayer2> constructPpbbPressure(List<NcSoundingLayer2> sndata) {
		if ( sndata != null ){
		int sndataSize = sndata.size();
		if ( sndataSize <= 2) return sndata;
		
		float pb = RMISSD, pt = RMISSD, zt = RMISSD, zb = RMISSD;
		int blev = IMISSD, tlev = IMISSD;
		for (int lev = 0; lev < sndataSize; lev++) {
			PressureLevel sndataLevPressure            = sndata.get(lev).getPressure();
			HeightAboveSeaLevel sndataLevHeight = sndata.get(lev).getGeoHeight(); 
			float pres = sndataLevPressure.getValue().floatValue();
			float hght = sndataLevHeight.getValue().floatValue();
			if (pres != RMISSD && hght != RMISSD) {
				tlev = lev;
				pt = pres;
				zt = hght;
			}

			if (blev != IMISSD && tlev != IMISSD) {
				for (int kk = blev + 1; kk < tlev; kk++) {
					HeightAboveSeaLevel sndatakkHeight = sndata.get( kk ).getGeoHeight();
					float z = RMISSD;
					if ( sndatakkHeight != null )
					        z = sndatakkHeight.getValue().floatValue();
					if ( sndatakkHeight.hasValidValue() ) {
						  float ppp = ( float ) ( pb * Math.exp( ( double ) ( ( z - zb )
							                         	          * Math.log( pt / pb )  / ( zt - zb ) ) ) );
						PressureLevel sndatakkPressure;
						try {
							sndatakkPressure = new PressureLevel();
							sndatakkPressure.setValue( new Amount (ppp, NcUnits.MILLIBAR) );
							sndata.get(kk).setPressure( sndatakkPressure );
						} catch (Exception e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}

					}
				}
			}
			blev = tlev;
			pb = pt;
			zb = zt;
		}

		////System.out.println ( " tlev: " + tlev + " sndata.size() -1 " + (sndata.size()-1));

		if (tlev == (sndataSize-1) || tlev == IMISSD ) {
			return sndata;
		} else {

			/*
			 * Compute missing pressure at top
			 * levels.
			 */
			PressureLevel sndataTlevPressure            = sndata.get( tlev-1 ).getPressure();
			HeightAboveSeaLevel sndataTlevHeight = sndata.get( tlev-1 ).getGeoHeight();
			
			if ( sndataTlevPressure != null )
			          pb = sndataTlevPressure.getValue().floatValue(); 
			
			if ( sndataTlevHeight != null )
			          zb = sndataTlevHeight.getValue().floatValue();

			for (int kk = tlev+1; kk < sndataSize; kk++) {
			    PressureLevel sndatakkPressure = sndata.get(kk).getPressure(); 
				if ( sndatakkPressure != null && ( !sndatakkPressure.hasValidValue() ) ) {
					PressureLevel sndatakk1Pressure            = sndata.get(kk-1).getPressure();
					HeightAboveSeaLevel sndatakk1Height =sndata.get(kk-1).getGeoHeight();
					HeightAboveSeaLevel sndatakkHeight =sndata.get(kk).getGeoHeight();
					if ( sndatakk1Pressure != null )
					          pt = sndatakk1Pressure.getValue().floatValue();
					
					if( sndatakk1Height != null )
					         zt = sndatakk1Height.getValue().floatValue();
					
					float zz = RMISSD;
					
					if( sndatakkHeight != null )
					        zz = sndatakkHeight.getValue().floatValue();
					
					float rmult = ( float ) ( ( zz- zb ) / (zt - zb ) );
					float pVal = ( float )( pb *  ( Math.pow ( pt / pb ,rmult ) ) );
					PressureLevel newSndatakkPressure;
					try {
						newSndatakkPressure = new PressureLevel();
						newSndatakkPressure.setValue( new Amount ( pVal, NcUnits.MILLIBAR));
						sndata.get(kk).setPressure( newSndatakkPressure );
						
						pb = pt;
						zb = zt;
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				}
			}
		}
	  }
		return sndata;
	}



	/*
	 * Reorder the sounding data so the first level is always the surface data.
	 */
	public List<NcSoundingLayer2> reOrderSounding(List<NcSoundingLayer2> sndata) {
		List<NcSoundingLayer2> outdat = new ArrayList<NcSoundingLayer2>();
		if ( sndata != null ){
		float tt = RMISSD, td = RMISSD, dd = RMISSD, ff = RMISSD;
		int klev = 0;
		int sndataSize = sndata.size();
		
		if ( sndataSize <= 1) return sndata;
		
		/*
		 * Find the surface level
		 */
		for (int kk = 0; kk < sndataSize; kk++) {
			AirTemperature sndatakkTemp         =  sndata.get( kk ).getTemperature();
			DewPointTemp sndatakkDewpoint =  sndata.get( kk ).getDewpoint();
			WindDirection sndatakkWindDir   =  sndata.get(kk).getWindDirection();
			WindSpeed    sndatakkWindSpeed =  sndata.get(kk).getWindSpeed();
			
			if ( sndatakkTemp != null )
			        tt = sndatakkTemp.getValue().floatValue();
			
			if ( sndatakkDewpoint != null )
			      td = sndatakkDewpoint.getValue().floatValue();
			
			if ( sndatakkWindDir != null  )
			      dd = sndatakkWindDir.getValue().floatValue();
			
			if ( sndatakkWindSpeed != null  )
			     ff = sndatakkWindSpeed.getValue().floatValue();
			
			if (tt == RMISSD && td == RMISSD && dd == RMISSD && ff == RMISSD) {
				// DO NOTHING
			} else {
				            klev = kk;
				            addDataToList(0, sndata, outdat);
			}
		}

		/*
		 * Reorder the data below the surface levels.
		 */
		for (int kk = 0; kk < klev; kk++) {
			addDataToList(kk, sndata, outdat);
		}

		for (int kk = klev + 1; kk < sndataSize; kk++) {
			addDataToList(kk, sndata, outdat);
		}
		
	}
		return outdat;
	}

	/*
	 * Construct missing temperature (iflag = 1), dewpoint temperature (iflag=2)
	 * and wind (iflag = 3). This method is called after reOrderSounding().
	 * MR_MISS
	 */
	public List<NcSoundingLayer2> constructMissing(int iflag,
			List<NcSoundingLayer2> sndata) {
		float pb = RMISSD, pt = RMISSD, data = RMISSD, pres = RMISSD, tb  = RMISSD, tt = RMISSD, tdb = RMISSD, tdt = RMISSD;
		int jlev = IMISSD, tlev = IMISSD;
		boolean contin = true;
		if ( sndata != null ){
		int sndataSize = sndata.size();
		if ( sndataSize <= 2) return sndata;
		for (int blev = 1; blev < sndataSize - 1 && contin; blev++) {
			jlev = blev;

			switch (iflag) {
			case 1: {
				data = sndata.get(blev).getTemperature().getValue().floatValue();
				break;
			}
			case 2: {
				data = sndata.get(blev).getDewpoint().getValue().floatValue();
				break;
			}
			case 3: {
				data = sndata.get(blev).getWindDirection().getValue().floatValue();
				break;
			}
			default: {
				//System.out.println("Invalid data flag");
				return sndata;
			}
			}

			if (data == RMISSD) {

				/*
				 * find data at level above. Data should already be at level
				 * below after reOrderSounding() call.
				 */
				boolean found = false;
				while (!found) {
					jlev++;
					switch (iflag) {
					case 1: {
						data = sndata.get(jlev).getTemperature().getValue().floatValue();
						break;
					}
					case 2: {
						data = sndata.get(jlev).getDewpoint().getValue().floatValue();
						break;
					}
					case 3: {
						data = sndata.get(jlev).getWindDirection().getValue().floatValue();
						break;
					}
					default: {
						//System.out.println("Invalid data flag");
					}
					}
					int top = sndataSize;
					if (data != RMISSD || jlev+1 >= top) {
						found = true;
						tlev = jlev;
						if (jlev >= top) {
							tlev = IMISSD;
							contin = false;
						}
					}
				}

				/*
				 * Add check to eliminate dew point layer more than 100mb.
				 */
				if (iflag == 2 && tlev != IMISSD) {
					if ( ( sndata.get(blev).getPressure().getValue().floatValue() - sndata.get(tlev)
							.getPressure().getValue().floatValue()) > 100.) {
						DewPointTemp invalidDewpoint;
						try {
							invalidDewpoint = new DewPointTemp();
							invalidDewpoint.setValueToMissing();
							for ( int kk = tlev; kk < sndataSize; kk++ ) {
								sndata.get(kk).setDewpoint( invalidDewpoint );
							}
							tlev = IMISSD;
							contin = false;
						} catch (Exception e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}

					}
				}

				/*
				 * Add check to eliminate interpolation of winds from below 100
				 * mb to above 100 mb. This eliminates interpolation to very
				 * high level winds.
				 */
				/*				if (iflag == 3 && tlev != IMISSD
						&& (sndata.get(blev - 1).getPressure() > 100.)
						&& (sndata.get(tlev).getPressure() < 100.)) {
					tlev = IMISSD;
				}
				 */
				/*
				 * Interpolate with respect to logP.
				 */

				if (tlev != IMISSD) {
					    PressureLevel sndataBlev1Pressure = sndata.get( blev - 1 ).getPressure();
					    PressureLevel sndataBlevPressure = sndata.get( blev ).getPressure();
					    PressureLevel sndataTlevPressure = sndata.get( tlev ).getPressure();
					    
					    if ( sndataBlev1Pressure != null )
					          pb = sndataBlev1Pressure.getValue().floatValue();
					    
					    if( sndataBlevPressure != null )
					          pres = sndataBlevPressure.getValue().floatValue();
					    
					    if( sndataTlevPressure != null )
					          pt = sndataTlevPressure.getValue().floatValue();
					
					float rmult = (float) (Math.log(pres / pb) / Math.log(pt / pb));
					
					switch (iflag) {
					
					case 1: {
						                AirTemperature sndataBlev1Temp = sndata.get( blev - 1 ).getTemperature();
						               AirTemperature sndataTlevTemp   = sndata.get( tlev ).getTemperature();
						
						               if ( sndataBlev1Temp != null  )
						                     tb = sndataBlev1Temp.getValue().floatValue();
						
						               if ( sndataTlevTemp != null )
						                     tt = sndataTlevTemp.getValue().floatValue();
						
						              if ( tb != RMISSD && tt != RMISSD ) {
							                 data = tb + (tt - tb) * rmult; 
							                 AirTemperature newTemp;
											try {
												newTemp = new AirTemperature();
								                 newTemp.setValue( new Amount (data, SI.CELSIUS ));
								                 sndata.get(blev).setTemperature( newTemp );
											} catch (Exception e) {
												// TODO Auto-generated catch block
												e.printStackTrace();
											}

						              }

						                DewPointTemp sndataBlev1Dewpoint = sndata.get( blev - 1 ).getDewpoint();
						                DewPointTemp sndataTlev1Dewpoint = sndata.get( tlev ).getDewpoint();
						                
						                if ( sndataBlev1Dewpoint != null  )
						                      tdb = sndataBlev1Dewpoint.getValue().floatValue();
						                
						                if ( sndataTlev1Dewpoint != null )
						                      tdt = sndataTlev1Dewpoint.getValue().floatValue();
						                
						                if ( tdb != RMISSD && tdt != RMISSD ) {
							                       data = tdb + (tdt - tdb) * rmult;
							                       DewPointTemp newSndataBlevDewpoint;
												try {
													        newSndataBlevDewpoint = new DewPointTemp();
								                            newSndataBlevDewpoint.setValue( new Amount ( data, SI.CELSIUS ));
								                            sndata.get(blev).setDewpoint( newSndataBlevDewpoint  );
												} catch (Exception e) {
													// TODO Auto-generated catch block
													e.printStackTrace();
												}

						               }
						break;
					}
					case 2: {
						
						                          DewPointTemp sndataBlev1Dewpoint = sndata.get( blev - 1 ).getDewpoint();
						                          DewPointTemp sndataTlevDewpoint = sndata.get( tlev ).getDewpoint();
						                          if ( sndataBlev1Dewpoint != null )
						                                  tdb = sndataBlev1Dewpoint.getValue().floatValue();
						                          if ( sndataTlevDewpoint != null )
						                                  tdt = sndataTlevDewpoint.getValue().floatValue();
						                          if (tdb != RMISSD && tdt != RMISSD) {
							                           data = tdb + (tdt - tdb) * rmult;
								                       DewPointTemp newSndataBlevDewpoint;
													try {
														     newSndataBlevDewpoint = new DewPointTemp();
									                         newSndataBlevDewpoint.setValue(new Amount ( data, SI.CELSIUS ));
									                         sndata.get(blev).setDewpoint( newSndataBlevDewpoint  );
													} catch (Exception e) {
														// TODO Auto-generated catch block
														e.printStackTrace();
													}

						                          }
						                        break;
					             }
					case 3: {		
						                    WindDirection sndataBlev1WindDir =sndata.get( blev - 1 ).getWindDirection() ;
						                    WindDirection sndataTlevWindDir =sndata.get( tlev ).getWindDirection() ;
						                    float drctb = RMISSD;
						                    float drctt  = RMISSD;
						                    if ( sndataBlev1WindDir != null )
						                          drctb = sndataBlev1WindDir.getValue().floatValue();
						                    
						                    if ( sndataTlevWindDir != null )
						                          drctt = sndataTlevWindDir.getValue().floatValue();

						                    if ( drctt != RMISSD && drctb!= RMISSD ) {
							                        drctb = drctb % 360.f;
							                        drctt = drctt % 360.f;
							                         if (Math.abs(drctb - drctt) > 180.f) {
								                            if (drctb < drctt) {
									                               drctb = drctb + 360.f;
								                            } else{ 
									                                drctt = drctt + 360.f;
								                             }
							                 }
							               
							                 float drct = ( drctb + (drctt - drctb) * rmult) % 360.f;
							                 WindDirection sndataBlevWindDir;
											try {
												      sndataBlevWindDir = new WindDirection();
								                      sndataBlevWindDir.setValue( drct ); 
								                      sndata.get( blev ).setWindDirection( sndataBlevWindDir );
											} catch (Exception e) {
												// TODO Auto-generated catch block
												e.printStackTrace();
											}


                   							// Interpolate wind speed
							                WindSpeed sndataBlev1WindSpeed =  sndata.get( blev - 1 ).getWindSpeed();
							                WindSpeed sndataTlevWindSpeed   =   sndata.get( tlev ).getWindSpeed();
							               
							                float spedb = RMISSD; 
							                	 spedb = sndataBlev1WindSpeed.getValue().floatValue();
							                	 
							                 float spedt = RMISSD; 
							                	 spedt = sndataTlevWindSpeed.getValue().floatValue();
							                
							                float sped = RMISSD;
							                if  ( spedb != RMISSD && spedt != RMISSD )
							                	   sped =  spedb + (spedt - spedb) * rmult;
							                       
							               WindSpeed interpWindSpeed;
										try {
											     interpWindSpeed = new WindSpeed();
								                 interpWindSpeed.setValue( sped );
								                 sndata.get( blev ).setWindSpeed( interpWindSpeed );
										} catch (Exception e) {
											// TODO Auto-generated catch block
											e.printStackTrace();
										}
						          }
						break;

					}
					}
				}
			}
		}
		
	}
		return sndata;
	}


	/*
	 * Re-order the data so the first level is always the ground level. MR_COND
	 */
	public List<NcSoundingLayer2> addUnderGround(List<NcSoundingLayer2> man, 
			List<NcSoundingLayer2> sndata ) {
		
		int sndataSize = sndata.size();
		if ( sndata == null 
				||  ( sndata.get(0).getPressure() != null  &&  ( !sndata.get(0).getPressure().hasValidValue() ) ) 
				|| man.size() < 1 
				|| sndataSize <= 1 ) {
			return sndata;
		}
	
		int blev = 0;
		boolean contin = true;
		while ( blev < sndataSize && contin ) {
		    PressureLevel manBlevPressure = man.get(blev).getPressure();
		    PressureLevel sndataFirstPressure = sndata.get( 0 ).getPressure();
		    float mandatoryPressVal = RMISSD;
		    float sndataPressVal = RMISSD;
		    
		    if (  manBlevPressure != null )
			        mandatoryPressVal = manBlevPressure.getValue().floatValue();
		    
		    if  ( sndataFirstPressure != null )
		    	    sndataPressVal  = sndataFirstPressure.getValue().floatValue();
		    
		    if ( mandatoryPressVal > sndataPressVal ) {
				blev++;
			} else {
				contin = false;
			}
		}
		
		if ( blev >= sndataSize) {
			return sndata;
		}

		/*
		 * Added below-ground mandatory levels to sounding layers.
		 */
		List<NcSoundingLayer2> outdat = new ArrayList<NcSoundingLayer2>();

		int nlev = sndataSize;

		// write to surface data first
		addDataToList (0, sndata, outdat);

		// add below-ground mandatory data
		if (blev > 0 && blev < sndataSize) {
			for (int kk = 0; kk < blev; kk++ ) {
				addDataToList(kk,man,outdat);
			}
		}

		// add the rest of the data
		for (int kk = 1; kk < nlev; kk++) {
			addDataToList(kk,sndata,outdat);
		}
		return outdat;

	}

	/*
	 * Re-order the data so the first level is always the ground level. MR_COND
	 */
	public List<NcSoundingLayer2> removeUnderGround(List<NcSoundingLayer2> sndata) {
		List<NcSoundingLayer2> outdat = new ArrayList<NcSoundingLayer2>();
		/*
		 * Remove below-ground mandatory levels from sounding layers.  Only the
		 * first 8 missing levels can be mandatory levels.
		 */
//		System.out.println("From removeUnderGround()");
		int sndataSize = sndata.size();
		if ( sndataSize <= 1) return sndata;
		for ( int kk = 0; kk < sndataSize; kk++ )  {
			PressureLevel                            sndatakkPressure       = sndata.get(kk).getPressure();
			HeightAboveSeaLevel              sndatakkHeight          = sndata.get(kk).getGeoHeight();
			AirTemperature                        sndatakkTemperature = sndata.get(kk).getTemperature();
			DewPointTemp                         sndatakkDewpoint      = sndata.get(kk).getDewpoint();
			WindSpeed                               sndatakkWindSpeed   =  sndata.get(kk).getWindSpeed();
			WindDirection                         sndatakkWindDirection   =  sndata.get(kk).getWindDirection();
			//TODO : cross check this with Chin
			if ( ( sndatakkPressure != null && sndatakkPressure.hasValidValue() ) 
			      && ( sndatakkTemperature != null && sndatakkTemperature.hasValidValue() ) 		
			      && ( sndatakkDewpoint != null && sndatakkDewpoint.hasValidValue() )
			      && ( sndatakkWindDirection != null && sndatakkWindDirection.hasValidValue() ) 
			      && ( sndatakkWindSpeed != null && sndatakkWindSpeed.hasValidValue() ) ){
				              addDataToList(kk, sndata, outdat);
			}
//			if ( sndata.get(kk).getTemperature() <= RMISSD && 
//					sndata.get(kk).getDewpoint() <= RMISSD	&&
//					sndata.get(kk).getWindDirection() <= RMISSD &&
//					sndata.get(kk).getWindSpeed() <= RMISSD ) {
//				
//			} 
//			   else if ( sndata.get(kk).getPressure() <= RMISSD ) {				
//			 
//			   } 
//			   else {
//				addDataToList(kk, sndata, outdat);
//			}
		}
		return outdat;
	}

	/*
	 * Interpolate data to a single level, including surface.
	 */
	public List<NcSoundingLayer2> getSingLevel (float pres, List<NcSoundingLayer2> sndata) {
//		System.out.println("From getSingLevel()");
		NcSoundingLayer2 sl; 
		try{
		sl =  new NcSoundingLayer2();
		List<NcSoundingLayer2> sls =  new ArrayList<NcSoundingLayer2>();
		if ( sndata != null ){
		sndata = removeUnderGround(sndata);
		int sndataSize = sndata.size();
		if ( sndataSize <= 1) return setMissing(); //Chin: check size, after remove under ground lavel, size changed
		for ( int kk = 1; kk < sndataSize-1; kk++ )  {
			PressureLevel firstPressure = sndata.get(0).getPressure(); 
			if ( ( firstPressure != null ) &&   ( pres > firstPressure.getValue().floatValue() || pres < 0.f )) {
				return setMissing();
			} 
			else {

				PressureLevel                            sndatakkPressure       = sndata.get(kk).getPressure();
				HeightAboveSeaLevel              sndatakkHeight          = sndata.get(kk).getGeoHeight();
				AirTemperature                        sndatakkTemperature = sndata.get(kk).getTemperature();
				DewPointTemp                         sndatakkDewpoint      = sndata.get(kk).getDewpoint();
				WindDirection                          sndatakkWindDir           =  sndata.get(kk).getWindDirection();
				WindSpeed                                sndatakkWindSpeed      =  sndata.get(kk).getWindSpeed();
				
				PressureLevel                            sndatakk1Pressure       = sndata.get( kk - 1 ).getPressure();
				HeightAboveSeaLevel              sndatakk1Height          = sndata.get( kk  - 1 ).getGeoHeight();
				AirTemperature                        sndatakk1Temperature = sndata.get( kk - 1 ).getTemperature();
				DewPointTemp                         sndatakk1Dewpoint      = sndata.get( kk - 1 ).getDewpoint();
				WindDirection                          sndatakk1WindDir        =  sndata.get( kk - 1 ).getWindDirection();
				WindSpeed                                sndatakk1WindSpeed   =  sndata.get( kk - 1 ).getWindSpeed();
				
				if ( ( sndatakkPressure !=  null ) &&   ( pres >= sndatakkPressure.getValue().floatValue() )) {
					float pt = RMISSD, pb = RMISSD, zt = RMISSD, zb = RMISSD, tt = RMISSD, tb = RMISSD, tdt = RMISSD, tdb = RMISSD, dt = RMISSD, db = RMISSD, st = RMISSD, sb = RMISSD;
					if ( sndatakk1Pressure != null )
					         pb = sndatakk1Pressure.getValue().floatValue();
					
					if ( sndatakkPressure != null )
					         pt = sndatakkPressure.getValue().floatValue();
					
					if ( sndatakk1Temperature != null )
					         tb = sndatakk1Temperature.getValue().floatValue();
					
					if ( sndatakkTemperature != null )
					         tt = sndatakkTemperature.getValue().floatValue();
					
					if ( sndatakk1Dewpoint != null )
					         tdb = sndatakk1Dewpoint.getValue().floatValue();
					
					if ( sndatakkDewpoint != null )
					          tdt = sndatakkDewpoint.getValue().floatValue();
					
					if ( sndatakk1WindDir != null )
					          db = sndatakk1WindDir.getValue().floatValue() % 360.f;
					
					if ( sndatakkWindDir != null )
					          dt =  sndatakkWindDir.getValue().floatValue() % 360.f;
					
					if ( sndatakk1WindSpeed != null )
					          sb = sndatakk1WindSpeed.getValue().floatValue();
					
					if ( sndatakkWindSpeed != null )
					         st = sndatakkWindSpeed.getValue().floatValue();
					
					if ( sndatakk1Height != null )
					         zb = sndatakk1Height.getValue().floatValue();
					
					if ( sndatakkHeight != null )
					         zt = sndatakkHeight.getValue().floatValue();				
					
					PressureLevel s1Press = new PressureLevel();
					s1Press.setValue( new Amount (pres, NcUnits.MILLIBAR) );
					sl.setPressure( s1Press );

					float rmult = ( float ) ( Math.log( pres / pb ) / Math.log ( pt /  pb ) );
					float tempVal        =  tb + ( tt - tb ) * rmult;
					float dewpointVal = tdb + ( tdt - tdb ) * rmult;
					AirTemperature s1Temp = new AirTemperature();
					s1Temp.setValue( new Amount ( tempVal, SI.CELSIUS ) );
					sl.setTemperature( s1Temp );
					
					DewPointTemp s1Dewpoint = new DewPointTemp();
					s1Dewpoint.setValue( new Amount ( dewpointVal, SI.CELSIUS ) );
					sl.setDewpoint( s1Dewpoint );
					
					if ( Math.abs ( db - dt ) > 180.) {
						if ( db < dt ) {
							db = db + 360.f;
						} else {
							dt = dt + 360.f;
						}
					}
					
					float windDirVal     = db + ( dt - db ) * rmult;
					float windSpeedVal = sb + ( st - sb ) * rmult;
					float geoHeightVal  = zb + ( zt - zb ) * rmult;
					
					WindDirection s1WindDir = new WindDirection(); 
					s1WindDir.setValue(windDirVal );
					
					WindSpeed s1WindSpeed = new WindSpeed();
					s1WindSpeed.setValue( windSpeedVal );
					
					HeightAboveSeaLevel s1Height = new HeightAboveSeaLevel();
					s1Height.setValue( geoHeightVal );
					
					sl.setWindDirection( s1WindDir );
					sl.setWindSpeed( s1WindSpeed );
					sl.setGeoHeight( s1Height );
					sls.add(sl);
//					System.out.println("Added one layer successfully");
					return sls;
				}
			}
		}
		}
	}
	catch ( Exception e){
	             e.printStackTrace();
	}
		
		return setMissing();
	}

	/*
	 * Add data to output sounding profile.
	 */
	public void addDataToList(int index,
			List<NcSoundingLayer2> indat, List<NcSoundingLayer2> outdat) {
		try{
			NcSoundingLayer2 sl = new NcSoundingLayer2();
			sl.setPressure(indat.get(index).getPressure());
			sl.setTemperature(indat.get(index).getTemperature());
			sl.setDewpoint(indat.get(index).getDewpoint());
			sl.setWindDirection(indat.get(index).getWindDirection());
			sl.setWindSpeed(indat.get(index).getWindSpeed());
			sl.setGeoHeight(indat.get(index).getGeoHeight());
			sl.setOmega(indat.get(index).getOmega());
			outdat.add(sl);
		}
		catch ( Exception e){
			e.printStackTrace();
		}
	}

	/*
	 * Set missing to output sounding profile.
	 */
	public List<NcSoundingLayer2> setMissing () {
		List<NcSoundingLayer2> outdat = new ArrayList<NcSoundingLayer2>(0);
		try{
		NcSoundingLayer2 sl = new NcSoundingLayer2();
		// the constructor for each of these MetParameters initializes the MetParameter to its predefined invalid value , i.e. RMISSD 
		sl.setPressure( new PressureLevel() );
		sl.setTemperature( new AirTemperature());
		sl.setDewpoint( new DewPointTemp() );
		sl.setWindDirection( new WindDirection());
		sl.setWindSpeed( new WindSpeed() );
		sl.setGeoHeight( new HeightAboveSeaLevel());
		sl.setOmega(new Omega());
		outdat.add(sl);
		}
		catch( Exception e){
			e.printStackTrace();
		}
		return outdat;
	}
}


