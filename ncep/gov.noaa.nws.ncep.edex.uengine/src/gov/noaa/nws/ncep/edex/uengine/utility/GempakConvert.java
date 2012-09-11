package gov.noaa.nws.ncep.edex.uengine.utility;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.DataOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.MercatorGridCoverage;
import com.raytheon.uf.common.gridcoverage.PolarStereoGridCoverage;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.message.CatalogAttribute;
import com.raytheon.uf.common.message.CatalogItem;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasMapCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.NcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.LambertConformalNcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.LatLonNcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.MercatorNcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.PolarStereoNcgridCoverage;

/**
 * GempakConvert
 * 
 * Performs various conversion tasks for GEMPAK/A2DB parameters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date                 Ticket#         Engineer                Description
 * ------------         ----------      -----------             --------------------------
 * 03/18/2009							mgamazaychikov			Initial Creation
 * 06/02/2009			92				mgamazaychikov			Added a Constructor
 * 10/13/2009			173				mgamazaychikov			Edited constructor
 * 																Added getSatHdrContent
 * 12/22/2009			173_partB		mgamazaychikov			Edited constructor to include
 * 																mcidas, mosaic and radar plugins
 * 																Added ConvertRadarRMC, getRadarThresholds,
 * 																getMcidasHdrContent
 * 06/02/2010			173_partC		mgamazaychikov			Edited constructor to include grid plugin
 * 																Added getGridNavigationContent, float2File,
 * 																serialize2File, data2File, flipData
 * 09/14/2010			284				mgamazaychikov			Add addHours method
 * </pre>
 * 
 * @author mgamazaychikov
 * @version 1
 */

public class GempakConvert {

	/*
	 * A mapping between the enumeration and the representation in the database
	 * table
	 */
	private static HashMap<String, String> radarProductMap = new HashMap<String, String>();
	static {
		radarProductMap.put("2",  "2");
		radarProductMap.put("19", "BREF_1.00");
		radarProductMap.put("20", "BREF_2.00");
		radarProductMap.put("27", "VEL_1.00");
		//radarProductMap.put("37", "37");
		radarProductMap.put("41", "TOPS_4.00");
		//radarProductMap.put("48", "48");
		radarProductMap.put("56", "SRVEL_1.00");
		radarProductMap.put("57", "VIL_4.00");
		radarProductMap.put("78", "PRCP1_2.00");
		radarProductMap.put("80", "PRCPT_2.00");
		//radarProductMap.put("141", "141");
		//radarProductMap.put("181", "181");
	}

	String[] inputStrings;
	String[] outputStrings;
	ResponseMessageCatalog rmc;

   /*
    * Default public constructor 
    */
    public GempakConvert() {
	}

   /*
	*  Constructor called in GempakCatalogQuery.py and GempakRadarCatalogQuery.py
	*/
	public GempakConvert(String aPlugin, ResponseMessageCatalog aCatalog)
			throws JAXBException {
		
		if ( aPlugin.equals("radar")  || 
			 aPlugin.equals("mosaic") ||
			 aPlugin.equals("mcidas")   ) {
			rmc = aCatalog;
			inputStrings = aCatalog.getValues();
		}
		else if ( aPlugin.equals("bufrua") ||
				  aPlugin.equals("grib") || aPlugin.equals("ncgrib") || 
		          aPlugin.equals("satellite") ) {			
			inputStrings = aCatalog.getValues();
		} 
		else if (aPlugin.equals("sfcobs")) {
			ArrayList<String> inpTimes = new ArrayList<String>();
			for (CatalogItem item : aCatalog.getItems()) {
				CatalogAttribute[] catAttr = item.getAttributes();
				for (CatalogAttribute ca : catAttr) {
					if (ca.getField().equals("refHour")) {
						String aTm = "time=";
						int indTime = ca.getValue().indexOf(aTm) + aTm.length();
						int indAre = ca.getValue().indexOf(",areFieldsSet");
						long millisec = Long.parseLong(ca.getValue().substring(
								indTime, indAre));
						DataTime aDataTime = new DataTime(new Date(millisec));
						inpTimes.add(aDataTime.toString());
					}
				}
				inputStrings = inpTimes.toArray(new String[inpTimes.size()]);
			}
		}
	}

	/*
	 * Converts GEMPAK DATTIM string into AWIPS2 date time string
	 */
	public String dattimToDbtime(String aDattim) {
		aDattim = aDattim.toUpperCase();
		String retDateTime = null;
		String[] inputStringArray = new String[2];
		CharSequence char0 = "F";
		if ( aDattim.contains(char0) ) {
			
			int ind1 = aDattim.indexOf("F00");
			int addChars = 3;
			if ( ind1 == -1 ) {
				ind1 = aDattim.indexOf("F0");
				addChars = 2;
			}
			if ( ind1 == -1 ) {
				ind1 = aDattim.indexOf("F");
				addChars = 1;
			}
			int ind2 = aDattim.length();
			
			String str1 = aDattim.substring(0, ind1);
			String str2 = aDattim.substring(ind1+addChars,ind2 );
			inputStringArray = str1.split("/");

			/*
			 * YYMMDD/HHMMfHHH -> YYYY-MM-DD HH:MM:SS.S 
			 * 090120/0225f005 -> 2009-01-20 02:25:00.0 
			 * 012345 0123
			 */
			retDateTime = "20" + inputStringArray[0].substring(0, 2) + "-"
			+ inputStringArray[0].substring(2, 4) + "-"
			+ inputStringArray[0].substring(4, 6) + "_"
			+ inputStringArray[1].substring(0, 2) + ":"
			+ inputStringArray[1].substring(2, 4) + ":00.0_("+ str2 + ")";
		}
		/*
		 * Process time that does NOT contain forecast hour info
		 */
		else {
			inputStringArray = aDattim.split("/");

			/*
			 * YYMMDD/HHMM -> YYYY-MM-DD HH:MM:SS.S 
			 * 090120/0225 -> 2009-01-2002:25:00.0 
			 * s012345 0123
			 */
			retDateTime = "20" + inputStringArray[0].substring(0, 2) + "-"
					+ inputStringArray[0].substring(2, 4) + "-"
					+ inputStringArray[0].substring(4, 6) + " "
					+ inputStringArray[1].substring(0, 2) + ":"
					+ inputStringArray[1].substring(2, 4) + ":00.0";
		}

		return retDateTime;
	}

	/*
	 * Converts AWIPS2 date time string into GEMPAK DATTIM string
	 */
	public String dbtimeToDattim(String aTime) {
		String aDattim = null;
		String[] inputStringArray = new String[2];
		
		CharSequence char0 = "(";
		/*
		 * Process time contains forecast hour info
		 */
		if ( aTime.contains(char0) ) {
			String zeroes = null;
			int ind1 = aTime.indexOf("(");
			int ind2 = aTime.indexOf(")");
			if ( ind2-ind1 == 2 ) {
				zeroes = "00";
			}
			else if ( ind2-ind1 == 3 ) {
				zeroes = "0";
			}
			String str1 = aTime.substring(0, ind1-1);
			String str2 = "";
			if ( zeroes != null) {
				str2 = "f"+zeroes+aTime.substring(ind1+1, ind2);
			}
			else {
				str2 = "f"+aTime.substring(ind1+1, ind2);
			}
			
			if ( aTime.contains("_") ) {
				inputStringArray = str1.split("_");
			}
			else if ( ! aTime.contains("_") ) {
				inputStringArray = str1.split(" ");
			}

			/*
			 * YYYY-MM-DD HH:MM:SS.S (HHH)-> YYMMDD/HHMMfHHH
			 * 2009-10-22 16:00:00.0 (5)-> 091022/1600f005
			 * 0123456789 0123456789
			 */
			aDattim = inputStringArray[0].substring(2, 4)
					+ inputStringArray[0].substring(5, 7)
					+ inputStringArray[0].substring(8, 10) + "/"
					+ inputStringArray[1].substring(0, 2)
					+ inputStringArray[1].substring(3, 5) + str2;
		}
		/*
		 * Process time that does NOT contain forecast hour info
		 */
		else {
			inputStringArray = aTime.split(" ");

			/*
			 * YYYY-MM-DD HH:MM:SS.S -> YYMMDD/HHMM
			 * 2009-01-20 02:25:00.0 -> 090120/0225
			 * 0123456789 0123456789
			 */
			aDattim = inputStringArray[0].substring(2, 4)
					+ inputStringArray[0].substring(5, 7)
					+ inputStringArray[0].substring(8, 10) + "/"
					+ inputStringArray[1].substring(0, 2)
					+ inputStringArray[1].substring(3, 5);
		}
		return aDattim;
	}

	/*
	 * Returns a range of time +/- 10 minutes given the input nomTime
	 */
	public String setTimeRange(String nomTime) {

		long offset = 600000;

		DataTime start = new DataTime(new Date(new DataTime(nomTime)
				.getValidTime().getTimeInMillis()
				- offset));
		DataTime end = new DataTime(new Date(new DataTime(nomTime)
				.getValidTime().getTimeInMillis()
				+ offset));
		RequestConstraint timeRange = new RequestConstraint();
		String[] constraintList = { start.toString(), end.toString() };
		timeRange.setBetweenValueList(constraintList);
		timeRange.setConstraintType(RequestConstraint.ConstraintType.BETWEEN);
		return timeRange.toString();
	}

	/*
	 * Place holder for possible conversion
	 */
	public String setArea(String aStation) {
		String retStation = aStation;
		return retStation;
	}

	public String[] getStrings() {
		return inputStrings;
	}

   /*
    * Construct the satellite header string
    */
	public static String getSatHdrContent(ISpatialObject obj) throws JAXBException {
        SatMapCoverage mapCoverage = (SatMapCoverage)obj;
        StringBuffer resultsBuf = new StringBuffer();
        resultsBuf.append(mapCoverage.getProjection());
        resultsBuf.append(";");
        resultsBuf.append(mapCoverage.getNx());
        resultsBuf.append(";");
        resultsBuf.append(mapCoverage.getNy());
        resultsBuf.append(";");
        Float dummy = mapCoverage.getLa1()*10000;
        resultsBuf.append(dummy.intValue());
        resultsBuf.append(";");
        dummy = mapCoverage.getLa2()*10000;
        resultsBuf.append(dummy.intValue());
        resultsBuf.append(";");
        dummy = mapCoverage.getLo1()*10000;
        resultsBuf.append(dummy.intValue());
        resultsBuf.append(";");
        dummy = mapCoverage.getLo2()*10000;
        resultsBuf.append(dummy.intValue());
        resultsBuf.append(";");
        dummy = mapCoverage.getLatin()*10000;
        resultsBuf.append(dummy.intValue());
        resultsBuf.append(";");
        dummy = mapCoverage.getLov()*10000;
        resultsBuf.append(dummy.intValue());
        resultsBuf.append(";");
        resultsBuf.append(mapCoverage.getDx().intValue());
        resultsBuf.append(";");
        resultsBuf.append(mapCoverage.getDy().intValue());
        
        String content = resultsBuf.toString();
        
        return content;
    }
	
   /*
	* Converts AWIPS2 date time string into GEMPAK DATTIM string
	*/
	public static String dbtimeToSatDattim(String aTime) {
		String aDattim = null;
		String[] inputStringArray = null;
		inputStringArray = aTime.split(" ");

		/*
		 * YYYY-MM-DD HH:MM:SS.S -> YYMMDD/HHMMSS 2009-01-20 02:25:00.0 ->
		 * 090120/022500 0123456789 0123456789
		 */
		aDattim = inputStringArray[0].substring(0, 4)
				+ inputStringArray[0].substring(5, 7)
				+ inputStringArray[0].substring(8, 10) + "/"
				+ inputStringArray[1].substring(0, 2)
				+ inputStringArray[1].substring(3, 5)
				+ inputStringArray[1].substring(6, 8);

		return aDattim;
	}
	
	/*
	 * Converts RMC containing radar data into a string 
	 */
	public ResponseMessageCatalog ConvertRadarRMC (String aField) throws Exception {
		String []valStrings = rmc.getValues();
		if (aField.equals("icao") || aField.equals("prodName")) {
			for (int ii = 0; ii<valStrings.length; ii++) {
				String str = valStrings[ii].toUpperCase();
				valStrings[ii] = str;
			}
		}
		if (aField.equals("resolution") ) {
			for (int ii = 0; ii<valStrings.length; ii++) {
				String str = valStrings[ii];
				Float f1 = new Float (str);
				Float thou = new Float (1000.);
				f1 = f1/thou;
				valStrings[ii] = f1.toString();
			}
		}
		else if (aField.equals("productCode") ){
			for (int ii = 0; ii<valStrings.length; ii++) {
				String str = radarProductMap.get(valStrings[ii]);
				valStrings[ii] = str;
			}
		}		
		else if (aField.equals("trueElevationAngle") ){
			for (int ii = 0; ii<valStrings.length; ii++) {
				String str = valStrings[ii];
				valStrings[ii] = str;
			}
		}
		
		rmc.setValues(valStrings);
		
        return rmc;
    }
	
	/*
	 * Returns radar thresholds values
	 */
	public String getRadarThresholds(PluginDataObject aDataRecord, IDataRecord[] records) {
		String thresholds = null;
		StringBuffer bf = new StringBuffer();
		short[] thresholdsVals = new short[16];
        for (IDataRecord dataRecord : records) {
            if ("Thresholds".equals(dataRecord.getName())) {
            	thresholdsVals = ((ShortDataRecord) dataRecord).getShortData();
            }
        }
        
        for (short vals: thresholdsVals) {
        	bf.append(vals);
        	bf.append(":");
        }
        
        thresholds = bf.toString();
        return thresholds;
	}
	
   /*
	* Construct the mcidas header string
    */

	public static String getMcidasHdrContent(ISpatialObject obj) throws JAXBException {
	        McidasMapCoverage mapCoverage = (McidasMapCoverage)obj;
	        StringBuffer resultsBuf = new StringBuffer();
	        resultsBuf.append(mapCoverage.getProjection());
	        resultsBuf.append(";");
	        resultsBuf.append(mapCoverage.getNx());
	        resultsBuf.append(";");
	        resultsBuf.append(mapCoverage.getNy());
	        resultsBuf.append(";");
	        Float dummy = mapCoverage.getLllat()*10000;
	        resultsBuf.append(dummy.intValue());
	        resultsBuf.append(";");
	        dummy = mapCoverage.getUrlat()*10000;
	        resultsBuf.append(dummy.intValue());
	        resultsBuf.append(";");
	        dummy = mapCoverage.getLllon()*10000;
	        resultsBuf.append(dummy.intValue());
	        resultsBuf.append(";");	        
	        dummy = mapCoverage.getUrlon()*10000;
	        resultsBuf.append(dummy.intValue());
	        resultsBuf.append(";");
	        dummy = mapCoverage.getStdlat1()*10000;
	        resultsBuf.append(dummy.intValue());
	        resultsBuf.append(";");
	        dummy = mapCoverage.getClon()*10000;
	        resultsBuf.append(dummy.intValue());
	        resultsBuf.append(";");
	        resultsBuf.append(mapCoverage.getDx().intValue());
	        resultsBuf.append(";");
	        resultsBuf.append(mapCoverage.getDy().intValue());
	        String content = resultsBuf.toString();
	        
	        return content;
	    }

	
    /*
     * Construct the grid navigation string
     */
     public static String getGridNavigationContent(ISpatialObject obj) throws JAXBException {
    	 
    	 GridCoverage gc = (GridCoverage)obj;
    	 StringBuffer resultsBuf = new StringBuffer();
         /*
         if (gc instanceof LatLonGridCoverage) {
                 LatLonGridCoverage llgc = (LatLonGridCoverage) gc;
         }
         */

         
         
         if (gc instanceof LatLonGridCoverage) {
        	 /*
        	  * TODO - finish with LatLonGridCoverage
        	  */
        	 LatLonGridCoverage llgc = (LatLonGridCoverage) gc;
         }
         else if (gc instanceof LambertConformalGridCoverage) {
        	 
             resultsBuf.append("LCC");
             resultsBuf.append(";");
        	 LambertConformalGridCoverage lcgc = (LambertConformalGridCoverage) gc;
        	 resultsBuf.append(lcgc.getNx());
             resultsBuf.append(";");
             resultsBuf.append(lcgc.getNy());
             resultsBuf.append(";");
             Double dummy = lcgc.getLa1()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = lcgc.getLo1()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = lcgc.getLatin1()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = lcgc.getLatin2()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = lcgc.getLov()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = lcgc.getDx()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = lcgc.getDy()*10000;
             resultsBuf.append(dummy.intValue());
         }
         else if (gc instanceof MercatorGridCoverage) {
        	 /*
        	  * TODO - finish with MercatorGridCoverage
        	  */
        	 MercatorGridCoverage mgc = (MercatorGridCoverage) gc;
         }
         else if (gc instanceof PolarStereoGridCoverage){
        	 /*
        	  * TODO - finish with PolarStereoGridCoverage
        	  */
        	 PolarStereoGridCoverage psgc = (PolarStereoGridCoverage) gc;
         }
         
         String content = resultsBuf.toString();
         return content;
    	 
     }
     
    /*
     * TODO - add documentation
     */
     public static void float2File(float[] outBytes, File outFile) 
     		throws IOException {

         if (!outFile.getParentFile().exists()) {
             outFile.getParentFile().mkdirs();
         }

         FileOutputStream out = null;
         

         try {
             out = new FileOutputStream(outFile);
             DataOutputStream dout = new DataOutputStream(out);

             // only write out 8*1024 bytes at a time
             for (int counter = 0; counter < outBytes.length; counter ++) {
            	 dout.writeFloat(outBytes[counter]);
             }
         } catch (Exception e) {
             throw new IOException("Unable to create file from bytes", e);
         } finally {

             if (out != null) {
                 out.close();
             }

         }
    	 
    	 
    	 return;
     }
     
     /*
      * TODO - add documentation
      */
      public static void serialize2File(float[] outBytes, File outFile)
			throws IOException {
    	  if (!outFile.getParentFile().exists()) {
  			outFile.getParentFile().mkdirs();
  		  }

		

		  FileOutputStream fos;

		  try {
			 fos = new FileOutputStream(outFile);
			 DynamicSerializationManager.getManager(SerializationType.Thrift)
					.serialize(outBytes, fos);

		  } catch (SerializationException se) {
			 se.printStackTrace();
		  } catch (FileNotFoundException fnfe) {
			 fnfe.printStackTrace();
		  }

		// FileOutputStream out = null;
		// StringBuffer strBuf = new StringBuffer();

		// try {
		//        	  
		// out = new FileOutputStream(outFile);
		// DataOutputStream dout = new DataOutputStream(out);
		//
		// // only write out 8*1024 bytes at a time
		// for (int counter = 0; counter < outBytes.length; counter ++) {
		// strBuf.append(outBytes[counter]);
		// strBuf.append(";");
		// //dout.writeFloat(outBytes[counter]);
		// }
		// } catch (Exception e) {
		// System.out.println("Unable to create file from bytes :"
		// +e.toString());
		// throw new IOException("Unable to create file from bytes", e);
		// } finally {
		//
		// if (out != null) {
		// out.close();
		// }
		//
		// }

		  return;
		
	}
      /*
       * TODO - add documentation
       */
      public static void data2File(float[] outBytes, int nx, int ny, File outFile)
		throws IOException {;
		StringBuffer strBuf = new StringBuffer();

		float [] outBytesFlipped = new float[outBytes.length];
		
		int kk = 0;
		
		for (int jj = 0; jj < ny; jj++){
			int m1 = nx*ny-nx*(jj+1);
			int m2 = nx*ny - nx*jj;
			for (int ii=m1; ii < m2; ii++){
				if ( outBytes[ii] < -900000.0) {
					outBytesFlipped[kk] = -9999.0f;
					kk++;
				}
				else {
					outBytesFlipped[kk] = outBytes[ii];
					kk++;
				}				
			}
		}
			
		for (int counter = 0; counter < outBytesFlipped.length-1; counter++) {
			strBuf.append(outBytesFlipped[counter]);
			//System.out.println(outBytes[counter]);
			strBuf.append(";");
			// dout.writeFloat(outBytes[counter]);
		}
		strBuf.append(outBytesFlipped[outBytesFlipped.length-1]);
		//System.out.println(outBytes[outBytes.length-1]);
		
		//System.out.println("writing this many data points :" + outBytes.length);
		String strOut = new String (strBuf.toString());
		//System.out.println("writing this strin \n" + strOut);
		BufferedWriter bfout = new BufferedWriter(new FileWriter(outFile));

		try {
			String out2 = null;
			bfout.write(strOut);			
			
		} catch (Exception e) {
//			System.out.println("Unable to create file from bytes :"
//					+ e.toString());
			throw new IOException("Unable to create file from bytes", e);
		} finally {
			bfout.write(strOut);
		}

		return;
      }
      
      /*
       * TODO - add documentation
       */
      public static float[] flipData(float[] inGrid, int nx, int ny) {
    	  
    	  float[] outGridFlipped = new float[inGrid.length];
    	  int kk = 0;
    	  for (int jj = ny-1; jj >= 0; jj--) {
    		  int m1 = nx * jj;
    		  int m2 = m1 + (nx-1);
    		  for (int ii = m1; ii <= m2; ii++) {
    			  if ( inGrid[ii] == -9999.0f) {
    				outGridFlipped[kk] = -999999.0f;
  					kk++;
  				  }
  				  else {
  					outGridFlipped[kk] = inGrid[ii];
  					kk++;
  				  }
    		  }
    			  
    	  }
    	  return outGridFlipped;
	   }
      
      /*
  	 * Converts AWIPS2 date time string into GEMPAK DATTIM string
  	 */
  	public String toFileName(String aModel, String aTime) {
  		String[] inputStringArray = new String[2];
  		String aFileName = null;
  		
  		CharSequence char0 = "(";
  		/*
  		 * Process time contains forecast hour info
  		 */
  		if ( aTime.contains(char0) ) {
  			String zeroes = null;
  			int ind1 = aTime.indexOf("(");
  			int ind2 = aTime.indexOf(")");
  			if ( ind2-ind1 == 2 ) {
  				zeroes = "00";
  			}
  			else if ( ind2-ind1 == 3 ) {
  				zeroes = "0";
  			}
  			String str1 = aTime.substring(0, ind1-1);
  			String str2 = "f"+zeroes+aTime.substring(ind1+1, ind2);
  			if ( aTime.contains("_") ) {
  				inputStringArray = str1.split("_");
  			}
  			else if ( aTime.contains("_") ) {
  				inputStringArray = str1.split(" ");
  			}
  			

  			/*
  			 * YYYY-MM-DD HH:MM:SS.S (HHH)-> YYMMDD/HHMMfHHH
  			 * 2009-10-22 16:00:00.0 (5)-> 091022/1600f005
  			 * 0123456789 0123456789
  			 */
  			aFileName = aModel + "_" + inputStringArray[0].substring(0, 4)
  					+ inputStringArray[0].substring(5, 7)
  					+ inputStringArray[0].substring(8, 10) + "/"
  					+ inputStringArray[1].substring(0, 2)
  					+ inputStringArray[1].substring(3, 5) + str2;
  		}
  		/*
  		 * Process time that does NOT contain forecast hour info
  		 */
  		else {
  			inputStringArray = aTime.split(" ");

  			/*
  			 * YYYY-MM-DD HH:MM:SS.S -> YYMMDD/HHMM
  			 * 2009-01-20 02:25:00.0 -> 090120/0225
  			 * 0123456789 0123456789
  			 */
  			aFileName = inputStringArray[0].substring(2, 4)
  					+ inputStringArray[0].substring(5, 7)
  					+ inputStringArray[0].substring(8, 10) + "/"
  					+ inputStringArray[1].substring(0, 2)
  					+ inputStringArray[1].substring(3, 5);
  		}
  		return aFileName;
  	}
     
    /*
     * Construct the grid navigation string
     */
     public static String getNcgridNavigationContent(ISpatialObject obj) throws JAXBException {
    	 
    	 NcgridCoverage gc = (NcgridCoverage)obj;
    	 StringBuffer resultsBuf = new StringBuffer();

         if (gc instanceof LatLonNcgridCoverage) {
        	 /*
        	  * LatLonGridCoverage
        	  */
        	 LatLonNcgridCoverage llgc = (LatLonNcgridCoverage) gc;
             resultsBuf.append("CED");
             resultsBuf.append(";");
        	 resultsBuf.append(llgc.getNx());
             resultsBuf.append(";");
             resultsBuf.append(llgc.getNy());
             resultsBuf.append(";");
             Double dummy = llgc.getLa1()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = llgc.getLo1()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = llgc.getLa2()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = llgc.getLo2()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = -9999.0;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = llgc.getDx()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = llgc.getDy()*10000;
             resultsBuf.append(dummy.intValue());
         }
         else if (gc instanceof LambertConformalNcgridCoverage) {
             resultsBuf.append("LCC");
             resultsBuf.append(";");
        	 LambertConformalNcgridCoverage lcgc = (LambertConformalNcgridCoverage) gc;
        	 resultsBuf.append(lcgc.getNx());
             resultsBuf.append(";");
             resultsBuf.append(lcgc.getNy());
             resultsBuf.append(";");
             Double dummy = lcgc.getLa1()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = lcgc.getLo1()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = lcgc.getLatin1()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = lcgc.getLatin2()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = lcgc.getLov()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = lcgc.getDx()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = lcgc.getDy()*10000;
             resultsBuf.append(dummy.intValue());
         }
         else if (gc instanceof MercatorNcgridCoverage) {
        	 /*
        	  * TODO - finish with MercatorGridCoverage
        	  */
        	 MercatorNcgridCoverage mgc = (MercatorNcgridCoverage) gc;
        	 resultsBuf.append("MER");
             resultsBuf.append(";");
        	 resultsBuf.append(mgc.getNx());
             resultsBuf.append(";");
             resultsBuf.append(mgc.getNy());
             resultsBuf.append(";");
             Double dummy = mgc.getLa1()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = mgc.getLo1()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = mgc.getLatin()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = mgc.getLa2()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = mgc.getLo2()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = mgc.getDx()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = mgc.getDy()*10000;
             resultsBuf.append(dummy.intValue());
         }
         else if (gc instanceof PolarStereoNcgridCoverage){
        	 /*
        	  * PolarStereoGridCoverage
        	  */
        	 PolarStereoNcgridCoverage psgc = (PolarStereoNcgridCoverage) gc;
        	 resultsBuf.append("STR");
             resultsBuf.append(";");
        	 resultsBuf.append(psgc.getNx());
             resultsBuf.append(";");
             resultsBuf.append(psgc.getNy());
             resultsBuf.append(";");
             Double dummy = psgc.getLa1()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = psgc.getLo1()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = -9999.0;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = -9999.0;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = psgc.getLov()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = psgc.getDx()*10000;
             resultsBuf.append(dummy.intValue());
             resultsBuf.append(";");
             dummy = psgc.getDy()*10000;
             resultsBuf.append(dummy.intValue());
         }
         
         String content = resultsBuf.toString();
         return content;
    	 
     }
     
    /*
 	 * Returns string representing updated by hours baseTime
 	 */
 	public String addHours(String baseTime, String hours) {
 		long offset = (int) Integer.parseInt(hours)*60*60*1000;
 		DataTime aDataTime = new DataTime(baseTime);		
		long newTimeInMillis = aDataTime.getValidTime().getTimeInMillis() + offset;
		DataTime newTime = new DataTime(new Date(newTimeInMillis));
 		return newTime.toString();
 	}
}
