package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.Layer;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyBlock;
import com.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket;
import com.raytheon.uf.common.dataplugin.radar.level3.TextSymbolPacket;
import com.raytheon.uf.common.dataplugin.shef.tables.DAARadar;
import com.raytheon.uf.common.dataplugin.shef.tables.DAARadarId;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Class to handle DAA radar product processing for MPE. 
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 2013 DCS 167    P. Tilles   Initial Creation
 * 
 * </pre>
 * 
 * 
 * @author Paul Tilles
 * 
 */


public class DAAProductProcessor
{
	
	
	
	private final static byte NO_DATA_FLAG = 0;
	private final static float DECODED_DAA_NO_DATA_FLAG = -98.0f;

	private int decodeWindowInMinutes = 30;
	private int minimumCoverageDuration = 60;
	private boolean useTimeWindowFilter = false;
	private String outputGridDirectory = "/tmp";

	IUFStatusHandler statusHandler = null;
	
	protected Class<?> daoClass = null;
	
	// ---------------------------------------------------------------------
	public DAAProductProcessor() 
	{
		this.statusHandler = null;
		
	//	readAppsDefaults();
		
	}
	
	public DAAProductProcessor(IUFStatusHandler statusHandler) 
	{
		this.statusHandler = statusHandler;
		
		readAppsDefaults();
		
	}
	
	// ---------------------------------------------------------------------

	private void readAppsDefaults()
	{	
		AppsDefaults ad = AppsDefaults.getInstance();
		
		decodeWindowInMinutes = ad.getInt("daa_decode_window", 5);
		minimumCoverageDuration = ad.getInt("daa_min_coverage_dur", 60);
		useTimeWindowFilter = ad.getBoolean("daa_filter_decode", true);
		
		outputGridDirectory = ad.getToken("daa_grid_dir", null);
		
		return;
	}

	// ---------------------------------------------------------------------

	public void process(RadarRecord record)
	{
		DAAHeaderData headerData = new DAAHeaderData();
		boolean isProductNull = false;

		processHeader(record, headerData);
		
		isProductNull = headerData.isProductNull();
		

		if (isProductNull)
		{
			processNullProduct(record, headerData);
		}
		else
		{
			processGriddedDataProduct(record, headerData);
		}
	}
	
	// ---------------------------------------------------------------------
	
	private void processHeader(RadarRecord record, DAAHeaderData headerData)
	{
					
		short nullProductFlag;
		
		float maxValh, biasValue;
		String mnemonic, icao;
		
		mnemonic = record.getMnemonic();	
		headerData.setMnemonic(mnemonic);
		
		icao = record.getIcao().substring(1);
		String radid = icao.toUpperCase();
		headerData.setRadarId(radid);

		
		String uri = record.getDataURI();
		headerData.setUri(uri);
		
		nullProductFlag = record.getProductDependentValue(2);
		headerData.setNullProductFlag(nullProductFlag);
		if (nullProductFlag > 0)
		{
			headerData.setIsProductNull(true);
		}
		else
		{
			headerData.setIsProductNull(false);
		}
			
		
		maxValh = (float)(record.getProductDependentValue(3)/10.);
		headerData.setMaxValh(maxValh);
		
		biasValue = (float)(record.getProductDependentValue(6)/100.);
		headerData.setBiasValue(biasValue);
		
		String obsTime = getProductObsTimeString(record, headerData);
		headerData.setObsTime(obsTime);
			
		String productGenerationTime = getProductGenerationTimeString(record);
		headerData.setProductGenerationTime(productGenerationTime);
			
		//  scale = in A1:  HW 31,32 . In A2: thresholds - 0,1 
		//  offset = in A1: HW 33,34 . In A2: thresholds - 2,3
		float scale = HydroNumericUtility.convertShortsToFloat(record.getThreshold(0), record.getThreshold(1));		
		headerData.setScale(scale);
				
		float offset = HydroNumericUtility.convertShortsToFloat(record.getThreshold(2), record.getThreshold(3));
		headerData.setOffset(offset);
		
		return;
	}
	
	
	// ---------------------------------------------------------------------
	private void processNullProduct(RadarRecord record, DAAHeaderData headerData)
	{
		
		float maxVald = -1;
		String fileName = "xxxxxxxxxxxxxxxx";
		boolean withinTOHWindow = false;
		
		statusHandler.handle(Priority.INFO,
				"Thread id = " + Thread.currentThread().getId());
		
		statusHandler.handle(Priority.INFO, "\n" +
				"DAA product: uri = " + headerData.getUri());

		statusHandler.handle(Priority.INFO, "\n" +
				"DAA product:  radar id  = "  + headerData.getRadarId()
				+ " null product flag = " + headerData.getNullProductFlag()
				+ " maxValh= "     + headerData.getMaxValh()
				+ " biasValue = "  + headerData.getBiasValue() + "\n"
				+ " obsTime  = "   + headerData.getObsTime()
				+ " productTime  = "   + headerData.getProductGenerationTime()
				+ " minOff   = "   + headerData.getMinutesOffTopOfHour());
		
		if(useTimeWindowFilter)
		{	
			withinTOHWindow = TopOfHourCheck(decodeWindowInMinutes, headerData.getObsTime(),
					headerData.getMinutesOffTopOfHour());
		}
		else
		{
			withinTOHWindow = true;
		}

		if(!withinTOHWindow)
		{
			statusHandler.handle(Priority.INFO, " DAA product is not TOH" );
		}
		else
		{
			String nullProductDateTimeString = processNullProductString(headerData, record);
			
			// compare null product date/time with end date/time (obstime) of product
			// if time diff is > daa_min_coverage_dur, then set decoded product to all 0.0
			// else set decoded product to all missing
			// if null product string does not have a date/time, then set decoded field to all missing
		
			if(nullProductDateTimeString != null)
			{
				//compare times
				// change date/time strings to millisec and subtract
				// compare result against daa_min_coverage_dur in millisec
				
				String obsTimeString = headerData.getObsTime();

				long nullProductTimeInMillis = HydroTimeUtility.getMillisFromAmericanDateTimeString(nullProductDateTimeString);
				long obsTimeInMillis = HydroTimeUtility.getMillisFromSQLString(obsTimeString);
				long minCoverageInMillis = minimumCoverageDuration * 60 * 1000;
				long millisSincePrecipDetected = obsTimeInMillis - nullProductTimeInMillis;

				int minutesSincePrecipDetected = (short) (millisSincePrecipDetected/60000);
				statusHandler.handle(Priority.INFO, "\n" +
						"  minutesSincePrecipDetected = "  + minutesSincePrecipDetected);
				headerData.setCoverageDur(minutesSincePrecipDetected);

                /*
                //The following commented-out code is a placeholder for future changes anticipated from
                //CCR NA12-00264  which will add the reporting of the total number of minutes of operation (<=60)
                //for which the product applies

				if(millisSincePrecipDetected >= minCoverageInMillis)
				{
					
					//set decoded field to all 0.0
				}
				else
				{
					
					// set decoded field to all missing
				}
                */
				
			}


            /*
                //See above comment about CCR NA12-00264

			else
			{
				
				// set decoded product to all missing
				
			}
            */

			// write record to the DAARadar table of the IHFS db	
			writeToDAARadarTable(maxVald, fileName, headerData);
		}
	}

	// ---------------------------------------------------------------------

	
	private String processNullProductString(DAAHeaderData headerData, RadarRecord record)
	{
		String dateTimeString = null;
		
		SymbologyBlock symbologyBlock = record.getSymbologyBlock();

		if (symbologyBlock == null)
		{
			// problem reading symbology block

			statusHandler.handle(Priority.INFO,
			"Null Product text not found (symbology block not found.");
		}
		else 
		{
			// symbology block read successfully

			for (Layer layer : symbologyBlock.getLayers())
			{
				for (SymbologyPacket packet : layer.getPackets())
				{
					if (packet instanceof TextSymbolPacket) 
					{
						TextSymbolPacket textSymbolPacket = (TextSymbolPacket) packet;

						// read and print null product string
						String nullProductText = textSymbolPacket.getTheText().trim();
						statusHandler.handle(Priority.INFO, nullProductText);

						// if string contains a date/time, then parse it out
						if(headerData.getNullProductFlag() == 5 && !nullProductText.matches(".*RPG.*"))
						{
							dateTimeString = nullProductText.substring(32,nullProductText.length()-1);
							statusHandler.handle(Priority.INFO, " date/time = " + dateTimeString);
							
						} //end if (!nullProductText.matches("RPG"))
					} //end if (packet instanceof TextSymbolPacket) 
				} //end  for (SymbologyPacket packet : layer.getPackets())
			} // end for (Layer layer : symbologyBlock.getLayers())

		} // end if (symbologyBlock == null)

		return dateTimeString;
		
	} //end processNullProductString()
	
	// ---------------------------------------------------------------------
	private String getProductObsTimeString (RadarRecord record, DAAHeaderData headerData)
	{
		int prodObsDate = record.getProductDependentValue(4);
		
		int prodObsTime = record.getProductDependentValue(5);
		int prodObsHour = prodObsTime/60;
		int prodObsMin  = prodObsTime % 60;
		
		headerData.setProdDate(prodObsDate);
		headerData.setProdHour(prodObsHour);
		headerData.setProdMin(prodObsMin);
		
		headerData.setProductObsTimeMinutes(prodObsMin);
				
		String obsTime = 
			String.format("%s %02d:%02d:00", 
					HydroTimeUtility.JulianDateConvertToYMD(prodObsDate),prodObsHour, prodObsMin);
		
		// minOff = minutes off the top-of-the-hour of product
		//          for values > 30, minOff = (-1) * (60 - minOff)
	
		short minOff = (short) headerData.getProductObsTimeMinutes();
		if(minOff > 30) 
		{
			minOff = (short) (-1 * (60 - minOff));
		}
		
		headerData.setMinutesOffTopOfHour(minOff);
		
		return obsTime;
	}
	// ---------------------------------------------------------------------
	private String getProductGenerationTimeString (RadarRecord record)
	{
		// product gen date is in HW = 24 of the Product Description Block
		// product gen time is in HW = 25, 26 of the PDB
		
		Date productGenDateTime = record.getVolScanTime();
		SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		String productGenerationDateTimeString = format.format(productGenDateTime);
		
		
		return productGenerationDateTimeString;
		
	}
	
	// ---------------------------------------------------------------------
	
	private void processGriddedDataProduct(RadarRecord record, DAAHeaderData headerData)
	{

		boolean withinTOHWindow = false;
		String obsTime = headerData.getObsTime();
		short minOff = headerData.getMinutesOffTopOfHour();
		
		statusHandler.handle(Priority.INFO,
				"Thread id = " + Thread.currentThread().getId());
		
		statusHandler.handle(Priority.INFO, "\n" +
				"DAA product: uri = " + headerData.getUri());

		statusHandler.handle(Priority.INFO, "\n" +
				"DAA product:  radar id  = "  + headerData.getRadarId()
				+ " null product flag = " + headerData.getNullProductFlag()
				+ " scale = "      + headerData.getScale()
				+ " offset = "     + headerData.getOffset()
				+ " maxValh= "     + headerData.getMaxValh()
				+ " biasValue = "  + headerData.getBiasValue() + "\n"
				+ " obsTime  = "   + obsTime
				+ " productTime  = "   + headerData.getProductGenerationTime()
				+ " minOff   = "   + minOff
				);
		
		// Check if product is within allowable window around the TOH AND
		//  if the current product is closer to TOH than a previous product
		//  deemed to be TOH
		// If product is outside of window, then do not process further 

		if(useTimeWindowFilter)
		{	
			withinTOHWindow = TopOfHourCheck(decodeWindowInMinutes, obsTime, minOff);
		}
		else
		{
			withinTOHWindow = true;
		}

		if(!withinTOHWindow)
		{
			statusHandler.handle(Priority.INFO, " DAA product is not within window around TOH" );
		}
		else
		{

			// Check if product is closer to TOH than a previous product
			//  deemed to be TOH
			// If current product is closer to TOH, then process it 
			// Else do not process the current product
			// If current product and a previous product are same number of minutes off TOH, then
			//  current product is considered closer and is processed
			
			boolean closerToTOH = CloserToTOHCheck(decodeWindowInMinutes, headerData.getRadarId(), obsTime, minOff);
			//boolean closerToTOH = true;
			
			if(!closerToTOH)
			{
				statusHandler.handle(Priority.INFO, " DAA product closer to TOH already processed -- current product not processed" );
			}
			else
			{

				byte[] dataArray = record.getRawData();

				if(dataArray == null)
				{
					statusHandler.handle(Priority.INFO, "DAA Product: ERROR --- dataArray is null");
				}

				else
				{
					int numlevel = record.getNumLevels();
					int numRangeBins = record.getNumBins();
					int numRadials = record.getNumRadials();

					float lat = record.getLatitude();
					float lon = record.getLongitude();

					float scale = headerData.getScale();
					float offset = headerData.getOffset();

					// transform polar grid to HRAP
					float[][] polarGrid = getPolarGridFromDataArray(dataArray, scale, offset, numRadials, numRangeBins);

					PolarToHRAPTransformer transformer = new PolarToHRAPTransformer();

					float[] hrapGrid = transformer.transform250MeterPolarToFullHRAP(polarGrid, lat, lon);

					float[] dbaArray = transformer.transformHrapPrecipToDBA(hrapGrid, DECODED_DAA_NO_DATA_FLAG);			

					//write DAA decoded file

					int prodDate = headerData.getProdDate();
					int prodHour = headerData.getProdHour();
					int prodMin = headerData.getProdMin();

					String radid = headerData.getRadarId();

					String fileName = String.format("%s%s%02d%02dZ", radid, 
							HydroTimeUtility.JulianDateConvertToMDY(prodDate),prodHour, prodMin);

					writeOutDataFile(record, dbaArray, fileName);

					// determine maximum precip value from hrapGrid array
					// store value in maxVald variable

					float maxVald = 0;

					for (int i = 0; i < (131*131); i++)
					{
						if(hrapGrid[i] > maxVald) maxVald = hrapGrid[i];
					}

					// write record to the DAARadar table of the IHFS db

					long minutesSincePrecipDetected = 0;
					writeToDAARadarTable(maxVald, fileName, headerData);

				} //end else dataArray != null

			} // end if(!closerToTOH)

		} // end if(!withinTOHWindow)

	} //end processGriddedDataProduct()
	
	//-----------------------------------------------------------------

	private boolean CloserToTOHCheck(int decodeWindowInMinutes, String radid, String obsTime, short minOff)
	{
		
		short tohHour, afterHour, beforeHour;
		String obsHourString, tohDate, afterDate, beforeDate, tohDateTime, dateTime;
		
		// Check if current product is closer to TOH than a previous product deemed to be TOH
		// If current product is closer to TOH, then process it 
		// Else do not process the current product
		// If current product and a previous product are same number of minutes off TOH, then
		//  current product is considered closer and is processed
		// If current product is exactly at TOH (minoff = 0), then process it (i.e. return true)
		
		// This check is for decoding only (i.e. not for archive)
		
		// Read records from the DAARadar table to determine if previous products for current hour
		//  have been processed
		
		// if hour of current product = 0, then products from before the TOH will have previous day's date with hour = 23
		// if hour of current product = 23, then products from after the TOH will have next day's date with hour = 0
		
		if (minOff == 0) 
		{
				return true;
		}
		
		obsHourString = obsTime.substring(11, 13);
		
		if(minOff > 0)
		{
			// determine TOH date/hour from product
			// TOH date/hour = after date/hour
			tohHour = Short.parseShort(obsHourString);
			tohDate = obsTime.substring(0,10);
			
			afterHour = tohHour;
			afterDate = tohDate;
			
			// determine before (previous) date and hour
			
			beforeHour = (short) (afterHour - 1);
			beforeDate = afterDate;
			if(beforeHour == -1)
			{
				
				beforeHour = 23;
				beforeDate = HydroTimeUtility.changeDate(afterDate, -1);
			}
			
		}
		else
		{
			// determine before date/hour from product and next date/hour
			
			beforeHour = Short.parseShort(obsHourString);
			beforeDate = obsTime.substring(0,10);
			
			// determine after date/hour 
			// determine TOH date/hour
			
			afterHour = (short) (beforeHour + 1);
			tohHour = afterHour;
			afterDate = beforeDate;
			
			if(afterHour == 24)
			{
				afterHour = 0;
				tohHour = 0;
				afterDate = HydroTimeUtility.changeDate(beforeDate, 1);	
			}
			
			tohHour = afterHour;
			tohDate = afterDate;
		}
		
		// search DAARadar table for TOH product
		// SELECT * FROM DAARadar WHERE obstime = tohDateTime
		// if record exists, then return false
		// else continue processing in next section of code
		
		tohDateTime = String.format("%s %02d:00:00", tohDate,tohHour);
			
		// check for an existing TOH
		DAARadar radar = selectDAARadarRecord(radid, tohDateTime); 
		if (radar != null )
		{	
			//record exists, so return false
			return false;
		}
		
		// no TOH product previously processed
		// search DAARadar table for records before the TOH and after the TOH and within the TOH window
		//  for the given radar
		// compare the minoff values for each product to determine if current product is closer to the TOH
		//  than a previous product
		// if minOff = 1, then fall through for loop and return true
		
		short aminOff = minOff;
		if (aminOff < 0) aminOff = (short) (-1 * minOff);
		
		boolean selectResult = false;
		
		for(int i = 1; i < aminOff; i++)
		{

			// check for record after TOH
			dateTime = String.format("%s %02d:%02d:00", afterDate,afterHour,i);

			radar = selectDAARadarRecord(radid, dateTime); 
			if (radar != null )
			{
				selectResult = true;
				break;
			}
			
			// check for record before TOH
			int ib = 60 - i;
			dateTime = String.format("%s %02d:%02d:00", beforeDate,beforeHour,ib);
		
			radar = selectDAARadarRecord(radid, dateTime); 
			if (radar != null )
			{
				selectResult = true;
				break;
			}
			
		}
		
		boolean isCloser = true;
		
		//We found an existing product closer to the top of the hour than the
		//product we are currently processing
		if (selectResult)
		{
			isCloser = false;
		}
		
		return isCloser;
	}

	// ---------------------------------------------------------------------
	private void writeToDAARadarTable(float maxValueFromData, String fileName, DAAHeaderData d)
	{
		statusHandler.handle(Priority.INFO, "\n" +
		"In routine writeToDAARadarTable - before write to DAAradar table");
	
		writeToDAARadarTable(d.getRadarId(), d.getObsTime(), d.getMinutesOffTopOfHour(),
				d.getMaxValh(), maxValueFromData, d.getBiasValue(), d.getProductGenerationTime(),
				d.getNullProductFlag(), d.getCoverageDur(), fileName);
		
		statusHandler.handle(Priority.INFO, "\n" +
		"In routine writeToDAARadarTable - after write to DAAradar table");

	}
	// ---------------------------------------------------------------------
	
	private DAARadar selectDAARadarRecord(String radarId, String obsTimeString)
	{
		Date obstime = HydroTimeUtility.getDateFromSQLString(obsTimeString);
		
		return selectDAARadarRecord(radarId, obstime);	
	}
	
	// ---------------------------------------------------------------------
	private DAARadar selectDAARadarRecord(String radarId, Date obstime)
	{
		List<DAARadar> recordList = selectDAARadarRecordList(radarId, obstime);
	
		DAARadar daaRadar = null;
		
		if ( (recordList != null) && (recordList.size() > 0) )
		{
			daaRadar = recordList.get(0);
			//System.out.printf("SELECTed daaRadar object = %s\n" , daaRadar.toString());
		
		}
		
		/*
		else
		{
			System.out.printf("daaRadar object is null\n");
		}
		*/
	
		return daaRadar;
	}
	
// -----------------------------------------------------------------------
	
	private List<DAARadar> selectDAARadarRecordList(String radarId, Date obstime)
	{
		List<DAARadar> recordList = null;
		
		try
		{		
			CoreDao dao = new CoreDao(DaoConfig.forDatabase("ihfs"));
			
			 /** The class associated with this dao */
			dao.setDaoClass("com.raytheon.uf.common.dataplugin.shef.tables.DAARadar");
		    DatabaseQuery query = new DatabaseQuery(dao.getDaoClass().getName());
			
			//key fields from table
			query.addQueryParam("id.radid", radarId);
			query.addQueryParam("id.obstime", obstime);
			
			recordList = (List<DAARadar> ) dao.queryByCriteria(query);
		} 
		
		catch (Exception e) 
		{
			e.printStackTrace();
			System.out.printf("daaRadar object == null\n");
		}
			
		return recordList;
			
	}
	
	// ---------------------------------------------------------------------

	private boolean TopOfHourCheck(int window, String obsTime, short minOff)
	{
		// aminoff = abs value of minOff
		short aminoff;
		boolean retCode = true;
		
		aminoff = minOff;
		if(aminoff < 0) aminoff = (short) (-1 * aminoff);
		
		if(aminoff <= window)
		{
			retCode = true; // product is within top-of-hour (TOH) window
		}
		else
		{
			retCode = false;  // product not within window around TOH
		}
		
		return retCode;
		
	}
	
	// ---------------------------------------------------------------------

	private float[][] getPolarGridFromDataArray(byte[] dataArray, float scale, float offset, int numRadials, int numRangeBins) {
		
		//convert 1D array to 2D polar array 
		//transform to precip value using scale and offset		
		// according to the ICD, each grid value is transformed to a precip value as
		//  F = (N - offset)/scale
		//  where F = resulting float precip value
		//        N = byte precip value read from the gridded data
		
		float[][] polarGrid = new float[numRadials][numRangeBins];
		int index = 0;
		
		for (int r = 0; r < numRadials; r++)
		{
			for (int b = 0; b < numRangeBins; b++)
			{
				byte byteValue = dataArray[index];
				short shortValue = (short)(byteValue & 0x00ff);
				
				float doubleValue = 0;
				
				if (shortValue == NO_DATA_FLAG)
				{
					doubleValue = NO_DATA_FLAG;
				}
				else 
				{
					doubleValue = (shortValue - offset)/scale;
				}
				
				polarGrid[r][b] = doubleValue;
								
				index++;
			} //end for b (bins)
			
			
		} //end for r (radials)
		
		return polarGrid;
	}

	// ---------------------------------------------------------------------
	
	private void printNonZeroRadialInDataArray(byte[] dataArray, 
											   int numRadials, int numRangeBins,
											   String fileName)
	{
		int index = -0;
		boolean foundRadial = false;
		
		int nonZeroRadial = -1;
		
		for (int r = 0; r < numRadials; r++)
		{
			for (int b = 0; b < numRangeBins; b++)
			{
				
				byte byteValue = dataArray[index];
				
				if (! foundRadial)
				{
					if (byteValue > 0)
					{
						nonZeroRadial = r;
						printRadial(dataArray, nonZeroRadial, numRadials, numRangeBins, fileName);
						foundRadial = true;
						break;
					}
				}
				index++;
			}
			if (foundRadial)
			{
				break;
			}
			
		}
	
	}

	// ---------------------------------------------------------------------
	
	private void printRadial(byte[] dataArray, int radialIndex,
							 int numRadials, int numRangeBins,
							 String fileName)
	{
		PrintWriter writer = null;
		
		try {
			String fullPathName = "/awips2/edex/logs/" + fileName;

			writer = new PrintWriter(new File(fullPathName));

			int index = 0;

			for (int r = 0; r < numRadials; r++)
			{
				if (r == radialIndex)
				{
					writer.print("Nonzero Radial = " + radialIndex + "\n");
				}
				
				for (int b = 0; b < numRangeBins; b++)
				{
					if (r == radialIndex)
					{
						byte byteValue = dataArray[index];
						short shortValue = (short)(byteValue & 0x00ff);
						
						writer.print(shortValue);
						writer.print(" ");
					}

					index++;
				}

			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		finally
		{
			if (writer != null)
			{
				try {
					writer.close();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
	}
	
	// ---------------------------------------------------------------------
	
	private void writeHrapGrid(double[] hrapGrid, String fileName) {
		
		PrintWriter writer = null;
		
		try {
			String fullPathName = "/awips2/edex/logs/" + fileName;

			writer = new PrintWriter(new File(fullPathName));

			int totalGridBins = 131 * 131;
			
			for (int i = 0; i < totalGridBins; i++)
			{
				Double doubleObject = hrapGrid[i];
				writer.write(doubleObject.toString() + " ");
			
				if (i % 131 == 0)
				{
					writer.write("\n");
				}
			}		
			
			writer.write("\n\n");

		} catch (Exception e) {

			e.printStackTrace();
		}	

		finally
		{
			if (writer != null)
			{
				try {
					writer.close();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
			
	} //end writeHrapGrid()
	
	// ---------------------------------------------------------------------
	
	private void writeDbaGrid(float[] dbaGrid, String fileName) {
		
		PrintWriter writer = null;
		
		try {
			String fullPathName = "/awips2/edex/logs/" + fileName;

			writer = new PrintWriter(new File(fullPathName));

			int totalGridBins = 131 * 131;
			
			for (int i = 0; i < totalGridBins; i++)
			{
				Float floatObject = dbaGrid[i];
				writer.write(floatObject.toString() + " ");
			
				if (i % 131 == 0)
				{
					writer.write("\n");
				}
			}		
			
			writer.write("\n\n");

		} catch (Exception e) {

			e.printStackTrace();
		}	

		finally
		{
			if (writer != null)
			{
				try {
					writer.close();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
			
	} //end writeDbaGrid()
	
	// ---------------------------------------------------------------------
	
	private void writePolarGrid(double[][] polarGrid, int numRadials,
			int numRangeBins, String fileName) {
		
		PrintWriter writer = null;
		
		try {
			String fullPathName = "/awips2/edex/logs/" + fileName;

			writer = new PrintWriter(new File(fullPathName));


			for (int r = 0; r < numRadials; r++)
			{
				for (int b = 0; b < numRangeBins; b++)
				{
					Double doubleObject = polarGrid[r][b];
					writer.write(doubleObject.toString() + " ");
				} //end for b (bins)
				writer.write("\n");
			}		
			writer.write("\n\n");

		} catch (Exception e) {

			e.printStackTrace();
		}	

		finally
		{
			if (writer != null)
			{
				try {
					writer.close();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
			
	} //end writePolarGrid()

	
	// ---------------------------------------------------------------------
	private void writeOutDataFile(RadarRecord record, float[] dbaArray, String fileName)
	{
		DataOutputStream outputStream = null;
		
		
		//fileName is of the form RRRmmddyyyyhhmmZ
		try {
			
			String fullPathName = outputGridDirectory + "/" + fileName;
			statusHandler.handle(Priority.INFO, "\n" + " writing to file = " + fullPathName);
			
			FileOutputStream fileOutputStream = new FileOutputStream(fullPathName);
			outputStream = new DataOutputStream(fileOutputStream);
		
			int hrapXMax = 131;
			int hrapYMax = 131;
			
			int totalGridBins = hrapXMax * hrapYMax;
			
			for (int i = 0; i < totalGridBins; i++)
			{			
				int intValue = HydroNumericUtility.getSwappedIntBytesFromFloat(dbaArray[i]);
				
				outputStream.writeInt(intValue);
			}			
			
			outputStream = null;
			
		} catch (Exception e) {

			e.printStackTrace();
		}	

		finally
		{
			if (outputStream != null)
			{
				try {
					outputStream.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
			
			//writer.print(d)
		
	}
	
	// ---------------------------------------------------------------------
	
	private void writeToDAARadarTable(String radid, String obstime, short minOff, float maxValh,
			float maxVald, float s1BiasValue, String productTime, short nullProductFlag,
			int coverageDur, String gridFilename)
	{
		
		//DAARadar is a PersistableDataObject
		DAARadarId id = new DAARadarId();
		id.setRadid(radid);
		
		Date obstimeDate = HydroTimeUtility.getDateFromSQLString(obstime);
		id.setObstime(obstimeDate);
		
		Date productTimeDate = HydroTimeUtility.getDateFromSQLString(productTime);
		DAARadar radarObject = new DAARadar(id, 
											minOff, maxValh, maxVald,
											s1BiasValue, productTimeDate,
											nullProductFlag, coverageDur,
											gridFilename);
		try {
		    CoreDao dao = new CoreDao(DaoConfig.forDatabase("ihfs"));
		    dao.saveOrUpdate(radarObject);
		                
		} catch (Exception e) {
			e.printStackTrace();
		}

	}
	
	// ---------------------------------------------------------------------
	
} //end DAAProductProcessor
