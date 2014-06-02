package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;
import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Date;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.shef.tables.DPRRadar;
import com.raytheon.uf.common.dataplugin.shef.tables.DPRRadarId;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus.Priority;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Class to handle DPA radar product processing for MPE. 
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



public class DPRProductProcessor
{
	private final static byte NO_DATA_FLAG = 0;
	
	private String outputGridDirectory = "/tmp";
	private static final int MAX_IHRAP = PolarToQuarterHRAPTransformer.MAX_IHRAP;
	private static final int MAX_JHRAP = PolarToQuarterHRAPTransformer.MAX_JHRAP;
	
	IUFStatusHandler statusHandler = null;
	
	// ---------------------------------------------------------------------
	
	public DPRProductProcessor(IUFStatusHandler statusHandler) 
	{
		this.statusHandler = statusHandler;
		
		readAppsDefaults();
	}
	
	// ---------------------------------------------------------------------

	private void readAppsDefaults()
	{	
		AppsDefaults ad = AppsDefaults.getInstance();
		
		outputGridDirectory = ad.getToken("dpr_grid_dir", null);
		
		return;
	}

	// ---------------------------------------------------------------------
	
	public void process(RadarRecord record)
	{
		DPRHeaderData headerData = new DPRHeaderData();

		// process header portion of DPR product
		// precip mode and non-precip mode products have headers
		
		processHeader(record, headerData);
		
		// if radar is in precip mode, then process gridded portion
		// else grid is all 0's

		if(headerData.getPrecipDetectedFlag() == 1)
		{

			// radar in precip mode
			processGriddedDataProduct(record, headerData);

		}
		else
		{

			// radar in clear air mode
			
			// write record to DPRRadar table
			writeToDPRRadarTable(headerData);
			
			// write decoded file
			// precip values are all 0

			float[][] hrapGrid = new float[MAX_IHRAP][MAX_JHRAP];

			for(int ihrap = 0; ihrap < MAX_IHRAP; ihrap++)
			{
				for (int jhrap = 0; jhrap < MAX_JHRAP; jhrap++)
				{
					hrapGrid[ihrap][jhrap] = 0.f;
				}
			}

			String fileName = headerData.getFileName();
			writeOutDataFile(record, headerData, hrapGrid, fileName);
		
		}
		
	}
	
	// ---------------------------------------------------------------------
	private void processHeader(RadarRecord record, DPRHeaderData headerData)
	{
					
		String mnemonic = record.getMnemonic();	
		headerData.setMnemonic(mnemonic);
		
		String icao = record.getIcao().substring(1);
		String radid = icao.toUpperCase();
		headerData.setRadarId(radid);
		
		String uri = record.getDataURI();
		headerData.setUri(uri);
		
		// according to the ICD:
		// high byte of halfWord30 is Precipitation Detected Flag
		// low  byte of halfWord30 is Gage Bias to be Applied Flag
		short halfWord30 = record.getProductDependentValue(2);
		short precipDetectedFlag = HydroNumericUtility.getHighOrderByte(halfWord30);
		headerData.setPrecipDetectedFlag(precipDetectedFlag);
		short biasAppliedFlag = HydroNumericUtility.getLowOrderByte(halfWord30);
		
		String obsTime = getProductObsTimeString(record, headerData);
		headerData.setObsTime(obsTime);
		
		float maxVal = (float)(record.getProductDependentValue(3)/1000.);
		headerData.setMaxVal(maxVal);
		
		short biasValue = (short)(record.getProductDependentValue(6));
		headerData.setBiasValue(biasValue);
		
		// operationalMode = 1 -- clear air mode
		//                 = 2 -- precip mode 
		short operationalMode = record.getOperationalMode().shortValue();
		headerData.setOperationalMode(operationalMode);
		
		short volumeCoveragePattern = record.getVolumeCoveragePattern().shortValue();
		headerData.setVolumeCoveragePattern(volumeCoveragePattern);
		
	    //  scale = in A1:  HW 31,32 . In A2: thresholds - 0,1 
		//  offset = in A1: HW 33,34 . In A2: thresholds - 2,3
		float scale = HydroNumericUtility.convertShortsToFloat(record.getThreshold(0), record.getThreshold(1));		
		headerData.setScale(scale);
				
		float offset = HydroNumericUtility.convertShortsToFloat(record.getThreshold(2), record.getThreshold(3));
		headerData.setOffSet(offset);
		
		int volScanDate = record.getProductDependentValue(0);
		headerData.setVolumeScanDate(volScanDate);
		
		int volScanTime = record.getProductDependentValue(1) * 60;
		headerData.setVolumeScanTime(volScanTime);
		
		int prodDate = headerData.getProdDate();
		int prodHour = headerData.getProdHour();
		int prodMin = headerData.getProdMin();
		String fileName = String.format("DPR%s%s%02d%02dZ", radid, 
				HydroTimeUtility.JulianDateConvertToMDY(prodDate),prodHour, prodMin);
		headerData.setFileName(fileName);
		
		statusHandler.handle(Priority.INFO,
				"Thread id = " + Thread.currentThread().getId());
		
		statusHandler.handle(Priority.INFO, "\n" +
				"DPR product: uri = " + headerData.getUri());

		statusHandler.handle(Priority.INFO, "\n" +
				"DPR product:  radar id  = "  + radid
				 + " obsTime  = "   + obsTime
				 + " volumeCoveragePattern = " + volumeCoveragePattern
				 + " operationalMode = " + operationalMode
				 + " maxVal = "     + maxVal + "\n"
				 + " scale = "      + scale
				 + " offset = "     + offset
				 + " biasValue = "  + biasValue + "\n"
				 + " volume scan date = " + volScanDate
				 + " volume scan time = " + volScanTime
 				 + " precipDetectedFlag = " + precipDetectedFlag
				 + " biasAppliedFlag = " + biasAppliedFlag
				 + " fileName = "        + fileName
				 		);	

	}
	
	// ---------------------------------------------------------------------
	
	private void processGriddedDataProduct(RadarRecord record, DPRHeaderData headerData)
	{

		// DAA and DSA product processing use: byte[] dataArray = record.getRawData();
		
		short[] rawShortDataArray = record.getRawShortData();
		if(rawShortDataArray == null)
		{
			statusHandler.handle(Priority.INFO, "\nDPR Product: ERROR --- rawShortDataArray is null");
		}
		else
		{

			int numRangeBins = record.getNumBins();
			int numRadials = record.getNumRadials();

			float lat = record.getLatitude();
			float lon = record.getLongitude();
			
			statusHandler.handle(Priority.INFO, "\n"
					+ "DPR product:"
					+ " numbin = " + numRangeBins
					+ " numradial = " + numRadials
					+ " lat = " + lat
					+ " lon = " + lon
			);
			
			// transform polar to HRAP grid
			
			float scale = headerData.getScale();
			float offset = headerData.getOffSet();
			String fileName = headerData.getFileName();
			
			float[][] polarGrid = getPolarGridFromDataArray(rawShortDataArray, scale, offset, numRadials, numRangeBins);

		//	GridChecker checker = new GridChecker();
		//	checker.writePolarGrid(polarGrid, numRadials, numRangeBins, outputGridDirectory + "/" + fileName + ".polar.txt");
			
			PolarToQuarterHRAPTransformer transformer = new PolarToQuarterHRAPTransformer(statusHandler);
			float[][] hrapGrid = transformer.transform250MeterPolarToQuarterHRAP(polarGrid, lat, lon);
			
			
		
			writeToDPRRadarTable(headerData);
			writeOutDataFile(record, headerData, hrapGrid, fileName);
			
		//	GridUtility checker = new GridUtility();
	        //	checker.writeFloatGrid(hrapGrid, MAX_IHRAP, MAX_JHRAP, outputGridDirectory + "/" + fileName + ".txt"); 
		}
		
	}
	
	// ---------------------------------------------------------------------
	
    private float[][] getPolarGridFromDataArray(short[] dataArray, float scale, float offset, int numRadials, int numRangeBins) {
		
		//convert 1D array to 2D polar array 
		//transform to precip value using scale and offset		
		// according to the ICD, each grid value is transformed to a precip value as
		//  F = (N - offset)/scale
		//  where F = resulting float precip value
		//        N = short precip value read from the gridded data
		
		float[][] polarGrid = new float[numRadials][numRangeBins];
		int index = 0;
		
		for (int r = 0; r < numRadials; r++)
		{
			for (int b = 0; b < numRangeBins; b++)
			{
				short shortValue = dataArray[index];
				
				float floatValue = 0;
				
				if (shortValue == NO_DATA_FLAG)
				{
					floatValue = NO_DATA_FLAG;
				}
				else 
				{
					floatValue = (shortValue - offset)/scale;
				//	floatValue *= 0.254f;//change units from hundredths of inches to mm 
					floatValue *= 25.4f;//change units from hundredths of inches to mm 
				}
				
				polarGrid[r][b] = floatValue; 
								
				index++;
			} //end for b (bins)
			
			
		} //end for r (radials)
		
		return polarGrid;
	}

	// ---------------------------------------------------------------------
	private String getProductObsTimeString(RadarRecord record, DPRHeaderData headerData)
	{
		
		//prodObsDate and prodObsTime for DPR products are in a different location in the 
		// ProductDependentValue array than the DAA products
		
		int prodObsDate = record.getProductDependentValue(0);
		
		int prodObsTime = record.getProductDependentValue(1);
		int prodObsHour = prodObsTime/60;
		int prodObsMin  = prodObsTime % 60;
		
		headerData.setProdDate(prodObsDate);
		headerData.setProdHour(prodObsHour);
		headerData.setProdMin(prodObsMin);
		headerData.setProductObsTimeMinutes(prodObsMin);
				
		String obsTime = 
			String.format("%s %02d:%02d:00", 
					HydroTimeUtility.JulianDateConvertToYMD(prodObsDate),prodObsHour, prodObsMin);
		
		//statusHandler.handle(Priority.INFO, "\n" +
		//		"DPR product: prodDate = " + prodObsDate 
		//				   + " prodObsTime = " + prodObsTime
		//		           + " prodHour = " + prodObsHour
		//		           + " prodMin  = " + prodObsMin
		//		);
		
		return obsTime;
	}
	
	// ---------------------------------------------------------------------
	private void writeToDPRRadarTable(DPRHeaderData d)
	{
		statusHandler.handle(Priority.INFO, "\n" +
		"In routine writeToDPRRadarTable - before write to DPRradar table");

		writeToDPRRadarTable(d.getRadarId(), d.getObsTime(),
				d.volumeCoveragePattern, d.operationalMode,
				d.getMaxVal(), d.getScale(), d.getOffSet(), 
				d.volumeScanDate, d.volumeScanTime,
				d.getBiasValue(), d.precipDetectedFlag,
				d.getFileName());
		
		statusHandler.handle(Priority.INFO, "\n" +
		"In routine writeToDPRRadarTable - after write to DPRradar table");

	}
	
	// ---------------------------------------------------------------------
	private void writeToDPRRadarTable(String radid, String obstime,
			short volumeCoveragePattern, short operationalMode,
			float maxVal, float scale, float offset,
			int volumeScanDate, int volumeScanTime,
			short biasValue, short precipDetectedFlag,
			String filename)
	{
		
		//DPRRadar is a PersistableDataObject
		DPRRadarId id = new DPRRadarId();
		id.setRadid(radid);
		
		Date obstimeDate = HydroTimeUtility.getDateFromSQLString(obstime);
		id.setObstime(obstimeDate);
		
		DPRRadar radarObject = new DPRRadar(id,
		                           volumeCoveragePattern, operationalMode,
								   maxVal, scale, offset,
		                           volumeScanDate, volumeScanTime,
		                           biasValue, precipDetectedFlag,
		 						   filename);
		

		try {
		    CoreDao dao = new CoreDao(DaoConfig.forDatabase("ihfs"));
		    dao.saveOrUpdate(radarObject);
            
		} catch (Exception e) {
			e.printStackTrace();
		}

	}
	
	// ---------------------------------------------------------------------
	
	private void writeOutDataFile(RadarRecord record, DPRHeaderData headerData, 
			float[][] precipGridArray, String fileName)
	{

		DataOutputStream outputStream = null;


		//fileName is of the form DPRXXXmmddyyyyhhmmZ
		
		try {

			String fullPathName = outputGridDirectory + "/" + fileName;
			statusHandler.handle(Priority.INFO, "\n" + " writing to file = " + fullPathName);

			FileOutputStream fileOutputStream = new FileOutputStream(fullPathName);
			outputStream = new DataOutputStream(fileOutputStream);

			int hrapXMax = MAX_IHRAP;
			int hrapYMax = MAX_JHRAP;
			
			int intValue = 0;
			
			// write out header portion of decoded DPR product
			
			int beginDate = -999;
			int beginTime = -999;
			int endDate = headerData.getVolumeScanDate();
			int endTime = headerData.getVolumeScanTime();
			int operationalMode = headerData.getOperationalMode();
			
			outputStream.writeInt(Integer.reverseBytes(beginDate));
			outputStream.writeInt(Integer.reverseBytes(beginTime));
			outputStream.writeInt(Integer.reverseBytes(operationalMode));
			
			outputStream.writeInt(Integer.reverseBytes(endDate));
			outputStream.writeInt(Integer.reverseBytes(endTime));
			outputStream.writeInt(Integer.reverseBytes(operationalMode));
			
			float max = -9999f;
			
			// write out data portion of decoded DPR product
			for (int x = 0; x < hrapXMax; x++)
			{		
				for (int y = 0; y < hrapYMax; y++)
				{		
					float floatValue = precipGridArray[x][y];
					intValue = HydroNumericUtility.getSwappedIntBytesFromFloat(floatValue);
					outputStream.writeInt(intValue);
					
					if (precipGridArray[x][y] > max)
					{
						max = precipGridArray[x][y];
					}
				}
			}	
			
			statusHandler.handle(Priority.INFO, "\n" +
					"DPR product: max value = "  + max);
					 

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
	} // end writeOutDataFile()
	
	
} //end class DPRProductProcessor 
