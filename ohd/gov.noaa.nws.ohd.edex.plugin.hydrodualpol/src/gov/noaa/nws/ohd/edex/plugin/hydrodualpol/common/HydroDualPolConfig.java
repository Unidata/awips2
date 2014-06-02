/**
 * This software was developed and / or modified by HSEB, OHD
 **/
package gov.noaa.nws.ohd.edex.plugin.hydrodualpol.common;

import gov.noaa.nws.ohd.edex.plugin.hydrodualpol.HydroDualPolGenerator;
import gov.noaa.nws.ohd.edex.plugin.hydrodualpol.HydroDualPolURIFilter;
import gov.noaa.nws.ohd.edex.plugin.hydrodualpol.HydroDualPolURIGenerateMessage;

import java.util.Date;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.plugin.scan.common.ScanCommonUtils;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2013            jtDeng     Initial creation
 * 
 * </pre>
 * 
 * @author deng2
 * @version 1.0
 */

public class HydroDualPolConfig {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(HydroDualPolConfig.class);

    /** generator ref **/
    private HydroDualPolGenerator generator = null;

    /** DSA/DPR/DAA URI **/
    private String dsaURI = null;

    private String dprURI = null;

    private String daaURI = null;

    /** DSA/DPR/DAA radar record **/
    private RadarRecord dsaRadarRec = null;
    private RadarRecord dprRadarRec = null;
    private RadarRecord daaRadarRec = null;

    /** Spatial Info object */
    private RadarStation spatialInfo = null;

    /** the icao */
    private String icao = null;

    /** Whether or not to run FFMP **/
    private boolean mode = false;

    /** FFMP record reftime as string **/
    private String dateString = null;

    /** FFMP record reftime **/
    private Date date = null;

    /** the latitude/longitude */
    private Coordinate latlon = null;
    
    private String productType = "OTHER";

    public HydroDualPolConfig(HydroDualPolURIGenerateMessage genMessage,
            HydroDualPolGenerator generator) throws Exception {
        this.generator = generator;

        String[] uriArray = genMessage.getUris();
        StringBuilder stringBuilder = new StringBuilder();
        
        for (int i = 0; i < uriArray.length; i++)
        {
        	if (i > 0)
        	{
        		stringBuilder.append("\n");
        	}
        	stringBuilder.append(uriArray[i]);
        }
        
        statusHandler.handle(Priority.INFO, "HydroDualPolConfig: URI =  " +
        		stringBuilder.toString());
			
        
        dsaURI = genMessage.getURI(HydroDualPolURIFilter.dsa);
        daaURI = genMessage.getURI(HydroDualPolURIFilter.daa);
        dprURI = genMessage.getURI(HydroDualPolURIFilter.dpr);
       
        setProductType("OTHER");
        
        try {
        
        	if (dsaURI != null){
        		dsaRadarRec = getRadarRecord(dsaURI);
        	}

              
        if (getDSA() != null)
        {
        	setProductType("DSA");

        
        		setIcao(genMessage.getIcao());
        		setMode(true);
        		setSpatialInfo(getDSA().getSpatialObject());
        		setLatLon(getDSA().getLatitude(), getDSA().getLongitude());
        
        }
        
        } catch (Exception e) {
            e.printStackTrace();
            throw new Exception("HydroDualPolConfig: HydroDualPolGenerator cannot run....");
        } 
        
      try {
         
    	  if (daaURI != null)
    	  {
    		  daaRadarRec = getRadarRecord(daaURI);                           
    	  }
    	  
    	  if (getDAA() != null)
    	  {
    		  setProductType("DAA");
 
    		  setIcao(genMessage.getIcao());
    		  setMode(true);
    		  setSpatialInfo(getDAA().getSpatialObject());
    		  setLatLon(getDAA().getLatitude(), getDAA().getLongitude());
    	  }

    	  
      } catch (Exception e) {
    	  e.printStackTrace();
    	  throw new Exception("HydroDualPolConfig: HydroDualPolGenerator cannot run....");
      } 

      try {

    	  if (dprURI != null)
    	  {
    		  dprRadarRec = getRadarRecord(dprURI);                           
    	  }
    	  
    	  if (getDPR() != null)
    	  {
    		  setProductType("DPR");
    		  
  			  setIcao(genMessage.getIcao());
   			  setMode(true);
   			  setSpatialInfo(getDPR().getSpatialObject());
   			  setLatLon(getDPR().getLatitude(), getDPR().getLongitude());
    	
    	  }

      } catch (Exception e) {
    	  e.printStackTrace();
    	  throw new Exception("HydroDualPolConfig: HydroDualPolGenerator cannot run....");
      } 

    }

    /**
     * set runnable or not
     * 
     * @param mode
     */
    public void setMode(boolean mode) {
        this.mode = mode;
    }

    /**
     * can we run Preciprate?
     * 
     * @return
     */
    public boolean getMode() {
        return mode;
    }

    /**
     * Get DSA
     * 
     * @return
     */
    
    public RadarRecord getRadarRecord(String uri)
    {
    	RadarRecord record = null;
    	
    	try
    	{
    		record = ScanCommonUtils.getRadarRecord(uri);	
    	}
    	catch (Exception e)
    	{
    		e.printStackTrace();
    	}
    	
    	return record;
    }
        
    public RadarRecord getDSA() {
        return dsaRadarRec;
    }

    public RadarRecord getDPR() {
        return dprRadarRec;
    }

    public RadarRecord getDAA() {
        return daaRadarRec;
    }

    /**
     * gets The HydroDualPol generator
     * 
     * @return
     */
    public HydroDualPolGenerator getGenerator() {
        return generator;
    }

    /**
     * gets the spatial object
     * 
     * @return
     */
    public RadarStation getSpatialInfo() {
        return spatialInfo;
    }

    /**
     * sets the spatial object
     */
    public void setSpatialInfo(RadarStation spatialinfo) {

        this.spatialInfo = spatialinfo;

    }

    /**
     * Gets the Lat/Lon coord
     * 
     * @return
     */
    public Coordinate getLatLon() {
        return latlon;
    }

    /**
     * set the center lat lon
     * 
     * @param lat
     * @param lon
     */
    public void setLatLon(double lat, double lon) {
        latlon = new Coordinate(lon, lat);
    }

    /**
     * sets the icao for these SCAN values
     * 
     * @param icao
     */
    public void setIcao(String icao) {
        this.icao = icao;
    }

    /**
     * gets the icao for this SCAN
     * 
     * @return
     */
    public String getIcao() {
        return icao;
    }

    /**
     * sets the date
     * 
     * @param date
     */
    public void setDate(Date date) {
        this.date = date;
    }

    /**
     * returns a formatted date 
     * 
     * @return
     */
    public Date getDate() {
        return date;
    }

    /**
     * sets the date string
     * 
     * @param date
     */
    public void setDateString(String dateString) {
        this.dateString = dateString;
    }

    /**
     * returns a formatted date
     * 
     * @return
     */
    public String getDateString() {
        return dateString;
    }

	private void setProductType(String productType) {
		this.productType = productType;
	}

	public String getProductType() {
		//Returns DAA, DPR, or DSA  as strings , aka mnemonic
		
		return productType;
	}
}
