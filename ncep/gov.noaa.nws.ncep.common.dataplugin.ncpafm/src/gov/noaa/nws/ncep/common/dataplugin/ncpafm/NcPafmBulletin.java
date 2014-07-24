/*
 * 
 * NcPafmBulletin
 * 
 * This class performs the mapping to the database tables for the Point/Area
 * Forecast Matrices (PAFM) Decoder Plug-In
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer		Description
 * ------------	----------- --------------	-----------------------------------
 * 08/05/2009	126			F. J. Yen		Initial creation
 * 01/06/2010   126	 		F. J. Yen		Migrated and refactored from to11dr3 to to11dr11
 * *
 * This code has been develped by the SIB for use in the AWIPS2 system.
 * </pre>
 * 
 * @author F. J. Yen, SIB
 * @version 1
 
 */

package gov.noaa.nws.ncep.common.dataplugin.ncpafm;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;

/**
 * NcPafmBulletin is the highest level class for PAFM (Point/Area Forecast
 * Matrices) data. Each instance holds all of the data parsed from one bulletin.
 * Children (NcPafmUgc) hold data from individual segments.
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 23Jun2009    126			F. J. Yen	Initial Coding.
 * 22Sep2009	126			F. J. Yen	Increase size of column bullMessage
 * 28Sep2009	126			F. J. Yen	Delete column bullMessage 
 * 30Sep2011	126			B. Hebbard  PafmRecord (old PDO) becomes NcPafmBulletin
 * 										-- still representing decoded content of a
 * 										whole bulletin, but no longer the PDO.
 * 										Removed persistence annotations.
 * 12Oct2011    126         G. Hull     use a surfaceObsLocation object for the lat/lon/stationId.
 * Jul 23, 2014 3410       bclement    location changed to floats
 * </pre>
 * 
 * @author F. J. Yen, SIB
 * @version 1.0
 */

public class NcPafmBulletin {
	
	/** Report type */
	private String reportType;
	
	// WMO header
	private String wmoHeader;
	
	// The issue office where the report from
	private String issueOffice;
		
	private Calendar issueTime;
	
	private Calendar initialTime;

	private String designatorBBB;
	
	// The mndTime
	private String mndTime;
	
	/** Matrix type */
	private String matrixType;	

	/** PAFM UGC Child Table */
	private Set<NcPafmUgc> pafmUGC = new HashSet<NcPafmUgc>();

	/**
     * Default Constructor
     */
    public NcPafmBulletin() {
    	this.issueOffice=null;
    	this.reportType="PAFM";
    	this.matrixType=" ";
    	this.issueTime=null;
    	this.wmoHeader=null;
    	this.designatorBBB=" ";
    	this.mndTime=null;
    }

    /**
     * Constructs a pafm record from a dataURI
     * 
     * @param uri
     *            The dataURI
     */

    /**
     * Constructs an array of NcPafmRecord PDOs from this one bulletin
     * record.  (Each bulletin record contain data from a whole
     * bulletin, while each NcPafmRecord is more fine-grained
     * -- representing the values for a single forecast hour of a
     * single location [NcPafmFips] of a single segment [NcPafmUgc]
     * of the entire bulletin [NcPafmBulletin]).  We do this "reshuffle"
     * because the NcPafmBulletin/NcPafmUgc/NcPafmFips hierarchy
     * efficiently represents the original structure as parsed from
     * the text, but we want the persistent PDO (NcPafmRecord) to be
     * optimized for efficient access.
     */
    public NcPafmRecord[] split() {
    	List<NcPafmRecord> records = new ArrayList<NcPafmRecord>();
    	for (NcPafmUgc ugc : getPafmUGC()) {
    		for (NcPafmFips fips : ugc.getPafmFIPS()) {
    			for (NcPafmParameters params : ugc.getPafmParms()) {
    				//  Create a new PDO
    				NcPafmRecord npr = new NcPafmRecord();
    				//  Copy some fields from the bulletin level...
    				npr.setReportType(getReportType());
    				npr.setWmoHeader(getWmoHeader());
    				npr.setIssueOffice(getIssueOffice());
    				npr.setIssueTime(getIssueTime());
    				//npr.setDataTime(getDataTime());  //NO!!  Must be DataTime of forecast hour
    				npr.setDesignatorBBB(getDesignatorBBB());
    				npr.setMndTime(getMndTime());
    				npr.setMatrixType(getMatrixType());
    				//  ...some fields from the UGC (group of locations) level...
    				npr.setUgc(ugc.getUgc());  //TODO:  Needed?
    				npr.setProdPurgeTime(ugc.getProdPurgeTime());
    				//TODO:  Needed?  npr.setSegment(ugc.getSegment());
    				//  ...some from the FIPS (individual location) level...
    				
    				// create a surfaceObsLocation object and set the lat/lon/stationId and elev.
    				// Note that we will use the fips here for the station id instead of the
    				// issuing station id.
    				//
    				SurfaceObsLocation loc = new SurfaceObsLocation( fips.getFips() );
//    				npr.setFips(fips.getFips());
//    				npr.setElev(fips.getElev());
//    				npr.setLat(fips.getLat());
//    				npr.setLon(fips.getLon());    	

                    loc.setLatitude(fips.getLat().floatValue());
                    loc.setLongitude(fips.getLon().floatValue());
    				loc.setElevation( fips.getElev().intValue() );
    				npr.setLocation( loc );

    				//  ...and finally the set of parameters for a
    				//  single forecast hour, at that location
    				npr.setPafmParms(params);

    				//  A (forecast-component) DataTime needs to be
    				//  constructed from the inferred initial time for
    				//  this bulletin and the absolute forecast time
    				npr.setDataTime( constructDataTime( params.getForecastTimeUtc() ) );

    				//  Add to records list
    				records.add(npr);
    			}
    		}
    	}
    	return records.toArray(new NcPafmRecord[0]);
    }
    
    private DataTime constructDataTime(Calendar forecastTimeUtc) {
    	//  Given an absolute forecast valid time, construct a
    	//  (initial-plus-forecast-delta style) DataTime
    	//  based on the initialTime deduced for this bulletin.
    	
    	int fcstTime = 0;  // seconds from initialTime to forecastTimeUtc
    	fcstTime = (int) (forecastTimeUtc.getTimeInMillis() - initialTime.getTimeInMillis());
    	fcstTime /= 1000;  //TODO:  Round in case off a few millis, somehow?  Sanity?
    	
		return new DataTime (initialTime, fcstTime);
	}
	
	private Calendar inferInitialTime(Calendar issueTime) {
		//  Given the issue time on the bulletin, infer a reasonable
		//  initial time on which to base forecast-component DataTime's.
		
		//  Algorithm:  First UTC multiple of 3 hours greater than or
		//              exactly equal to the issue time.
		//              TODO:  Check against PFM/AFM product spec if
		//              necessary to ensure this is reasonable.  For
		//              example, is it possible that this issue time
		//              is (slightly) after the first (or even more)
		//              forecast hour in the product?  If so, we'd
		//              wind up with a DataTime with negative fcstTime!
		//              Maybe we should find the earliest forecast hour
		//              that appears in the bulletin, instead of
		//              inferring from the issueTime?
		//  Check this................
		
		final long threeHoursInMillis = 3 * 60 * 60 * 1000;
		
		long issueTimeInMillis = issueTime.getTimeInMillis();
		long initialTimeInMillis = 0;
		
		if (issueTimeInMillis % threeHoursInMillis == 0) {
			initialTimeInMillis = issueTimeInMillis;
		}
		else {
			initialTimeInMillis = issueTimeInMillis / threeHoursInMillis * threeHoursInMillis + threeHoursInMillis;
		}
		
		Calendar initialTime = (Calendar) issueTime.clone();
		initialTime.setTimeInMillis(initialTimeInMillis);
		
		return initialTime ;
	}

	public String getReportType() {
    	return reportType;
    }
    public void setReportType(String reportType) {
    	this.reportType = reportType;
    }
    
    public String getMatrixType() {
    	return matrixType;
    }
    public void setMatrixType(String matrixType) {
    	this.matrixType = matrixType;
    }

	public String getWmoHeader(){
		return wmoHeader;
	}
	public void setWmoHeader(String wmoHeader){
		this.wmoHeader=wmoHeader;
	}
	  
	public String getIssueOffice() {
		return issueOffice;
	}
	public void setIssueOffice(String issueOffice) {
		this.issueOffice = issueOffice;
	}

	public Calendar getIssueTime(){
		return issueTime;
	}
	public void setIssueTime(Calendar issueTime){
		this.issueTime=issueTime;
		this.initialTime=inferInitialTime(issueTime);
	}

	public Calendar getInitialTime() {
		return initialTime;
	}

	public void setInitialTime(Calendar initialTime) {
		this.initialTime = initialTime;
	}

	public String getDesignatorBBB(){
		return designatorBBB;
	}
	public void setDesignatorBBB(String designatorBBB){
		this.designatorBBB=designatorBBB;
	}

	public String getMndTime() {
		return mndTime;
	}
	public void setMndTime(String mndTime) {
		this.mndTime = mndTime;
	}

	public Set<NcPafmUgc> getPafmUGC() {
		return pafmUGC;
	}
	public void setPafmUgc(Set<NcPafmUgc> pafmUgcs) {
		this.pafmUGC = pafmUgcs;
	}

	/*
	 * 	Add  pafmUgc to set
	 */
	public void addPafmUGC(NcPafmUgc pugc) {
		pafmUGC.add(pugc);
		pugc.setParentID(this);
	}

	/**
	 * Override existing set method to modify any
	 * classes that use the dataURI as a foreign key
     */
    //public void setIdentifier(Object dataURI){
	//	 this.identifier = dataURI;      
	//     if(this.getPafmUGC() != null && this.getPafmUGC().size() > 0)
	//     {
	//         for (Iterator<NcPafmUgc> iter = this.getPafmUGC().iterator(); iter.hasNext();) {
	//            NcPafmUgc ws = iter.next();
	//            ws.setParentID(this);
	//         }
	//     }      
    //}
    
}
