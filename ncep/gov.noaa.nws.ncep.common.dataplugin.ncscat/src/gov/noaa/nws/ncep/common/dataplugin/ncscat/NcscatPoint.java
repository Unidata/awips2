/**
 * ScatPoint
 * 
 * This java class  contains all the attributes for each point on the scan line of ASCAT,Quikscat
 * 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 10/2009		Uma Josyula	Initial creation	
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */


package gov.noaa.nws.ncep.common.dataplugin.ncscat;

import java.util.Calendar;




public class NcscatPoint {
// The latitude  from the report 
	
	private short lat;
	
	// The longitude  from the report 
   
	private  short  lon;
	
	// The iql  from the report 
	
	private short iql;
	
	// The isp  from the report 

	private short  isp;
	
	// The idr  from the report 

	private short idr;
	
	// The irn  from the report 
	
	private short  irn;
	
	// The blank1  from the report 
	
	private short ib1;
	
	
	// The blank2  from the report 

	private short  ib2;
	
	// The blank3  from the report 

	private short ib3;

	// The start time   from the report 
	
	private Calendar stTime;
	
	public static final short SHORT_MISSING = -99;


	/**
     * Default Constructor
     */
    public NcscatPoint() {

    	this.lat=SHORT_MISSING;
    	this.lon=SHORT_MISSING;
    	this.iql=SHORT_MISSING;
    	this.isp=SHORT_MISSING;
    	this.idr=SHORT_MISSING;
    	this.irn=SHORT_MISSING;
    	this.ib1=SHORT_MISSING;
    	this.ib2=SHORT_MISSING;
    	this.ib3=SHORT_MISSING;

    }

	public short getLat() {
		return lat;
	}

	public void setLat(short lat) {
		this.lat = lat;
	}

	public short getLon() {
		return lon;
	}

	public void setLon(short lon) {
		this.lon = lon;
	}

	public short getIql() {
		return iql;
	}

	public void setIql(short iql) {
		this.iql = iql;
	}

	public short getIsp() {
		return isp;
	}

	public void setIsp(short isp) {
		this.isp = isp;
	}

	public short getIdr() {
		return idr;
	}

	public void setIdr(short idr) {
		this.idr = idr;
	}

	public short getIrn() {
		return irn;
	}

	public void setIrn(short irn) {
		this.irn = irn;
	}

	public short getIb1() {
		return ib1;
	}

	public void setIb1(short ib1) {
		this.ib1 = ib1;
	}

	public short getIb2() {
		return ib2;
	}

	public void setIb2(short ib2) {
		this.ib2 = ib2;
	}

	public short getIb3() {
		return ib3;
	}

	public void setIb3(short ib3) {
		this.ib3 = ib3;
	}

	public void setStTime(Calendar stTime) {
		this.stTime = stTime;
	}

	public Calendar getStTime() {
		return stTime;
	}
}


	   
	   











