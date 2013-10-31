package gov.damcat.data;

import java.util.*;
import javax.swing.*;
import gov.dambreak.util.*;

/**
 * This class holds information from the damcat_dam table and links to structures
 * containing data about the other gov.damcat.data tables.
 */
 
public class DamInfo {
	public String nidid,dam_name,other_dam_name,dam_former_name,stateid,section_t_r,county,river,owner_name;
    public String owner_type,dam_designer,private_on_federal,dam_type,core,foundation,purposes,year_completed;
    public String year_modified,downstream_hazard,emerg_action_plan,inspection_date,inspection_freq,st_reg_dam;
    public String st_reg_agency,spillway_type,outlet_gates;
    public String fed_funding,fed_design,fed_construction,fed_regulatory,fed_inspection,fed_operation;
    public String fed_owner,fed_other,source_agency,topo_map,hsa,rfc,comments,updated;
    public String prebreak_avail;
   	public Float spillway_width,volume_dam,number_locks,length_locks,width_locks,drainage_area;
   	public Float longitude_dam,latitude_dam,dam_length,dam_height,structural_height,hydraulic_height;
   	public Float nid_height,max_discharge;
   	public Float max_storage,normal_storage,nid_storage,surface_area,elev; 
   	public Integer return_flow_region; 
   
///////////////////////////////////////////////////////////////////////////
//   	
///////////////////////////////////////////////////////////////////////////

	public ArrayList _inputEntries,_crossSectionEntries;
	public ArrayList _downstreamEntries,_outputEntries;

	public int _nFilled = 0;

	private DBAccess dbAccess;
/**
 * Insert the method's description here.
 * Creation date: (7/15/2003 3:09:33 PM)
 */
public DamInfo() {
	_nFilled = 0;

	nidid = "";
	dam_name = "";
	other_dam_name = "";
	dam_former_name = "";
	stateid = "";
	section_t_r = "";
	county = "";
	river = "";
	owner_name = "";
    owner_type = "";
    dam_designer = "";
    private_on_federal = "";
    dam_type = "";
    core = "";
    foundation = "";
    purposes = "";
    year_completed = "";
    year_modified = "";
    downstream_hazard = "";
    emerg_action_plan = "";
    inspection_date = "";
    inspection_freq = "";
    st_reg_dam = "";
    st_reg_agency = "";
    spillway_type = "";
    outlet_gates = "";
    fed_funding = "";
    fed_design = "";
    fed_construction = "";
    fed_regulatory = "";
    fed_inspection = "";
    fed_operation = "";
    fed_owner = "";
    fed_other = "";
    source_agency = "";
    topo_map = "";
    hsa = "";
    rfc = "";
    comments = "";
    updated = "";
    prebreak_avail = "";
	spillway_width = new Float(0.0f);
 	volume_dam = new Float(0.0f);
 	number_locks = new Float(0.0f);
 	length_locks = new Float(0.0f);
 	width_locks = new Float(0.0f);
 	drainage_area = new Float(0.0f);
    longitude_dam = new Float(0.0f);
    latitude_dam = new Float(0.0f);
    dam_length = new Float(0.0f);
    dam_height = new Float(0.0f);
    structural_height = new Float(0.0f);
    hydraulic_height = new Float(0.0f);
    nid_height = new Float(0.0f);
    max_discharge = new Float(0.0f);
    max_storage = new Float(0.0f);
    normal_storage = new Float(0.0f);
    nid_storage = new Float(0.0f);
    surface_area = new Float(0.0f);
    elev = new Float(0.0f);
    return_flow_region = new Integer(0);
    _inputEntries = new ArrayList();
    _crossSectionEntries = new ArrayList();
 	_downstreamEntries = new ArrayList();
 	_outputEntries = new ArrayList();
}
/**
 * Constructor for class
 * Creation date: (7/14/2003 12:37:48 PM)
 * @param _damName java.lang.String
 * @param _riverName java.lang.String
 * @param _countyName java.lang.String
 * @param _NIDID java.lang.String
 * @param _otherName java.lang.String
 */
public DamInfo(String _damName, String _riverName, String _countyName, String _NIDID, String _hsa, String _rfc, String _downstreamHazard, Float _max_storage, Float _latitude, Float _longitude) {
	dam_name = _damName;
	if(dam_name == null)
		dam_name = "";
	river = _riverName;
	if(river == null)
		river = "";
	county = _countyName;
	if(county == null)
		county = "";
	nidid = _NIDID;
	if(nidid == null)
		nidid = "";
	hsa = _hsa;
	if(hsa == null);
		hsa = "";
	rfc = _rfc;
	if(rfc == null)
		rfc = "";
	downstream_hazard = _downstreamHazard;
	if(downstream_hazard == null)
		downstream_hazard = "";
	max_storage = _max_storage;
	if(max_storage == null)
		max_storage = new Float(0.0f);
	latitude_dam = _latitude;
	if(latitude_dam == null)
		latitude_dam = new Float(0.0f);
	longitude_dam = _longitude;
	if(longitude_dam == null)
		longitude_dam = new Float(0.0f);
	
	_nFilled = 1;
}
/**
 * Insert the method's description here.
 * Creation date: (7/17/2003 12:11:45 PM)
 * @return java.lang.String
 * @param f java.lang.Float
 */
public static String formatIfPossible(Float f) {
	if (f == null)
		return "";
	
	java.text.DecimalFormat df = new java.text.DecimalFormat("#######0.0000");
	String ret;
	try {
		ret = df.format(f.doubleValue());
	} catch (IllegalArgumentException e) {
		ret = "NaN";		
	}
	return ret;
}
/**
 * Generates an AnalysisData structure from this DamInfo structure.
 * This is a bit complicated because the model and the database
 * represent input data in slightly different ways.
 *
 *	NOTE that ModelOutput Data is attached to Logical Structure via ModelScenario
 *
 *  NOTE that DownStreamPoint structure is attached to BOTH 
 *          AnalysisData and ModelOutput structures
 *
 * Creation date: (7/30/2003 3:26:12 PM)
 */
public AnalysisData getAnalysisData() {
	
	AnalysisData retVal = new AnalysisData();

	retVal.damNidid = nidid.trim();

	retVal.damName = dam_name.trim();
	
	retVal.riverName = river;

	String methodName = "DamInfo.getAnalysisData";

	// System.out.println (methodName);
	
	// create ModelScenarios from the 'input' table
	for (int i=0; i < _inputEntries.size(); i++) {
				
		InputEntryInfo input = (InputEntryInfo)_inputEntries.get(i);

		ModelScenario s = new ModelScenario();
		
		s.source = input.src;
		s.name = input.scenario;
		s.damType = input.idam.intValue();
		s.HDE = input.hde.floatValue();
		s.BME = input.bme.floatValue();
		s.VOL = input.vol.floatValue();
		s.BW = input.bw.floatValue();
		s.SA = input.sa.floatValue();
		s.TFM = input.tfm.floatValue();
		s.QO = input.qo.floatValue();
		s.dbugType = 0;							// dbugType is not part of InputEntryInfo

		// if a downstream point exists, use the last one as the "point of interest"
		if (_downstreamEntries.size() > 0) 
		{
			s.DISTTN = ((DownstreamEntryInfo)_downstreamEntries.get(_downstreamEntries.size()-1)).distance_from_dam.floatValue();
			s.CMS = ((DownstreamEntryInfo)_downstreamEntries.get(_downstreamEntries.size()-1)).mann_oc.floatValue();
		} else 
		{
			s.DISTTN = 1.0f;
			s.CMS = 0.05f;
		}

		// build ModelOutput and DownStreamPoint structures for this ModelScenario
		s.output = getModelOutput(i);
		
		if (s.output == null)
		{
			s.bOutputAvailable = false;	
		} 
		else
		{
			s.bOutputAvailable= true;	
			// assign reference from OutputModel back to ModelScenario
			s.output.inputScenario = s;
		}	
		
		retVal.scenarios.add(s);
	}

	// if DownstreamPoints exist use the name of the last one as the 'point of interest name'
	if (_downstreamEntries.size() > 0)
		retVal.pointOfInterestName = ((DownstreamEntryInfo)_downstreamEntries.get(_downstreamEntries.size()-1)).down_name;
	else
		retVal.pointOfInterestName = "Town";

	// create DownstreamPoints from the DownstreamEntryInfo structures
	for (int i=0; i<_downstreamEntries.size(); i++) {
		
		DownstreamPoint d = new DownstreamPoint();

		DownstreamEntryInfo down = (DownstreamEntryInfo)_downstreamEntries.get(i);

		d.name = down.getDown_name();
		
		// System.out.println(i + " Point = " + d.name);
		
		d.comments = down.getComments();
		d.xsecBestType = down.getXsec_best_type();
		
		d.distanceToSection = down.getDistance_from_dam().floatValue();
		
		/* 
		 * flood depths in DAMCAT are actually flood elevations
		 * SMPDBK expects actual flood "depth"  			
		 */
		// *** note that "flood depth" is actually stored in database as "flood elevation" !
		// d.floodDepth = down.getFlood_depth().floatValue();
		//float fix = 100.0f;
		
		d.floodDepth = down.getFlood_depth().floatValue() - down.getElevation().floatValue();		
		//System.out.println("in getAnalysisData() floodDepth: " + d.floodDepth);
		d.floodWidth = down.getFlood_width().floatValue();
		d.floodFlow = down.getFlood_flow().floatValue();
		d.latitude = down.getLatitude().floatValue();
		d.longitude = down.getLongitude().floatValue();
		d.elevation = down.getElevation().floatValue();

		d.mann_oc = down.getMann_oc().floatValue(); 	

		d.bestXS = -1;		// initially unknown

		String lastXsecType = "";
		int countXsecType = 0;

		// *** Assign Section Geometry 

		SectionGeometry xsection = new SectionGeometry();
		
		for (int j=0; j<_crossSectionEntries.size(); j++) {
			
			CrossSectionEntryInfo cross = (CrossSectionEntryInfo)_crossSectionEntries.get(j);
			
			if (!cross.down_name.trim().equalsIgnoreCase(down.down_name.trim()))
			{
				continue;		// *** disregard if not associated with this downstream point !!!
			}
			
			if (!lastXsecType.equalsIgnoreCase(cross.xsec_type))	// test for first occurrence of xsec_type
			{
				if (d.xsecBestType.equalsIgnoreCase(cross.xsec_type)) 		// test if xsec_type is same as best
				{
					d.bestXS = countXsecType;								// set best index
				}

				countXsecType = countXsecType + 1;							// increment index
				
				if (lastXsecType != "")
				{
					d.xsections.add(xsection);								// if not the first
				}
				xsection = new SectionGeometry();
				lastXsecType = cross.xsec_type;
			} 

			xsection.setXSType(cross.xsec_type);

			// *** Note order of columns : elev, tw, inactive_width, mann_n
			
			xsection.setElevationData(cross.pair_num.intValue(),0,cross.elev.floatValue());
			xsection.setElevationData(cross.pair_num.intValue(),1,cross.tw.floatValue());
			xsection.setElevationData(cross.pair_num.intValue(),2,cross.inactive_width.floatValue());
			xsection.setElevationData(cross.pair_num.intValue(),3,cross.mann_n.floatValue());
						
			xsection.setLastRowUsed(cross.pair_num.intValue());		// keep track of how many rows in array are filled
			
		}

		if (xsection.getLastRowUsed() == -1 || xsection.getXSType() == null)
		{
			;
		} else {
			d.xsections.add(xsection);		// add last xsection as long as it's not empty
		}

		retVal.downstream.add(d);
	}

	// *** Add logic to attach ModelOutput and DownstreamPoint structures to appropriate 
	// *** ModelScenario structure

	for (int k = 0; k < _outputEntries.size(); k++)
	{
		OutputEntryInfo outEntry = (OutputEntryInfo)_outputEntries.get(k);
		continue;
		/*
				OutputEntryInfo outEntry = new OutputEntryInfo();
				
				outEntry.src			
				outEntry.scenario		
				outEntry.down_name		= outRow.getDown_name();				
				outEntry.comments		= outRow.getComments();
				
				long updatedLong		= outRow.getUpdated();
    			outEntry.updated = DbTimeHelper.getDateTimeStringFromLongTime(updatedLong);
    			
    			outEntry.slope			= new Float(outRow.getSlope());
    			outEntry.max_flow		= new Float(outRow.getMax_flow());
				outEntry.max_depth		= new Float(outRow.getMax_depth());
				outEntry.time_max_depth = new Float(outRow.getTime_max_depth());
				outEntry.time_flood		= new Float(outRow.getTime_flood());
				outEntry.time_deflood	= new Float(outRow.getTime_deflood());

		*/
	}


	retVal.rootDamInfo.add(this);		// finally provide reference to DamInfo so original dam data is preserved
	
	return retVal;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getComments() {
	return comments;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getCore() {
	return core;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getCounty() {
	return county;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getDam_designer() {
	return dam_designer;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getDam_former_name() {
	return dam_former_name;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getDam_height() {
	return dam_height;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getDam_length() {
	return dam_length;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getDam_name() {
	return dam_name;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getDam_type() {
	return dam_type;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getDownstream_hazard() {
	return downstream_hazard;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getDrainage_area() {
	return drainage_area;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getElev() {
	return elev;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getEmerg_action_plan() {
	return emerg_action_plan;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getFed_construction() {
	return fed_construction;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getFed_design() {
	return fed_design;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getFed_funding() {
	return fed_funding;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getFed_inspection() {
	return fed_inspection;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getFed_operation() {
	return fed_operation;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getFed_other() {
	return fed_other;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getFed_owner() {
	return fed_owner;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getFed_regulatory() {
	return fed_regulatory;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getFoundation() {
	return foundation;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getHsa() {
	return hsa;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getHydraulic_height() {
	return hydraulic_height;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getInspection_date() {
	return inspection_date;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getInspection_freq() {
	return inspection_freq;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getLatitude_dam() {
	return latitude_dam;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getLength_locks() {
	return length_locks;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getLongitude_dam() {
	return longitude_dam;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getMax_discharge() {
	return max_discharge;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.Float
 */
public java.lang.Float getMax_storage() {
	return max_storage;
}
/**
 * Builds a ModelOutput structure from OutputEntryInfo classes and
 *  appropriate DownStreamPoint and SectionGeometry structures
 *  from DownstreamEntryInfo and CrossSectionEntryInfo classes
 *
 * Creation date: (7/22/2003 8:59:36 AM)
 * @return ModelOutput
 * @param scenario int
 */
public ModelOutput getModelOutput(int scenario) {

	String methodName = "*** DamInfo.getModelOutput()";
	// System.out.println(methodName + " " + scenario);
	
	ModelOutput ret = new ModelOutput();
	
	try {
		// return empty ModelOutput structure if database was not read properly
		if (_nFilled != 2) {
			ret = null;
			return ret;
		}

		// retrieve appropriate damcat_in fields and 
		// COPY to new ModelScenario referenced inside ModelOutput structure
		// *** is this still necessary ???
		
		InputEntryInfo in = (InputEntryInfo)_inputEntries.get(scenario);

		ret.inputScenario.source = in.src;
		ret.inputScenario.name = in.scenario;
		// System.out.println( "src-scenario = " + in.src +" " + in.scenario);
			
		ret.inputScenario.damType = in.idam.intValue();
			
		String strScenario = in.scenario;
		
		ret.inputScenario.BME = in.bme.floatValue();
		ret.inputScenario.BW = in.bw.floatValue();
		ret.inputScenario.HDE = in.hde.floatValue();
		ret.inputScenario.QO = in.qo.floatValue();
		ret.inputScenario.SA = in.sa.floatValue();
		ret.inputScenario.TFM = in.tfm.floatValue();
		ret.inputScenario.VOL = in.vol.floatValue();

		ret.inputScenario.dbugType = 0;					// dbugType is not part of InputEntryInfo
		ret.inputScenario.changeFlag = 0;				// set to no changes 
		ret.inputScenario.bOutputAvailable = false;

		// get point of interest data from last Downstream Point
		ret.pointOfInterestName = ((DownstreamEntryInfo)_downstreamEntries.get(_downstreamEntries.size()-1)).getDown_name();
		
		ret.inputScenario.CMS = 0.5f;
		ret.inputScenario.DISTTN = ((DownstreamEntryInfo)_downstreamEntries.get(_downstreamEntries.size()-1)).getDistance_from_dam().floatValue();

		// determine how many points and how many pairs	
		int nDownPts = _downstreamEntries.size();
		int nPairs = _crossSectionEntries.size() / nDownPts;		// don't use, this appears to be a bad assumption !!!

		// create DownstreamPoints from the DownstreamEntryInfo structures
		for (int i=0; i < nDownPts; i++) {
			
			DownstreamEntryInfo down = (DownstreamEntryInfo)_downstreamEntries.get(i);
			DownstreamPoint dp = new DownstreamPoint();

			dp.name = down.getDown_name();
			dp.comments = down.getComments();
			dp.xsecBestType = down.getXsec_best_type();			
			dp.distanceToSection = down.getDistance_from_dam().floatValue();				
			/* 
			 * DAMCAT stores "flood depth" as "flood elevation" in database 
			 * SMPDBK expects actual "flood depth"			
			*/
			// dp.floodDepth = down.getFlood_depth().floatValue();
			
			//float fix = 100.0f;
			dp.floodDepth = down.getFlood_depth().floatValue() - down.getElevation().floatValue();
			dp.floodWidth = down.getFlood_width().floatValue();
			dp.floodFlow = down.getFlood_flow().floatValue();
			dp.latitude = down.getLatitude().floatValue();
			dp.longitude = down.getLongitude().floatValue();
			dp.elevation = down.getElevation().floatValue();
			dp.mann_oc = down.getMann_oc().floatValue();

			dp.bestXS = -1;						// index pointing to best XS			
			dp.changeFlag = 0;					// 0 = no changes; 1 = updated; 2 = inserted; 3 = deleted
				
			// dp.xsections = new ArrayList(); 	// initialized in ModelOutput constructor

			// *** Assign Section Geometry to DownstreamPoints by examining CrossSectionEntryInfo structures 			
			String prevXsecType = "";
			int countXsecType = 0;

			SectionGeometry xsection = new SectionGeometry();
		
			for (int j=0; j < _crossSectionEntries.size(); j++) {
					
				CrossSectionEntryInfo cross = (CrossSectionEntryInfo)_crossSectionEntries.get(j);
			
				if (!cross.down_name.trim().equalsIgnoreCase(down.down_name.trim()))
				{
					continue;		// *** disregard if not associated with this downstream point
				}
			
				if (!prevXsecType.equalsIgnoreCase(cross.xsec_type))	// test for change in xsec_type
				{
					if (dp.xsecBestType.equalsIgnoreCase(cross.xsec_type)) 		// test if xsec_type is same as best
					{
						dp.bestXS = countXsecType;								// if so, set best index
					}
					
					countXsecType = countXsecType + 1;							// increment index
				
					if (prevXsecType != "")										// if not the first
					{
						dp.xsections.add(xsection);								// add previous
					}
					
					xsection = new SectionGeometry();							// instantiate new structure
					prevXsecType = cross.xsec_type;								// save previous xsec_type
				} 

				xsection.setXSType(cross.xsec_type);

				// *** Note order of columns : elev, tw, inactive_width, mann_n			
				xsection.setElevationData(cross.pair_num.intValue(),0,cross.elev.floatValue());
				xsection.setElevationData(cross.pair_num.intValue(),1,cross.tw.floatValue());
				xsection.setElevationData(cross.pair_num.intValue(),2,cross.inactive_width.floatValue());
				xsection.setElevationData(cross.pair_num.intValue(),3,cross.mann_n.floatValue());
						
				xsection.setLastRowUsed(cross.pair_num.intValue());		// keep track of how many rows in array are filled
			
			}

			dp.xsections.add(xsection);		// add last xsection

			ret.inputDownstream.add(dp);

		}			

		// get damcat_out fields

		int row = 0;

		for (int j=0; j < _outputEntries.size(); j++) 
		{	
			OutputEntryInfo out = (OutputEntryInfo)_outputEntries.get(j);
			// find only rows which contain output for the given src and scenario
			if ((out.src.equalsIgnoreCase(in.src)) && (out.scenario.equalsIgnoreCase(in.scenario)))
			{ 				
				
				ret.source = out.src;
				ret.modelScenario = out.scenario;
				ret.outSlope = out.slope.floatValue();
			
				ret.maxFlow[row] = out.max_flow.floatValue();
				// *** watch max_depth and max_elevation definitions !!!
				ret.maxDepth[row] = out.max_depth.floatValue() - ((DownstreamEntryInfo)_downstreamEntries.get(row)).elevation.floatValue();
				ret.maxElevation[row] = out.max_depth.floatValue();
				ret.timeDeflood[row] = out.time_deflood.floatValue();
				ret.timeFlood[row] = out.time_flood.floatValue();
				ret.timeMaxDepth[row] = out.time_max_depth.floatValue();

				if (ret.maxFlow[row] > ret.maxFl)
				{
					 ret.maxFl = ret.maxFlow[row];
				}
				if (ret.maxElevation[row] > ret.maxEl)
				{
					 ret.maxEl = ret.maxElevation[row];
				}				
				if (ret.maxDepth[row] > ret.maxDep)
				{
					 ret.maxDep = ret.maxDepth[row];
				}				

				// *** Note these fields from database are not passed to OutputModel

				// 						= out.down_name
				// 						= out.comments
				//						= out.updated

				row = row + 1;
			}
		}

		ret.damName = dam_name;
		ret.riverName = river;

		ret.bHasWarning = false;
		ret.warning = null;
		ret.bHasFullText = false;
		ret.fullText = null;
		ret.bHasPrerunText = false;
		ret.prerunText = null;

		ret.changeFlag = 0;
		
		// *** Are any of these fields below being used ???
		ret.xsectionType = "";
		ret.numberXSections = row;			// count of number of entries in arrays (relative to 1)
		// ret.maxFl = 0.0f;
		// ret.maxEl = 0.0f;
		// ret.maxDep = 0.0f;
		ret.timeMaxDep = 0.0f;
		ret.timeFl = 0.0f;
		ret.timeDefl = 0.0f;	

	} catch (Exception e) {
		e.printStackTrace();
		return null;
	}
	return ret;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getNid_height() {
	return nid_height;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getNid_storage() {
	return nid_storage;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getNidid() {
	return nidid;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getNormal_storage() {
	return normal_storage;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Float
 */
public java.lang.Float getNumber_locks() {
	return number_locks;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getOther_dam_name() {
	return other_dam_name;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getOutlet_gates() {
	return outlet_gates;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getOwner_name() {
	return owner_name;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getOwner_type() {
	return owner_type;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getPrebreak_avail() {
	return prebreak_avail;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getPrivate_on_federal() {
	return private_on_federal;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getPurposes() {
	return purposes;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.Integer
 */
public java.lang.Integer getReturn_flow_region() {
	return return_flow_region;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getRfc() {
	return rfc;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getRiver() {
	return river;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getSection_t_r() {
	return section_t_r;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @return java.lang.String
 */
public java.lang.String getSource_agency() {
	return source_agency;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.String
 */
public java.lang.String getSpillway_type() {
	return spillway_type;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.Float
 */
public java.lang.Float getSpillway_width() {
	return spillway_width;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.String
 */
public java.lang.String getSt_reg_agency() {
	return st_reg_agency;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.String
 */
public java.lang.String getSt_reg_dam() {
	return st_reg_dam;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.String
 */
public java.lang.String getStateid() {
	return stateid;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.Float
 */
public java.lang.Float getStructural_height() {
	return structural_height;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.Float
 */
public java.lang.Float getSurface_area() {
	return surface_area;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.String
 */
public java.lang.String getTopo_map() {
	return topo_map;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.String
 */
public java.lang.String getUpdated() {
	return updated;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.Float
 */
public java.lang.Float getVolume_dam() {
	return volume_dam;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.Float
 */
public java.lang.Float getWidth_locks() {
	return width_locks;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.String
 */
public java.lang.String getYear_completed() {
	return year_completed;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @return java.lang.String
 */
public java.lang.String getYear_modified() {
	return year_modified;
}
/**
 * This method finds correct place in downstream ArrayList for insertion of downstream point
 * Creation date: (1/29/2004 2:18:38 PM)
 */
public boolean insertDown(DownstreamEntryInfo down) 
{
	String compareKey = down.down_name;
	float compareDistance = down.distance_from_dam.floatValue();
	float oldDistance = 0.0f;
	
	int j;
	
	for (j = 0 ; j < _downstreamEntries.size(); j++)
	{
		DownstreamEntryInfo oldDown = (DownstreamEntryInfo)_downstreamEntries.get(j);

		oldDistance = oldDown.distance_from_dam.floatValue();

		
		if (compareDistance > oldDistance)
		// if(compareKey.compareToIgnoreCase(oldDown.down_name) > 0 )
		{
			continue;
		}
		else
		{
			break;
		}
		
	}	
	_downstreamEntries.add(j, down);
	return false;
}
/**
 * This method finds place in input ArraList for insertion source and scenatio. 
 * Creation date: (1/29/2004 2:18:38 PM)
 */
public boolean insertInput(InputEntryInfo in) 
{
	String compareKey = in.src.concat(in.scenario);
	int j;
	for (j = 0 ; j < _inputEntries.size(); j++)
	{
		InputEntryInfo oldInput = (InputEntryInfo)_inputEntries.get(j);
		String oldKey = oldInput.src.concat(oldInput.scenario);
		if(compareKey.compareToIgnoreCase(oldKey) > 0 )
		{
			continue;
		}
		else
		{
			break;
		}
		
	}	
	_inputEntries.add(j, in);
	return false;
}
/**
 * This method finds place in output ArrayList for insertion source, scenario and down_name,
 * checks integrity with damcat_down and input tables. 
 * Creation date: (1/29/2004 2:18:38 PM)
 */
public boolean insertOutput(DamInfo damInfo, OutputEntryInfo out) 
{
	int j;
	boolean compareFlag = false;
	boolean compareDown = false;
	boolean compareSrc = false;
	boolean compareScenario = false;
	int countFalse = 0;
	
	String compareKey = out.src.concat(out.scenario).concat(out.down_name);
	
	if (out.src.startsWith("#"))
	{
		compareFlag = true;
		for (j = 0 ; j < _outputEntries.size(); j++)
		{
			OutputEntryInfo oldOutput = (OutputEntryInfo)_outputEntries.get(j);
			String oldKey = oldOutput.src.concat(oldOutput.scenario).concat(oldOutput.down_name);
			if(compareKey.compareToIgnoreCase(oldKey) > 0 )
			{
				continue;
			}
			else
			{
				break;
			}
		
		}	
		_outputEntries.add(j, out);
		
		for (int i=0; i<damInfo._inputEntries.size(); i++) 
		{
			
			InputEntryInfo in = (InputEntryInfo)damInfo._inputEntries.get(i);
			if ((out.src.equalsIgnoreCase(in.src)) && (out.scenario.equalsIgnoreCase(in.scenario)))
			{
				countFalse++;
			}
			else
			{
				//System.out.println("They are not duplicate!");
			}
		}
		
		if (countFalse == 0)
		{
		//	System.out.println("Not duplicate damcat_in will be updated!");
			InputEntryInfo in = new InputEntryInfo();
			String damID = damInfo.getNidid();
			in.src = out.src;
			in.scenario = out.scenario;
			DBAccess dbAccess = new DBAccess();
		//  insert record to the damcat_in in the databse where src marked with '#'
			boolean bError = dbAccess.insertSdbIn(damID,in);
		// 	insert record to the "Input" GUI where src marked with '#'
			this.insertInput(in);	
		}	
	}
	
	else
	{
		for (int i=0; i<damInfo._inputEntries.size(); i++) 
		{
			InputEntryInfo in = (InputEntryInfo)damInfo._inputEntries.get(i);
		
			if (out.src.equalsIgnoreCase(in.src))
			{
				compareSrc = true;
			}
		
			if (out.scenario.equalsIgnoreCase(in.scenario))
			{
				compareScenario = true;
			}
		}
		for (int i=0; i<damInfo._downstreamEntries.size(); i++) 
		{
			DownstreamEntryInfo down = (DownstreamEntryInfo)damInfo._downstreamEntries.get(i);
		
			if (out.down_name.equalsIgnoreCase(down.down_name))
			{
				compareDown = true;
				break;
			}
		}
	
		if((compareDown == true) && (compareSrc == true) && (compareScenario == true))
		{
			//System.out.println("In if, all flags are true");
			compareFlag = true;
			for (j = 0 ; j < _outputEntries.size(); j++)
			{
				OutputEntryInfo oldOutput = (OutputEntryInfo)_outputEntries.get(j);
				String oldKey = oldOutput.src.concat(oldOutput.scenario).concat(oldOutput.down_name);
				if(compareKey.compareToIgnoreCase(oldKey) > 0 )
				{
					continue;
				}
				else
				{
					break;
				}
		
			}	
			_outputEntries.add(j, out);

			System.out.println("flags in else:" + compareSrc + compareScenario + compareDown);
			compareFlag = false;
			if (compareSrc == false)
			{
				JOptionPane.showMessageDialog(null,"Error: Could not insert a new output -\nsource must be added to the damcat_in table first.");
			}
			if (compareScenario == false)
			{
				JOptionPane.showMessageDialog(null,"Error: Could not insert a new output -\nscenario must be added to the damcat_in table first.");
			}
			if (compareDown == false)
			{
				JOptionPane.showMessageDialog(null,"Error: Could not insert a new output -\ndownstream point must be added to the damcat_down table first.");
			}
		}
	}
	//System.out.println("compareFlag " + compareFlag);
	return compareFlag;
}
/**
 * This method finds place in cross-section ArraList for insertion pair_name and pair_num,
 * checks integrity with damcat_down table. 
 * Creation date: (1/29/2004 2:18:38 PM)
 */
public boolean insertPair(DamInfo damInfo, CrossSectionEntryInfo pair) 
{
	int j;
	boolean compareFlag = false;
	String compareKey = pair.down_name.concat(pair.xsec_type).concat(pair.pair_num.toString());

	for (int i=0; i<damInfo._downstreamEntries.size(); i++) 
	{
		DownstreamEntryInfo down = (DownstreamEntryInfo)damInfo._downstreamEntries.get(i);
		
		if (pair.down_name.equalsIgnoreCase(down.down_name))
		{
			compareFlag = true;
			break;
		}
	}
	if(compareFlag == true)
	{
		for (j = 0 ; j < _crossSectionEntries.size(); j++)
		{
			CrossSectionEntryInfo oldPair = (CrossSectionEntryInfo)_crossSectionEntries.get(j);
			String oldKey = oldPair.down_name.concat(oldPair.xsec_type).concat(oldPair.pair_num.toString());
		
			if(compareKey.compareToIgnoreCase(oldKey) > 0 )
			{
				continue;
			}
			else
			{
				break;
			}
		}
	
		_crossSectionEntries.add(j, pair);
	}
	else
	{	
		JOptionPane.showMessageDialog(null,"Error: Could not insert a new cross-section -\ndownstream point must be added to the damcat_down table first .");
	}
	return compareFlag;
}
/**
 * Insert the method's description here.
 * Creation date: (7/22/2003 7:31:23 AM)
 * @return java.lang.Float
 * @param s java.lang.String
 */
public static Float safeParseFloat(String s) {
	try {
		return Float.valueOf(s);
	} catch (Exception e) {
		return new Float(0.0f);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (7/22/2003 7:31:23 AM)
 * @return java.lang.Float
 * @param s java.lang.String
 */
public static Integer safeParseInteger(String s) {
	try {
		return Integer.valueOf(s);
	} catch (Exception e) {
		return new Integer(-1);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newComments java.lang.String
 */
public void setComments(java.lang.String newComments) {
	comments = newComments;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newCore java.lang.String
 */
public void setCore(java.lang.String newCore) {
	core = newCore;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newCounty java.lang.String
 */
public void setCounty(java.lang.String newCounty) {
	county = newCounty;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newDam_designer java.lang.String
 */
public void setDam_designer(java.lang.String newDam_designer) {
	dam_designer = newDam_designer;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newDam_former_name java.lang.String
 */
public void setDam_former_name(java.lang.String newDam_former_name) {
	dam_former_name = newDam_former_name;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newDam_height java.lang.Float
 */
public void setDam_height(java.lang.Float newDam_height) {
	dam_height = newDam_height;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newDam_length java.lang.Float
 */
public void setDam_length(java.lang.Float newDam_length) {
	dam_length = newDam_length;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newDam_name java.lang.String
 */
public void setDam_name(java.lang.String newDam_name) {
	dam_name = newDam_name;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newDam_type java.lang.String
 */
public void setDam_type(java.lang.String newDam_type) {
	dam_type = newDam_type;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newDownstream_hazard java.lang.String
 */
public void setDownstream_hazard(java.lang.String newDownstream_hazard) {
	downstream_hazard = newDownstream_hazard;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newDrainage_area java.lang.Float
 */
public void setDrainage_area(java.lang.Float newDrainage_area) {
	drainage_area = newDrainage_area;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newElev java.lang.Float
 */
public void setElev(java.lang.Float newElev) {
	elev = newElev;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newEmerg_action_plan java.lang.String
 */
public void setEmerg_action_plan(java.lang.String newEmerg_action_plan) {
	emerg_action_plan = newEmerg_action_plan;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newFed_construction java.lang.String
 */
public void setFed_construction(java.lang.String newFed_construction) {
	fed_construction = newFed_construction;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newFed_design java.lang.String
 */
public void setFed_design(java.lang.String newFed_design) {
	fed_design = newFed_design;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newFed_funding java.lang.String
 */
public void setFed_funding(java.lang.String newFed_funding) {
	fed_funding = newFed_funding;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newFed_inspection java.lang.String
 */
public void setFed_inspection(java.lang.String newFed_inspection) {
	fed_inspection = newFed_inspection;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newFed_operation java.lang.String
 */
public void setFed_operation(java.lang.String newFed_operation) {
	fed_operation = newFed_operation;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newFed_other java.lang.String
 */
public void setFed_other(java.lang.String newFed_other) {
	fed_other = newFed_other;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newFed_owner java.lang.String
 */
public void setFed_owner(java.lang.String newFed_owner) {
	fed_owner = newFed_owner;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newFed_regulatory java.lang.String
 */
public void setFed_regulatory(java.lang.String newFed_regulatory) {
	fed_regulatory = newFed_regulatory;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newFoundation java.lang.String
 */
public void setFoundation(java.lang.String newFoundation) {
	foundation = newFoundation;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newHsa java.lang.String
 */
public void setHsa(java.lang.String newHsa) {
	hsa = newHsa;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newHydraulic_height java.lang.Float
 */
public void setHydraulic_height(java.lang.Float newHydraulic_height) {
	hydraulic_height = newHydraulic_height;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newInspection_date java.lang.String
 */
public void setInspection_date(java.lang.String newInspection_date) {
	inspection_date = newInspection_date;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newInspection_freq java.lang.String
 */
public void setInspection_freq(java.lang.String newInspection_freq) {
	inspection_freq = newInspection_freq;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newLatitude_dam java.lang.Float
 */
public void setLatitude_dam(java.lang.Float newLatitude_dam) {
	latitude_dam = newLatitude_dam;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newLength_locks java.lang.Float
 */
public void setLength_locks(java.lang.Float newLength_locks) {
	length_locks = newLength_locks;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newLongitude_dam java.lang.Float
 */
public void setLongitude_dam(java.lang.Float newLongitude_dam) {
	longitude_dam = newLongitude_dam;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newMax_discharge java.lang.Float
 */
public void setMax_discharge(java.lang.Float newMax_discharge) {
	max_discharge = newMax_discharge;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newMax_storage java.lang.Float
 */
public void setMax_storage(java.lang.Float newMax_storage) {
	max_storage = newMax_storage;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newNid_height java.lang.Float
 */
public void setNid_height(java.lang.Float newNid_height) {
	nid_height = newNid_height;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newNid_storage java.lang.Float
 */
public void setNid_storage(java.lang.Float newNid_storage) {
	nid_storage = newNid_storage;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newNidid java.lang.String
 */
public void setNidid(java.lang.String newNidid) {
	nidid = newNidid;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newNormal_storage java.lang.Float
 */
public void setNormal_storage(java.lang.Float newNormal_storage) {
	normal_storage = newNormal_storage;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newNumber_locks java.lang.Float
 */
public void setNumber_locks(java.lang.Float newNumber_locks) {
	number_locks = newNumber_locks;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newOther_dam_name java.lang.String
 */
public void setOther_dam_name(java.lang.String newOther_dam_name) {
	other_dam_name = newOther_dam_name;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newOutlet_gates java.lang.String
 */
public void setOutlet_gates(java.lang.String newOutlet_gates) {
	outlet_gates = newOutlet_gates;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newOwner_name java.lang.String
 */
public void setOwner_name(java.lang.String newOwner_name) {
	owner_name = newOwner_name;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newOwner_type java.lang.String
 */
public void setOwner_type(java.lang.String newOwner_type) {
	owner_type = newOwner_type;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newPrebreak_avail java.lang.String
 */
public void setPrebreak_avail(java.lang.String newPrebreak_avail) {
	prebreak_avail = newPrebreak_avail;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newPrivate_on_federal java.lang.String
 */
public void setPrivate_on_federal(java.lang.String newPrivate_on_federal) {
	private_on_federal = newPrivate_on_federal;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newPurposes java.lang.String
 */
public void setPurposes(java.lang.String newPurposes) {
	purposes = newPurposes;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newReturn_flow_region java.lang.Integer
 */
public void setReturn_flow_region(java.lang.Integer newReturn_flow_region) {
	return_flow_region = newReturn_flow_region;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newRfc java.lang.String
 */
public void setRfc(java.lang.String newRfc) {
	rfc = newRfc;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newRiver java.lang.String
 */
public void setRiver(java.lang.String newRiver) {
	river = newRiver;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newSection_t_r java.lang.String
 */
public void setSection_t_r(java.lang.String newSection_t_r) {
	section_t_r = newSection_t_r;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:07 PM)
 * @param newSource_agency java.lang.String
 */
public void setSource_agency(java.lang.String newSource_agency) {
	source_agency = newSource_agency;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newSpillway_type java.lang.String
 */
public void setSpillway_type(java.lang.String newSpillway_type) {
	spillway_type = newSpillway_type;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newSpillway_width java.lang.Float
 */
public void setSpillway_width(java.lang.Float newSpillway_width) {
	spillway_width = newSpillway_width;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newSt_reg_agency java.lang.String
 */
public void setSt_reg_agency(java.lang.String newSt_reg_agency) {
	st_reg_agency = newSt_reg_agency;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newSt_reg_dam java.lang.String
 */
public void setSt_reg_dam(java.lang.String newSt_reg_dam) {
	st_reg_dam = newSt_reg_dam;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newStateid java.lang.String
 */
public void setStateid(java.lang.String newStateid) {
	stateid = newStateid;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newStructural_height java.lang.Float
 */
public void setStructural_height(java.lang.Float newStructural_height) {
	structural_height = newStructural_height;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newSurface_area java.lang.Float
 */
public void setSurface_area(java.lang.Float newSurface_area) {
	surface_area = newSurface_area;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newTopo_map java.lang.String
 */
public void setTopo_map(java.lang.String newTopo_map) {
	topo_map = newTopo_map;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newUpdated java.lang.String
 */
public void setUpdated(java.lang.String newUpdated) {
	updated = newUpdated;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newVolume_dam java.lang.Float
 */
public void setVolume_dam(java.lang.Float newVolume_dam) {
	volume_dam = newVolume_dam;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newWidth_locks java.lang.Float
 */
public void setWidth_locks(java.lang.Float newWidth_locks) {
	width_locks = newWidth_locks;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newYear_completed java.lang.String
 */
public void setYear_completed(java.lang.String newYear_completed) {
	year_completed = newYear_completed;
}
/**
 * Insert the method's description here.
 * Creation date: (1/5/2004 1:38:08 PM)
 * @param newYear_modified java.lang.String
 */
public void setYear_modified(java.lang.String newYear_modified) {
	year_modified = newYear_modified;
}
}
