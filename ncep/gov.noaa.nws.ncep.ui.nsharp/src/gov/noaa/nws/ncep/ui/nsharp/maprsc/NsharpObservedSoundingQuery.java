package gov.noaa.nws.ncep.ui.nsharp.maprsc;
/**
 * 
 * 
 * 
 * This java class performs the NSHARP observed sounding data query functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 11/1/2010	362			Chin Chen	Initial coding
 * 12/16/2010   362         Chin Chen   add support of BUFRUA observed sounding and PFC (NAM and GFS) model sounding data
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;
import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
//Chin-T import com.raytheon.uf.common.sounding.SoundingLayer;

import com.vividsolutions.jts.geom.Coordinate;

public class NsharpObservedSoundingQuery {
	//private static final String METADATA_DB_NAME = "metadata";
	//private static final String UAIR_OBS_TBL_NAME = "uair_obslevels";
	//private static final String UAIR_MAXWIND_TBL_NAME = "uair_maxwind";
	//private static final String UAIR_TROPO_TBL_NAME = "uair_tropopause";
	//private static final UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND.getConverterTo(NonSI.KNOT);

	 /*
     * Create python script to query data from edex
     * Example:
     * import NcSoundingDataRequest
	 * sndRq = NcSoundingDataRequest.NcSoundingDataRequest()
	 * sndRq.setSndType('UAIR')
	 * sndRq.setDataType('ALLDATA')
	 * sndRq.setDbIdList([1002248,1003668])
	 * return sndRq.execute()
     */
	/*
    private static String scriptCreator(List<Integer> dbIds,  NcSoundingProfile.ObsSndType sndType) {
    	
    	StringBuilder query = new StringBuilder();
    	if(dbIds.size()>0){
    		query.append("import NcSoundingDataRequest\n");
    		query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
    		query.append("sndRq.setSndType('" +sndType + "')\n");
    		query.append("sndRq.setDataType('ALLDATA')\n");
    		query.append("sndRq.setDbIdList([");
    		for(int i =0; i < dbIds.size()-1; i++){
    			query.append(dbIds.get(i) + ",");
    		}
    		query.append(dbIds.get(dbIds.size()-1));
    		query.append("])\n");

    		query.append("return sndRq.execute()");
    		System.out.println(query.toString());
    	}
    	return query.toString();
    }*/
    /* use lat/lon/synoptictime for query
    private static String scriptCreator(NsharpStationInfo stn, NcSoundingProfile.ObsSndType sndType) {
    	
    	StringBuilder query = new StringBuilder();

    	query.append("import NcSoundingDataRequest\n");
    	query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
    	query.append("sndRq.setSndType('" +sndType + "')\n");
    	//query.append("sndRq.setDataType('ALLDATA')\n");
    	//query.append("sndRq.setLat("+ stn.getLatitude()+ ")\n");
    	//query.append("sndRq.setLon(" + stn.getLongitude() + ")\n");
    	query.append("sndRq.setRefTime(" +stn.getReftime().getTime() + "L)\n"); 
    	query.append("sndRq.setMerge(1)\n");
    	//query.append("return sndRq.execute()");
    	query.append("return sndRq.getSoundingDataByLatLonArray([["+stn.getLatitude()+","+stn.getLongitude()+"]])");
    	System.out.println(query.toString());

    	return query.toString();
    }*/
    /*
	private static List<Object[]> queryDb(String queryStr){
		List<Object[]> list = null;
		try {
			list = DirectDbQuery.executeQuery( queryStr, METADATA_DB_NAME, QueryLanguage.SQL);
		}
		catch (Exception e ){
			System.out.println("DB exception: this query--"+queryStr+ " error msg: "+e.getMessage());			
		}
		return list;
	}
	*/
    
    //Chin-T public static void getObservedSndData(List<NsharpStationInfo> stnPtDataLineLst, Map<String, List<SoundingLayer>> soundingLysLstMap) {
	//Chin: note that Nsharp currently GUI only allow user pick one stn at one time, but could be many refTimes.
	public static void getObservedSndData(List<NsharpStationInfo> stnPtDataLineLst, boolean rawData, Map<String, List<NcSoundingLayer>> soundingLysLstMap) {
		//String pickedStnInfo = "";
		List<Coordinate> coords= new ArrayList<Coordinate>();
		List<Long> refTimeLst = new ArrayList<Long>();
		//create refTime array and lat/lon array
		for(NsharpStationInfo StnPt :  stnPtDataLineLst){
			//one StnPt represent one data time line 
			//List<Integer> Ids  = StnPt.getDbId();
			System.out.println("stn lat ="+StnPt.getLatitude()+ " lon="+StnPt.getLongitude());
			boolean exist = false;
			for(Coordinate c: coords){
				if(c.x == StnPt.getLongitude() && c.y == StnPt.getLatitude()){
					exist= true;
					break;
				}
			}			
			if(exist==false) {
				Coordinate coord = new Coordinate(StnPt.getLongitude(),StnPt.getLatitude());
				coords.add(coord);
			}
			exist = false;
			for(long t: refTimeLst){
				if(t == StnPt.getReftime().getTime()){
					exist= true;
					break;
				}
			}			
			if(exist==false) {
				refTimeLst.add(StnPt.getReftime().getTime());
			}
			
				
		}
		double[][] latLon = new double[coords.size()][2];
		for (int i=0; i< coords.size(); i++){
			latLon[i][0]= coords.get(i).y; //lat
			latLon[i][1]= coords.get(i).x; //lon
		}
		NcSoundingCube cube = NcSoundingQuery.uaGenericSoundingQuery(refTimeLst.toArray(new Long[0]), latLon, stnPtDataLineLst.get(0).getSndType(),
				NcSoundingLayer.DataType.ALLDATA, !rawData, "-1");
		//NcSoundingCube cube = NcSoundingQuery.soundingQueryByLatLon(stnPtDataLineLst.get(0).getReftime().getTime(), coords, stnPtDataLineLst.get(0).getSndType(),
		//		NcSoundingLayer.DataType.ALLDATA, !rawData, "-1");
		if(cube != null && cube.getSoundingProfileList().size()>0 && cube.getRtnStatus()==NcSoundingCube.QueryStatus.OK){
			for(NcSoundingProfile sndPf : cube.getSoundingProfileList()){
				List<NcSoundingLayer> rtnSndLst = sndPf.getSoundingLyLst();
				//if(rtnSndLst != null &&  rtnSndLst.size() > 0){  

					//NcSoundingProfile sndPf = cube.getSoundingProfileList().get(0);
					//System.out.println("size of profile = "+ cube.getSoundingProfileList().size());
					//debug
					//for(NcSoundingProfile pf: cube.getSoundingProfileList()){
					//	System.out.println("sounding profile: lat="+pf.getStationLatitude()+" lon="+pf.getStationLongitude()+ " stnId="+ pf.getStationId() );
					//}
					//List<NcSoundingLayer> rtnSndLst = sndPf.getSoundingLyLst();
					// Chin-T List<SoundingLayer>  sndLyList = NsharpSoundingQueryCommon.convertToSoundingLayerList(rtnSndLst);
					if(rtnSndLst != null &&  rtnSndLst.size() > 0){  
						//update sounding data so they can be used by Skewt Resource and PalletWindow
						if(rawData)
							rtnSndLst = NsharpDataHandling.sortObsSoundingDataForShow(rtnSndLst, sndPf.getStationElevation());
						else
							rtnSndLst = NsharpDataHandling.organizeSoundingDataForShow(rtnSndLst, sndPf.getStationElevation());
						//minimum rtnSndList size will be 2 (50 & 75 mb layers), but that is not enough
						// We need at least 2 regular layers for plotting
						if(rtnSndLst != null &&  rtnSndLst.size() > 4){
							String dispInfo="";
							for(NsharpStationInfo StnPt :  stnPtDataLineLst){
								if(StnPt.getReftime().getTime() == sndPf.getFcsTime()){
									dispInfo = StnPt.getStnDisplayInfo();
									break;
								}
							}
							soundingLysLstMap.put(dispInfo, rtnSndLst);
						}
					}
				}
			//}
		}
			
	}
	
}
