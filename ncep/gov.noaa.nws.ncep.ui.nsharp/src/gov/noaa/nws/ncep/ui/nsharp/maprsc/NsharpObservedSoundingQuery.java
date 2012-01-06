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

	public static void getObservedSndData(List<NsharpStationInfo> stnPtDataLineLst, boolean rawData, Map<String, List<NcSoundingLayer>> soundingLysLstMap) {
		//String pickedStnInfo = "";
		for(NsharpStationInfo StnPt :  stnPtDataLineLst){
			//one StnPt represent one data time line 
			//List<Integer> Ids  = StnPt.getDbId();
			//System.out.println("stn lat ="+StnPt.getLatitude()+ " lon="+StnPt.getLongitude());
			List<Coordinate> coords= new ArrayList<Coordinate>();
			Coordinate coord = new Coordinate(StnPt.getLongitude(),StnPt.getLatitude());
			coords.add(coord);
			//testing
			/*Coordinate coord = new Coordinate(91.93000030517578, 21.43000030517578);
			coords.add(coord);
			coord = new Coordinate(91.80999755859375, 22.350000381469727);
			coords.add(coord);
			coord = new Coordinate(90.36000061035156, 22.75);
			coords.add(coord);
			coord = new Coordinate(89.16000366210938, 23.18000030517578);
			coords.add(coord);
			coord = new Coordinate(90.37999725341797, 23.760000228881836);
			coords.add(coord);
			coord = new Coordinate(89.05000305175781, 24.1299991607666);
			coords.add(coord);
			coord = new Coordinate(91.87999725341797, 24.899999618530273);
			coords.add(coord);
			coord = new Coordinate(89.36000061035156, 24.850000381469727);
			coords.add(coord);*/
			NcSoundingCube cube = NcSoundingQuery.soundingQueryByLatLon(StnPt.getReftime().getTime(), coords, StnPt.getSndType(),
					NcSoundingLayer.DataType.ALLDATA, !rawData, "-1");
			if(cube != null && cube.getRtnStatus()==NcSoundingCube.QueryStatus.OK){
				NcSoundingProfile sndPf = cube.getSoundingProfileList().get(0);
				//System.out.println("size of profile = "+ cube.getSoundingProfileList().size());
				//debug
				//for(NcSoundingProfile pf: cube.getSoundingProfileList()){
				//	System.out.println("sounding profile: lat="+pf.getStationLatitude()+" lon="+pf.getStationLongitude()+ " stnId="+ pf.getStationId() );
				//}
				List<NcSoundingLayer> rtnSndLst = sndPf.getSoundingLyLst();
				// Chin-T List<SoundingLayer>  sndLyList = NsharpSoundingQueryCommon.convertToSoundingLayerList(rtnSndLst);
				if(rtnSndLst != null &&  rtnSndLst.size() > 0){  
					//update sounding data so they can be used by Skewt Resource and PalletWindow
					if(rawData)
						rtnSndLst = NsharpDataHandling.sortObsSoundingDataForShow(rtnSndLst, sndPf.getStationElevation());
					else
						rtnSndLst = NsharpDataHandling.organizeSoundingDataForShow(rtnSndLst, sndPf.getStationElevation());
					//minimum rtnSndList size will be 2 (50 & 75 mb layers), but that is not enough
					// We need at least 2 regular layers for plotting
					if(rtnSndLst != null &&  rtnSndLst.size() > 4)
						soundingLysLstMap.put(StnPt.getStnDisplayInfo(), rtnSndLst);
				}
			}
			
		} // end for loop of  stnPtsLst
	}
	
}
