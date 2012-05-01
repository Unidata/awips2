package gov.noaa.nws.ncep.ui.nsharp.maprsc;
/**
 * 
 * 
 * 
 * This java class performs the NSHARP pfc sounding data query functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 11/1/2010	362			Chin Chen	Initial coding
 * 12/16/2010   362         Chin Chen   add support of BUFRUA observed sounding and PFC (NAM and GFS) model sounding data
 * 02/15/2012               Chin Chen   add  PFC sounding query algorithm for better performance getPfcSndDataBySndTmRange()
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;
import gov.noaa.nws.ncep.viz.common.soundingQuery.NcSoundingQuery;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import java.util.List;
import java.util.Map;
import java.sql.Timestamp;


// Chin-T import com.raytheon.uf.common.sounding.SoundingLayer;

public class NsharpPfcSoundingQuery {
    /*
     * Create python script to query data from edex
     */
	/*
    private static String scriptCreator( double lat, double lon, Timestamp refTime, Timestamp validTime, NcSoundingProfile.PfcSndType sndType) {
    	
    	StringBuilder query = new StringBuilder();
        query.append("import NcSoundingDataRequest\n");
        query.append("sndRq = NcSoundingDataRequest.NcSoundingDataRequest()\n");
        //query.append("sndRq.setLat(" +lat+ ")\n");
        //query.append("sndRq.setLon(" + lon + ")\n");
        query.append("sndRq.setRefTime(" +refTime.getTime() + "L)\n");
        query.append("sndRq.setValidTime(" +validTime.getTime() + "L)\n");
        query.append("sndRq.setSndType('" +sndType + "')\n");
        //query.append("return sndRq.execute()");
        query.append("return sndRq.getSoundingDataByLatLonArray([["+lat+","+lon+"]])");
    	System.out.println(query.toString());
    	return query.toString();
    } */

	//Chin-T public static void getPfcSndData(List<NsharpStationInfo> stnPtDataLineLst, Map<String, List<SoundingLayer>> soundingLysLstMap) {
	/*public static void getPfcSndData(List<NsharpStationInfo> stnPtDataLineLst, Map<String, List<NcSoundingLayer>> soundingLysLstMap) {
		for(NsharpStationInfo StnPt :  stnPtDataLineLst){
			//one StnPt represent one data time line 
			//NcSoundingProfile sndPf= PfcSoundingQuery.getPfcSndData(StnPt.getDatauri(),(float)StnPt.getLatitude(), (float)StnPt.getLongitude(), StnPt.getReftime(), 
			//		StnPt.getRangestarttime(), PfcSoundingQuery.PfcSndType.NAMSND);

			//query using NcSoundingQuery class
			double[][] latLon = {{StnPt.getLatitude(), StnPt.getLongitude()}};
			for(NsharpStationInfo.timeLineSpecific tmlinSpc: StnPt.getTimeLineSpList() ){
				Timestamp rangeTime = tmlinSpc.getTiemLine();
				String stnDispInfo = tmlinSpc.getDisplayInfo();
				NcSoundingCube cube = NcSoundingQuery.pfcSoundingQueryByLatLon(StnPt.getReftime().getTime(),rangeTime.getTime(), latLon, StnPt.getSndType(), NcSoundingLayer.DataType.ALLDATA, false, "-1");
				//System.out.println(stnDispInfo + " "+ rangeTime);
				if(cube != null&& cube.getSoundingProfileList().size()>0){
					NcSoundingProfile sndPf = cube.getSoundingProfileList().get(0);

					List<NcSoundingLayer> rtnSndLst = sndPf.getSoundingLyLst();
					// Chin-T List<SoundingLayer>  sndLyList = NsharpSoundingQueryCommon.convertToSoundingLayerList(rtnSndLst);
					if(rtnSndLst != null &&  rtnSndLst.size() > 0){  
						//update sounding data so they can be used by Skewt Resource and PalletWindow
						//we should not have to do this, if EDEX has done this correctly....
						//sndLyList = NsharpDataHandling.updateObsSoundingDataForShow(sndLyList, (float)StnPt.getElevation());

						//Remove sounding layers that not used by NSHARP
						rtnSndLst = NsharpDataHandling.organizeSoundingDataForShow(rtnSndLst, sndPf.getStationElevation());
						//minimum rtnSndList size will be 2 (50 & 75 mb layers), but that is not enough
						// We need at least 2 regular layers for plotting
						if(rtnSndLst != null &&  rtnSndLst.size() > 4)
							//TBD soundingLysLstMap.put(StnPt.getStnDisplayInfo(), rtnSndLst);
							soundingLysLstMap.put(stnDispInfo, rtnSndLst);
						//System.out.println(stnDispInfo + " with sound layer size of "+ rtnSndLst.size());
					}
				}
			}
		}
	}*/
	/*
	 * Chin, use sounding time range array  to query.
	 * 2/14/2012
	 */
	public static void getPfcSndDataBySndTmRange(List<NsharpStationInfo> stnPtDataLineLst, Map<String, List<NcSoundingLayer>> soundingLysLstMap) {
		String stnDispInfo = "";
		NcSoundingCube cube;
		for(NsharpStationInfo StnPt :  stnPtDataLineLst){
			//one StnPt represent one data time line 
			//NcSoundingProfile sndPf= PfcSoundingQuery.getPfcSndData(StnPt.getDatauri(),(float)StnPt.getLatitude(), (float)StnPt.getLongitude(), StnPt.getReftime(), 
			//		StnPt.getRangestarttime(), PfcSoundingQuery.PfcSndType.NAMSND);

			long[] rangeTimeArray = new long[StnPt.getTimeLineSpList().size()];
			int i=0;
			for(NsharpStationInfo.timeLineSpecific tmlinSpc: StnPt.getTimeLineSpList() ){
				Timestamp rangeTime = tmlinSpc.getTiemLine();
				rangeTimeArray[i]=rangeTime.getTime();
				i++;
			}
			//chin for testing pfcSoundingQueryByLatLon()
			//double[][] latLon = new double [1][2];
			//latLon[0][0]=StnPt.getLatitude();
			//latLon[0][1]=StnPt.getLongitude();
			//cube = NcSoundingQuery.pfcSoundingQueryByLatLon(StnPt.getReftime().getTime(), rangeTimeArray[0],latLon , StnPt.getSndType(), NcSoundingLayer.DataType.ALLDATA, false, "-1");
			//end test 
			cube = NcSoundingQuery.pfcSoundingQueryByRangeTimeArray(StnPt.getReftime().getTime(),
					rangeTimeArray, StnPt.getLatitude(),StnPt.getLongitude(), StnPt.getSndType(), NcSoundingLayer.DataType.ALLDATA, false, "-1");
			if(cube != null&& cube.getSoundingProfileList().size()>0){
				for(NcSoundingProfile sndPf : cube.getSoundingProfileList()){
					List<NcSoundingLayer> rtnSndLst = sndPf.getSoundingLyLst();
					if(rtnSndLst != null &&  rtnSndLst.size() > 0){  
						rtnSndLst = NsharpDataHandling.organizeSoundingDataForShow(rtnSndLst, sndPf.getStationElevation());
						//minimum rtnSndList size will be 2 (50 & 75 mb layers), but that is not enough
						// We need at least 2 regular layers for plotting
						if(rtnSndLst != null &&  rtnSndLst.size() > 4){
							stnDispInfo="NA";
							for(int j=0; j < StnPt.getTimeLineSpList().size(); j++ ){
								NsharpStationInfo.timeLineSpecific tmlinSpcj = StnPt.getTimeLineSpList().get(j);
								//System.out.println("rtnSndTIme="+ sndPf.getFcsTime() + " requestTime"+j+"="+tmlinSpcj.getTiemLine().getTime());
								if(tmlinSpcj.getTiemLine().getTime()== sndPf.getFcsTime()){
									stnDispInfo = tmlinSpcj.getDisplayInfo();
									break;
								}
							}
							soundingLysLstMap.put(stnDispInfo, rtnSndLst);
						
							//System.out.println(stnDispInfo + " with sound layer size of "+ rtnSndLst.size());
						}
					}
				}
			}
		}
	}

}
