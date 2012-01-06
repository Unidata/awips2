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
 *
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
	public static void getPfcSndData(List<NsharpStationInfo> stnPtDataLineLst, Map<String, List<NcSoundingLayer>> soundingLysLstMap) {
		String pickedStnInfo = "";
		
		for(NsharpStationInfo StnPt :  stnPtDataLineLst){
			//one StnPt represent one data time line 
			//NcSoundingProfile sndPf= PfcSoundingQuery.getPfcSndData(StnPt.getDatauri(),(float)StnPt.getLatitude(), (float)StnPt.getLongitude(), StnPt.getReftime(), 
			//		StnPt.getRangestarttime(), PfcSoundingQuery.PfcSndType.NAMSND);
		
			//query using NcSoundingQuery class
			float[][] latLon = {{StnPt.getLatitude(), StnPt.getLongitude()}};
			NcSoundingCube cube = NcSoundingQuery.pfcSoundingQueryByLatLon(StnPt.getReftime().getTime(),StnPt.getRangestarttime().getTime(), latLon, StnPt.getSndType(), NcSoundingLayer.DataType.ALLDATA, false, "-1");
			if(cube != null&& cube.getSoundingProfileList().size()>0){
				NcSoundingProfile sndPf = cube.getSoundingProfileList().get(0);
				
				List<NcSoundingLayer> rtnSndLst = sndPf.getSoundingLyLst();
				// Chin-T List<SoundingLayer>  sndLyList = NsharpSoundingQueryCommon.convertToSoundingLayerList(rtnSndLst);
				if(rtnSndLst != null &&  rtnSndLst.size() > 0){  
					if(pickedStnInfo == ""){
						//use as first stn to show
						pickedStnInfo = StnPt.getStnDisplayInfo();
					}
					//update sounding data so they can be used by Skewt Resource and PalletWindow
					//we should not have to do this, if EDEX has done this correctly....
					//sndLyList = NsharpDataHandling.updateObsSoundingDataForShow(sndLyList, (float)StnPt.getElevation());
					
					//Remove sounding layers that not used by NSHARP
					rtnSndLst = NsharpDataHandling.organizeSoundingDataForShow(rtnSndLst, sndPf.getStationElevation());
					//minimum rtnSndList size will be 2 (50 & 75 mb layers), but that is not enough
					// We need at least 2 regular layers for plotting
					if(rtnSndLst != null &&  rtnSndLst.size() > 4)
						soundingLysLstMap.put(StnPt.getStnDisplayInfo(), rtnSndLst);
					//System.out.println(StnPt.getStnDisplayInfo() + " with sound layer size of "+ soundLyLst.size());
				}
			}
		}
	}

}
