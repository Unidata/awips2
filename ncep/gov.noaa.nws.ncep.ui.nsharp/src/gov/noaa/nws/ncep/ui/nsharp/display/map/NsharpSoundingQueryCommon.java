package gov.noaa.nws.ncep.ui.nsharp.display.map;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.display.map.NsharpSoundingQueryCommon
 * 
 * This java class performs the NSHARP Modal functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 10/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.sounding.SoundingLayer;

public class NsharpSoundingQueryCommon {
	public static List<SoundingLayer>  convertToSoundingLayerList(List<NcSoundingLayer>  sndLst){
		List<SoundingLayer>   newLst = new ArrayList<SoundingLayer>();
		for(NcSoundingLayer inLayer: sndLst){
			SoundingLayer outLayer = new SoundingLayer();
			outLayer.setDewpoint(inLayer.getDewpoint());
			outLayer.setTemperature(inLayer.getTemperature());
			outLayer.setPressure(inLayer.getPressure());
			outLayer.setGeoHeight(inLayer.getGeoHeight());
			outLayer.setWindDirection(inLayer.getWindDirection());
			outLayer.setWindSpeed(inLayer.getWindSpeed());
			outLayer.setOmega(inLayer.getOmega());
			
			newLst.add(outLayer);
		}
		return newLst;
	}

}
