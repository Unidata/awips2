package gov.noaa.nws.ncep.ui.nsharp.view;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.view.NsharpHandleArchiveFile
 * 
 * This java class performs the NSHARP loading archived files functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 12/23/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpStationInfo;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpDataHandling;
import gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNativeConstants;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

public class NsharpHandleArchiveFile {
	public static void openArchiveFile(Shell shell){
		/* Chin: 3/14/2013
		 * A typical saved file contents is as following....
		 * SNDTYPE=MDL  TITLE=KMFR 130131/1200 NCUAIR STNID=KMFR  LAT=42.38 LON=-122.87
		 * PRESSURE  HGHT	   TEMP	  DWPT    WDIR     WSPD    OMEG
		 * 997.500000  129.000000  -3.250006  -3.381190  10.619656  1.627882  0.000000
		 * ........
		 */
		FileDialog fd = new FileDialog(shell, SWT.MULTI);//SWT.OPEN);
		fd.setText("Open");
		fd.setFilterPath("C:/");
		String[] filterExt = { "*.nsp", "*","*.txt", "*.doc", ".rtf", "*.*" };
		fd.setFilterExtensions(filterExt);
		String selected;
		String code= fd.open();
		if(code == null){
			return;
		}
		String [] selecteds = fd.getFileNames();
		
		//if(selected!= null){
		if(selecteds!= null && selecteds.length > 0){
			Map<String, List<NcSoundingLayer>> soundingLysLstMap = new HashMap<String, List<NcSoundingLayer>>();
			//List<String> timeList = new ArrayList<String>();	
			//String timeLine ;
			String stnDispInfoStr="N/A N/A";
			//read in archive file
			InputStream is = null;
			NsharpStationInfo stninfo = new NsharpStationInfo();
			String sndType="N/A";
			try {
				for( int j = 0; j < selecteds.length; j++){
					selected = "";
					StringBuilder strContent = new StringBuilder("");
					selected = selected + fd.getFilterPath();
					if (selected.charAt(selected.length() - 1) != File.separatorChar) {
						selected = selected + (File.separatorChar);
					}
					selected = selected + selecteds[j];
					//System.out.println(selected);

					is = new FileInputStream(selected);
					int byteread;
					while((byteread = is.read()) != -1){
						strContent.append((char)byteread);
						//System.out.println((char)byteread);
					}
					if(strContent.length() <=100)
						return;
					//System.out.println(strContent);
					int lat = strContent.indexOf("LAT=");
					if(lat > 0 )
					{
						lat = lat+4;
						int endIndex = strContent.substring(lat).indexOf(";");
						if(endIndex>0){
							String latStr= strContent.substring(lat,lat+endIndex);
							stninfo.setLatitude(Float.parseFloat(latStr));
						}
					}
					int lon = strContent.indexOf("LON=");
					if(lon > 0 )
					{
						lon = lon+4;
						int endIndex = strContent.substring(lon).indexOf(";");
						if(endIndex>0){
							String lonStr= strContent.substring(lon,lon+endIndex);
							stninfo.setLongitude(Float.parseFloat(lonStr));
						}
					}
					int snd = strContent.indexOf("SNDTYPE=");
					if(snd > 0 )
					{
						snd = snd+8;
						int endIndex = strContent.substring(snd).indexOf(";");
						if(endIndex>0){
							String sndStr= strContent.substring(snd,snd+endIndex);
							stninfo.setSndType(sndStr);
							if(NsharpLoadDialog.getAccess()!= null ){
								if(sndStr.equals("PFC"))
									NsharpLoadDialog.getAccess().setActiveLoadSoundingType(NsharpLoadDialog.PFC_SND);
								else if(sndStr.equals("MDL"))
									NsharpLoadDialog.getAccess().setActiveLoadSoundingType(NsharpLoadDialog.MODEL_SND);
								else if(sndStr.equals("OBS"))
									NsharpLoadDialog.getAccess().setActiveLoadSoundingType(NsharpLoadDialog.OBSER_SND);
							}
						}
					}
					int title = strContent.indexOf("TITLE=");
					if(title > 0 )
					{
						title = title+6;
						int endIndex = strContent.substring(title).indexOf(";");
						if(endIndex>0){
							String titleStr= strContent.substring(title,title+endIndex);
							stninfo.setStnDisplayInfo(titleStr);
							stnDispInfoStr = titleStr;
						}
					}
					//timeLine = new String("");
					//stnDispInfoStr = new String("");
					List<NcSoundingLayer> sndLyList = new ArrayList<NcSoundingLayer>();
					NcSoundingLayer sndLy = null;
					StringTokenizer st = new StringTokenizer(strContent.toString());
					int i =0;
					int dataStartIndex = 15;
					int dataCycleLength = 7;
					while (st.hasMoreTokens()) {
						i++;
						//System.out.println(st.nextToken());
						//Chin's NOTE: Our input file should have the same format as the text information shown with "Show Text"
						// button. The "Save" Button will save text data in same format as well.
						//first token is stn display info string,
						//2nd and 3rd token are time line. 
						//4th and 5th tokens are LAT=xxx, LON=xxx of stn
						//token 6 to 12 are PRESSURE, HGHT, TEMP, DWPT, WDIR, WSPD, OMEG words. We don't need to read them.
						//From token 13, we have pressure, height, temp,..., omega, pressure, height,..omega.
						//These weather data will be repeated every 7 tokens.
						String tok = st.nextToken();
						if(tok.equals("OMEG")){
							dataStartIndex = i+1;
						}
						if (i >=dataStartIndex){
						
							if((i-dataStartIndex)%dataCycleLength ==0){
								sndLy = new NcSoundingLayer();
								sndLyList.add(sndLy);
								if( Float.isNaN(Float.parseFloat(tok) ))
									sndLy.setPressure(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
								else
									sndLy.setPressure(Float.parseFloat(tok));
								
							}else if((i-dataStartIndex)%dataCycleLength ==1){
								if( Float.isNaN(Float.parseFloat(tok) ))
									sndLy.setGeoHeight(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
								else
									sndLy.setGeoHeight(Float.parseFloat(tok));
							}else if((i-dataStartIndex)%dataCycleLength ==2){
								if( Float.isNaN(Float.parseFloat(tok) ))
									sndLy.setTemperature(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
								else
									sndLy.setTemperature(Float.parseFloat(tok));
							}else if((i-dataStartIndex)%dataCycleLength ==3){
								if( Float.isNaN(Float.parseFloat(tok) ))
									sndLy.setDewpoint(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
								else
									sndLy.setDewpoint(Float.parseFloat(tok));
							}else if((i-dataStartIndex)%dataCycleLength ==4){
								if( Float.isNaN(Float.parseFloat(tok) ))
									sndLy.setWindDirection(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
								else
									sndLy.setWindDirection(Float.parseFloat(tok));
							}else if((i-dataStartIndex)%dataCycleLength ==5){
								if( Float.isNaN(Float.parseFloat(tok) ))
									sndLy.setWindSpeed(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
								else
									sndLy.setWindSpeed(Float.parseFloat(tok));
							}else if((i-dataStartIndex)%dataCycleLength ==6){
								if( Float.isNaN(Float.parseFloat(tok) ))
									sndLy.setOmega(NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA);
								else
									sndLy.setOmega(Float.parseFloat(tok));
								
							}

						}
						//System.out.println("line " + i + "="+ tok);

					}
					//System.out.println("total line " + i);
					//System.out.println("time line " + timeLine + " stn disp info = " + stnDispInfo);
					if(sndLyList.size()> 0)
					//Remove sounding layers that not used by NSHARP, and assume first layer is sfc layer from input data
						sndLyList = NsharpDataHandling.organizeSoundingDataForShow(sndLyList, sndLyList.get(0).getGeoHeight());
					//minimum rtnSndList size will be 2 (50 & 75 mb layers), but that is not enough
					// We need at least 2 regular layers for plotting
					if(sndLyList != null &&  sndLyList.size() > 4)
						soundingLysLstMap.put(stnDispInfoStr, sndLyList);
				}
				if(soundingLysLstMap.size()>0){
					// create an editor NsharpSkewTEditor
					NsharpEditor editor = NsharpEditor.createOrOpenEditor();
					NsharpResourceHandler rsc = editor.getRscHandler();
					rsc.addRsc(soundingLysLstMap, stninfo);
					rsc.setSoundingType(sndType);
					NsharpEditor.bringEditorToTop();
				}
				else {
					//Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
					MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING
							| SWT.OK);
					mb.setMessage("Invalid sounding data retrieved from archive file!!");
					mb.open();
				}
			} catch (FileNotFoundException e) {
				
				e.printStackTrace();
			} catch (IOException e) {
				
				e.printStackTrace();
			} catch (NumberFormatException e) {
				System.out.println("number format exception happened");
				e.printStackTrace();
			}
			finally {
				if (is != null) {
					try {
						is.close();
					} catch (IOException ioe) {
						System.out.println("Could not close input file ");
					}
				}
			}
		}
		
	}
}
