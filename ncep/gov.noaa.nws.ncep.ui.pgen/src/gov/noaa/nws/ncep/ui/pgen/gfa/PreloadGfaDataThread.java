/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.PreloadGfaDataThread
 * 
 * July 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;

//import org.apache.log4j.Logger;

import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Class to pre-load classes and the data from the databases and tables.
 * 
 * Note: this thread is run when the PGEN is activated (PgenPaletteWindow.init)
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/2010		#223		M.Laryukhin	Initial Creation.
 * 04/2011					J. Wu		Load coastal water, MTBC table,
 * 										and SNAP points.
 * 05/2011					J. Wu		Move Gfa bound loading into GfaClip.
 * 07/2011      #430        Q. Zhou     Preload station table.
 * </pre>
 * 
 * @author mlaryukhin
 */
public class PreloadGfaDataThread extends Thread {
	
	public static boolean loaded;
	
	public boolean started;
	
//	private final static Logger logger = Logger.getLogger(PreloadGfaDataThread.class);
	
	private String [] toLoad = {LocalizationManager.class.getName(), 
			PgenCycleTool.class.getName(), 
			SigmetInfo.class.getName(), 
			GfaClip.class.getName()};
	
	@Override
	public void run() {
		
		if(started || loaded) return;
		
		started = true;
		
		long time = System.currentTimeMillis();
		
		ClassLoader loader = this.getClass().getClassLoader();
		try {
			for(String c: toLoad){
				loader.loadClass(c);
			}
			
			LocalizationManager.getBaseDir();
			GfaInfo.getDocument();
			new SigmetInfo();
			
			//Load all GFA bounds.
			GfaClip.getInstance().loadGfaBounds();
			ReduceGfaPointsUtil.getStationTable(); //put here because Gfa uses canFormatted
			
		} catch (Exception e) {
//			logger.error(e);
			e.printStackTrace();
//			logger.debug("LoadGfaClassesThread loading time: " + (System.currentTimeMillis() - time) + " ms");
		}
		
//		logger.debug("LoadGfaClassesThread loading time: " + (System.currentTimeMillis() - time) + " ms");
		
		loaded = true;
	}
}
