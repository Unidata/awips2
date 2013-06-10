/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.AttrSettings
 * 
 * 26 March 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.elements.Track;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.ui.pgen.productmanage.ProductConfigureDialog;

import java.io.File;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Singleton for the default attribute settings of PGEN DrawableElements.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/09		84			J. Wu  		Initial Creation.
 * 04/09		89			J. Wu  		Added Arc.
 * 07/10		270			B. Yin		Use AbstractDrawableComponent instead of IAttribute
 * 12/10		359			B. Yin		Change loadPgenSettings to public 
 * 07/11        450         G. Hull     settings tbl from NcPathManager
 * 08/11		?			B. Yin		Put current time for Storm Track
 * 11/11		?  			B. Yin		Load settings for different Activity Type.
 * 09/12					B. Hebbard	Merge changes by RTS OB12.9.1 (format only, this file)
 * 
 * </pre>
 * 
 * @author	J. Wu
 */

public class AttrSettings {
	
	private static AttrSettings  INSTANCE = null;
	
	private static HashMap<String, AbstractDrawableComponent> settings = null;

    private static String settingsTblLocal = "." + File.separator;   

    public static String settingsFileName = "settings_tbl.xml";
    
	/**
	 * Private constructor
     * 
	 * @throws VizException
	 */
	private AttrSettings() throws VizException {		
        super();
        settings = new HashMap<String, AbstractDrawableComponent>();
        loadSettingsTable();
	}
	
	/**
     * Creates a AttrSettings instance if it does not exist and returns the
     * instance. If it exists, return the instance.
	 *  
	 * @return
	 */
	public static AttrSettings getInstance() {
		
		if ( INSTANCE == null ) {
					
			try {
				INSTANCE = new AttrSettings( );
			} catch (VizException e) {
				e.printStackTrace();
			}
			
		}
		
		return INSTANCE;
		
	}
	
	/**
	 * Gets the default settings loaded from the settings_tbl.xml.
	 */
	public HashMap<String, AbstractDrawableComponent> getSettings() {				
		return settings;
	}

	/**
     * @param value
     *            the DrawableElement to set gilbert: noticed this wasn't being
     *            used AND I wasn't sure how to modify it for the new pgenType
     *            and pgenCategory DE attributes, so I juct commented it out for
     *            now.
	 * 
     *            public void setSettings( IAttribute de ) {
     * 
     *            String pgenID = null; DrawableElement elem = null;
     * 
     *            if ( de instanceof IMultiPoint ) { pgenID =
     *            de.getLinePattern(); } else { pgenID = de.getType(); }
     * 
     *            elem = (DrawableElement)settings.get( pgenID ); elem.update(
     *            de );
     * 
     *            settings.put( pgenID, elem ); }
	 */

	public void setSettings( AbstractDrawableComponent de ) {

		String pgenID = de.getPgenType();

		settings.put( pgenID, de);
	}
	
	/**
     * Load default settings from settings_tbl.xml First try to load from user's
     * local directory; if not found, load the base directory.
	 */
	private void loadSettingsTable() {		
		
		/*
		 *  Get the settings table file from localization
		 */		
		File settingsFile =		PgenStaticDataProvider.getProvider().getFile(
                PgenStaticDataProvider.getProvider().getPgenLocalizationRoot()
                        + settingsFileName);
		
		if ( settingsFile == null ) {
			System.out.println("Unable to fing pgen settings table");
		}	
		
		loadPgenSettings( settingsFile.getAbsolutePath() );
	}
	
	public void loadProdSettings( String prodName ){
		
		if ( prodName == null || prodName.isEmpty() ){
			loadSettingsTable();
        } else {
			try {
				
                String pt = ProductConfigureDialog.getProductTypes()
                        .get(prodName).getType();
				String pt1 = pt.replaceAll(" ", "_");
				
                LocalizationFile lFile = PgenStaticDataProvider.getProvider()
                        .getStaticLocalizationFile(
                                ProductConfigureDialog.getSettingFullPath(pt1));
				
				String filePath =  lFile.getFile().getAbsolutePath();
				if ( !new File( filePath).canRead()){
					loadSettingsTable();
                } else {
					loadPgenSettings( filePath);
				}
            } catch (Exception e) {
				loadSettingsTable();
			}
		}
	}
	
	public boolean loadPgenSettings( String fileName ){
		
		boolean ret = false;

		HashMap<String, AbstractDrawableComponent> newSettings = new HashMap<String, AbstractDrawableComponent>();

		File sFile = new File( fileName );
		
		try {
		if ( sFile.canRead() ){
			Products products = FileTools.read( fileName );
	        
	        List<gov.noaa.nws.ncep.ui.pgen.elements.Product> prds;
	        
	        prds = ProductConverter.convert( products );
	               
	        for ( gov.noaa.nws.ncep.ui.pgen.elements.Product p:prds ) {
	        	
                    for (gov.noaa.nws.ncep.ui.pgen.elements.Layer layer : p
                            .getLayers()) {
	        		
                        for (gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent de : layer
                                .getDrawables()) {
	        		    
	                    String pgenID = null;
	                    pgenID = de.getPgenType();

	        			if ( pgenID != null ) {
	       				    newSettings.put( pgenID, de );        			
	        			}
	        			
	        			if ( pgenID.equalsIgnoreCase("General Text")){
	        				((Text)de).setText(new String[]{""});
                            } else if (pgenID.equalsIgnoreCase("STORM_TRACK")) {
	        				//set Track time to current time
                                Calendar cal1 = Calendar.getInstance(TimeZone
                                        .getTimeZone("GMT"));
	        				Calendar cal2 = (Calendar)cal1.clone();
	        				cal2.add(Calendar.HOUR_OF_DAY, 1);
	        				
	        				((Track)de).setFirstTimeCalendar( cal1);
	        				((Track)de).setSecondTimeCalendar( cal2);

	        			}
	        		}      	          		
	        	}
	        }
	        
	        if ( newSettings.size() > 0 ) {
	        	settings.clear();
	        	settings.putAll(newSettings);
	        	ret = true;
	        }
	        
		}
        } catch (Exception e) {
			ret = false;
		}
		
		return ret;
	}
	   
}
