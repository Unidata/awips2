/*
 * gov.noaa.nws.ncep.common.staticData.IStaticDataProvider
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.common.staticdata;

import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.MultiPolygon;

/**
 * Interface that contains all methods to load NCEP static data.
 * This interface is also used to look up the data provider service in client side, such as PGEN. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?		B. Yin   	Initial Creation.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public interface IStaticDataProvider {
	public StationTable getSfStnTbl();
	public StationTable getAnchorTbl();
	public StationTable getVorTbl();
	public StationTable getVolcanoTbl();
    public HashMap<String,String> getClstTbl();
	public List<SPCCounty> getSPCCounties();
	public SPCCounty findCounty( String fips);
	public List<SPCCounty> getCountiesInGeometry(Geometry geo );
	
	public List<USState> getAllstates();
	public HashMap<String, String> getStateAbrvMap();
	
	//localization
	public String getPgenLocalizationRoot();
	public String getFileAbsolutePath(String fileLoczlizationPath);
	public File getFile(String fileLoczlizationPath);
	public LocalizationFile getStaticLocalizationFile( String fileName );
	public LocalizationFile getLocalizationFile( LocalizationContext context, String fileName );

	public LocalizationContext getLocalizationContext( LocalizationType type, LocalizationLevel level);
	
	public File getStaticFile( String fname );
	public File getGeogFile();
	public File getSfcStnFile();
	public File getFirBoundsFile();

	//This is for sigmet
	public List<Object[]> queryNcepDB(String field, String table);
	
	//for TCA
	public HashMap<String, String> getZoneMap();

	//for GFA
	public List<FAArea> getFAAreas();
	public List<FAArea> getFAAreaX();
	public List<FARegion> getFARegions();
	public List<GreatLake> getGreatLakes();
	public List<CostalWater> getCostalWaters();

	//for g2g
	public ArrayList<MultiPolygon> getG2GBounds(String tableAlias, String columnName, String columnValue);

}
