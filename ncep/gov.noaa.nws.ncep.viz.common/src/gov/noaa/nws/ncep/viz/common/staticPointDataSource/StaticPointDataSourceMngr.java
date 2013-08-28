package gov.noaa.nws.ncep.viz.common.staticPointDataSource;

import java.util.List;

import gov.noaa.nws.ncep.viz.common.staticPointDataSource.IStaticPointDataSource.StaticPointDataSourceType;

// a crude factory to create PointDataSources. 
// 
// TODO : beef this up : 
//    - have different factory for each type and make more extensible.
//    - only create one instance of each data source. Define them in localization
//      with files similar to LocatorDataSources now. 
//    - change/remove locatorDataSources and just refer to the pointdatasources.
//    - if memory is an issue (since it wouldn't be released.) create a cache to 
//      that can release the data source if not referenced recently.
//    - add reference counts for each created source object and release when ref count goes
//      to zero and is not used for x amount of time.
//
public class StaticPointDataSourceMngr {
	
	public static IStaticPointDataSource createPointDataSource( 
			StaticPointDataSourceType srcType, String srcName, List<String> initParams ) {
	
		switch( srcType ) {

		case LPI_FILE :
		// 1 parameter ; the lpi filename and the label field
			if( initParams.size() != 1 ) {
				System.out.println("wrong number of parameters for LPIFILE PointDataSourceMngr." );				
			}
			return new LpiPointDataSource( srcName, initParams.get(0) );
		
		// 1 parameter ; the spi filename and the label field
		case SPI_FILE :
			if( initParams.size() != 1 ) {
				System.out.println("wrong number of parameters for SPIFILE PointDataSourceMngr." );				
			}			
			return new SpiPointDataSource( srcName, initParams.get(0) );
			
		// 2 parameters; the catalogType and the db field for the label value
		case STATIONS_DB_TABLE :
			if( initParams.size() != 1 ) {
				System.out.println("wrong number of parameters for Nc Db tbl PointDataSourceMngr." );				
			}
			return new StationsDbTablePointDataSource( srcName, initParams.get(0) );
			
		// 3 parameters; the db name, table and field.
		case NCEP_DB_TABLE :			
			if( initParams.size() != 1 ) {
				System.out.println("wrong number of parameters for Nc Db tbl PointDataSourceMngr." );				
			}
			return new DbTablePointDataSource(srcName, initParams.get(0) );
		case NCEP_STATIONS_TBL_FILES :
			if( initParams.size() != 1 ) {
				System.out.println("wrong number of parameters for NCEP_STATIONS_TBL_FILES PointDataSource." );				
			}
			return new StationFilePointDataSource(srcName, initParams.get(0) );
			
		case MAPS_DB_TABLE : 
			// not implemented
			System.out.println("MAPS_DB_TABLE PointDataSource not implemented." );
		default :
			System.out.println("Unsupported PointDataSourceTYpe in PointDataSourceMngr:"+srcType );
		}
		return null;
	}
}
