package gov.noaa.nws.ncep.viz.common.staticPointDataSource;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

public interface IStaticPointDataSource {
	
	public static enum StaticPointDataSourceType {
		NCEP_DB_TABLE,
		MAPS_DB_TABLE,  // maps.mapdata  // not implemented
		LPI_FILE,
		SPI_FILE,
		STATIONS_DB_TABLE, // metadata's common_obs_spatial db table
		NCEP_STATIONS_TBL_FILES // xml files converted from NMAP // not implemented
	}
	
	public abstract StaticPointDataSourceType getSourceType();
	
	public abstract void loadData( ) throws VizException;
	
	public abstract void insertPoint( LabeledPoint lp );

	public abstract List<LabeledPoint> getPointData();

	public abstract Map<String,LabeledPoint> getPointDataByLabel();
	
	//public abstract LabeledPoint getPointDataByLabel( String lbl );
	
	public abstract PointDirDist calculateNearestPoint2(Coordinate loc )  throws VizException; 
}
