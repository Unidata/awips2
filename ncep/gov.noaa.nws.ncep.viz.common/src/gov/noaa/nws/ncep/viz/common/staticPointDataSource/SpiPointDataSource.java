package gov.noaa.nws.ncep.viz.common.staticPointDataSource;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import com.raytheon.viz.pointdata.StaticPlotInfoPV;
import com.raytheon.viz.pointdata.StaticPlotInfoPV.SPIEntry;
import com.vividsolutions.jts.geom.Coordinate;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

public class SpiPointDataSource extends AbstractPointDataSource {

	private String spiFilename;
	
	private String labelField="colum 4"; //ignore / not used  
	
	private List<LabeledPoint> spiPoints = new ArrayList<LabeledPoint>();
	
	public SpiPointDataSource( String fname, String lblFld ) {
		spiFilename = fname;
		labelField = lblFld;
	}
	
	@Override
	public StaticPointDataSourceType getSourceType() {
		return StaticPointDataSourceType.SPI_FILE;
	}
	
	@Override
	public void loadData() throws VizException {
		File file = new File( spiFilename );
		if( !file.isAbsolute() ) {			    
			file = NcPathManager.getInstance().getStaticFile( 
					NcPathConstants.BASEMAPS_DIR+File.separator+ spiFilename );
		}
		HashMap<String, SPIEntry> spiEntries = StaticPlotInfoPV.readStaticPlotInfoPV(
				file.getAbsolutePath(), true).getSpiList();

		for( String icao : spiEntries.keySet() ) {
			SPIEntry spi = spiEntries.get( icao ); 
			LabeledPoint lp = new LabeledPoint( icao, spi.latlon.y, spi.latlon.x );
			
			spiPoints.add( lp );
			
			insertPoint( lp );
		}
	}

	@Override
	public List<LabeledPoint> getPointData() {
		return spiPoints;
	}

	// not implemented
	@Override
	public Map<String, LabeledPoint> getPointDataByLabel() {
		return null;
	}
}
