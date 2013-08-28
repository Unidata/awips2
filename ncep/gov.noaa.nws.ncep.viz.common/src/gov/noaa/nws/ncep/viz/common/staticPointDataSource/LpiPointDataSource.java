package gov.noaa.nws.ncep.viz.common.staticPointDataSource;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

public class LpiPointDataSource extends AbstractPointDataSource {

	private String lpiFilename;
	
	private String labelField="colum 4"; //ignore / not used  
	
	private List<LabeledPoint> lpiPoints = new ArrayList<LabeledPoint>();
	
	public LpiPointDataSource( String fname, String lblFld ) {
		lpiFilename = fname;
		labelField = lblFld;
	}
	
	@Override
	public StaticPointDataSourceType getSourceType() {
		return StaticPointDataSourceType.LPI_FILE;
	}
	
	@Override
	public void loadData() throws VizException {
		try {
			File file = new File( lpiFilename );
			if( !file.isAbsolute() ) {			    
				file = NcPathManager.getInstance().getStaticFile( 
						NcPathConstants.BASEMAPS_DIR+File.separator+ lpiFilename );
			}
			
			BufferedReader in = new BufferedReader(new FileReader(file));

			String s = in.readLine();
			while (s != null) {
				LabeledPoint lp = readPoint(s);
				if( lp != null ) {
					lpiPoints.add( lp );
					insertPoint( lp );
				}
				s = in.readLine();
			}
			in.close();

		} 
		catch( FileNotFoundException e ) {
			throw new VizException("Can't find file for :"+lpiFilename );
		} 
		catch( IOException e ) {
			throw new VizException("I/O error on file :"+lpiFilename );
		}

	}

	public LabeledPoint readPoint(String s) throws IOException {

		int maxLen = 0;
		Scanner in = new Scanner(s);

		if (!in.hasNextDouble() ) {
			return null;
		}
		double lat = in.nextDouble();
		
		if( !in.hasNextDouble() ) {
			return null;
		}
		double lon = in.nextDouble();

		if( !in.hasNextDouble() ) {
			return null;
		}
		
		double dmy = in.nextDouble();
	// if we need dist then add it as another label
	//	p.dist = in.nextDouble();
		
		if (!in.hasNext())
			return null;
		
		String lbl = in.findInLine("[^\\|]*").trim();

		if( lbl.length() > maxLen ) {
			maxLen = lbl.length();
		}

		return new LabeledPoint( lbl, lat, lon );		
	}


	@Override
	public List<LabeledPoint> getPointData() {
		return lpiPoints;
	}

	// not implemented
	@Override
	public Map<String, LabeledPoint> getPointDataByLabel() {
		return null;
	}
}
