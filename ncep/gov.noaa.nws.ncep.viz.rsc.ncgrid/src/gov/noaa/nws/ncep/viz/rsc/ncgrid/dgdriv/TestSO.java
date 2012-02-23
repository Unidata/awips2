package gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv;
/**
 * 
 */

import com.raytheon.uf.viz.core.rsc.DisplayType;

import gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv.DgdrivException;
import gov.noaa.nws.ncep.edex.common.dataRecords.NcFloatDataRecord;

public class TestSO {
	
	private static final int NUM_TIMES = 50;
	
	public static void main( String args[] ) {
		
		for ( int n=0; n<NUM_TIMES; n++ ) {
			float rmin=Float.MAX_VALUE, rmax=Float.MIN_VALUE;
			NcFloatDataRecord rec = getMeAGrid();
			rec = getMeAnotherGrid();
			rec=getMeAThirdGrid();
			float[] grid = rec.getXdata();
			for ( int j=0; j<grid.length; j++ ) {
				rmin = Math.min(rmin, grid[j] );
				rmax = Math.max(rmax, grid[j] );
			}
			System.out.println( "GRID "+grid.length+"   rmin = "+rmin+"   rmax = "+rmax );
		}

	}
	
	private static NcFloatDataRecord getMeAGrid() {
		
		NcFloatDataRecord rec = null;
		
		TestDgdriv aDgdriv = new TestDgdriv();
		//aDgdriv.setCycleForecastTimes(dataTimesForDgdriv);
		//aDgdriv.setSpatialObject(cov);
    	aDgdriv.setGdattim( "2011-10-03 12:00:00.0 (3)" );
    	aDgdriv.setGarea("dset");
    	aDgdriv.setGdfile( "NAM104" );
    	aDgdriv.setGdpfun("knts((mag(wnd)))");
    	aDgdriv.setGlevel("200");
    	aDgdriv.setGvcord("pres");
    	aDgdriv.setScale("0                             ");
    	aDgdriv.setDataSource( "ncgrib" );

    	DisplayType displayType = DisplayType.CONTOUR;
    	if (displayType == DisplayType.ARROW || displayType == DisplayType.BARB || displayType == DisplayType.STREAMLINE) {
    		/*
        	 *  Specify vector data retrieval from GEMPAK GD
        	 */
        	aDgdriv.setScalar(false);
    	}
    	else {
        	/*
        	 *  Specify scalar data retrieval from GEMPAK GD
        	 */
        	aDgdriv.setScalar(true);
    	}
    	
    	try {
    		rec = aDgdriv.execute();
		} catch (DgdrivException e) {
			System.out.println("GEMPAK GD error stack:\n" + e.getMessage());
		}
		
		return rec;
	}

	private static NcFloatDataRecord getMeAnotherGrid() {
		
		NcFloatDataRecord rec = null;
		
		TestDgdriv aDgdriv = new TestDgdriv();
		//aDgdriv.setCycleForecastTimes(dataTimesForDgdriv);
		//aDgdriv.setSpatialObject(cov);
    	aDgdriv.setGdattim( "2011-10-03 12:00:00.0 (3)" );
    	aDgdriv.setGarea("dset");
    	aDgdriv.setGdfile( "NAM104" );
    	aDgdriv.setGdpfun("sm5s(hght)");
    	aDgdriv.setGlevel("200");
    	aDgdriv.setGvcord("pres");
    	aDgdriv.setScale("0                             ");
    	aDgdriv.setDataSource( "ncgrib" );

    	DisplayType displayType = DisplayType.CONTOUR;
    	if (displayType == DisplayType.ARROW || displayType == DisplayType.BARB || displayType == DisplayType.STREAMLINE) {
    		/*
        	 *  Specify vector data retrieval from GEMPAK GD
        	 */
        	aDgdriv.setScalar(false);
    	}
    	else {
        	/*
        	 *  Specify scalar data retrieval from GEMPAK GD
        	 */
        	aDgdriv.setScalar(true);
    	}
    	
    	try {
    		rec = aDgdriv.execute();
		} catch (DgdrivException e) {
			System.out.println("GEMPAK GD error stack:\n" + e.getMessage());
		}
		
		return rec;
	}

	private static NcFloatDataRecord getMeAThirdGrid() {
		
		NcFloatDataRecord rec = null;
		
		TestDgdriv aDgdriv = new TestDgdriv();
		//aDgdriv.setCycleForecastTimes(dataTimesForDgdriv);
		//aDgdriv.setSpatialObject(cov);
    	aDgdriv.setGdattim( "2011-10-03 12:00:00.0 (3)" );
    	aDgdriv.setGarea("dset");
    	aDgdriv.setGdfile( "NAM104" );
    	aDgdriv.setGdpfun("kntv(wnd)");
    	aDgdriv.setGlevel("200");
    	aDgdriv.setGvcord("pres");
    	aDgdriv.setScale("0                             ");
    	aDgdriv.setDataSource( "ncgrib" );

    	DisplayType displayType = DisplayType.BARB;
    	if (displayType == DisplayType.ARROW || displayType == DisplayType.BARB || displayType == DisplayType.STREAMLINE) {
    		/*
        	 *  Specify vector data retrieval from GEMPAK GD
        	 */
        	aDgdriv.setScalar(false);
    	}
    	else {
        	/*
        	 *  Specify scalar data retrieval from GEMPAK GD
        	 */
        	aDgdriv.setScalar(true);
    	}
    	
    	try {
    		rec = aDgdriv.execute();
		} catch (DgdrivException e) {
			System.out.println("GEMPAK GD error stack:\n" + e.getMessage());
		}
		
		return rec;
	}

}

