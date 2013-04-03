package gov.noaa.nws.ncep.viz.gempak.util;

import java.util.ArrayList;

import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.PolarStereoGridCoverage;
import gov.noaa.nws.ncep.viz.gempak.grid.jna.GridDiag;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.geotools.referencing.datum.DefaultGeodeticDatum;
import org.geotools.referencing.datum.DefaultPrimeMeridian;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.sun.jna.Native;
import com.sun.jna.ptr.IntByReference;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author gamaz
 *
 */
/**
 * @author gamaz
 *
 */
public class GempakGrid {
	
	private static GridDiag gd = GridDiag.getInstance();
	
	public final static String gempakPluginName = "gempak_gd"; // ghull added Dec 19, 2010
	
	/**
	 * Gets the cycle times for a GEMPAK dataLocation from uEngine in YYMMDD_HHMM format.
	 *
	 * @param aDataLocation
	 *        eg "$MODEL/gfs"
	 * @return
	 * @throws VizException
	 */
	public static String[] getGridCycleTimes( String aDataLocation,String gdFile ) throws VizException{
		byte[] cycles = new byte[1024];
		IntByReference iret = new IntByReference(0);
		gd.gem.in_bdta_(iret);
		gd.gem.gd_init_ (iret);
		gd.gem.gdc_gcyc_(gdFile, cycles, iret);
		if ( iret.getValue() == 0 ) {
			String cycleTime = Native.toString(cycles);
			String cycleTime1 = cycleTime.replaceAll("/", "_");
			String []cycleTimes = cycleTime1.trim().split(";");
			return cycleTimes;
		}
		else {
			throw new VizException();
		}
//		long st1 = System.currentTimeMillis();
//		StringBuilder query = new StringBuilder();
//		query.append("import NcModelCycleQuery\n");
//		query.append("req = NcModelCycleQuery.NcModelCycleQuery()\n");
//		query.append("req.setDataLocation('" + aDataLocation + "')\n");
//		query.append("req.setModelName('" + modelname + "')\n");
//		query.append("return req.execute()");
//
//		String script = query.toString();
//		try {
//			String cycleListStr = (String) Connector.getInstance().connect(
//					script, null, 600000)[0];
//
//			String[] cycleList = cycleListStr.split("\\|");
//			long st2 = System.currentTimeMillis();
//			System.out.println ("***get cycleTimes throught Connector took:" + (st2-st1));
//			return cycleList;
//		} catch (VizException e) {
//			throw new VizException(e);
//		}
	}
	
	/**
	 * Gets the available times for a GEMPAK dataLocation and a current cycle
	 * from uEngine in "YYYY-MM-DD HH:MM:SS.S (fHHH)" format.
	 * 
	 * @param aDataLocation
	 *        eg "$MODEL/gfs"
	 * @param aCurrentCycle
	 *        eg "2010-11-28 18:00:00.0"
	 * @return
	 * @throws VizException
	 */
	public static String[] getAvailableGridTimes( String aDataLocation, String aCurrentCycle, String gdFile ) 
	throws VizException{
		byte[] availables = new byte[10000];
		IntByReference iret = new IntByReference(0);
		gd.gem.in_bdta_(iret);
		gd.gem.gd_init_ (iret);
		String cycle="";
		if ( aCurrentCycle != null ){
			String []dtStr = aCurrentCycle.split(" ");
			cycle =dtStr[0].split("-")[1] +  dtStr[0].split("-")[2] +"/" +dtStr[1].split(":")[0];
		}
		gd.gem.gdc_gtmf_(gdFile, cycle, availables, iret);
		if ( iret.getValue() == 0 ) {
			String availableTimes = Native.toString(availables);
			String [] avaTimeStr = availableTimes.trim().split("\\|");
			String [] avaTimesList = new String [avaTimeStr.length];
			int i = 0;
			for ( String ava:avaTimeStr) {
				if ( ava.contains("F")) {
					avaTimesList[i] = aCurrentCycle +" (" + Integer.parseInt(ava.split("F")[1]) + ")";
				}
				else {
					avaTimesList[i] = aCurrentCycle +" (0)";
				}
				i ++;
			}
			return avaTimesList;
		}
		else {
			throw new VizException();
		}
//		long st1 = System.currentTimeMillis();
//        StringBuilder query = new StringBuilder();
//		query.append("import NcModelAvailableTimesQuery\n");
//		query.append("req = NcModelAvailableTimesQuery.NcModelAvailableTimesQuery()\n");
//		query.append("req.setDataLocation('" + aDataLocation + "')\n");
//		query.append("req.setModelName('" + modelName + "')\n");
//		query.append("req.setCurrentCycle('" + aCurrentCycle + "')\n");
//		query.append("return req.execute()");
//
//		String script = query.toString();
//		try {
//			String avTimesListStr = (String) Connector.getInstance().connect(
//					script, null, 600000)[0];
//			String[] avTimesList = avTimesListStr.split("\\|");
//			long st2 = System.currentTimeMillis();
//			System.out.println ("===get availableTimes throught connector took:" + (st2-st1));
//			return avTimesList;
//		} catch (VizException e) {
//			throw new VizException(e);
//		}
	}
	
	/**
	 * Gets the navigation information from a GEMPAK grid file and returns the coverage object.
	 * 
	 * @param aDataLocation
	 *        eg "$MODEL/gfs"
	 * @param aCurrentCycle
	 *        eg "2010-11-28 18:00:00.0"
	 * @return
	 * @throws VizException
	 */
	public static ISpatialObject getGridNavigation (String anAlias, String aDataLocation, String aCurrentCycle) 
	throws VizException{
		int numberOfLayers = aDataLocation.split("/").length;
		String model = aDataLocation.split("/")[numberOfLayers-1];
		String gdfile = constructGridFilename (anAlias, aDataLocation,aCurrentCycle);
		float[] rnav = getGridNavigationBlock (gdfile);
		String proj = getGridProjection (rnav);

		DefaultGeographicCRS WGS84 = DefaultGeographicCRS.WGS84;
		CoordinateReferenceSystem crs;
		String crsWKT;
		Polygon geometry;
		
		if (proj.equalsIgnoreCase("CED")) {
			double minLat = MapUtil.correctLat(rnav[6]);
	        double maxLat = MapUtil.correctLat(rnav[8]);
	        double minLon = MapUtil.correctLon(rnav[7]);
	        double maxLon = MapUtil.correctLon(rnav[9]);
	        if (maxLon < minLon) {
	            maxLon += 360.0;
	        }
	        if (maxLon > 180) {
	        	 crs = new DefaultGeographicCRS(new DefaultGeodeticDatum("WGS84",
	                    WGS84.getDatum().getEllipsoid(), new DefaultPrimeMeridian(
	                            "DateLine", 180.0)), WGS84.getCoordinateSystem());
	        } else {
	            crs = WGS84;
	        }
	        crsWKT = crs.toWKT();
	        try {
	            geometry = MapUtil.createGeometry(minLat, minLon, maxLat, maxLon);
	        } catch (Exception e) {
	            throw new VizException("Error creating geometry", e);
	        }
	        CharSequence spacingUnit = "degree";
	        
	        LatLonGridCoverage cov = new LatLonGridCoverage();
	        cov.setSpacingUnit(spacingUnit.toString());
	        cov.setDx(rnav[2]);
	        cov.setDy(rnav[3]);
	        int nx = (int) rnav[4];
	        cov.setNx(Integer.valueOf(nx));
	        int ny = (int) rnav[5];
	        cov.setNy(Integer.valueOf(ny));
	        cov.setCrs(crs);
	        cov.setCrsWKT(crsWKT);
	        cov.setGeometry(geometry);
	        cov.setLa1(minLat);
	        cov.setLa2(maxLat);
	        cov.setLo1(minLon);
	        cov.setLo2(maxLon);
	        cov.setName("GEMPAK " + model + rnav[2] + "x" + rnav[3]+  " " +  spacingUnit + " grid");
	        cov.setGridGeometry(MapUtil.getGridGeometry(cov));
	        cov.setDescription("GEMPAK CED grid");
			return cov;
		}
		else if (proj.equalsIgnoreCase("STR")) {
			double majorAxis = 6371229.0;
			double minorAxis = 6371229.0;
			double lov = rnav[11];
			double la1 = rnav[6];
			double lo1 = rnav[7];
			int nx = (int) rnav[4];
			int ny = (int) rnav[5];
			/* TODO calculate dx, dx */
			double dx = 90.755;
			double dy = 90.755;
			crs = MapUtil.constructNorthPolarStereo(majorAxis, minorAxis, 60.0, lov);
			crsWKT = crs.toWKT();
			CharSequence spacingUnit = "km";
		    try {
		        Unit<?> spacingUnitObj = Unit.valueOf(spacingUnit);
		        if (spacingUnitObj.isCompatible(SI.METRE)) {
		            UnitConverter converter = spacingUnitObj
		                    .getConverterTo(SI.METRE);
		            geometry = MapUtil.createGeometry(crs, la1, lo1, converter
		                    .convert(dx), converter.convert(dy), nx, ny);
		        } else {
		            throw new VizException("Unable to convert " + spacingUnit
	                        + " to meters while creating geometry!");
		        }
		    } catch (Exception e) {
		        throw new VizException("Error creating geometry", e);
		    }
		    PolarStereoGridCoverage cov = new PolarStereoGridCoverage();
	        cov.setCrs(crs);
	        cov.setCrsWKT(crsWKT);
	        cov.setGeometry(geometry);
	        cov.setLa1(la1);
	        cov.setLo1(lo1);
	        cov.setLov(lov);
	        cov.setNx(Integer.valueOf(nx));
	        cov.setNy(Integer.valueOf(ny));
	        cov.setGridGeometry(MapUtil.getGridGeometry(cov));
	        cov.setName("GEMPAK " + model + rnav[2] + "x" + rnav[3]+  " " +  spacingUnit + " grid");
	        cov.setDescription("GEMPAK STR grid");
			return cov;
		}
		else if (proj.equalsIgnoreCase("LCC")) {
			/* TODO add code for LCC proj */
			double majorAxis = 6371229.0;
			double minorAxis = 6371229.0;
			double lov = rnav[11];
			double latin1 = rnav[10];
			double latin2 = rnav[12];
			double la1 = rnav[6];
			double lo1 = rnav[7];
			int nx = (int) rnav[4];
			int ny = (int) rnav[5];
			CharSequence spacingUnit = "km";
			double dx = 12.191;
			double dy = 12.191;
			crs = MapUtil.constructLambertConformal(majorAxis, minorAxis, latin1,
	                latin2, lov);
	        crsWKT = crs.toWKT();
	        try {
	            Unit<?> spacingUnitObj = Unit.valueOf(spacingUnit);
	            if (spacingUnitObj.isCompatible(SI.METRE)) {
	                UnitConverter converter = spacingUnitObj
	                        .getConverterTo(SI.METRE);
	                geometry = MapUtil.createGeometry(crs, la1, lo1, converter
	                        .convert(dx), converter.convert(dy), nx, ny);
	            } else {
	                throw new VizException("Unable to convert " + spacingUnit
	                        + " to meters while creating geometry!");
	            }

	        } catch (Exception e) {
	            throw new VizException("Error creating geometry", e);
	        }
	        LambertConformalGridCoverage cov = new LambertConformalGridCoverage();
	        cov.setCrs(crs);
	        cov.setCrsWKT(crsWKT);
	        cov.setGeometry(geometry);
	        cov.setLa1(la1);
	        cov.setLo1(lo1);
	        cov.setLov(lov);
	        cov.setNx(Integer.valueOf(nx));
	        cov.setNy(Integer.valueOf(ny));
	        cov.setGridGeometry(MapUtil.getGridGeometry(cov));
	        cov.setName("GEMPAK " + model + rnav[2] + "x" + rnav[3]+  " " +  spacingUnit + " grid");
	        cov.setDescription("GEMPAK LCC grid");
			return cov;
		}
		else if (proj.equalsIgnoreCase("MER")) {
			/* TODO add code for MER proj */
			return null;
		}
		else {
			return null;
		}
	}
	
	/**
	 * Constructs a GEMPAK grid filename eg "gfs_2010112818f003"
	 * 
	 * @param anAlias
	 *        eg "GFS"
	 * @param aLocation
	 *        eg "$MODEL/gfs"
	 * @param aTime
	 *        eg "2010-11-28 18:00:00.0 (3)"
	 * @return
	 */
	private static String constructGridFilename(String anAlias, String aLocation, String aTime) {
		try {
			String fileNameTemplate = getGempakGridTemplate(anAlias);
			IntByReference iret = new IntByReference(0);
			byte[] theFileName = new byte[50];
			String gTime = dbtimeToDattim(aTime);
			String fullPath = aLocation+ "/" + fileNameTemplate;
			gd.gem.cfl_mnam_ ( gTime, fullPath, theFileName, iret);
			if ( iret.getValue () != 0 ) {
				return null;
			}
			return Native.toString(theFileName);
		} catch (VizException e) {
			// TODO Auto-generated catch block
			return null;
		}
	}
	
	/**
	 * Extracts navigation block from GEMPAK grid
	 * 
	 * @param aGridFile
	 * @return
	 */
	private static float [] getGridNavigationBlock(String aGridFile) {
		float[] rnav = new float[20];
		float[] anl  = new float[20];
		IntByReference wrtflg = new IntByReference(-1);
		IntByReference mxanl = new IntByReference(1);
		IntByReference mxnav = new IntByReference(20);
		IntByReference iacss = new IntByReference(0);
		IntByReference msxgrd = new IntByReference(0);
		IntByReference iret = new IntByReference(0);
		IntByReference mode = new IntByReference(0);

		gd.gem.in_bdta_(iret);
		gd.gem.gd_init_ (iret);
		gd.gem.gg_init_ (mode,iret);
		gd.gem.dg_intl_ (iret);

		gd.gem.gdc_open_ (aGridFile, wrtflg, mxanl, mxnav, iacss, 
				anl, rnav, msxgrd, iret);
		
		return rnav;
		
	}
	
	/**
	 * Extracts grid projections from GEMPAK grid navigation block
	 * 
	 * @param rnav
	 * @return
	 */
	private static String getGridProjection (float[] rnav) {
		IntByReference iret = new IntByReference(0);
		byte[] cproj = new byte[5];
		IntByReference kx = new IntByReference(0);
		IntByReference ky = new IntByReference(0);
		gd.gem.grc_rnav_ (rnav, cproj, kx, ky, iret);
		return Native.toString(cproj);
	}
	
	/**
	 * Serves as a wrapper for the legacy ctb_dtpath function
	 * 
	 * @param anAlias
	 *        eg "GFS"
	 * @return
	 */
	public static String getGempakGridPath ( String anAlias) throws VizException {
		IntByReference iret = new IntByReference(0);
		byte[] thePath = new byte[30];
		gd.gem.ctb_dtpath_ (anAlias, thePath, iret);
		if ( iret.getValue () != 0 ) {
			throw new VizException("Alias " + anAlias + " not found in legacy datatype.tbl");
		}
		return Native.toString(thePath);
	}
	
	/**
	 * Serves as a wrapper for the legacy ctb_dttmpl function
	 * 
	 * @param anAlias
	 *        eg "GFS"
	 * @return
	 */
	public static String getGempakGridTemplate ( String anAlias) throws VizException {
		IntByReference iret = new IntByReference(0);
		byte[] theTemplate = new byte[50];
		gd.gem.ctb_dttmpl_ (anAlias, theTemplate, iret);
		if ( iret.getValue () != 0 ) {
			throw new VizException("Alias " + anAlias + " not found in legacy datatype.tbl");
		}
		return Native.toString(theTemplate);
	}
	
	/**
	 *
	 * Converts AWIPS2 date time string into GEMPAK DATTIM string
	 *
	 * @param aTime
	 *        eg "2011-10-09 06:20:00.0 (1)"
	 * @return
	 *        eq "111009/0620f001"
	 */
	private static String dbtimeToDattim(String aTime) {
		String aDattim = null;
		String[] inputStringArray = new String[2];
		
		CharSequence char0 = "(";
		/*
		 * Process time contains forecast hour info
		 */
		if ( aTime.contains(char0) ) {
			String zeroes = null;
			int ind1 = aTime.indexOf("(");
			int ind2 = aTime.indexOf(")");
			if ( ind2-ind1 == 2 ) {
				zeroes = "00";
			}
			else if ( ind2-ind1 == 3 ) {
				zeroes = "0";
			}
			String str1 = aTime.substring(0, ind1-1);
			String str2 = "";
			if ( zeroes != null) {
				str2 = "f"+zeroes+aTime.substring(ind1+1, ind2);
			}
			else {
				str2 = "f"+aTime.substring(ind1+1, ind2);
			}
			
			if ( aTime.contains("_") ) {
				inputStringArray = str1.split("_");
			}
			else if ( ! aTime.contains("_") ) {
				inputStringArray = str1.split(" ");
			}

			/*
			 * YYYY-MM-DD HH:MM:SS.S (HHH)-> YYMMDD/HHMMfHHH
			 * 2009-10-22 16:00:00.0 (5)-> 091022/1600f005
			 * 0123456789 0123456789
			 */
			aDattim = inputStringArray[0].substring(2, 4)
					+ inputStringArray[0].substring(5, 7)
					+ inputStringArray[0].substring(8, 10) + "/"
					+ inputStringArray[1].substring(0, 2)
					+ inputStringArray[1].substring(3, 5) + str2;
		}
		/*
		 * Process time that does NOT contain forecast hour info
		 */
		else {
			inputStringArray = aTime.split(" ");

			/*
			 * YYYY-MM-DD HH:MM:SS.S -> YYMMDD/HHMM
			 * 2009-01-20 02:25:00.0 -> 090120/0225
			 * 0123456789 0123456789
			 */
			aDattim = inputStringArray[0].substring(2, 4)
					+ inputStringArray[0].substring(5, 7)
					+ inputStringArray[0].substring(8, 10) + "/"
					+ inputStringArray[1].substring(0, 2)
					+ inputStringArray[1].substring(3, 5);
		}
		return aDattim;
	}
	
	/*
	 * Converts GEMPAK DATTIM string into AWIPS2 date time string
	 */
	public static String dattimToDbtime(String aDattim) {
		aDattim = aDattim.toUpperCase();
		String retDateTime = null;
		String[] inputStringArray = new String[2];
		CharSequence char0 = "F";
		if ( aDattim.contains(char0) ) {
			
			int ind1 = aDattim.indexOf("F00");
			int addChars = 3;
			if ( ind1 == -1 ) {
				ind1 = aDattim.indexOf("F0");
				addChars = 2;
			}
			if ( ind1 == -1 ) {
				ind1 = aDattim.indexOf("F");
				addChars = 1;
			}
			int ind2 = aDattim.length();
			
			String str1 = aDattim.substring(0, ind1);
			String str2 = aDattim.substring(ind1+addChars,ind2 );
			inputStringArray = str1.split("/");

			/*
			 * YYMMDD/HHMMfHHH -> YYYY-MM-DD HH:MM:SS.S 
			 * 090120/0225f005 -> 2009-01-20 02:25:00.0 
			 * 012345 0123
			 */
			retDateTime = "20" + inputStringArray[0].substring(0, 2) + "-"
			+ inputStringArray[0].substring(2, 4) + "-"
			+ inputStringArray[0].substring(4, 6) + "_"
			+ inputStringArray[1].substring(0, 2) + ":"
			+ inputStringArray[1].substring(2, 4) + ":00.0_("+ str2 + ")";
		}
		/*
		 * Process time that does NOT contain forecast hour info
		 */
		else {
			inputStringArray = aDattim.split("/");

			/*
			 * YYMMDD/HHMM -> YYYY-MM-DD HH:MM:SS.S 
			 * 090120/0225 -> 2009-01-2002:25:00.0 
			 * s012345 0123
			 */
			retDateTime = "20" + inputStringArray[0].substring(0, 2) + "-"
					+ inputStringArray[0].substring(2, 4) + "-"
					+ inputStringArray[0].substring(4, 6) + " "
					+ inputStringArray[1].substring(0, 2) + ":"
					+ inputStringArray[1].substring(2, 4) + ":00.0";
		}

		return retDateTime;
	}

}
