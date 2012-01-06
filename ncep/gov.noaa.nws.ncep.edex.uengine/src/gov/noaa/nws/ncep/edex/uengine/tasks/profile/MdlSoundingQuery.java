package gov.noaa.nws.ncep.edex.uengine.tasks.profile;

/**
 * 
 * gov.noaa.nws.ncep.edex.uengine.tasks.profile.MdlSoundingQuery
 * 
 * This java class performs the Grid model sounding data query functions.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 04/04/2011	301			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingModel;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTimeLines;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;
import java.util.ArrayList;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import com.raytheon.edex.plugin.modelsounding.common.SoundingSite;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.edex.uengine.tasks.query.TableQuery;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;

import java.awt.Point;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralDirectPosition;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;

//import org.opengis.geometry.Envelope;

import org.opengis.geometry.DirectPosition;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;


import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.impl.CoordinateArraySequence;

public class MdlSoundingQuery {
	private static final String NCGRIB_TBL_NAME = "ncgrib";
	private static final String D2DGRIB_TBL_NAME = "grib";
	
	private static String NC_PARMS = "HGHT, UREL, VREL, TMPK, DWPK, SPFH, OMEG, RELH";
	private static String D2D_PARMS = "GH, uW, vW,T, DWPK, SPFH,OMEG, RH";
	private enum NcParmNames {
		HGHT, UREL, VREL, TMPK, DWPK, SPFH, OMEG, RELH
	};


	private enum D2DParmNames {
		GH, uW, vW, T, DWPK, SPFH, OMEG, RH
	};

	public static UnitConverter kelvinToCelsius = SI.KELVIN
			.getConverterTo(SI.CELSIUS);

	public static NcSoundingTimeLines getMdlSndTimeLine(String mdlType,
			String currentDBTblName) {
		NcSoundingTimeLines tl = new NcSoundingTimeLines();

		if(currentDBTblName.equals(NCGRIB_TBL_NAME)){
			Object[] refTimeAry = null;
			String queryStr = new String("Select Distinct reftime FROM "
					+ currentDBTblName + " where modelname='" + mdlType
					+ "' ORDER BY reftime DESC");

			CoreDao dao = new CoreDao(DaoConfig.forClass(NcgribRecord.class));
			refTimeAry = (Object[]) dao.executeSQLQuery(queryStr);
			tl.setTimeLines(refTimeAry);
		}else if(currentDBTblName.equals(D2DGRIB_TBL_NAME)){
			TableQuery query;
			try {
				query = new TableQuery("metadata", GribRecord.class.getName());
				query.setDistinctField("dataTime.refTime");
				query.addParameter("modelInfo.modelName", mdlType);
				query.setSortBy("dataTime.refTime", false);
				@SuppressWarnings("unchecked")
				List<GribRecord> recList = (List<GribRecord>) query.execute();
				tl.setTimeLines(recList.toArray());
			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		}
		
		

		return tl;
	}

	public static NcSoundingTimeLines getMdlSndRangeTimeLine(String mdlType,
			String refTimeStr, String currentDBTblName) {
		NcSoundingTimeLines tl = new NcSoundingTimeLines();
		if(currentDBTblName.equals(NCGRIB_TBL_NAME)){
			Object[] refTimeAry = null;
			String queryStr = new String("Select Distinct rangestart FROM "
				+ currentDBTblName + " where modelname='" + mdlType + "' AND "
				+ "reftime='" + refTimeStr + ":00:00'"
				+ " ORDER BY rangestart DESC");
			System.out.println("queryStr  " + queryStr);

			CoreDao dao = new CoreDao(DaoConfig.forClass(SoundingSite.class));
			refTimeAry = (Object[]) dao.executeSQLQuery(queryStr);
			tl.setTimeLines(refTimeAry);
		}
		else if(currentDBTblName.equals(D2DGRIB_TBL_NAME)){
			TableQuery query;
			try {
				query = new TableQuery("metadata", GribRecord.class.getName());
				query.setDistinctField("dataTime.validPeriod.start");
				query.addParameter("modelInfo.modelName", mdlType);
				query.addParameter("dataTime.refTime", refTimeStr + ":00:00");
				query.setSortBy("dataTime.validPeriod.start", false);
				@SuppressWarnings("unchecked")
				List<GribRecord> recList = (List<GribRecord>) query.execute();
				tl.setTimeLines(recList.toArray());
			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		}
		return tl;
	}	// public static NcSoundingProfile getMdlSndData(double lat, double lon,
	// String stn, long refTimeL, long validTimeL, String sndTypeStr,
	// SndQueryKeyType queryType, String mdlName) {
	// //*System.out.println("getPfcSndData input ref time = "+
	// refTimeL+" valid time is " + validTimeL);
	// Calendar refTimeCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
	// refTimeCal.setTimeInMillis(refTimeL);
	// Calendar validTimeCal =
	// Calendar.getInstance(TimeZone.getTimeZone("GMT"));
	// validTimeCal.setTimeInMillis(validTimeL);
	// return getMdlSndData( lat, lon, refTimeCal, validTimeCal, "ncgrib",
	// mdlName);
	// }

	/**
	 * Returns a profile for a specified location (lat,lon), time, and model for
	 * grib or ncgrib data.
	 * 
	 * @param lat
	 *            location latitude
	 * @param lon
	 *            location longitude
	 * @param refTimeCal
	 *            data record reference time
	 * @param validTimeCal
	 *            data record valid time
	 * @param pluginName
	 *            the name of the data table ('grib' or 'ncgrib')
	 * @param mdlName
	 *            the name of the model
	 * @return the profile
	 */
	public static NcSoundingProfile getMdlSndData(double lat, double lon,
			String refTime, String validTime, String pluginName, String mdlName) {
		long t01 = System.currentTimeMillis();
		NcSoundingProfile pf = new NcSoundingProfile();
		//NcSoundingCube cube = new NcSoundingCube();
		List<NcSoundingProfile>  soundingProfileList = new ArrayList<NcSoundingProfile>();
		List<?> levels = getModelLevels(refTime, validTime, pluginName, mdlName);
		if (levels.size() == 0) {
			System.out.println("getModelLevels return 0;  file=" + refTime+ " stime="+validTime + " gribtype="+ pluginName + " modeltype="+mdlName);
			pf.setRtnStatus(NcSoundingCube.QueryStatus.FAILED);
			return pf;
		}

		Point pnt = getLatLonIndices(lat, lon, refTime, validTime, levels.get(0).toString(),
				pluginName, mdlName);
		if (pnt == null) {
			System.out.println("getLatLonIndices return 0; lat=" + lat + " lon="+lon+" stime="+validTime + " gribtype="+ pluginName + " modeltype="+mdlName);
			
			pf.setRtnStatus(NcSoundingCube.QueryStatus.LOCATION_NOT_FOUND);
			return pf;

		}

		List<NcSoundingLayer> layerList = getModelSoundingLayerList(pnt,
				refTime, validTime, pluginName, mdlName, levels);
		if (layerList.size() == 0) {
			System.out.println("getModelSoundingLayerList return 0; lat=" + lat + " lon="+lon+" stime="+validTime + " gribtype="+ pluginName + " modeltype="+mdlName);
			
			
			pf.setRtnStatus(NcSoundingCube.QueryStatus.FAILED);
			return pf;
		}
		
		pf.setStationLatitude((float) lat);
		pf.setStationLongitude((float) lon);
		Float sfcPressure = getModelSfcPressure(pnt, refTime, validTime,
				pluginName, mdlName);
		if (sfcPressure == null) {
			pf.setSfcPress(-9999.f);
		}
		else {
			if (pluginName.equalsIgnoreCase(D2DGRIB_TBL_NAME)) 
				pf.setSfcPress(sfcPressure/100F);
			else
				pf.setSfcPress(sfcPressure);
		}
		System.out.println("surface pressure ="+pf.getSfcPress());
		//calculate dew point if necessary
		MergeSounding ms = new MergeSounding();
		ms.spfhToDewpoint(layerList);
		ms.rhToDewpoint(layerList);
		
			
		pf.setSoundingLyLst(layerList);
		
	
		soundingProfileList.add(pf);
		//cube.setSoundingProfileList(soundingProfileList);
		//cube.setRtnStatus(NcSoundingCube.QueryStatus.OK);
		long t02 = System.currentTimeMillis();
		System.out.println("MDL cube retreival took " + (t02 - t01));
		return pf;

	}

	public static NcSoundingModel getMdls(String perspectiveName) {
		// TO do by M. G.
		NcSoundingModel mdls = new NcSoundingModel();

		return mdls;
	}

	public static boolean isPointWithinGridGeometry(double lat, double lon,
			String refTime, String validTime, String pluginName,
			String modelName) {
		
		ISpatialObject spatialArea = null;
		MathTransform crsFromLatLon = null;
		if (pluginName.equalsIgnoreCase(NCGRIB_TBL_NAME)) {
			CoreDao dao = new CoreDao(DaoConfig.forClass(NcgribRecord.class));
			DatabaseQuery query = new DatabaseQuery(
					NcgribRecord.class.getName());
			query.setMaxResults(new Integer(1));
			query.addQueryParam("modelName", modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);
			String spacingUnit = null;

			try {
				List<NcgribRecord> recList = ((List<NcgribRecord>) dao
						.queryByCriteria(query));
				if (recList.size() == 0) {
					return false;
				} else {
					NcgribRecord rec = recList.get(0);
					spatialArea = rec.getSpatialObject();
				}
			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return false;
			}

		} else if (pluginName.equalsIgnoreCase(D2DGRIB_TBL_NAME)) {
			CoreDao dao = new CoreDao(DaoConfig.forClass(GribRecord.class));
			DatabaseQuery query = new DatabaseQuery(GribRecord.class.getName());

			query.setMaxResults(new Integer(1));
			query.addQueryParam("modelInfo.modelName", modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);

			try {
				List<GribRecord> recList = ((List<GribRecord>) dao
						.queryByCriteria(query));
				if (recList.size() == 0) {
					return false;
				} else {
					GribRecord rec = recList.get(0);
					spatialArea = rec.getSpatialObject();
				}

			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return false;
			}
		}

		try {
			crsFromLatLon = MapUtil
                    .getTransformFromLatLon(spatialArea.getCrs());
        } catch (FactoryException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        DirectPosition lowerCorner = MapUtil.getGridGeometry(spatialArea)
                .getEnvelope().getLowerCorner();
        DirectPosition upperCorner = MapUtil.getGridGeometry(spatialArea)
                .getEnvelope().getUpperCorner();

        GeometryFactory gf = new GeometryFactory();

        Coordinate p1 = new Coordinate(lowerCorner.getOrdinate(0), lowerCorner
                .getOrdinate(1));
        Coordinate p2 = new Coordinate(lowerCorner.getOrdinate(0), upperCorner
                .getOrdinate(1));
        Coordinate p3 = new Coordinate(upperCorner.getOrdinate(0), upperCorner
                .getOrdinate(1));
        Coordinate p4 = new Coordinate(upperCorner.getOrdinate(0), lowerCorner
                .getOrdinate(1));

        LinearRing lr = gf.createLinearRing(new Coordinate[] { p1, p2, p3, p4,
                p1 });

        Polygon gridGeometry = gf.createPolygon(lr, null);
        
        DirectPosition ll = new GeneralDirectPosition(MapUtil.LATLON_PROJECTION);
        
        Coordinate coord = new Coordinate(lon, lat);
        ll.setOrdinate(0, coord.x);
        ll.setOrdinate(1, coord.y);
//        DirectPosition crs = new GeneralDirectPosition(spatialArea.getCrs());
//        try {
//            crsFromLatLon.transform(ll, crs);
//        } catch (MismatchedDimensionException e) {
//            // TODO Auto-generated catch block
//            e.printStackTrace();
//        } catch (TransformException e) {
//            // TODO Auto-generated catch block
//            e.printStackTrace();
//        }
//        
//        Coordinate newC = new Coordinate(crs.getOrdinate(0), crs.getOrdinate(1));
        Coordinate newC = new Coordinate(ll.getOrdinate(0), ll.getOrdinate(1));

        com.vividsolutions.jts.geom.Point p = gf.createPoint(newC);

        return gridGeometry.contains(p);

	}
	
	public static boolean isPointWithinGridGeometry2(double lat, double lon,
			String refTime, String validTime, String pluginName,
			String modelName) {
		
		ISpatialObject spatialArea = null;
		MathTransform crsFromLatLon = null;
		if (pluginName.equalsIgnoreCase(NCGRIB_TBL_NAME)) {
			CoreDao dao = new CoreDao(DaoConfig.forClass(NcgribRecord.class));
			DatabaseQuery query = new DatabaseQuery(
					NcgribRecord.class.getName());
			query.setMaxResults(new Integer(1));
			query.addQueryParam("modelName", modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);
			String spacingUnit = null;

			try {
				List<NcgribRecord> recList = ((List<NcgribRecord>) dao
						.queryByCriteria(query));
				if (recList.size() == 0) {
					return false;
				} else {
					NcgribRecord rec = recList.get(0);
					spatialArea = rec.getSpatialObject();
				}
			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return false;
			}

		} else if (pluginName.equalsIgnoreCase(D2DGRIB_TBL_NAME)) {
			CoreDao dao = new CoreDao(DaoConfig.forClass(GribRecord.class));
			DatabaseQuery query = new DatabaseQuery(GribRecord.class.getName());

			query.setMaxResults(new Integer(1));
			query.addQueryParam("modelInfo.modelName", modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);

			try {
				List<GribRecord> recList = ((List<GribRecord>) dao
						.queryByCriteria(query));
				if (recList.size() == 0) {
					return false;
				} else {
					GribRecord rec = recList.get(0);
					spatialArea = rec.getSpatialObject();
				}

			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return false;
			}
		}
        
        Geometry g = spatialArea.getGeometry();
        
        GeometryFactory geometryFactory = new GeometryFactory();
        CoordinateSequence sequence = new CoordinateArraySequence(g.getCoordinates());
        
        
        Coordinate[] oldCoords = sequence.toCoordinateArray();
        Coordinate[] newCoords = new Coordinate[oldCoords.length];
        /*
         * adjust longitude for global grids whose lon span goes from 0 to 360
         * and the asked lon is negative.
         */
        for ( Coordinate c:oldCoords ){
        	double x = c.x;
        	double y = c.y;
        	double z = c.z;
        	if ( x >=180.0 && x <=360.0 && lon < 0.0) {
        		lon = lon + 360.0;
        		break;
        	}
        }
        Coordinate coord = new Coordinate(lon, lat);

		LinearRing ring = new LinearRing(sequence, geometryFactory);
		Polygon gridGeometry = new Polygon(ring, null, geometryFactory);
		com.vividsolutions.jts.geom.Point p = geometryFactory.createPoint(coord);
		
        return gridGeometry.contains(p);

	}

	/**
	 * Returns the value of surface pressure for a specified location, time, and
	 * model for grib or ncgrib data.
	 * 
	 * @param pnt
	 *            location
	 * @param pluginName
	 *            the name of the data table ('grib' or 'ncgrib')
	 * @param modelName
	 *            the name of the model
	 * @return surface pressure
	 */
	public static Float getModelSfcPressure(Point pnt, String refTime,
			String validTime, String pluginName, String modelName) {

		if (pluginName.equalsIgnoreCase(NCGRIB_TBL_NAME)) {
			CoreDao dao = new CoreDao(DaoConfig.forClass(NcgribRecord.class));
			DatabaseQuery query = new DatabaseQuery(
					NcgribRecord.class.getName());
			query.addQueryParam("glevel1", 0);
			query.addQueryParam("glevel2", -9999);
			query.addQueryParam("parm", "PRES");
			query.addQueryParam("vcord", "NONE");
			query.addQueryParam("modelName", modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);

			NcgribRecord rec = null;
			try {
				List<NcgribRecord> recList = ((List<NcgribRecord>) dao
						.queryByCriteria(query));
				if (recList.size() == 0) {
					return null;
				} else {
					rec = recList.get(0);
					PointIn pointIn = new PointIn(pluginName, rec, pnt.x, pnt.y);
					try {
						float fdata = pointIn.getPointData();
						return new Float(fdata);
					} catch (PluginException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
						return null;
					}
				}

			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return null;
			}

		} else if (pluginName.equalsIgnoreCase(D2DGRIB_TBL_NAME)) {
			CoreDao dao = new CoreDao(DaoConfig.forClass(GribRecord.class));
			DatabaseQuery query = new DatabaseQuery(GribRecord.class.getName());

			query.addQueryParam("modelInfo.level.levelonevalue", "0.0");
			query.addQueryParam("modelInfo.level.leveltwovalue", "-999999.0");
			query.addQueryParam("modelInfo.level.masterLevel.name", "MSL");
			query.addQueryParam("modelInfo.parameterAbbreviation", "PMSL");
			query.addQueryParam("modelInfo.modelName", modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);

			GribRecord rec = null;
			try {
				List<GribRecord> recList = ((List<GribRecord>) dao
						.queryByCriteria(query));
				if (recList.size() == 0) {
					return null;
				} else {
					rec = recList.get(0);
					PointIn pointIn = new PointIn(pluginName, rec, pnt.x, pnt.y);
					try {
						float fdata = pointIn.getPointData();
						return new Float(fdata);
					} catch (PluginException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
						return null;
					}
				}

			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return null;
			}
		}
		return null;

	}

	/**
	 * Returns a list of NcSoundingLayer for a specified location, time, and
	 * model for grib or ncgrib data.
	 * 
	 * @param pnt
	 *            location
	 * @param pluginName
	 *            the name of the data table ('grib' or 'ncgrib')
	 * @param modelName
	 *            the name of the model
	 * @param levels
	 *            list of vertical levels
	 * @return list of NcSoundingLayer objects
	 */

	private static List<NcSoundingLayer> getModelSoundingLayerList(Point pnt,
			String refTime, String validTime, String pluginName,
			String modelName, List<?> levels) {
		// CoreDao dao = new CoreDao(DaoConfig.forClass(NcgribRecord.class));

		List<NcSoundingLayer> soundLyList = new ArrayList<NcSoundingLayer>();

		long t01 = System.currentTimeMillis();
		StringBuilder parmList = new StringBuilder();
		boolean ht, ur, vr, t, d,sh, rh, om;
		if (pluginName.equalsIgnoreCase(NCGRIB_TBL_NAME)) {
			
			TableQuery query;
			try {
				for (Object level : levels) {
					NcSoundingLayer soundingLy = new NcSoundingLayer();
					ht = ur= vr= t= d=sh= rh= om= false;
					query = new TableQuery("metadata",
							NcgribRecord.class.getName());
					query.addParameter("vcord", "PRES");
					query.addParameter("modelName", modelName);
					query.addList("parm",NC_PARMS);//parmList.toString()); //
					query.addParameter("dataTime.refTime", refTime);
					query.addParameter("dataTime.validPeriod.start", validTime);
					query.addParameter("glevel1", level.toString());
					//query.setSortBy("glevel1", false);
					//System.out.println("level = "+ level.toString());

					List<NcgribRecord> recList = (List<NcgribRecord>) query
					.execute();					
					//System.out.println("level = "+ level.toString() + " has "+recList.size()+ " records" );
					if (recList.size() != 0) {
						for (NcgribRecord rec1 : recList) {
							soundingLy.setPressure((float) rec1
									.getGlevel1());
							String prm = rec1.getParm();
							//System.out.println("point.x="+ pnt.x + " .y="+pnt.y+"pressure="+rec1
							//		.getGlevel1());
							
							//System.out.println("point.x="+ pnt.x + " .y="+pnt.y+"prm="+prm);
							
							switch (NcParmNames.valueOf(prm)) {
							case HGHT:
								if(ht== false){
									PointIn pointIn = new PointIn(pluginName, rec1,
											pnt.x, pnt.y);
									float fdata = pointIn.getPointData();
									soundingLy.setGeoHeight(fdata);
									ht = true;
								}
								break;
							case UREL:
								// HDF5 data in
								// unit of
								// Knots, no
								// conversion
								// needed
								if(ur== false){
									PointIn pointIn = new PointIn(pluginName, rec1,
											pnt.x, pnt.y);
									float fdata = pointIn.getPointData();
									soundingLy.setWindU(fdata); 
									ur = true;
								}
								break;
							case VREL:
								// HDF5 data in
								// unit of
								// Knots, no
								// conversion
								// needed
								if(vr== false){
									PointIn pointIn = new PointIn(pluginName, rec1,
											pnt.x, pnt.y);
									float fdata = pointIn.getPointData();
									soundingLy.setWindV(fdata);  
									vr = true;
								}
								break;
							case TMPK:
								if(t== false){
									PointIn pointIn = new PointIn(pluginName, rec1,
											pnt.x, pnt.y);
									float fdata = pointIn.getPointData();
									soundingLy.setTemperature((float) kelvinToCelsius
											.convert(fdata)); 
									t = true;
								}
								break;
							case DWPK:								
								if(d== false){
									PointIn pointIn = new PointIn(pluginName, rec1,
											pnt.x, pnt.y);
									float fdata = pointIn.getPointData();
									soundingLy.setDewpoint((float) kelvinToCelsius
											.convert(fdata)); 
									d = true;
								}
								break;
							case SPFH:
								if(sh== false){
									PointIn pointIn = new PointIn(pluginName, rec1,
											pnt.x, pnt.y);
									float fdata = pointIn.getPointData();
									soundingLy.setSpecHumidity(fdata);
									sh = true;
								}
								break;
							case OMEG:
								if(om== false){
									PointIn pointIn = new PointIn(pluginName, rec1,
											pnt.x, pnt.y);
									float fdata = pointIn.getPointData();
									soundingLy.setOmega(fdata);
									om = true;
								}
								break;
							case RELH:
								if(rh== false){
									PointIn pointIn = new PointIn(pluginName, rec1,
											pnt.x, pnt.y);
									float fdata = pointIn.getPointData();
									soundingLy.setRelativeHumidity(fdata);
									rh = true;
								}
								break;
							}

						}
						soundLyList.add(soundingLy);
					}
				}

			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}
		else if (pluginName.equalsIgnoreCase(D2DGRIB_TBL_NAME)) {
			

			for (Object level : levels) {
				ht = ur= vr= t= d=sh= rh= om= false;
				NcSoundingLayer soundingLy = new NcSoundingLayer();
				try {
					//Chin should change for performance
					TableQuery query = new TableQuery("metadata",
							GribRecord.class.getName());
					query.addParameter("modelInfo.level.levelonevalue",
							level.toString());
					query.addParameter("modelInfo.level.leveltwovalue",
							"-999999.0");
					query.addParameter("modelInfo.level.masterLevel.name", "MB");
					query.addParameter("modelInfo.modelName", modelName);
					query.addList("modelInfo.parameterAbbreviation",
							D2D_PARMS);
					query.addParameter("dataTime.refTime", refTime);
					query.addParameter("dataTime.validPeriod.start", validTime);
					query.setSortBy("modelInfo.level.levelonevalue", false);
					//System.out.println("level = "+ level.toString());
					try {
						List<GribRecord> recList = (List<GribRecord>) query
								.execute();
						if (recList.size() != 0) {
							for (GribRecord rec1 : recList) {
								soundingLy.setPressure((float) rec1
										.getModelInfo().getLevel()
										.getLevelonevalue());
								String prm = rec1.getModelInfo()
										.getParameterAbbreviation();

								
								//System.out.println("point.x="+ pnt.x + " .y="+pnt.y+"prm="+prm+" value="+fdata);
								switch (D2DParmNames.valueOf(prm)) {
								case GH:
									if(ht == false){
										PointIn pointIn = new PointIn(pluginName, rec1,
												pnt.x, pnt.y);
										float fdata = pointIn.getPointData();
										soundingLy.setGeoHeight(fdata);
										ht= true;
									}
									break;
								case uW:
									// HDF5 data in
																// unit of
																// Knots, no
																// conversion
																// needed
									if(ur == false){
										PointIn pointIn = new PointIn(pluginName, rec1,
												pnt.x, pnt.y);
										float fdata = pointIn.getPointData();
										soundingLy.setWindU(fdata); 
										ur= true;
									}
									break;
								case vW:
									// HDF5 data in
																// unit of
																// Knots, no
																// conversion
																// needed
									if(vr == false){
										PointIn pointIn = new PointIn(pluginName, rec1,
												pnt.x, pnt.y);
										float fdata = pointIn.getPointData();
										soundingLy.setWindV(fdata); 
										vr= true;
									}
									break;
								case T:
									if(t == false){
										PointIn pointIn = new PointIn(pluginName, rec1,
												pnt.x, pnt.y);
										float fdata = pointIn.getPointData();
										soundingLy.setTemperature((float) kelvinToCelsius
												.convert(fdata));
								t= true;
									}
									break;
								case DWPK:
									if(d == false){
										PointIn pointIn = new PointIn(pluginName, rec1,
												pnt.x, pnt.y);
										float fdata = pointIn.getPointData();
										soundingLy.setDewpoint((float) kelvinToCelsius
												.convert(fdata));
										d= true;
									}
									break;
								case RH:
									if(rh == false){
										PointIn pointIn = new PointIn(pluginName, rec1,
												pnt.x, pnt.y);
										float fdata = pointIn.getPointData();
										soundingLy.setRelativeHumidity(fdata);
										rh= true;
									}
									break;
								case OMEG:
									if(om == false){
										PointIn pointIn = new PointIn(pluginName, rec1,
												pnt.x, pnt.y);
										float fdata = pointIn.getPointData();
										soundingLy.setOmega(fdata);
										om= true;
									}
									break;
								}

							}
							soundLyList.add(soundingLy);
						}

					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				} catch (DataAccessLayerException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

			}
		}

		long t02 = System.currentTimeMillis();
		System.out.println("MDL profile retreival took " + (t02 - t01));
		return soundLyList;
	}


	/**
	 * Return a list of data vertical levels for a specified time and model for
	 * grib or ncgrib data.
	 * 
	 * @param pluginName
	 *            the name of the data table ('grib' or 'ncgrib')
	 * @param modelName
	 *            the name of the model
	 * @return list of vertical levels
	 */
	public static List<?> getModelLevels(String refTime, String validTime,
			String pluginName, String modelName) {

		// List<?>vals = null;
		if (pluginName.equalsIgnoreCase(NCGRIB_TBL_NAME)) {
			CoreDao dao = new CoreDao(DaoConfig.forClass(NcgribRecord.class));
			DatabaseQuery query = new DatabaseQuery(
					NcgribRecord.class.getName());
			query.addDistinctParameter("glevel1");
			query.addQueryParam("parm", "HGHT");
			query.addQueryParam("vcord", "PRES");

			query.addQueryParam("modelName", modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);
			query.addOrder("glevel1", false);

			try {
				return (List<?>) dao.queryByCriteria(query);
			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return null;
			}

		} else if (pluginName.equalsIgnoreCase(D2DGRIB_TBL_NAME)) {
			CoreDao dao = new CoreDao(DaoConfig.forClass(GribRecord.class));
			DatabaseQuery query = new DatabaseQuery(GribRecord.class.getName());
			query.addDistinctParameter("modelInfo.level.levelonevalue");
			query.addQueryParam("modelInfo.parameterAbbreviation", "GH");
			query.addQueryParam("modelInfo.level.masterLevel.name", "MB");

			query.addQueryParam("modelInfo.modelName", modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);
			query.addOrder("modelInfo.level.levelonevalue", false);

			try {
				return (List<?>) dao.queryByCriteria(query);

			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return null;
			}
		}

		return null;

	}

	/**
	 * Returns the indices of the model grid of the closest point to the
	 * specified latitude, longitude.
	 * 
	 * @param lat
	 *            latitude
	 * @param lon
	 *            longitude
	 * @param level
	 *            vertical level
	 * @param pluginName
	 *            the name of the data table ('grib' or 'ncgrib')
	 * @param modelName
	 *            the name of the model
	 * @return the point indices
	 */
	public static Point getLatLonIndices(double lat, double lon,
			String refTime, String validTime, String level, String pluginName,
			String modelName) {
		ISpatialObject spatialArea = null;

		Point pnt = null;

		if (pluginName.equalsIgnoreCase(NCGRIB_TBL_NAME)) {
			CoreDao dao = new CoreDao(DaoConfig.forClass(NcgribRecord.class));
			DatabaseQuery query = new DatabaseQuery(
					NcgribRecord.class.getName());
			query.addQueryParam("parm", "HGHT");
			query.addQueryParam("vcord", "PRES");
			query.addQueryParam("modelName", modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);
			query.addQueryParam("glevel1", level);

			NcgribRecord rec;
			try {
				List<NcgribRecord> recList = ((List<NcgribRecord>) dao
						.queryByCriteria(query));
				if (recList.size() == 0) {
					return null;
				} else {
					rec = recList.get(0);
					spatialArea = rec.getSpatialObject();
				}
			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return null;
			}
			

		} else if (pluginName.equalsIgnoreCase(D2DGRIB_TBL_NAME)) {
			CoreDao dao = new CoreDao(DaoConfig.forClass(GribRecord.class));
			DatabaseQuery query = new DatabaseQuery(GribRecord.class.getName());

			query.addQueryParam("modelInfo.parameterAbbreviation", "GH");
			query.addQueryParam("modelInfo.level.masterLevel.name", "MB");
			query.addQueryParam("modelInfo.modelName", modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);
			query.addQueryParam("modelInfo.level.levelonevalue", level);
			query.addQueryParam("modelInfo.level.leveltwovalue", "-999999.0");

			GribRecord rec;
			try {
				List<GribRecord> recList = ((List<GribRecord>) dao
						.queryByCriteria(query));
				if (recList.size() == 0) {
					return null;
				} else {
					rec = recList.get(0);
					spatialArea = rec.getSpatialObject();
				}
			} catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return null;
			}

		}
		else 
			return null;


		GridGeometry2D geom = MapUtil.getGridGeometry(spatialArea);

		CoordinateReferenceSystem crs = geom.getCoordinateReferenceSystem();
		Coordinate coord = new Coordinate(lon, lat);

		try {
			pnt = PointUtil.determineIndex(coord, crs, geom);
			Integer nx = spatialArea.getNx();
			Integer ny = spatialArea.getNy();

			if ( pnt.x > nx || pnt.y > ny) {
				return null;
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return pnt;

	}

}
