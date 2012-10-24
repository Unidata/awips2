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
 * 02/28/2012               Chin Chen   modify several sounding query algorithms for better performance
 * 03/28/2012               Chin Chen   Add new API to support query multiple Points at one shoot and using
 * 										dataStore.retrieveGroups()
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
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribModel;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;
import java.util.ArrayList;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import com.raytheon.edex.plugin.modelsounding.common.SoundingSite;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.edex.uengine.tasks.query.TableQuery;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;

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
	private static final String D2DGRIB_TBL_NAME = "grid";
	
	private static String NC_PARMS = "HGHT, UREL, VREL, TMPK, OMEG, RELH";
	//private static String NC_PARMS = "HGHT, UREL, VREL, TMPK, DWPK, SPFH, OMEG, RELH";
	private static String D2D_PARMS = "GH, uW, vW,T, DWPK, SPFH,OMEG, RH";
	private enum NcParmNames {
		HGHT, UREL, VREL, TMPK, DWPK, SPFH, OMEG, RELH
	};


	private enum D2DParmNames {
		GH, uW, vW, T, DWPK, SPFH, OMEG, RH
	};

	public static UnitConverter kelvinToCelsius = SI.KELVIN
			.getConverterTo(SI.CELSIUS);
	private static final UnitConverter metersPerSecondToKnots = SI.METERS_PER_SECOND.getConverterTo(NonSI.KNOT);

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
				query = new TableQuery("metadata", GridRecord.class.getName());
				query.setDistinctField("dataTime.refTime");
				query.addParameter(GridConstants.DATASET_ID, mdlType);
				query.setSortBy("dataTime.refTime", false);
				@SuppressWarnings("unchecked")
				List<GridRecord> recList = (List<GridRecord>) query.execute();
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
				+ " ORDER BY rangestart");
			System.out.println("queryStr  " + queryStr);

			CoreDao dao = new CoreDao(DaoConfig.forClass(SoundingSite.class));
			refTimeAry = (Object[]) dao.executeSQLQuery(queryStr);
			tl.setTimeLines(refTimeAry);
		}
		else if(currentDBTblName.equals(D2DGRIB_TBL_NAME)){
			TableQuery query;
			try {
				query = new TableQuery("metadata", GridRecord.class.getName());
				query.setDistinctField("dataTime.validPeriod.start");
				query.addParameter(GridConstants.DATASET_ID, mdlType);
				query.addParameter("dataTime.refTime", refTimeStr + ":00:00");
				query.setSortBy("dataTime.validPeriod.start", true);
				@SuppressWarnings("unchecked")
				List<GridRecord> recList = (List<GridRecord>) query.execute();
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
	 * Returns a list of profile for location (lat,lon) array, time, and model for
	 * grib or ncgrib data.
	 * 
	 * @param double[][] latLonArray, e.g. at nth element, lat=[n][0], lon=[n][1] 
	 * @param refTimeCal
	 *            data record reference time
	 * @param validTimeCal
	 *            data record valid time
	 * @param pluginName
	 *            the name of the data table ('grib' or 'ncgrib')
	 * @param mdlName
	 *            the name of the model
	 * @return the profile
	 * created @ 3/28/2012
	 */
	public static List<NcSoundingProfile> getMdlSndDataProfileList(double[][] latLonArray,
			String refTime, String validTime, String pluginName, String mdlName) {
		double lat, lon;
		//System.out.println("getMdlSndData lat=" + lat + " lon="+lon);
		long t01 = System.currentTimeMillis();
		NcSoundingProfile pf = new NcSoundingProfile();
		//NcSoundingCube cube = new NcSoundingCube();
		List<NcSoundingProfile>  soundingProfileList = new ArrayList<NcSoundingProfile>();
		List<?> levels = getModelLevels(refTime, validTime, pluginName, mdlName);
		if (levels.size() == 0) {
			System.out.println("getModelLevels return 0;  file=" + refTime+ " stime="+validTime + " gribtype="+ pluginName + " modeltype="+mdlName);
			return soundingProfileList;
		}
		//System.out.println("getModelLevels = "+ levels.size()+" levels, took "+ (System.currentTimeMillis()-t01) + " ms");
		
		List<Point> points = new ArrayList<Point>();
		for(int k =0; k< latLonArray.length; k++){
			lat=latLonArray[k][0];
			lon=latLonArray[k][1] ;
			Point pnt = getLatLonIndices(lat, lon, refTime, validTime, levels.get(0).toString(),
					pluginName, mdlName);
			if (pnt == null) {
				System.out.println("getLatLonIndices return 0; lat=" + lat + " lon="+lon+" stime="+validTime + " gribtype="+ pluginName + " modeltype="+mdlName);
			}
			else{
				points.add(pnt);
							}
		}
		if(points.size()==0){
			return soundingProfileList;
		}
		long t011 = System.currentTimeMillis();
		soundingProfileList = queryProfileListByPointGroup(points,
				refTime, validTime, pluginName, mdlName, levels);
		System.out.println("queryProfileListByPointGroup took "+ (System.currentTimeMillis()-t011) + " ms");

		return soundingProfileList;
		/* The floowing should be done in queryProfileListByPointGroup()
		//System.out.println("getModelSoundingLayerList= "+  layerList.size()+ " layers, took "+ (System.currentTimeMillis()-t012) + " ms");
		//pf.setStationLatitude( lat);
		//pf.setStationLongitude( lon);
		//Float sfcPressure = getModelSfcPressure(pnt, refTime, validTime,
		//		pluginName, mdlName);
		//System.out.println("getModelSfcPressure took "+ (System.currentTimeMillis()-t013) + " ms");
		//if (sfcPressure == null) {
		//	pf.setSfcPress(-9999.f);
		//}
		//else {
		//	if (pluginName.equalsIgnoreCase(D2DGRIB_TBL_NAME)) 
		//		pf.setSfcPress(sfcPressure/100F);
		//	else
		//		pf.setSfcPress(sfcPressure);
		//}
		//System.out.println("surface pressure ="+pf.getSfcPress()+ " lat= "+lat+ " lon="+lon);
		//calculate dew point if necessary
		long t014 = System.currentTimeMillis();
		MergeSounding ms = new MergeSounding();
		//ms.spfhToDewpoint(layerList);
		ms.rhToDewpoint(layerList);
		System.out.println("MergeSounding took "+ (System.currentTimeMillis()-t014) + " ms");
		
			
		pf.setSoundingLyLst(layerList);
		
	
		soundingProfileList.add(pf);
		//cube.setSoundingProfileList(soundingProfileList);
		//cube.setRtnStatus(NcSoundingCube.QueryStatus.OK);
		long t02 = System.currentTimeMillis();
		System.out.println("MDL cube retreival took " + (t02 - t01));
		return pf;
		*/

	}
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
	 *
	public static NcSoundingProfile getMdlSndData(double lat, double lon,
			String refTime, String validTime, String pluginName, String mdlName) {
		System.out.println("getMdlSndData lat=" + lat + " lon="+lon);
		long t01 = System.currentTimeMillis();
		NcSoundingProfile pf = new NcSoundingProfile();
		List<?> levels = getModelLevels(refTime, validTime, pluginName, mdlName);
		if (levels.size() == 0) {
			System.out.println("getModelLevels return 0;  file=" + refTime+ " stime="+validTime + " gribtype="+ pluginName + " modeltype="+mdlName);
			pf.setRtnStatus(NcSoundingCube.QueryStatus.FAILED);
			return pf;
		}
		System.out.println("getModelLevels = "+ levels.size()+" levels, took "+ (System.currentTimeMillis()-t01) + " ms");
		long t011 = System.currentTimeMillis();
		Point pnt = getLatLonIndices(lat, lon, refTime, validTime, levels.get(0).toString(),
				pluginName, mdlName);
		if (pnt == null) {
			System.out.println("getLatLonIndices return 0; lat=" + lat + " lon="+lon+" stime="+validTime + " gribtype="+ pluginName + " modeltype="+mdlName);
			
			pf.setRtnStatus(NcSoundingCube.QueryStatus.LOCATION_NOT_FOUND);
			return pf;

		}
		System.out.println("getLatLonIndices pntX=" + pnt.getX()+ " pntY=" + pnt.getY()+ " took "+ (System.currentTimeMillis()-t011) + " ms");
		long t012 = System.currentTimeMillis();
		List<NcSoundingLayer> layerList = getModelSoundingLayerList(pnt,
				refTime, validTime, pluginName, mdlName, levels);
		if (layerList.size() == 0) {
			System.out.println("getModelSoundingLayerList return 0; lat=" + lat + " lon="+lon+" stime="+validTime + " gribtype="+ pluginName + " modeltype="+mdlName);
			
			
			pf.setRtnStatus(NcSoundingCube.QueryStatus.FAILED);
			return pf;
		}
		
		System.out.println("getModelSoundingLayerList= "+  layerList.size()+ " layers, took "+ (System.currentTimeMillis()-t012) + " ms");
		
		pf.setStationLatitude( lat);
		pf.setStationLongitude( lon);
		Float sfcPressure = getModelSfcPressure(pnt, refTime, validTime,
				pluginName, mdlName);
		//System.out.println("getModelSfcPressure took "+ (System.currentTimeMillis()-t013) + " ms");
		if (sfcPressure == null) {
			pf.setSfcPress(-9999.f);
		}
		else {
			if (pluginName.equalsIgnoreCase(D2DGRIB_TBL_NAME)) 
				pf.setSfcPress(sfcPressure/100F);
			else
				pf.setSfcPress(sfcPressure);
		}
		//System.out.println("surface pressure ="+pf.getSfcPress()+ " lat= "+lat+ " lon="+lon);
		//calculate dew point if necessary
		long t014 = System.currentTimeMillis();
		MergeSounding ms = new MergeSounding();
		//ms.spfhToDewpoint(layerList);
		ms.rhToDewpoint(layerList);
		System.out.println("MergeSounding took "+ (System.currentTimeMillis()-t014) + " ms");
		
			
		pf.setSoundingLyLst(layerList);
		
		long t02 = System.currentTimeMillis();
		System.out.println("MDL cube retreival took " + (t02 - t01));
		return pf;

	}*/

	public static NcSoundingModel getMdls(String pluginName) {
		NcSoundingModel mdls = new NcSoundingModel();
		Object[] mdlName = null;
		if (pluginName.equalsIgnoreCase(NCGRIB_TBL_NAME)) {
			CoreDao dao = new CoreDao(DaoConfig.forClass(NcgribModel.class));
			String queryStr = new String("Select Distinct modelname FROM ncgrib_models ORDER BY modelname");
			mdlName = (Object[]) dao.executeSQLQuery(queryStr);
		}
		else if (pluginName.equalsIgnoreCase(D2DGRIB_TBL_NAME)) {
			CoreDao dao = new CoreDao(DaoConfig.forClass(NcgribModel.class));
			String queryStr = new String("Select Distinct modelname FROM grib_models ORDER BY modelname");
			mdlName = (Object[]) dao.executeSQLQuery(queryStr);
		}
		if(mdlName!=null && mdlName.length>0){
			List<String> mdlList = new ArrayList<String>();
			for(Object mn : mdlName){
				mdlList.add((String)mn);
			}
			mdls.setMdlList(mdlList);
		}
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
			CoreDao dao = new CoreDao(DaoConfig.forClass(GridRecord.class));
			DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());

			query.setMaxResults(new Integer(1));
			query.addQueryParam(GridConstants.DATASET_ID, modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);

			try {
				List<GridRecord> recList = ((List<GridRecord>) dao
						.queryByCriteria(query));
				if (recList.size() == 0) {
					return false;
				} else {
					GridRecord rec = recList.get(0);
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
			CoreDao dao = new CoreDao(DaoConfig.forClass(GridRecord.class));
			DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());

			query.setMaxResults(new Integer(1));
			query.addQueryParam(GridConstants.DATASET_ID, modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);

			try {
				List<GridRecord> recList = ((List<GridRecord>) dao
						.queryByCriteria(query));
				if (recList.size() == 0) {
					return false;
				} else {
					GridRecord rec = recList.get(0);
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
			CoreDao dao = new CoreDao(DaoConfig.forClass(GridRecord.class));
			DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());

			query.addQueryParam(GridConstants.LEVEL_ONE, "0.0");
			query.addQueryParam(GridConstants.LEVEL_TWO, "-999999.0");
			query.addQueryParam(GridConstants.MASTER_LEVEL_NAME, "MSL");
			query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, "PMSL");
			query.addQueryParam(GridConstants.DATASET_ID, modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);

			GridRecord rec = null;
			try {
				List<GridRecord> recList = ((List<GridRecord>) dao
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
	 * Returns a list of NcSoundingProfile for a group of Point with specific ref and range time, and
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
	 * 
	 * Created @ 3/28/2012
	 */

	private static List<NcSoundingProfile> queryProfileListByPointGroup(List<Point> points,
			String refTime, String validTime, String pluginName,
			String modelName, List<?> levels) {
		
		List<NcSoundingProfile>  soundingProfileList = new ArrayList<NcSoundingProfile>();
		List<float[]> fdataArrayList = new ArrayList<float[]>();
		//long t01 = System.currentTimeMillis();
		if (pluginName.equalsIgnoreCase(NCGRIB_TBL_NAME)) {
			List<NcgribRecord> recList = new ArrayList<NcgribRecord>(); ;
			TableQuery query;
			try {
				query = new TableQuery("metadata",
						NcgribRecord.class.getName());
				query.addParameter("vcord", "PRES");
				query.addParameter("modelName", modelName);
				query.addList("parm",NC_PARMS);//parmList.toString()); //
				query.addParameter("dataTime.refTime", refTime);
				query.addParameter("dataTime.validPeriod.start", validTime);
				//query.addParameter("glevel1", level.toString());
				query.setSortBy("glevel1", false);
				recList = (List<NcgribRecord>) query.execute();					
				//System.out.println("Ncgrib group query0 result size ="+ recList.size());

				if (recList.size() != 0) {

					PointIn pointIn = new PointIn(pluginName, recList.get(0));
					//Chin note:
					// We query multiple points, and for each point, query all levels (pressure) and all parameters 
					//(at that level) with one shot.
					// The return array list (fdataArrayList) are listed in the same order as querying list "points" 
					// Each element (float[]) of the returned array list, represent a Point data, contains the same number of 
					// parameters and listed in the same order as querying rec array (recList.toArray())
					//However, returned element (float[]) does not tell you which parameter itself is.
					//Therefore, we have to use information in query rec array to find out returned value's type (which parameter it is)
					// Further, we have to sort and store returned values to NcSoundingLayer based on its level (pressure)
					// Parameters in same level should be stored in one same NcSoundingLayer, 
					// NcSoundingLayers for same Point should be stored in same NcSoundingProfile.
					fdataArrayList = pointIn.getHDF5GroupDataPoints(recList.toArray(),points);
				}
			}catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			int index=0;
			GridGeometry2D geom = MapUtil.getGridGeometry(spatialArea);
			CoordinateReferenceSystem crs = geom.getCoordinateReferenceSystem();
			Coordinate coord= new Coordinate(45,45);

			for(float[]  fdataArray: fdataArrayList ){
				//one fdataArray is for one Point or say one profile
				NcSoundingProfile pf = new NcSoundingProfile();
				List<NcSoundingLayer> soundLyList = new ArrayList<NcSoundingLayer>();
				Point pnt = points.get(index);
				Object[] recArray = recList.toArray();
				for (Object level : levels){
					NcSoundingLayer soundingLy = new NcSoundingLayer();
					int pressure= (Integer)level;
					soundingLy.setPressure( pressure);

					for (int i=0; i < recArray.length; i++) {
						NcgribRecord rec1 = (NcgribRecord)recArray[i];
						float fdata = fdataArray[i];
						if(rec1.getGlevel1() == pressure){
							String prm = rec1.getParm();

							//long t01 = System.currentTimeMillis();				
							switch (NcParmNames.valueOf(prm)) {
							case HGHT:
								soundingLy.setGeoHeight(fdata);
								break;
							case UREL:
								// HDF5 data in unit of m/s, convert to Knots 4/12/2012 
								soundingLy.setWindU((float)metersPerSecondToKnots.convert(fdata)); 
								break;
							case VREL:
								// HDF5 data in unit of m/s, convert to Knots 4/12/2012 
								soundingLy.setWindV((float)metersPerSecondToKnots.convert(fdata));  
								break;
							case TMPK:
								soundingLy.setTemperature((float) kelvinToCelsius
										.convert(fdata)); 
								break;
							case DWPK:								
								soundingLy.setDewpoint((float) kelvinToCelsius
										.convert(fdata)); 
								break;
							case SPFH:
								soundingLy.setSpecHumidity(fdata);
								break;
							case OMEG:
								soundingLy.setOmega(fdata);
								break;
							case RELH:
								soundingLy.setRelativeHumidity(fdata);
								break;
							}
						}
					}
					soundLyList.add(soundingLy);
				}
				try {
					coord = PointUtil.determineLatLon(pnt, crs, geom);
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				//System.out.println(" point coord.y="+coord.y+ " coord.x="+ coord.x);
				pf.setStationLatitude(coord.y);
				pf.setStationLongitude( coord.x);
				//Float sfcPressure = getModelSfcPressure(pnt, refTime, validTime,
				//		pluginName, modelName);
				//System.out.println("getModelSfcPressure took "+ (System.currentTimeMillis()-t013) + " ms");
				//if (sfcPressure == null) {
					pf.setSfcPress(-9999.f);
				//}
				//else {
				//	pf.setSfcPress(sfcPressure);
				//}
				//System.out.println("surface pressure ="+pf.getSfcPress());
				//calculate dew point if necessary
				MergeSounding ms = new MergeSounding();
				//ms.spfhToDewpoint(layerList);
				ms.rhToDewpoint(soundLyList);
				//System.out.println("MergeSounding took "+ (System.currentTimeMillis()-t014) + " ms");
				pf.setSoundingLyLst(soundLyList);
				soundingProfileList.add(pf);
				index++;
			}
		}	
		else if (pluginName.equalsIgnoreCase(D2DGRIB_TBL_NAME)) {
			List<GridRecord> recList = new ArrayList<GridRecord>(); ;
			TableQuery query;
			try {
				query = new TableQuery("metadata",
						GridRecord.class.getName());
				query.addParameter(GridConstants.MASTER_LEVEL_NAME, "MB");
				query.addParameter(GridConstants.DATASET_ID, modelName);
				query.addList(GridConstants.PARAMETER_ABBREVIATION,
						D2D_PARMS);
				query.addParameter("dataTime.refTime", refTime);
				query.addParameter("dataTime.validPeriod.start", validTime);
				query.setSortBy(GridConstants.LEVEL_ONE, false);
				recList = (List<GridRecord>) query.execute();					
				if (recList.size() != 0) {
					PointIn pointIn = new PointIn(pluginName, recList.get(0));
					fdataArrayList = pointIn.getHDF5GroupDataPoints(recList.toArray(),points);
				}
			}catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			int index=0;
			GridGeometry2D geom = MapUtil.getGridGeometry(spatialArea);
			CoordinateReferenceSystem crs = geom.getCoordinateReferenceSystem();
			Coordinate coord= new Coordinate(45,45);

			for(float[]  fdataArray: fdataArrayList ){
				//one fdataArray is for one Point or say one profile
				NcSoundingProfile pf = new NcSoundingProfile();
				List<NcSoundingLayer> soundLyList = new ArrayList<NcSoundingLayer>();
				Point pnt = points.get(index);
				Object[] recArray = recList.toArray();
				for (Object level : levels){
					NcSoundingLayer soundingLy = new NcSoundingLayer();
					double pressure= (Double)level;
					soundingLy.setPressure( (float)pressure);
					for (int i=0; i < recArray.length; i++) {
						GridRecord rec1 = (GridRecord)recArray[i];
						float fdata = fdataArray[i];
						if(rec1.getLevel().getLevelonevalue() == pressure){
							String prm = rec1.getParameter().getAbbreviation();
							switch (D2DParmNames.valueOf(prm)) {
							case GH:
								soundingLy.setGeoHeight(fdata);
								break;
							case uW:
								// HDF5 data in unit of m/s, convert to Knots 4/12/2012 
								soundingLy.setWindU((float)metersPerSecondToKnots.convert(fdata)); 
								break;
							case vW:
								// HDF5 data in unit of m/s, convert to Knots 4/12/2012 
								soundingLy.setWindV((float)metersPerSecondToKnots.convert(fdata));  
								break;
							case T:
								soundingLy.setTemperature((float) kelvinToCelsius
										.convert(fdata)); 
								break;
							case DWPK:								
								soundingLy.setDewpoint((float) kelvinToCelsius
										.convert(fdata)); 
								break;
							case OMEG:
								soundingLy.setOmega(fdata);
								break;
							case RH:
								soundingLy.setRelativeHumidity(fdata);
								break;
							}
						}
					}
					soundLyList.add(soundingLy);
				}
				try {
					coord = PointUtil.determineLatLon(pnt, crs, geom);
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				//System.out.println(" point coord.y="+coord.y+ " coord.x="+ coord.x);
				pf.setStationLatitude(coord.y);
				pf.setStationLongitude( coord.x);
				//Float sfcPressure = getModelSfcPressure(pnt, refTime, validTime,
				//		pluginName, modelName);
				//System.out.println("getModelSfcPressure took "+ (System.currentTimeMillis()-t013) + " ms");
				///if (sfcPressure == null) {
					pf.setSfcPress(-9999.f);
				//}
				//else {
				//	pf.setSfcPress(sfcPressure/100F);
				//}
				//System.out.println("surface pressure ="+pf.getSfcPress()+ " lat= "+lat+ " lon="+lon);
				//calculate dew point if necessary
				MergeSounding ms = new MergeSounding();
				//ms.spfhToDewpoint(layerList);
				ms.rhToDewpoint(soundLyList);
				pf.setSoundingLyLst(soundLyList);
				soundingProfileList.add(pf);
				index++;
			}
		}
		return soundingProfileList;
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
	 *

	private static List<NcSoundingLayer> getModelSoundingLayerList(Point pnt,
			String refTime, String validTime, String pluginName,
			String modelName, List<?> levels) {
		List<NcSoundingLayer> soundLyList = new ArrayList<NcSoundingLayer>();

		//long t01 = System.currentTimeMillis();
		if (pluginName.equalsIgnoreCase(NCGRIB_TBL_NAME)) {
			
			TableQuery query;
			try {
				query = new TableQuery("metadata",
						NcgribRecord.class.getName());
				query.addParameter("vcord", "PRES");
				query.addParameter("modelName", modelName);
				query.addList("parm",NC_PARMS);//parmList.toString()); //
				query.addParameter("dataTime.refTime", refTime);
				query.addParameter("dataTime.validPeriod.start", validTime);
				//query.addParameter("glevel1", level.toString());
				query.setSortBy("glevel1", false);
				

				List<NcgribRecord> recList = (List<NcgribRecord>) query.execute();					
				System.out.println("Ncgrib group query0 result size ="+ recList.size());
				
				if (recList.size() != 0) {
					
					PointIn pointIn = new PointIn(pluginName, recList.get(0),
							pnt.x, pnt.y);
					//Chin note:
					// We query all levels (pressure) and all parameters (at that level) at once.
					// The return array (fdataArray) are listed in the same order as query array (recList.toArray())
					//However, returned array does not tell you which parameter itself is.
					//Therefore, we have to use information in query array to find out returned value's type (which parameter it is)
					// Further, we have to sort and store returned values to NcSoundingLayer based on its level (pressure)
					// Parameters in same level should be stored in one same NcSoundingLayer
					float[] fdataArray = pointIn.getHDF5GroupDataPoint(recList.toArray());
					Object[] recArray = recList.toArray();
					for (Object level : levels){
						NcSoundingLayer soundingLy = new NcSoundingLayer();
						int pressure= (Integer)level;
						soundingLy.setPressure( pressure);
						
						for (int i=0; i < recArray.length; i++) {
							NcgribRecord rec1 = (NcgribRecord)recArray[i];
							float fdata = fdataArray[i];
							if(rec1.getGlevel1() == pressure){
								String prm = rec1.getParm();
								//System.out.println("point.x="+ pnt.x + " .y="+pnt.y+"pressure="+rec1
								//		.getGlevel1()+ " Parm="+prm );
								//long t01 = System.currentTimeMillis();				
								switch (NcParmNames.valueOf(prm)) {
								case HGHT:
										soundingLy.setGeoHeight(fdata);
									break;
								case UREL:
									// HDF5 data in unit of Knots, no conversion needed
										soundingLy.setWindU(fdata); 
									break;
								case VREL:
									// HDF5 data in unit of Knots, no conversion needed
										soundingLy.setWindV(fdata);  
									break;
								case TMPK:
										soundingLy.setTemperature((float) kelvinToCelsius
												.convert(fdata)); 
									break;
								case DWPK:								
										soundingLy.setDewpoint((float) kelvinToCelsius
												.convert(fdata)); 
									break;
								case SPFH:
										soundingLy.setSpecHumidity(fdata);
									break;
								case OMEG:
										soundingLy.setOmega(fdata);
									break;
								case RELH:
										soundingLy.setRelativeHumidity(fdata);
									break;
								}
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
			//System.out.println("getModelSoundingLayerList:total level = "+ totalLevel + " total records= "+totalRecords );
			
		}
		else if (pluginName.equalsIgnoreCase(D2DGRIB_TBL_NAME)) {
			try {
				TableQuery query = new TableQuery("metadata",
						GribRecord.class.getName());
				//query.addParameter("modelInfo.level.levelonevalue",
				//		level.toString());
				//query.addParameter("modelInfo.level.leveltwovalue",
				//		"-999999.0");
				query.addParameter("modelInfo.level.masterLevel.name", "MB");
				query.addParameter("modelInfo.modelName", modelName);
				query.addList("modelInfo.parameterAbbreviation",
						D2D_PARMS);
				query.addParameter("dataTime.refTime", refTime);
				query.addParameter("dataTime.validPeriod.start", validTime);
				query.setSortBy("modelInfo.level.levelonevalue", false);
				//System.out.println("level = "+ level.toString());

				List<GribRecord> recList = (List<GribRecord>) query.execute();
				System.out.println("Grib group query0 result size ="+ recList.size());

				if (recList.size() > 0) {
					PointIn pointIn = new PointIn(pluginName, recList.get(0),
							pnt.x, pnt.y);
					float[] fdataArray = pointIn.getHDF5GroupDataPoint(recList.toArray());
					Object[] recArray = recList.toArray();
					for (Object level : levels){
						NcSoundingLayer soundingLy = new NcSoundingLayer();
						double pressure= (Double)level;
						soundingLy.setPressure( (float)pressure);

						for (int i=0; i < recArray.length; i++) {
							GribRecord rec1 = (GribRecord)recArray[i];
							float fdata = fdataArray[i];
							if(rec1.getModelInfo().getLevelOneValue() == pressure){
								String prm = rec1.getModelInfo().getParameterAbbreviation();
								//System.out.println("point.x="+ pnt.x + " .y="+pnt.y+"pressure="+pressure+ " Parm="+prm );
								//long t01 = System.currentTimeMillis();				
								switch (D2DParmNames.valueOf(prm)) {
								case GH:
									soundingLy.setGeoHeight(fdata);
									break;
								case uW:
									// HDF5 data in unit of Knots, no conversion needed
									soundingLy.setWindU(fdata); 
									break;
								case vW:
									// HDF5 data in unit of Knots, no conversion needed
									soundingLy.setWindV(fdata);  
									break;
								case T:
									soundingLy.setTemperature((float) kelvinToCelsius
											.convert(fdata)); 
									break;
								case DWPK:								
									soundingLy.setDewpoint((float) kelvinToCelsius
											.convert(fdata)); 
									break;
								case OMEG:
									soundingLy.setOmega(fdata);
									break;
								case RH:
									soundingLy.setRelativeHumidity(fdata);
									break;
								}
							}
						}
						soundLyList.add(soundingLy);
					}
				}
			}
			catch (DataAccessLayerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		//long t02 = System.currentTimeMillis();
		//System.out.println("MDL profile retreival took " + (t02 - t01));
		
		for(NcSoundingLayer layer: soundLyList){
			System.out.println("pre="+ layer.getPressure()+ " h="+layer.getGeoHeight()+ " T="+layer.getTemperature()+" D="+
					layer.getDewpoint()+ " WS="+layer.getWindSpeed()+ " WD="+layer.getWindDirection() + " SH="+layer.getSpecHumidity()+ " RH="+layer.getRelativeHumidity());
		}
		return soundLyList;
	}
	*/
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
			CoreDao dao = new CoreDao(DaoConfig.forClass(GridRecord.class));
			DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());
			query.addDistinctParameter(GridConstants.LEVEL_ONE);
			query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, "GH");
			query.addQueryParam(GridConstants.MASTER_LEVEL_NAME, "MB");

			query.addQueryParam(GridConstants.DATASET_ID, modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);
			query.addOrder(GridConstants.LEVEL_ONE, false);

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
	private static ISpatialObject spatialArea = null;
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
		//ISpatialObject spatialArea = null;

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
			CoreDao dao = new CoreDao(DaoConfig.forClass(GridRecord.class));
			DatabaseQuery query = new DatabaseQuery(GridRecord.class.getName());

			query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, "GH");
			query.addQueryParam(GridConstants.MASTER_LEVEL_NAME, "MB");
			query.addQueryParam(GridConstants.DATASET_ID, modelName);
			query.addQueryParam("dataTime.refTime", refTime);
			query.addQueryParam("dataTime.validPeriod.start", validTime);
			query.addQueryParam(GridConstants.LEVEL_ONE, level);
			query.addQueryParam(GridConstants.LEVEL_TWO, "-999999.0");

			GridRecord rec;
			try {
				List<GridRecord> recList = ((List<GridRecord>) dao
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
