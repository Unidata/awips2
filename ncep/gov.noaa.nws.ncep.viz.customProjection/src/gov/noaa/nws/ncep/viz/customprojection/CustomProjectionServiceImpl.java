package gov.noaa.nws.ncep.viz.customprojection;

import gov.noaa.nws.ncep.gempak.parameters.core.marshaller.garea.GraphicsAreaCoordinates;

import org.apache.log4j.Logger;
import org.geotools.parameter.Parameter;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.NoSuchIdentifierException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

public class CustomProjectionServiceImpl implements ICustomProjectionService {
    private static final IUFStatusHandler statusHandler = UFStatus.getHandler(CustomProjectionServiceImpl.class);
	private Logger logger = Logger.getLogger(this.getClass()); 
	
	private GempakProjectionValuesUtil projectionValuesObject; 
	private GraphicsAreaCoordinates graphicsAreaCoordinatesObject; 
//	private String geogFilePath, stationFilePath; 
	private boolean gempakGraphicsAreaStringValid; 
	private boolean gempakProjectionStringValid; 
	private boolean projectionWithoutAngleValues; 
	private boolean graphicAreaTextWithValidGeogName; 
	private boolean graphicAreaTextWithValidStationName; 
	
    private double lowerLeftLat, lowerLeftLon, upperRightLat, upperRightLon, centerLat, centerLon;

	private DefaultMathTransformFactory factory; 

	public CustomProjectionServiceImpl(String gempakProjectionString, String gempakGraphicAreaString, 
			String geogFilePath, String stationFilePath) {
		projectionValuesObject = new GempakProjectionValuesUtil(gempakProjectionString); 
		graphicsAreaCoordinatesObject = new GraphicsAreaCoordinates(gempakGraphicAreaString); 
//		this.geogFilePath = geogFilePath; 
//		this.stationFilePath = stationFilePath; 
		
		graphicsAreaCoordinatesObject.setGeogFileName(geogFilePath); 
		graphicsAreaCoordinatesObject.setStationFileName(stationFilePath); 
		
		gempakGraphicsAreaStringValid = graphicsAreaCoordinatesObject.parseGraphicsAreaString(gempakGraphicAreaString); 
		gempakProjectionStringValid = projectionValuesObject.isGempakProjectionStringValid(); 
		
		/*
		 * handle valid geog name in graphic area
		 */
		if(isGraphicAreaTextWithValidGeogName() && isDefaultProjectionBeingUsed()) {
			handleValidGeogName(); 
		}
	}

	@Override
	public void handleValidGeogName() {
		projectionValuesObject = new GempakProjectionValuesUtil(graphicsAreaCoordinatesObject.getMapProjectionString()); 
		projectionWithoutAngleValues = projectionValuesObject.isProjectionStringWithoutAngleValues(); 

	}
	
	
	@Override
	public boolean isGempakGraphicsAreaStringValid() {
		return gempakGraphicsAreaStringValid;
	}

	@Override
	public boolean isGempakProjectionStringValid() {
		return gempakProjectionStringValid;
	}

	@Override
	public boolean isProjectionWithoutAngleValues() {
		if(projectionValuesObject != null)
			projectionWithoutAngleValues = projectionValuesObject.isProjectionStringWithoutAngleValues(); 
		return projectionWithoutAngleValues;
	}

	@Override
	public boolean isGraphicAreaTextWithValidGeogName() {
		if(graphicsAreaCoordinatesObject != null)
			graphicAreaTextWithValidGeogName = graphicsAreaCoordinatesObject.isValidGeogName(); 
		return graphicAreaTextWithValidGeogName;
	}

	@Override
	public boolean isGraphicAreaTextWithValidStationName() {
		if(graphicsAreaCoordinatesObject != null)
			graphicAreaTextWithValidStationName = graphicsAreaCoordinatesObject.isValidStationName(); 
		return graphicAreaTextWithValidStationName;
	}

	@Override
	public double getLowerLeftLat() {
		if(graphicsAreaCoordinatesObject != null)
			lowerLeftLat = graphicsAreaCoordinatesObject.getLowerLeftLat(); 
		return lowerLeftLat;
	}

//	@Override
//	public void setLowerLeftLat(double lowerLeftLat) {
//		if(graphicsAreaCoordinatesObject != null)
//		this.lowerLeftLat = lowerLeftLat;
//	}

	@Override
	public double getLowerLeftLon() {
		if(graphicsAreaCoordinatesObject != null)
			lowerLeftLon = graphicsAreaCoordinatesObject.getLowerLeftLon(); 
		return lowerLeftLon;
	}

//	@Override
//	public void setLowerLeftLon(double lowerLeftLon) {
//		if(graphicsAreaCoordinatesObject != null)
//		this.lowerLeftLon = lowerLeftLon;
//	}

	@Override
	public double getUpperRightLat() {
		if(graphicsAreaCoordinatesObject != null)
			upperRightLat = graphicsAreaCoordinatesObject.getUpperRightLat(); 
		return upperRightLat;
	}

//	@Override
//	public void setUpperRightLat(double upperRightLat) {
//		if(graphicsAreaCoordinatesObject != null)
//		this.upperRightLat = upperRightLat;
//	}

	@Override
	public double getUpperRightLon() {
		if(graphicsAreaCoordinatesObject != null)
			upperRightLon = graphicsAreaCoordinatesObject.getUpperRightLon(); 
		return upperRightLon;
	}

//	@Override
//	public void setUpperRightLon(double upperRightLon) {
//		if(graphicsAreaCoordinatesObject != null)
//			graphicsAreaCoordinatesObject.
//		this.upperRightLon = upperRightLon;
//	}
	
	@Override
	public double getCenterLat() {
		if(graphicsAreaCoordinatesObject != null)
			centerLat = graphicsAreaCoordinatesObject.getCenterLat(); 
		return centerLat;
	}

	@Override
	public double getCenterLon() {
		if(graphicsAreaCoordinatesObject != null)
			centerLon = graphicsAreaCoordinatesObject.getCenterLon(); 
		return centerLon;
	}

	@Override
	public boolean isDefaultProjectionBeingUsed() {
		if(projectionValuesObject != null)
			return projectionValuesObject.isDefaultProjection(); 
		return false;
	}

	@Override
	public boolean isLowerLeftAndUpperRightLatLonValuesValid() {
		if(graphicsAreaCoordinatesObject != null)
			return graphicsAreaCoordinatesObject.isValidLowerLeftAndUpperRightLatLonValues(); 
		return false;
	}

	@Override
	public boolean isCenterDeltaLatLonValuesValid() {
		if(graphicsAreaCoordinatesObject != null)
			return graphicsAreaCoordinatesObject.isValidCenterDeltaLatLonValues(); 
		return false;
	}

	@Override
	public double[] getDefaultGempakAngleValueArrayUsingLowerLeftAndUpperRightLatLon() {
		double [] gempakAngleValueArray = createDoubleArray(3);
		if(graphicsAreaCoordinatesObject != null && projectionValuesObject != null && 
				!StringUtil.isStringEmpty(projectionValuesObject.getGempakProjectionName())) {
			String gempakProjectionName = projectionValuesObject.getGempakProjectionName(); 
			//logger.debug("==method: getDefaultGempakAngleValueArrayUsingLowerLeftAndUpperRightLatLon==, gempakProjectionName="+gempakProjectionName); 
			gempakAngleValueArray[0] = graphicsAreaCoordinatesObject.getDefaultAngle1UsingGempakProjectionName(gempakProjectionName);
			gempakAngleValueArray[1] = graphicsAreaCoordinatesObject.getDefaultAngle2UsingGempakProjectionName(gempakProjectionName);
			if(projectionValuesObject.isGempakConProjection(gempakProjectionName)) {
				gempakAngleValueArray[2] = graphicsAreaCoordinatesObject.getDefaultAngle1UsingGempakProjectionName(gempakProjectionName);			}
		}
		return gempakAngleValueArray; 
	}

	@Override
	public double[] getGempakAngleValueArrayUsingGempakProjectionValuesObject() {
		double [] gempakAngleValueArray = createDoubleArray(3);
		if(projectionValuesObject != null) {
			if("TVM".equalsIgnoreCase(projectionValuesObject.getGempakProjectionName())) {
				gempakAngleValueArray[0] = projectionValuesObject.getAngle2(); 
				gempakAngleValueArray[1] = projectionValuesObject.getAngle1(); 
				gempakAngleValueArray[2] = projectionValuesObject.getAngle3(); 
			} else {
				gempakAngleValueArray[0] = projectionValuesObject.getAngle1(); 
				gempakAngleValueArray[1] = projectionValuesObject.getAngle2(); 
				gempakAngleValueArray[2] = projectionValuesObject.getAngle3(); 
			}
		}
		return gempakAngleValueArray; 
	}

	@Override
	public String getGempakProjectionName() {
		if(projectionValuesObject != null) 
			return projectionValuesObject.getGempakProjectionName(); 
		return null;
	}

	@Override
	public CoordinateReferenceSystem getCoordinateReferenceSystem() {
		if(factory == null)
	        factory = new DefaultMathTransformFactory();
		ParameterValueGroup parameters = null; 
		double [] gempakAngleValueArray = getGempakProjectionAngleValueArray(); 
		String geotoolsProjectionName = getGeoProjectionName(gempakAngleValueArray); 
		if(GempakProjectionValuesUtil.isCylindricalProjectionForGeotools(geotoolsProjectionName))
			parameters = getParameterValueGroupForCylindrical(factory, geotoolsProjectionName, gempakAngleValueArray); 
		else if(GempakProjectionValuesUtil.isAzmProjectionForGeotools(geotoolsProjectionName))
			parameters = getParameterValueGroupForAzm(factory, geotoolsProjectionName, gempakAngleValueArray); 
		else if(GempakProjectionValuesUtil.isConProjectionForGeotools(geotoolsProjectionName))
			parameters = getParameterValueGroupForCon(factory, geotoolsProjectionName, gempakAngleValueArray); 
		
		CoordinateReferenceSystem crs = null; 
		if(parameters != null) {
            String name = parameters.getDescriptor().getName().getCode();
            try {
				crs = MapUtil.constructProjection(name, parameters);
			} catch (NoSuchIdentifierException e) {
				logger.error("Failed to create CoordinateReferenceSystem, error="+e.getMessage());
			} catch (FactoryException e) {
				logger.error("Failed to create CoordinateReferenceSystem, error="+e.getMessage());
			}
		}
		
		return crs; 
	}

	
	
	/*
	 * all helper methods go here
	 */
	private double[] getGempakProjectionAngleValueArray() {
		double [] gempakAngleValueArray = null; 
		if(isDefaultProjectionBeingUsed() || 
				isProjectionWithoutAngleValues()) {
			gempakAngleValueArray = getDefaultGempakAngleValueArrayUsingLowerLeftAndUpperRightLatLon(); 
		} else {
			gempakAngleValueArray = getGempakAngleValueArrayUsingGempakProjectionValuesObject(); 
		}
		return gempakAngleValueArray; 
	}
	
	private String getGeoProjectionName(double[] angleValueArray) {
//		double [] gempakAngleValueArray = null; 
//		if(isDefaultProjectionBeingUsed() || 
//				isProjectionWithoutAngleValues()) {
//			gempakAngleValueArray = getDefaultGempakAngleValueArrayUsingLowerLeftAndUpperRightLatLon(); 
//		} else {
//			gempakAngleValueArray = getGempakAngleValueArrayUsingGempakProjectionValuesObject(); 
//		}
		
		String modifiedGampakProjectionName = GempakProjectionValuesUtil.getModifiedGempakProjectionName(getGempakProjectionName(), 
				angleValueArray[0]); 
		String geotoolsProjectionName = GempakProjectionValuesUtil.getGeotoolProjectionName(modifiedGampakProjectionName);
		return geotoolsProjectionName; 
	}
	
	private ParameterValueGroup getParameterValueGroupForCylindrical(DefaultMathTransformFactory factory, String geotoolsProjectionName, 
			double[] angleValueArray) {
		ParameterValueGroup parameters = null; 
    	
        try {
            parameters = factory.getDefaultParameters(geotoolsProjectionName);

            for (Object obj : parameters.values()) {
                Parameter<?> param = (Parameter<?>) obj;
                String paramName = param.getDescriptor().getName().getCode();
                Object paramValue = param.getValue();

                if (("semi_major".equals(paramName) || "semi_minor"
                        .equals(paramName))
                        && (paramValue == null)) {
                    paramValue = MapUtil.AWIPS_EARTH_RADIUS;
                    param.setValue(paramValue); 
                } else if("latitude_of_origin".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[0])) {
                		paramValue = angleValueArray[0]; 
                        param.setValue(paramValue); 
                	}
                } else if("central_meridian".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[1]))  { 
                		paramValue = angleValueArray[1]; 
                		param.setValue(paramValue); 
                	}
                }
            }
        } catch (NoSuchIdentifierException e) {
            statusHandler.handle(Priority.PROBLEM, 
                    "Unexpected error retrieving parameters for projection", e);
        }
        
        return parameters; 
	}
	
	private ParameterValueGroup getParameterValueGroupForAzm(DefaultMathTransformFactory factory, String geotoolsProjectionName, 
			double[] angleValueArray) {
		ParameterValueGroup parameters = null; 
    	
        try {
            parameters = factory.getDefaultParameters(geotoolsProjectionName);

            for (Object obj : parameters.values()) {
                Parameter<?> param = (Parameter<?>) obj;
                String paramName = param.getDescriptor().getName().getCode();
                Object paramValue = param.getValue();

                if (("semi_major".equals(paramName) || "semi_minor"
                        .equals(paramName))
                        && (paramValue == null)) {
                    paramValue = MapUtil.AWIPS_EARTH_RADIUS;
                    param.setValue(paramValue); 
                } else if("latitude_of_origin".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[0])) { //get angle 1
                		paramValue = angleValueArray[0]; 
                        param.setValue(paramValue); 
                	}
                } else if("central_meridian".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[1])) { //get angle 2
                		paramValue = angleValueArray[1]; 
                        param.setValue(paramValue); 
                	}
                } else if("latitude_of_center".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[0]))  {
                		paramValue = angleValueArray[0]; 
                        param.setValue(paramValue); 
                	}
                } else if("longitude_of_center".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[1])) { //get angle 2
                		paramValue = angleValueArray[1]; 
                        param.setValue(paramValue); 
                	}
                }

            }
        } catch (NoSuchIdentifierException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unexpected error retrieving parameters for projection", e);
        }
        
        return parameters; 
	}
	
	private ParameterValueGroup getParameterValueGroupForCon(DefaultMathTransformFactory factory, String geotoolsProjectionName, 
			double[] angleValueArray) {
		ParameterValueGroup parameters = null; 
    	
        try {
            parameters = factory.getDefaultParameters(geotoolsProjectionName);

            for (Object obj : parameters.values()) {
                Parameter<?> param = (Parameter<?>) obj;
                String paramName = param.getDescriptor().getName().getCode();
                Object paramValue = param.getValue();

                if (("semi_major".equals(paramName) || "semi_minor"
                        .equals(paramName))
                        && (paramValue == null)) {
                    paramValue = MapUtil.AWIPS_EARTH_RADIUS;
                    param.setValue(paramValue); 
                } else if("latitude_of_origin".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[0])) {
                		paramValue = angleValueArray[0]; 
                        param.setValue(paramValue); 
                	}
                } else if("central_meridian".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[1]))  {
                		paramValue = angleValueArray[1]; 
                        param.setValue(paramValue); 
                	}
                } else if("standard_parallel_1".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[2])) {
                		paramValue = angleValueArray[2]; 
                        param.setValue(paramValue); 
                	}
                } 

            }

        } catch (NoSuchIdentifierException e) {
            statusHandler.handle(Priority.PROBLEM, 
                    "Unexpected error retrieving parameters for projection", e);
        }
        
        return parameters; 
	}
	
    private double[] createDoubleArray(int arraySize) {
    	double[] doubleArray = new double[arraySize]; 
    	for(int i=0; i<doubleArray.length; i++)
    		doubleArray[i] = Double.NaN; 
    	return doubleArray; 
    }

//	private boolean isStringEmpty(String str) {
//		boolean valid = true; 
//		if(str != null && str.trim().length() != 0) 
//			valid = false; 
//		return valid; 
//	}


}
