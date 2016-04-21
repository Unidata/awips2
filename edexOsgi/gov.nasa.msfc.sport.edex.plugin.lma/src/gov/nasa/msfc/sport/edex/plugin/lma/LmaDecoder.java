package gov.nasa.msfc.sport.edex.plugin.lma;


import gov.nasa.msfc.sport.edex.plugin.lma.util.LMAVarsDict;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.geotools.geometry.GeneralDirectPosition;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.operation.MathTransform;

import ucar.ma2.Array;
import ucar.ma2.IndexIterator;
import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;
import ucar.units.SI;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.lookup.GridCoverageLookup;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.lookup.ParameterLookup;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.ArraysUtil;
import com.raytheon.uf.edex.plugin.level.dao.LevelDao;


/**
 * The Class LmaDecoder decodes the files created by the lightning mapping arrays that is outputted in netcdf format. Files are
 * in netcdf 3 format. And contain the headers that have needed information.
 */
public class LmaDecoder {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LmaDecoder.class);
    /** The grid coverage lookup is a utility class that helps to define grid of the lma output data. */
    public GridCoverageLookup gridCoverageLookup;
    
    /** The level dao allows looking up of the levels. */
    public LevelDao levelDao;
    
    /**
     * Decode the LMA netcdf 3 files. These files contain gridded data at 17 vertical levels. The first level is the sum of all levels.
     *
     * @param fileInput the file input
     * @return the plugin data object[]
     * @throws Exception the exception
     */
    public PluginDataObject[] decode(File fileInput) throws Exception {
        //Create an empty records to hold the data once decoded.
        List<GridRecord> recordsList = new ArrayList<GridRecord>();
        
        /** The variable dictionary used to check which variables are supported by the ingest **/
        LMAVarsDict lmaVarsDict = LMAVarsDict.getInstance();
        
        /** The Param lookup  allows looking up of the parameters in the database. */
        ParameterLookup paramLookup = ParameterLookup.getInstance();
        
        //Open the netcdf file for reading.
        NetcdfFile file = NetcdfFile.open(fileInput.getAbsolutePath());
        try {
		    //Read in time related attributes
            Attribute validTimeAtt = file.findGlobalAttribute("validTimeInMilliSecondsSinceEpoch");
            Attribute timeSpanAtt = file.findGlobalAttribute("timespan");
            int timespan = timeSpanAtt.getNumericValue().intValue();
            Date date = new Date();
            date.setTime(validTimeAtt.getNumericValue().longValue());
            TimeRange range = new TimeRange(date.getTime(),date.getTime()+(timespan*1000)-1);
            DataTime dataTime = new DataTime(date);

            //Read global attributes about the grid itself for use later.
//            Attribute lowerLeftLatAtt = file.findGlobalAttribute("lllat");
//            Attribute lowerLeftLonAtt = file.findGlobalAttribute("lllon");
//            Attribute upperRightLatAtt = file.findGlobalAttribute("urlat");
//            Attribute upperRightLonAtt = file.findGlobalAttribute("urlon");
//            float upperLeftLon= lowerLeftLonAtt.getNumericValue().floatValue();
//            float upperLeftLat= upperRightLatAtt.getNumericValue().floatValue();
//            float lowerRightLon= upperRightLonAtt.getNumericValue().floatValue();
//            float lowerRightLat= lowerLeftLatAtt.getNumericValue().floatValue(); 

            //Read Global attributes to help with define grid
            Attribute centerLatAtt = file.findGlobalAttribute("clat");
            float centerLat= centerLatAtt.getNumericValue().floatValue(); 
            Attribute centerLonAtt = file.findGlobalAttribute("clon");
            float centerLon= centerLonAtt.getNumericValue().floatValue(); 
            Attribute xresAtt = file.findGlobalAttribute("xres_m");
            float dx = xresAtt.getNumericValue().floatValue(); 
            Attribute yresAtt = file.findGlobalAttribute("yres_m");
            float dy= yresAtt.getNumericValue().floatValue();
            int nx = file.findDimension("x").getLength();
            int ny = file.findDimension("y").getLength();
		    //Construct the grid coverage with attributes 
            GridCoverage cov =createMapCoverage(centerLon, centerLat, nx, ny, dx, dy);
		    //Lookup Coverage to make sure it does not already exist in the db.
            cov =  gridCoverageLookup.getCoverage(cov, true);
            //Get network name for the LMA.
            Attribute networkNameAtt = file.findGlobalAttribute("networkName");

            //Find variables
            List<Variable> vars = file.getVariables();
            Iterator<Variable> varIter = vars.iterator();
            ArrayList<String> varsToProcessList = new ArrayList<String>();
		    //Compare the variables in the files to the variables we accept based on localization file
            while (varIter.hasNext()) {
                Variable tempVar = varIter.next();
                if (lmaVarsDict.isVarSupported(tempVar.getFullName())) {
                        varsToProcessList.add(tempVar.getFullName());
                }
            }

            int sizeOfVariablesToProcess =varsToProcessList.size();

            String[] listOfVariablesToProcess = new String[sizeOfVariablesToProcess];
            varsToProcessList.toArray(listOfVariablesToProcess);
            
            //Iterate over variables and create GridRecords for each variable and level
            for (int i = 0; i<sizeOfVariablesToProcess; ++i ) {
                Variable v = file.findVariable(listOfVariablesToProcess[i]);
                int nz = v.getDimension(1).getLength();
                for (int j=0; j< nz; ++j) {
                    GridRecord record = new GridRecord();
                    record.setPersistenceTime(TimeUtil.newDate());
                    record.setDataTime(dataTime);
                    ///If the level is the first then it is the SUM level.
                    if (j==0) {
                        MasterLevel masterLevel = levelDao.lookupMasterLevel(new MasterLevel("SFC"), true);
                        Level level = new Level();
                        level.setLevelonevalue(0);
                        level.setMasterLevel(masterLevel);
                        level = levelDao.lookupLevel(level);
                        record.setLevel(level);
                    } else {
                        MasterLevel masterLevel = levelDao.lookupMasterLevel(new MasterLevel("FHAG"), true);
                        masterLevel.setUnitString("m");
                        Level level = new Level();
                        level.setLevelonevalue((j+1)*1000.);
                        level.setMasterLevel(masterLevel);
                        level = levelDao.lookupLevel(level);
                        record.setLevel(level);
                    }
                    record.setDatasetId(networkNameAtt.getStringValue());
                    record.setLocation(cov);
                    String id = lmaVarsDict.getVarForStorage(v.getFullName());
                    Parameter param = null;
                    try {
                        param = paramLookup.getParameter(id);
                    } catch (Exception e) {
                        
                    }
                    if (param == null) {
                        statusHandler.info("Need to create new parameter "+id+","+lmaVarsDict.getVarNameForStorage(v.getFullName()));
                        param = new Parameter(id);
                        param.setName(lmaVarsDict.getVarNameForStorage(v.getFullName()));
                        
                    }
                    record.setParameter(param);
                    record.setLocation(cov);
                    
                    //Need to transform the array to a float array, then flip the array.
                    Array array = v.read(); 
                    int total = ny*nx;
                    float[] endArray = new float[total];
                    int countlocal = 0;
                	IndexIterator ii = array.getIndexIterator();
                    while (ii.hasNext() && countlocal < total) {
                        endArray[countlocal]= ii.getFloatNext();
                    	              ++countlocal;
                     }
                    ///Need to flip the array to match how it is stored in AWIPS II versus netcdf.
                    ArraysUtil.flipHoriz(endArray, ny, nx);
                   	record.setMessageData(endArray);
                   	recordsList.add(record);
                }
            }
           

        } catch (Exception e) {
            statusHandler.warn("Problem processing lma data: "+e);
        }
        try {
            file.close();
        } catch (Exception e){
            statusHandler.warn("Problem closing the lma netcdf file: "+e);
        }
        if (recordsList.size() >0) {
            GridRecord[] records = new GridRecord[recordsList.size()];
            recordsList.toArray(records);
            return records;
        } else {
            return null;
        }

    }

    /**
     * Creates the map coverage.
     *
     * @param centerLon the center longitude of the lma grid
     * @param centerLat the center latitude of the lma grid
     * @param nx the number of x grid cells
     * @param ny the number of y grid cells
     * @param dx the distance in meters of each y grid cell
     * @param dy the distance in meters in each x grid cell
     * @return the grid coverage returned from the input information. This grid coverage describes the grid that data is contained.
     * @throws Exception the exception
     */
    private  GridCoverage createMapCoverage(

    Float centerLon, Float centerLat, Integer nx, Integer ny,

    Float dx, Float dy)

    throws Exception {


        ProjectedCRS crs = null;

        double nxhalf = (nx / 2.0);

        double nyhalf = (ny / 2.0);

        double distancex = nxhalf * dx;

        double distancey = nyhalf * dy;

        // Create wkt for the lambert azimuthal projection

        String wkt = "PROJCS[\"Lambert_Azimuthal_Equal_Area\","+
        "GEOGCS[\"GCS_WGS_1984\",DATUM[\"WGS_1984\",SPHEROID[\"WGS_1984\",6378137,298.257223563]],"+
        "PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]],"+
        "PROJECTION[\"Lambert_Azimuthal_Equal_Area\"]," +
        "PARAMETER[\"False_Easting\",0]," +
        "PARAMETER[\"False_Northing\",0]," +
        "PARAMETER[\"Central_Meridian\"," + centerLon + "]," +
        "PARAMETER[\"Latitude_Of_Origin\"," + centerLat + "]," +
        "UNIT[\"Meter\",1]," +
        "AUTHORITY[\"EPSG\",\"102020\"]]";

        // Create coordinate reference systems, the lambert projection
        // containing the grid and the WGS84 grid to get out the lat lon.

        crs = (ProjectedCRS) CRS.parseWKT(wkt);

        CoordinateReferenceSystem wgs84 = DefaultGeographicCRS.WGS84;

        // Calculate the math transform between the lambert grid and the wgs84.

        MathTransform transform = CRS.findMathTransform(crs, wgs84);

        // Setup the positions in the lambert grid for each of the corner
        // points, upper left, lower left, upper right, and lower right

        GeneralDirectPosition posll = new GeneralDirectPosition(-distancex,
                -distancey);

        // Create positions to hold the transformed coordinated for each of the
        // corners.

        GeneralDirectPosition lowerLeft = new GeneralDirectPosition(0, 0);
        // Perform the transformation of each of the points.

        transform.transform(posll, lowerLeft);

        LambertConformalGridCoverage mapCoverage = new LambertConformalGridCoverage();
        mapCoverage.setMajorAxis(6378137);
        mapCoverage.setMinorAxis(6356752.314245);
        mapCoverage.setLatin1(centerLat);
        mapCoverage.setLatin2(centerLat);
        mapCoverage.setLov(centerLon);
        mapCoverage.setLa1(lowerLeft.getOrdinate(1));
        mapCoverage.setLo1(lowerLeft.getOrdinate(0));
        mapCoverage.setNx(nx);
        mapCoverage.setNy(ny);
        mapCoverage.setDx(dx);
        mapCoverage.setDy(dy);
        mapCoverage.setSpacingUnit(SI.METRE.getSymbol());
        mapCoverage.setFirstGridPointCorner(Corner.LowerLeft);

        return mapCoverage;

    }
    
    /**
     * Sets the grid coverage lookup.
     *
     * @param gridCoverageLookup the new grid coverage lookup
     */
    public void setGridCoverageLookup(GridCoverageLookup gridCoverageLookup) {
        this.gridCoverageLookup = gridCoverageLookup;
    }
    
    /**
     * Sets the level dao. Level Dao is used to lookup the proper level for the ingested data.
     *
     * @param levelDao the new level dao
     */
    public void setLevelDao(LevelDao levelDao) {
        this.levelDao = levelDao;
    }


}
