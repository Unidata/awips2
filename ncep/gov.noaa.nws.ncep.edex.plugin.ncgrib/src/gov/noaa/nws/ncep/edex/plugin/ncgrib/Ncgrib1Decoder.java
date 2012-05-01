/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package gov.noaa.nws.ncep.edex.plugin.ncgrib;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import ucar.grib.GribNumbers;
import ucar.grib.NotSupportedException;
import ucar.grib.grib1.Grib1Data;
import ucar.grib.grib1.Grib1GDSVariables;
import ucar.grib.grib1.Grib1GridDefinitionSection;
import ucar.grib.grib1.Grib1Input;
import ucar.grib.grib1.Grib1Pds;
import ucar.grib.grib1.Grib1ProductDefinitionSection;
import ucar.grib.grib1.Grib1Record;
import ucar.grib.grib1.GribPDSParamTable;
import ucar.grid.GridParameter;
import ucar.unidata.io.RandomAccessFile;

import gov.noaa.nws.ncep.edex.plugin.ncgrib.dao.NcgribDao;
import gov.noaa.nws.ncep.edex.plugin.ncgrib.spatial.NcgribSpatialCache;
import gov.noaa.nws.ncep.edex.plugin.ncgrib.util.NcgribModelCache;
import gov.noaa.nws.ncep.edex.util.ncgrib.Ncgrib1TableMap;
import gov.noaa.nws.ncep.edex.util.ncgrib.NcgribModelLookup;
import gov.noaa.nws.ncep.edex.util.ncgrib.NcgribParamTranslator;
import gov.noaa.nws.ncep.edex.util.ncgrib.NcgribTableLookup;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribLevel;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribModel;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribParameter;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.NcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.LambertConformalNcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.LatLonNcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.MercatorNcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.spatial.projections.PolarStereoNcgridCoverage;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.subgrid.SubNcgrid;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.util.NcgridModel;
import gov.noaa.nws.ncep.edex.util.grib1vcrd.Grib1Vcrd;
import gov.noaa.nws.ncep.edex.util.grib1vcrd.Grib1VcrdTable;
import gov.noaa.nws.ncep.edex.util.grib2vars.Grib2VarsTableLookup;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Grib decoder implementation for decoding grib version 1 files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/11/10      4758        bphillip    Initial Creation
 * 9/08/10                  X. Guo      Add new column
 * 11/02/11                 X. Guo      Check octet size for isEnsmble()
 * 3/2012					T. Lee		Changed perturbation number to String
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class Ncgrib1Decoder extends AbstractDecoder {
	private static final transient IUFStatusHandler statusHandler = UFStatus
    .getHandler(Ncgrib1Decoder.class);
	
    /** Missing value string */
    private static final String MISSING = "Missing";

    /** Set of Time range types for accumulations and averages */
    private static final Set<Integer> AVG_ACCUM_LIST = new HashSet<Integer>();

    //private String traceId = "";
	private NcgribDao dao;
	
	private String fileName="";
	
	private boolean addCol = false;

    private static final int[] fourtyOne = new int[] { 1, 1, 2, 3, 2, 3, 2, 3,
        2, 3, 2, 3 };

    private static final int[] fourtyTwo = new int[] { 1, 2, 1, 1, 2, 2, 3, 3,
        4, 4, 5, 5 };

    private static final String[] perturbation = new String[] { "1", "2", "3", "4", "5", "6", "7",
        "8", "9", "10", "11", "12" };

    static {
        AVG_ACCUM_LIST.add(3);
        AVG_ACCUM_LIST.add(4);
        AVG_ACCUM_LIST.add(6);
        AVG_ACCUM_LIST.add(7);
        
        IPathManager pm = PathManagerFactory.getPathManager();
        String ucarUserFile = pm.getFile(
                pm.getContext(LocalizationType.EDEX_STATIC,
                        LocalizationLevel.BASE), "/ncgrib/ucar/userTables.lst")
                .getPath();
        try {
            GribPDSParamTable.addParameterUserLookup(ucarUserFile);
        } catch (IOException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Error reading user parameter tables for ucar grib decoder",
                            e);
        }
    }

    /**
     * Creates a new Grib1Decoder
     */
    public Ncgrib1Decoder() {
        super();
    }

    public NcgribDao getDao() {
		return dao;
	}

	public void setDao(NcgribDao dao) {
		this.dao = dao;
	}
	
    /**
     * Decodes the grib file provided.
     * 
     * @param gribFileName
     *            The name of the file to be decoded
     * @return The decoded GribRecords
     * @throws GribException
     *             If decoding the file fails or encounters problems
     */
    public NcgribRecord[] decode(String gribFileName) throws GribException {
        File gribFile = new File(gribFileName);
        fileName = gribFileName;
        RandomAccessFile raf = null;
        
        //System.out.println (" grib file name =" + gribFileName);

        try {
            try {
                raf = new RandomAccessFile(gribFile.getAbsolutePath(), "r");
            } catch (IOException e) {
                throw new GribException(
                        "Unable to create RandomAccessFile for grib file: ["
                                + gribFile + "]");
            }
            raf.order(RandomAccessFile.BIG_ENDIAN);
            Grib1Input g1i = new Grib1Input(raf);
            try {
                g1i.scan(false, false);
            } catch (Exception e) {
                throw new GribException("Error scanning grib 1 file: ["
                        + gribFile + "]");
            }
            ArrayList<Grib1Record> records = g1i.getRecords();
//            NcgribRecord[] gribRecords = new NcgribRecord[records.size()];
            List<NcgribRecord> gribRecords = new ArrayList<NcgribRecord>();
            for (int i = 0; i < records.size(); i++) {
            	NcgribRecord rec = decodeRecord((Grib1Record) records.get(i), raf);
            	if ( rec != null ) {
            		gribRecords.add(rec);
            	}
            }
            return gribRecords.toArray(new NcgribRecord[] {});
        } finally {
            if (raf != null) {
                try {
                    raf.close();
                } catch (IOException e) {
                    throw new GribException(
                            "Failed to close RandomAccessFile for grib file: ["
                                    + gribFile + "]", e);
                }
            }
        }
    }

    /**
     * Decodes a single record from the grib file
     * 
     * @param rec
     *            The record to decode
     * @param raf
     *            The file object
     * @return The decoded GribRecord
     * @throws GribException
     *             If the record cannot be decoded properly
     */
    private NcgribRecord decodeRecord(Grib1Record rec, RandomAccessFile raf)
            throws GribException {

    	int discipline=255,category=255,pid=255;
        NcgribRecord retVal = new NcgribRecord();

        // Extract the sections from the grib record
        Grib1ProductDefinitionSection pds = rec.getPDS();
        Grib1GridDefinitionSection gds = rec.getGDS();
        Grib1Pds pdsVars = pds.getPdsVars();
        Grib1GDSVariables gdsVars = gds.getGdsVars();

        // Initialize some common values
        int centerid = pdsVars.getCenter();
        int subcenterid = pdsVars.getSubCenter();
        String parameterAbbreviation = null;
        String parameterName = null;
        String parameterUnit = null;

        // Some centers use other center's parameter tables so we need to check
        // for that
//        System.out.println ("==centerid:" + centerid + " subcenterid:" + subcenterid + " pdsVars.getTableVersion():" + pdsVars.getTableVersion());
        int[] tableValue = Ncgrib1TableMap.getInstance().getTableAlias(centerid,
                subcenterid, pdsVars.getParameterTableVersion());
        int centerAlias = tableValue[0];
        int subcenterAlias = tableValue[1];
        int tableAlias = tableValue[2];
        int vcrdid = pdsVars.getLevelType1(); 
 
        /*
         * Decodes the parameter information from the record. An attempt is
         * first made to map the grib 1 parameter to the equivalent grib 2
         * parameter. If this cannot be successfully executed, the grib 1
         * parameter will be used from the parameter tables contained in the
         * unidata decoder.
         */
        NcgribParameter parameter = NcgribTableLookup.getInstance()
                .getNcgrib2Parameter(centerAlias, subcenterAlias, tableAlias,
                        pdsVars.getParameterNumber());

        if (parameter == null || parameter.getName().equals(MISSING)) {
            try {
                logger
                        .warn("Unable to map Grib 1 parameter to equivalent Grib 2 parameter for center ["
                                + centerid
                                + "] subcenter ["
                                + subcenterid
                                + "] table number ["
                                + pdsVars.getParameterTableVersion()
                                + "] parameter number ["
                                + pdsVars.getParameterNumber()
                                + "]  Using grib 1 parameter mapping");
                GridParameter param = GribPDSParamTable.getParameterTable(
                        centerid, subcenterid, pdsVars.getParameterTableVersion())
                        .getParameter(pdsVars.getParameterNumber());
                parameterName = param.getDescription();
                parameterAbbreviation = param.getName();
                parameterUnit = param.getUnit();
            } catch (NotSupportedException e) {
                throw new GribException("Error getting grib 1 parameter", e);
            }
        } else {
            parameterName = parameter.getName();
            parameterUnit = parameter.getUnit();
            if (parameter.getD2dAbbrev() == null) {
                parameterAbbreviation = parameter.getAbbreviation();
            } else {
                parameterAbbreviation = parameter.getD2dAbbrev();
            }
            discipline = parameter.getDiscipline();
            category = parameter.getCategory();
            pid = parameter.getNumber();
        }

        // Gets the spatial information
        NcgridCoverage gridCoverage = getGridCoverage(gdsVars);

        // Create the GribModel object with the appropriate values from the grib
        // record
        NcgribModel model = new NcgribModel();
        model.setLocation(gridCoverage);
        model.setCenterid(centerid);
        model.setSubcenterid(subcenterid);
        model.setBackGenprocess(GribNumbers.UNDEFINED);
        model.setGenprocess(pdsVars.getGenProcessId());
        model.setParameterName(parameterName);
        model.setParameterAbbreviation(parameterAbbreviation);
        model.setParameterUnit(parameterUnit);
       
        // unidata does not handle isEnsemble call when
        // octet size is less than 40.
        if (pdsVars.getLength() > 40 && pdsVars.isEnsemble()) {
            model.setNumForecasts(pdsVars.getNumberForecasts());
            model.setTypeEnsemble(pdsVars.getType());
            // rcg: added code to get perturbation
            int pos41 = pdsVars.getOctet(41);
            int pos42 = pdsVars.getOctet(42);
            String pert = String.valueOf(pdsVars.getID());
            for (int i = 0; i < perturbation.length; i++) {
                if (pos41 == fourtyOne[i] && pos42 == fourtyTwo[i]) {
                    pert = perturbation[i];
                    break;
                }
            }
            model.setPerturbationNumber(pert);
//            model.setPerturbationNumber(pdsVars.getPerturbationNumber());
        } else {
            model.setNumForecasts(null);
            model.setTypeEnsemble(null);
            model.setPerturbationNumber(null);
        }
        model.setGridid(gridCoverage.getName());

        createModelName(model);

        // Get the level information
        float glevel1 = (float) pdsVars.getLevelValue1();
        float glevel2 = (float) pdsVars.getLevelValue2();
        if ( vcrdid == 117 ) {
        	if ( glevel1 >= 32768 ) glevel1 = 32768 - glevel1;
        }
        float[] levelMetadata = this.convertGrib1LevelInfo(pdsVars
                .getLevelType1(), (float) pdsVars
                .getLevelValue1(), pdsVars
                .getLevelType2(), (float) pdsVars
                .getLevelValue2());
        getLevelInfo(model, centerid, subcenterid, levelMetadata[0],
                levelMetadata[1], levelMetadata[2], levelMetadata[3],
                levelMetadata[4], levelMetadata[5]);
        //int vcrd1 = (int)levelMetadata[0];
        int vcrd2 = (int)levelMetadata[3];

        // Construct the DataTime
        GregorianCalendar refTime = new GregorianCalendar();
        refTime.setTimeInMillis(pdsVars.getReferenceTime());
        int forecastTime = convertToSeconds(pdsVars.getForecastTime(), pdsVars
                .getTimeUnit());
        DataTime dataTime = constructDataTime(refTime, forecastTime,
                getTimeInformation(refTime, pdsVars.getTimeRangeIndicator(), pdsVars
                        .getP1(), pdsVars.getP2()), model);

        /*
         * Extract the data values from the file. The AVG_ACCUM_LIST is checked
         * to see if this is an accumulation or average grid being initialized.
         * A check is also made for thinned grids.
         */
        Grib1Data gd = new Grib1Data(raf);
        float[] data = null;
        
        try {
            boolean bmsPresent = pdsVars.bmsExists();
            int scanMode = gdsVars.getScanMode();
            int timeRange = pdsVars.getTimeRangeIndicator();

            // Check for initialization of average or accumulation parameters
            if ((AVG_ACCUM_LIST.contains(timeRange) && dataTime
                    .getValidPeriod().getDuration() == 0)) {
                data = new float[gdsVars.getNx() * gdsVars.getNy()];
            } else {
                data = gd.getData(rec.getDataOffset() - gdsVars.getLength(),
                        rec.getDataOffset(), pdsVars.getDecimalScale(), pdsVars
                                .bmsExists());
            }
            correctForScanMode(data, gdsVars.getNx(), gdsVars.getNy(),
                    bmsPresent, scanMode);
            if ( getAddCol()) {
            	float []dataArry = this.addOneColumnDataTo1D(data, gdsVars.getNx(),gdsVars.getNy() );
            	retVal.setMessageData(dataArry);
            }
            else
            	retVal.setMessageData(data);
        } catch (IOException e) {
            throw new GribException("Error getting data from grib file", e);
        }

        // Check for subgridding
        String modelName = model.getModelName();
        NcgridCoverage subCoverage = NcgribSpatialCache.getInstance()
                .getSubGridCoverage(modelName);

        if (subCoverage != null) {
            SubNcgrid subGrid = NcgribSpatialCache.getInstance().getSubGrid(
                    modelName);
            int nx  = gridCoverage.getNx();
            if ( getAddCol()) {
            	nx = nx - 1;
            }
            // resize the data array
            float[][] dataArray = this.resizeDataTo2D(data, nx,
            		gridCoverage.getNy());
            dataArray = this.subGrid(dataArray, subGrid.getStartX(), subGrid
                    .getStartY(), subGrid.getNX(), subGrid.getNY());
            data = this.resizeDataTo1D(dataArray, subGrid.getNY(), subGrid
                    .getNX());
            if (getAddCol()) {
            	float [] dataArray1 = this.addOneColumnDataTo1D(data, subGrid.getNX(), subGrid.getNY());
            	retVal.setMessageData(dataArray1);
            }
            else
                retVal.setMessageData(data);
            model.setLocation(subCoverage);
        }
        setAddCol(false);

        String newAbbr = NcgribParamTranslator.getInstance().translateParameter(
                1, model, dataTime);

        if (newAbbr == null) {
            if (!model.getParameterName().equals(MISSING)
                    && dataTime.getValidPeriod().getDuration() > 0) {
                model.setParameterAbbreviation(model.getParameterAbbreviation()
                        + String.valueOf(dataTime.getValidPeriod()
                                .getDuration() / 3600000) + "hr");
            }
        } else {
            model.setParameterAbbreviation(newAbbr);
        }
        
        if (!model.getParameterName().equals(MISSING)) {
        	if ( modelName.toUpperCase().equals("GFS")) {
        		model.generateId(fileName);
        	}
            try {
                model = NcgribModelCache.getInstance().getModel(model);
            } catch (DataAccessLayerException e) {
                throw new GribException(
                        "Unable to get model info from the cache!", e);
            }
        }

        retVal.setModelInfo(model);
        retVal.setPluginName("ncgrib");
        retVal.setPersistenceTime(new Date());
        retVal.setDataTime(dataTime);
        retVal.setResCompFlags(gdsVars.getResolution());
        retVal.setModelName(model.getModelName());
        String []fileTokens = fileName.split("/");
        String flName="";
        if ( fileTokens.length > 0 ) {
        	flName = fileTokens[fileTokens.length-1];
        }
        retVal.setFileName(flName);
        String eventName=flName;
        int index = flName.indexOf(".");
        
        if ( index > 0) {
        	eventName = flName.substring(0, index);
        }
        retVal.setEventName(eventName);
        retVal.setDiscipline(discipline);
        retVal.setCategory(category);
        retVal.setParameterId(pid);
        retVal.setProcessedDataType(pdsVars.getGenProcessId());
        retVal.setDecodedLevel1((float)pdsVars.getLevelValue1());
        retVal.setDecodedLevel2((float)pdsVars.getLevelValue2());
        retVal.setVcrdId1(vcrdid);
        retVal.setVcrdId2(vcrdid);
        retVal.setGridVersion(1);

        String vcord = "NONE";

        Grib1Vcrd grib1Vcrd = Grib1VcrdTable.getGrib1VcrdById(vcrdid);
        if ( grib1Vcrd != null ) {

        	vcord = grib1Vcrd.getGnam();
        	double scale = Double.parseDouble( grib1Vcrd.getScale() );
        	scale = Math.pow( 10., scale);
        	
            if (glevel1 != 0 && scale != 1 ) {
                retVal.setGlevel1((int)Math.round(glevel1 * scale));
            }
            else {
                retVal.setGlevel1(Math.round(glevel1));
            }
                
            if ( vcrd2 == 255 ) {
                retVal.setGlevel2(-9999);
            }
            else {
                if (glevel2 != 0 && scale != 1 ) {
                    retVal.setGlevel2((int)Math.round(glevel2 * scale));
                }
                else {
                    retVal.setGlevel2( Math.round(glevel2) );
                }
            }
            
        }
        else {
          retVal.setGlevel1((int)model.getLevel().getLevelonevalue());
          retVal.setGlevel2((int)model.getLevel().getLeveltwovalue());
        }
        retVal.setVcord(vcord);
        //retVal.setScale(scale);        
        String parm = Grib2VarsTableLookup.getVarGnam4Grib1 (discipline,category,pid);
        retVal.setParm(parm);
        
        
        
        // Special case handling for ffg grids
        try {
            if (model.getCenterid() == 9) {
                retVal.constructDataURI();
                NcgribDao gribDao = (NcgribDao) PluginFactory.getInstance()
                        .getPluginDao("ncgrib");
                QueryResult result = (QueryResult) gribDao
                        .executeNativeSql("select max(gridVersion) from awips.grib where datauri like '"
                                + retVal.getDataURI().substring(0,
                                        retVal.getDataURI().lastIndexOf("/"))
                                + "%'");
                int resultCount = result.getResultCount();
                if (resultCount == 1 && result.getRowColumnValue(0, 0) != null) {
                    int newVersion = ((Integer) result.getRowColumnValue(0, 0)) + 1;
                    retVal.setGridVersion(newVersion);
                }
                retVal.setDataURI(null);
            }
        } catch (Exception e) {
            throw new GribException("Error decoding FFG grid", e);
        }

        try {
            retVal.constructDataURI();
        } catch (PluginException e) {
            throw new GribException("Error constructing grib dataURI", e);
        }
        return retVal;
    }

    /**
     * Resizes a 1-D data array into a 2-D array based on the provided row and
     * column count
     * 
     * @param data
     *            The 1-D array of data
     * @param columnCount
     *            The number of columns to map the data to
     * @param rowCount
     *            The number of rows to map the data to
     * @return The 2-D array of data
     */
    private float[][] resizeDataTo2D(float[] data, int columnCount, int rowCount) {
        float[][] newGrid = new float[rowCount][columnCount];

        for (int row = 0; row < rowCount; row++) {
            for (int column = 0; column < columnCount; column++) {
                newGrid[row][column] = data[row * columnCount + column];
            }
        }

        return newGrid;
    }

    /**
     * Resizes a 1-D data array into a 2-D array based on the provided row and
     * column count and add one column for each row, then convert to 1-D array. 
     * 
     * @param data
     *            The 1-D array of data
     * @param columnCount
     *            The number of columns to map the data to
     * @param rowCount
     *            The number of rows to map the data to
     * @return The 1-D array of data
     */
    private float[] addOneColumnDataTo1D(float[] data, int columnCount, int rowCount) {
        float[][] newGrid = new float[rowCount][columnCount+1];

        for (int row = 0; row < rowCount; row++) {
            for (int column = 0; column < columnCount; column++) {
                newGrid[row][column] = data[row * columnCount + column];
            }
            newGrid[row][columnCount] = newGrid[row][0];
        }

        return resizeDataTo1D(newGrid,rowCount,columnCount+1);
    }

    /**
     * Resizes a 2-D array of data to a 1-D array
     * 
     * @param data
     *            The 2-D array of data
     * @param rowCount
     *            The number of rows in the 2-D data array
     * @param columnCount
     *            The number of columns in the 2-D data array
     * @return The 1-D array of data
     */
    private float[] resizeDataTo1D(float[][] data, int rowCount, int columnCount) {
        float[] newGrid = new float[rowCount * columnCount];

        for (int row = 0; row < rowCount; row++) {
            for (int column = 0; column < columnCount; column++) {
                newGrid[row * columnCount + column] = data[row][column];
            }
        }
        return newGrid;
    }

    /**
     * Extracts a sub-grid of data from a 2-D array of data
     * 
     * @param data
     *            The 2-D array of data
     * @param startColumn
     *            The start column of the sub-grid
     * @param startRow
     *            The start row of the sub-grid
     * @param columnCount
     *            The number of columns in the sub-grid
     * @param rowCount
     *            The number of rows in the sub-grid
     * @return The sub-grid of data
     */
    private float[][] subGrid(float[][] data, int startColumn, int startRow,
            int columnCount, int rowCount) {
        float[][] newGrid = new float[rowCount][columnCount];

        for (int row = startRow; row < rowCount + startRow; row++) {
            for (int column = startColumn; column < columnCount + startColumn; column++) {
                newGrid[row - startRow][column - startColumn] = data[row][column];
            }
        }
        return newGrid;
    }

    /**
     * Flips the grid according to the scan mode in the GDS
     * 
     * @param data
     *            The data
     * @param nx
     *            Nx value
     * @param ny
     *            Ny value
     * @param bmsExists
     *            If the bitmap need be applied
     * @param scanMode
     *            The scan mode
     */
    private void correctForScanMode(float[] data, int nx, int ny,
            boolean bmsExists, int scanMode) {
        for (int i = 0; i < data.length; i++) {
            if (bmsExists && data[i] == -9999) {
                data[i] = -999999;
            }
        }
        switch (scanMode) {
        case 0:
            break;
        case 64:
            Util.flipHoriz(data, ny, nx);
            break;
        case 128:
            Util.flipVert(data, ny, nx);
            break;
        case 192:
            Util.rotate180(data, ny, nx);
            break;
        }
    }

    /**
     * Gets the time ranges for the data
     * 
     * @param refTime
     *            The reference time
     * @param timeRange
     *            The time range indicator from the PDS section
     * @param p1
     *            The p1 value
     * @param p2
     *            The p2 value
     * @return An array containing the start time and end time
     */
    private Calendar[] getTimeInformation(Calendar refTime, int timeRange,
            int p1, int p2) {

        GregorianCalendar endTime = null;
        GregorianCalendar startTime = null;
        switch (timeRange) {
        case 2:
        case 3:
        case 4:
        case 5:
        case 51:
            startTime = (GregorianCalendar) refTime.clone();
            startTime.add(Calendar.HOUR_OF_DAY, p1);
            endTime = (GregorianCalendar) refTime.clone();
            endTime.add(Calendar.HOUR_OF_DAY, p2);
            break;
        case 6:
            startTime = (GregorianCalendar) refTime.clone();
            startTime.add(Calendar.HOUR_OF_DAY, p1 * -1);
            endTime = (GregorianCalendar) refTime.clone();
            endTime.add(Calendar.HOUR_OF_DAY, p2 * -1);
            break;
        case 7:
            startTime = (GregorianCalendar) refTime.clone();
            startTime.add(Calendar.HOUR_OF_DAY, p1 * -1);
            endTime = (GregorianCalendar) refTime.clone();
            endTime.add(Calendar.HOUR_OF_DAY, p2);
            break;
        default:
            break;
        }
        return new Calendar[] { startTime, endTime };
    }

    /**
     * Gets the spatial information from the grib record
     * 
     * @param gdsVars
     *            The gds values
     * @return The spatial information
     * @throws GribException
     *             If the spatial information cannot be determined correctly
     */
    private NcgridCoverage getGridCoverage(Grib1GDSVariables gdsVars)
            throws GribException {
        NcgridCoverage coverage = null;

        int gridType = gdsVars.getGdtn();
        int nx;
        float Lon1, Lon2,lo2;
        switch (gridType) {
        case 0:
            LatLonNcgridCoverage latLonCoverage = new LatLonNcgridCoverage();
            nx = gdsVars.getNx();
            lo2 = gdsVars.getLo2()+360.0F;
            Lon1 = gdsVars.getLo1() + lo2;
            Lon2 = lo2 + gdsVars.getDx();
            if ( Lon1 == 360.0 || Lon2 == 360.0 ) {
            	nx = nx + 1;
            	lo2 = Lon2;
            	setAddCol (true);
            }
            latLonCoverage.setNx(nx);
            latLonCoverage.setNy(gdsVars.getNy());
            latLonCoverage.setLa1(gdsVars.getLa1());
            latLonCoverage.setLo1(gdsVars.getLo1());
            latLonCoverage.setLa2(gdsVars.getLa2());
            latLonCoverage.setLo2(lo2);
            if (gdsVars.getGridUnits().equals("degrees")) {
                latLonCoverage.setSpacingUnit("degree");
                latLonCoverage.setDx(gdsVars.getDx());
                latLonCoverage.setDy(gdsVars.getDy());
            } else {
                latLonCoverage.setSpacingUnit("km");
                latLonCoverage.setDx(gdsVars.getDx() / 1000);
                latLonCoverage.setDy(gdsVars.getDy() / 1000);
            }
            if (latLonCoverage.getDy() == -9999) {
                latLonCoverage.setDy(latLonCoverage.getDx());
            }
            latLonCoverage.setId(latLonCoverage.hashCode());
            coverage = getGridFromCache(latLonCoverage);
            break;
        case 1:
            MercatorNcgridCoverage mercator = new MercatorNcgridCoverage();
            mercator.setMajorAxis(gdsVars.getMajorAxis() * 1000);
            mercator.setMinorAxis(gdsVars.getMinorAxis() * 1000);
            nx = gdsVars.getNx();
            lo2 = gdsVars.getLo2()+360.0F;
            Lon1 = gdsVars.getLo1() + lo2;
            Lon2 = lo2 + gdsVars.getDx();
            if ( Lon1 == 360.0 || Lon2 == 360.0 ) {
            	nx = nx + 1;
            	lo2 = Lon2;
            	setAddCol (true);
            }
            else {
            	lo2 = correctLon(lo2);
            }
            mercator.setNx(nx);
            mercator.setNy(gdsVars.getNy());
            mercator.setLa1(correctLat(gdsVars.getLa1()));
            mercator.setLo1(correctLon(gdsVars.getLo1()));
            mercator.setLa2(correctLat(gdsVars.getLa2()));
            mercator.setLo2(lo2);
            mercator.setLatin(correctLat(gdsVars.getLatin1()));
            if (gdsVars.getGridUnits().equals("degrees")) {
                mercator.setSpacingUnit("degree");
                mercator.setDx(gdsVars.getDx());
                mercator.setDy(gdsVars.getDy());
            } else {
                mercator.setSpacingUnit("km");
                mercator.setDx(gdsVars.getDx() / 1000);
                mercator.setDy(gdsVars.getDy() / 1000);
            }
            mercator.setId(mercator.hashCode());
            coverage = getGridFromCache(mercator);
            break;
        case 3:
            LambertConformalNcgridCoverage lambert = new LambertConformalNcgridCoverage();
            lambert.setMajorAxis(gdsVars.getMajorAxis() * 1000);
            lambert.setMinorAxis(gdsVars.getMinorAxis() * 1000);
            lambert.setNx(gdsVars.getNx());
            lambert.setNy(gdsVars.getNy());
            lambert.setLa1(correctLat(gdsVars.getLa1()));
            lambert.setLo1(correctLon(gdsVars.getLo1()));
            lambert.setLatin1(correctLat(gdsVars.getLatin1()));
            lambert.setLatin2(correctLat(gdsVars.getLatin2()));
            lambert.setLov(correctLon(gdsVars.getLoV()));
            if (gdsVars.getGridUnits().equals("degrees")) {
                lambert.setSpacingUnit("degree");
                lambert.setDx(gdsVars.getDx());
                lambert.setDy(gdsVars.getDy());
            } else {
                lambert.setSpacingUnit("km");
                lambert.setDx(gdsVars.getDx() / 1000);
                lambert.setDy(gdsVars.getDy() / 1000);
            }
            lambert.setId(lambert.hashCode());
            coverage = getGridFromCache(lambert);

            break;
        case 5:
            PolarStereoNcgridCoverage polar = new PolarStereoNcgridCoverage();
            polar.setMajorAxis(gdsVars.getMajorAxis() * 1000);
            polar.setMinorAxis(gdsVars.getMinorAxis() * 1000);
            polar.setNx(gdsVars.getNx());
            polar.setNy(gdsVars.getNy());
            polar.setLa1(correctLat(gdsVars.getLa1()));
            polar.setLo1(correctLon(gdsVars.getLo1()));
            polar.setLov(correctLon(gdsVars.getLoV()));

            if (gdsVars.getGridUnits().equals("degrees")) {
                polar.setSpacingUnit("degree");
                polar.setDx(gdsVars.getDx());
                polar.setDy(gdsVars.getDy());
            } else {
                polar.setSpacingUnit("km");
                polar.setDx(gdsVars.getDx() / 1000);
                polar.setDy(gdsVars.getDy() / 1000);
            }
            polar.setId(polar.hashCode());
            coverage = getGridFromCache(polar);
            break;
        default:
            break;
        }

        return coverage;
    }

    /**
     * Checks the cache for the GridCoverage object.
     * 
     * @param coverage
     *            The coverage to check the cache for
     * @return The GridCoverage object
     * @throws GribException
     *             If the GridCoverage object cannot be obtained
     */
    private NcgridCoverage getGridFromCache(NcgridCoverage coverage)
            throws GribException {

        NcgridCoverage grid = NcgribSpatialCache.getInstance().getGrid(coverage);

        if (grid == null) {
            NcgribSpatialCache.getInstance().putGrid(coverage, true);
            grid = NcgribSpatialCache.getInstance().getGrid(coverage.getId());
        }
        return grid;
    }

    /**
     * Creates a model name from a GribModel object
     * 
     * @param model
     *            The GribModel object to determine the name for
     */
    private void createModelName(NcgribModel model) {
        int center = model.getCenterid();
        int subcenter = model.getSubcenterid();

        String gridid = model.getGridid();
        int process = model.getGenprocess();
        String template = model.getTemplate();
        NcgridModel gridModel = NcgribModelLookup.getInstance().getModel(center,
                subcenter, gridid, process, template, model);
        String name = null;
        if (gridModel == null) {
            name = "UnknownModel:" + String.valueOf(center) + ":"
                    + String.valueOf(subcenter) + ":" + String.valueOf(process)
                    + ":" + gridid;
        } else {
            name = gridModel.getName();
        }
        model.setModelName(name);
    }

    /**
     * Constructs a data time object from the time information extracted from
     * the grib record
     * 
     * @param refTime
     *            The reference time
     * @param forecastTime
     *            The forecast time
     * @param times
     *            The start and end time of the data if applicable
     * @param model
     *            The GribModel object
     * @return The resulting DataTime object
     */
    private DataTime constructDataTime(Calendar refTime, int forecastTime,
            Calendar[] times, NcgribModel model) {
        DataTime dataTime = null;
        // Construct the DataTime object
        Calendar startTime = times[0];
        Calendar endTime = times[1];
        if (endTime == null) {
            dataTime = new DataTime(refTime, forecastTime);
        } else {
            TimeRange timeRange = new TimeRange(startTime.getTimeInMillis(),endTime.getTimeInMillis());
            dataTime = new DataTime(refTime, forecastTime, timeRange);
        }
        return dataTime;
    }

    /**
     * Converts a time value to seconds
     * 
     * @param value
     *            The value to convert
     * @param fromUnit
     *            The unit of the value from the PDS section
     * @return The time value in seconds
     */
    private int convertToSeconds(int value, int fromUnit) {

        int retVal = value;

        switch (fromUnit) {
        case 0:
            retVal = value * 60;
            break;
        case 1:
            retVal = value * 3600;
            break;
        case 2:
            retVal = value * 86400;
            break;
        case 3:
            retVal = value * 2678400;
            break;
        case 4:
            retVal = value * 977616000;
            break;
        case 5:
            retVal = value * 10 * 977616000;
            break;
        case 6:
            retVal = value * 30 * 977616000;
            break;
        case 7:
            retVal = value * 100 * 977616000;
            break;
        case 10:
            retVal = value * 3 * 3600;
            break;
        case 11:
            retVal = value * 6 * 3600;
            break;
        case 12:
            retVal = value * 12 * 3600;
            break;
        default:
            break;
        }

        return retVal;
    }

    /**
     * Converts a grib 1 level to the equivalent grib 2 representation
     * 
     * @param ltype1
     *            The type of level
     * @param lval1
     *            The value of the level
     * @return The converted level type information
     */
    private float[] convertGrib1LevelInfo(int ltype1, float lval1, int ltype2,
            float lval2) {
        float level1Type = ltype1;
        float level1Scale = 0;
        float level1Value = 0;

        float level2Type = 255;
        float level2Scale = 0;
        float level2Value = 0;

        switch (ltype1) {
        case 100:
            level1Value = lval1 * 100;
            break;
        case 101:
            level1Type = 100;
            level1Value = lval1 * 1000;
            level2Type = 100;
            level2Value = lval2 * 1000;
            break;
        case 102:
            level1Type = 101;
            break;
        case 103:
            level1Type = 102;
            level1Value = lval1;
            break;
        case 104:
            level1Type = 102;
            level1Value = lval1;
            level2Type = 102;
            level2Value = lval2;
            break;
        case 105:
            level1Type = 103;
            level1Value = lval1;
            break;
        case 106:
            level1Type = 103;
            level1Value = lval1 * 100;
            level2Type = 103;
            level2Value = lval2 * 100;
            break;
        case 107:
            level1Type = 104;
            level1Scale = 4;
            level1Value = lval1;
            break;
        case 108:
            level1Type = 104;
            level1Scale = 2;
            level1Value = lval1;
            level2Type = 104;
            level2Scale = 2;
            level2Value = lval2;
            break;
        case 109:
            level1Type = 105;
            level1Value = lval1;
            break;
        case 110:
            level1Type = 105;
            level1Value = lval1;
            level2Type = 105;
            level2Value = lval2;
            break;
        case 111:
            level1Type = 106;
            level1Scale = 2;
            level1Value = lval1;
            break;
        case 112:
            level1Type = 106;
            level1Scale = 2;
            level1Value = lval1;
            level2Type = 106;
            level2Scale = 2;
            level2Value = lval2;
            break;
        case 113:
            level1Type = 107;
            level1Value = lval1;
            break;
        case 114:
            level1Type = 107;
            level1Value = 475 * lval1;
            level2Type = 107;
            level2Value = 475 * lval2;
            break;
        case 115:
            level1Type = 108;
            level1Value = lval1 * 100;
            break;
        case 116:
            level1Type = 108;
            level1Value = lval1 * 100;
            level2Type = 108;
            level2Value = lval2 * 100;
            break;
        case 117:
            level1Type = 109;
            level1Scale = 9;
            level1Value = lval1;
            if (((int) lval1 & 0x8000) != 0) {
                level1Value = -1 * (lval1 % 32768);
            }
            break;
        case 119:
            level1Type = 111;
            level1Scale = 4;
            level1Value = lval1;
            break;
        case 120:
            level1Type = 111;
            level1Scale = 2;
            level1Value = lval1;
            level2Type = 111;
            level2Scale = 2;
            level2Value = lval2;
            break;
        case 121:
            level1Type = 100;
            level1Value = (1100 * lval1) * 100;
            level2Type = 100;
            level2Value = (1100 * lval2) * 100;
            break;
        case 125:
            level1Type = 103;
            level1Scale = 2;
            level1Value = lval1;
            break;
        case 128:
            level1Type = 104;
            level1Scale = 3;
            level1Value = 1100 * lval1;
            level2Type = 104;
            level2Scale = 3;
            level2Value = 1100 * lval2;
            break;
        case 141:
            level1Type = 100;
            level1Value = lval1 * 100;
            level2Type = 100;
            level2Value = (1100 * lval2) * 100;
            break;
        case 160:
            level1Type = 160;
            level1Value = lval1;
            break;
        default:
            if (ltype1 > 99 && ltype1 < 200) {
                level1Type = 255;
                logger.warn("GRIB1 level " + ltype1 + " not recognized");
            }
            break;
        }

        return new float[] { level1Type, level1Scale, level1Value, level2Type,
                level2Scale, level2Value };
    }

    /**
     * Gets the level information
     * 
     * @param model
     *            The GribModel object
     * @param centerID
     *            The center
     * @param subcenterID
     *            The subcenter
     * @param levelOneNumber
     *            The level one type
     * @param scaleFactor1
     *            The level one scale factor
     * @param value1
     *            The level one value
     * @param levelTwoNumber
     *            The level two type
     * @param scaleFactor2
     *            The level two scale factor
     * @param value2
     *            The level two value
     * @throws GribException
     */
    private void getLevelInfo(NcgribModel model, int centerID, int subcenterID,
            float levelOneNumber, float scaleFactor1, float value1,
            float levelTwoNumber, float scaleFactor2, float value2) throws GribException {
        String levelName = null;
        String levelUnit = null;
        double levelOneValue = Level.getInvalidLevelValue();
        double levelTwoValue = Level.getInvalidLevelValue();

        NcgribLevel gribLevel = (NcgribLevel) NcgribTableLookup.getInstance()
                .getTableValue(centerID, subcenterID, "4.5",
                        (int) levelOneNumber);

        if (gribLevel != null) {
            levelName = gribLevel.getAbbreviation();
            levelUnit = gribLevel.getUnit();
        } else {
            logger.warn("No level information for center[" + centerID
                    + "], subcenter[" + subcenterID
                    + "], tableName[4.5], level value[" + levelOneValue + "]");
        }

        if ((levelName == null) || levelName.isEmpty()) {
            levelName = LevelFactory.UNKNOWN_LEVEL;
        }

        // Scale the level one value if necessary
        if (scaleFactor1 == 0 || value1 == 0) {
            levelOneValue = value1;
        } else {
            levelOneValue = new Double((float) (value1 * Math.pow(10,
                    scaleFactor1 * -1)));
        }

        levelTwoValue = levelOneValue;

        if (levelName.equals("SFC") && levelOneValue != 0) {
            levelOneValue = 0.0;
        }

        // If second level is present, scale if necessary
        if (levelTwoNumber == 255) {
            levelTwoValue = Level.getInvalidLevelValue();
        } else if (levelTwoNumber == 1) {
            levelTwoValue = Level.getInvalidLevelValue();
        } else {
            if (scaleFactor2 == 0 || value2 == 0) {
                levelTwoValue = value2;
            } else {
                levelTwoValue = (double) (value2 * Math.pow(10, scaleFactor2
                        * -1));
            }
        }
        try {
        	Level level = LevelFactory.getInstance().getLevel(levelName,
        	        levelOneValue, levelTwoValue, levelUnit);
        	model.setLevel(level);
        } catch (CommunicationException e) {
            throw new GribException("Error loading level.", e);
        }

    }

    /**
     * Bounds a longitude to between -180 and 180
     * 
     * @param lon
     *            The longitude to correct
     * @return The corrected longitude
     */
    public static float correctLon(float lon) {

        if (lon < 0) {
            lon = lon % 360;
        } else {
            lon = lon % 360;
        }

        if (lon > 180) {
            lon = (180 - lon % 180) * -1;
        } else if (lon < -180) {
            lon = (180 - (-lon % 180));
        }

        return lon;
    }

    /**
     * Bounds a latitude to between -90 and 90
     * 
     * @param lat
     *            The latitude to correct
     * @return The corrected latitude
     */
    public static float correctLat(float lat) {

        if (lat < 0) {
            lat = lat % 180;
        } else {
            lat = lat % 180;
        }

        if (lat > 90) {
            lat = 90 - lat % 90;
        } else if (lat < -90) {
            lat = (90 - (-lat % 90)) * -1;
        }
        return lat;
    }
    
    private void setAddCol ( boolean addCol ) {
    	this.addCol = addCol;
    }
    
    private boolean getAddCol ( ) {
    	return this.addCol;
    }
}
