package gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv;
/**
 * 
 */

import java.io.File;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.measure.converter.ConversionException;
import java.text.ParseException;

import com.sun.jna.Native;
import com.sun.jna.ptr.FloatByReference;
import com.sun.jna.ptr.IntByReference;

//import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.edex.util.UnitConv;
import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.grid.dataquery.GridQueryAssembler;
import com.raytheon.uf.common.dataplugin.grid.datastorage.GridDataRetriever; 
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
//import com.raytheon.uf.common.derivparam.tree.DataTree;
//import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

import gov.noaa.nws.ncep.viz.gempak.grid.jna.GridDiag;
import gov.noaa.nws.ncep.viz.gempak.grid.jna.GridDiag.gempak;
import gov.noaa.nws.ncep.viz.gempak.util.GempakGrid;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.NcgribLogger;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv.DgdrivException;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv.NcgridDataCache.NcgridData;
import gov.noaa.nws.ncep.viz.gempak.grid.inv.NcGridInventory;
import gov.noaa.nws.ncep.edex.common.dataRecords.NcFloatDataRecord;

import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.MercatorGridCoverage;
import com.raytheon.uf.common.gridcoverage.PolarStereoGridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.gridcoverage.Corner;
import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc.NcEnsembleResourceData;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc.NcgridResourceData;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.util.GempakGridParmInfoLookup;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.util.GempakGridVcrdInfoLookup;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.customCoverage.CustomLatLonCoverage;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.customCoverage.CustomLambertConformalCoverage;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.customCoverage.CustomMercatorCoverage;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.customCoverage.CustomPolarStereoCoverage;

/**
 * The Dgdriv class provides setters GEMPAK for grid diagnostic parameters
 * and then executes the grid retrieval methods.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer    	Description
 * ------------ ----------	----------- 	--------------------------
 * 3/2009 		168			T. Lee			Initial creation
 * 4/2010					mgamazaychikov	Added returnedObject, changed returned 
 * 											error message from StringDataRecord to String,
 * 											added return object type NcFloatDataVector;
 * 											added flipping of the data.
 * 6/2010		168			mgamazaychikov	Changed from NcFloatDataVector to NcFloatDataRecord
 * 											took out listing of the data
 * 10/2010		168			mgamazaychikov	Moved from uengine, added dbtimeToDattim, 
 * 											flipData, getSErver methods
 * 10/2010		168			mgamazaychikov	Added call to db_wsetnavtime_ native function
 * 10/2010	    277			M. Li			Add min and max printout
 * 11/2010		168			mgamazaychikov	Added call to db_init
 * 01/2011					M. Li			Fix vector scale problem
 * 02/2011      168         mgamazaychikov  removed call to db_wsetserver_, getServer()
 * 03/2011                  M. Li			Add log and dataSource
 * 03/2011      168         mgamazaychikov  add inner class for callback functionality
 * 04/2011		168			mgamazaychikov	add flipping of data returned _to_ GEMPAK, and
 * 											flopping of data returned _from_ GEMPAK
 * 04/2011					M. Li			replace gvect and gfunc with gdpfun
 * 05/2011		168			mgamazaychikov  re-implemented data retrieval callback 
 * 											functionality in inner class
 * 06/2011		168			mgamazaychikov	added callback classes for dataURI, navigation 
 * 											retrievals, and for diagnostic (debug and info) 
 * 											messages from gempak.so
 * 09/2011		168			mgamazaychikov	added callback class for ensemble member name
 * 											retrievals, set native logging from Dgdriv,
 * 											made changes for ensemble functionality.
 * 10/2011		168			mgamazaychikov	added methods to removed dependency on datatype.tbl
 * 10/2011                  X. Guo          Added grib inventory
 * 11/22/2011               X. Guo          Re-contrain datauri request map
 * 12/12/2011               X. Guo          Updated Ensemble requests
 * 01/12/2011               X. Guo          Updated getFilename()
 * 02/02/2012               X. Guo          Updated query ensemble navigation
 * 03/13/2012               X. Guo          Clean up
 * 03/28/2012               X. Guo          Don't need to convert gdfile toUppercase
 * 05/10/2012               X. Guo          Calculated sub-grid
 * 05/15/2012               X. Guo          Used the new NcGribInventory()
 * 05/23/2012               X. Guo          Loaded ncgrib logger
 * 06/07/2012               X. Guo          Catch datauri&grid data, use DbQueryResponse to query
 *                                          dataURI instead of ScriptCreator
 * 09/06/2012               X. Guo          Query glevel2
 * 09/26/2012               X. Guo          Fixed missing value problems
 * </pre>
 *
 * @author tlee
 * @version 1.0
 */

//@SuppressWarnings("deprecation")
public class Dgdriv {
	private static NcepLogger logger = NcepLoggerManager.getNcepLogger(Dgdriv.class);
	private static GridDiag gd;
	private String gdfile, gdpfun, gdattim, glevel, gvcord, scale, garea, proj, dataSource;
	private boolean scalar, arrowVector,flop,flip;
	private String gdfileOriginal;
	private NcgridResourceData gridRscData;
	
	private String ncgribPreferences;
	
	private ISpatialObject spatialObj, subgSpatialObj; 
	private ArrayList<DataTime> dataForecastTimes;
	private static Connector conn;
	
	private NcgridDataCache cacheData;
	
	private static NcgribLogger ncgribLogger = NcgribLogger.getInstance();;
	
    public static final int LLMXGD = 1000000; //Max # grid points
    
   /*
    * TODO Work around solution - need to find away to set logging level programmatically
    */
    private static final DecimalFormat forecastHourFormat = new DecimalFormat("000");
    private static String[] nativeLogTypes = {"|critical","|error","|info","|debug" };
    private static int nativeLogLevel = 10;
	private static boolean nativeLogging = true;
//	private static boolean nativeLogging = false;

	//ENSEMBLE Calculation flag
	private static boolean isEnsCategory = false;
	
	private static GempakGridParmInfoLookup gempakParmInfo;
	
	private static GempakGridVcrdInfoLookup gempakVcordInfo;

	public Dgdriv() {
		/*
		 * Initialize GEMPLT, DG and grid libraries.
		 */
//		gd.initialize();
		gempakParmInfo = GempakGridParmInfoLookup.getInstance();
		gempakVcordInfo = GempakGridVcrdInfoLookup.getInstance();
		gd = GridDiag.getInstance();
		gdfile = "";
		gdfileOriginal="";
		gdpfun = "";
		gdattim = "";
		glevel = "";
		gvcord = "";
		scale = "";
		garea = "";
		proj = "";
		ncgribPreferences = null;
		subgSpatialObj = null;
		scalar = false;
		arrowVector = false;
		flop = false;
		flip = false;
		dataForecastTimes = new ArrayList<DataTime>();
		try {
			conn = Connector.getInstance();
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void setResourceData(NcgridResourceData rscData) {
		this.gridRscData = rscData;
	}
	
	public String getGdpfun() {
		return gdpfun;
	}

	public void setGdpfun(String gdpfun) {
		this.gdpfun = gdpfun;
	}

	public boolean isScalar() {
		return scalar;
	}

	public void setScalar(boolean scalar) {
		this.scalar = scalar;
	}

	public boolean isArrowVector() {
		return arrowVector;
	}

	public void setArrowVector(boolean arrowVector) {
		this.arrowVector = arrowVector;
	}

	public void setGdfile (String gdfile) {
		String tmpGdFiles = chechEnsembleGdFiles(gdfile);
		this.gdfile = tmpGdFiles;
		this.gdfileOriginal = tmpGdFiles;
	}
	
	
	public void setGdattim (String gdattim) {
		this.gdattim = gdattim;
	}
	
	public void setGlevel (String glevel) {
		this.glevel = glevel;
	}
	
	public void setGvcord (String gvcord) {
		this.gvcord = gvcord;
	}
	
	public void setScale (String scale) {
		this.scale = scale;
	}
	
	public void setGarea (String garea) {
		this.garea = garea.toUpperCase();
	}
	
	public void setProj (String proj) {
		this.proj = proj;
	}
	
	public void setDataSource(String dataSource) {
		this.dataSource = dataSource.trim().toUpperCase();
		if ( this.dataSource.contains("GEMPAK") ) {
			flop = true;
		}
	}
	
	public void setSpatialObject(ISpatialObject spatialObject) {
		this.spatialObj = spatialObject;
		setGridFlip (spatialObject);
	}
	
	public ISpatialObject getSubgSpatialObj () {
		return this.subgSpatialObj;
	}
	
	public void setSubgSpatialObj ( ISpatialObject obj) {
		this.subgSpatialObj = obj;
	}
	
	public void setPreferences ( String preferences) {
		this.ncgribPreferences = preferences;
	}
	
	public void setCacheData ( NcgridDataCache data ) {
		this.cacheData = data;
	}
	
	final int BUFRLENGTH = 128;
	final int PRMLENGTH = 40;
	final int IMISSD = -9999;
	
	IntByReference iret = new IntByReference(0);
	IntByReference ier = new IntByReference(0);
	IntByReference iscale = new IntByReference(0);
    IntByReference iscalv = new IntByReference(0);
    IntByReference chngnv = new IntByReference(1);
    IntByReference coladd = new IntByReference(0);
    IntByReference gottm = new IntByReference(0);
    IntByReference drpflg = new IntByReference(0);
    IntByReference level1 = new IntByReference(0);
    IntByReference level2 = new IntByReference(0);
    IntByReference ivcord = new IntByReference(0);
    IntByReference fileflg = new IntByReference(0);
    IntByReference termflg = new IntByReference(1);
    
    IntByReference maxgrd = new IntByReference(LLMXGD);
    IntByReference ix1 = new IntByReference(0);
    IntByReference iy1 = new IntByReference(0);
    IntByReference ix2 = new IntByReference(0);
    IntByReference iy2 = new IntByReference(0);
    IntByReference kx = new IntByReference(IMISSD);
    IntByReference ky = new IntByReference(IMISSD);
    IntByReference igx = new IntByReference(0);
    IntByReference igy = new IntByReference(0);
	IntByReference numerr = new IntByReference(0);
    
    FloatByReference rmax = new FloatByReference(0.F);
    FloatByReference rmin = new FloatByReference(0.F);

    String garout = "";
    String prjout = "";
    String satfil = "";
    String outfil = "";
    String skip = "N";
    String errorURI ="";
    byte[] pfunc = new byte[BUFRLENGTH];
	byte[] parmu = new byte[PRMLENGTH];
	byte[] parmv = new byte[PRMLENGTH];
	byte[] time = new byte [PRMLENGTH];
    byte [] time1 = new byte [BUFRLENGTH];
    byte [] time2 = new byte [BUFRLENGTH];
	byte [] gareabuf = new byte [BUFRLENGTH];
	byte [] prjbuf = new byte [BUFRLENGTH];
	boolean proces = true;
	Map<Integer, String> hm = new HashMap<Integer, String>();
//	private String eventName;
	DiagnosticsCallback diagCallback=null;
	ReturnFileNameCallback flnmCallback = null;
	ReturnCycleForecastHoursCallback fhrsCallback = null;
	ReturnNavigationCallback navCallback = null;
	ReturnDataCallback dataCallback = null;
	ReturnDataURICallback duriCallback = null;
	ReturnSubgridCRSCallback subgCrsCallback = null;
	
	private class DiagnosticsCallback implements gempak.DbCallbackWithMessage {

		@Override
		public boolean callback(String msg) {
			String sep = "::";
			if ( ! nativeLogging ) return true;
			int lvl = checkNativeLoggerLevel( msg );
			if ( lvl > nativeLogLevel ) return true;
			if ( msg.contains("|debug") ) {
				String logMessage = msg.split("\\|")[0] + ":" + msg.split(sep)[1];
				System.out.println("\t\tC DEBUG MESSAGE " + logMessage);
				logger.debug("C DEBUG MESSAGE " +logMessage);
			}
			else if ( msg.contains("|info") ) {
				String logMessage;
				if ( msg.split("\\|").length > 2 ) {
					logMessage = msg.split("\\|")[0] + ":" + msg.split(sep)[1];
				}
				else {
					logMessage = msg;
				}
				System.out.println("\tC INFO MESSAGE " + logMessage);
				logger.info("C INFO MESSAGE " +logMessage);
			}
			else if ( msg.contains("|error") ) {
				String logMessage = msg.split("\\|")[0] + ":" + msg.split(sep)[1];
				System.out.println("\nC ERROR MESSAGE " + logMessage);
				logger.error("C ERROR MESSAGE " +logMessage);
			}
			
			return true;
		}
	}
	
	private class ReturnDataCallback implements gempak.DbCallbackWithMessage {

		@Override
		public boolean callback(String msg) {
			if (msg.contains("/")) {				
				try {
					if ( ncgribLogger.enableDiagnosticLogs() ) {
						logger.info(" enter ReturnDataCallback:" + msg );
					}
					String[] msgArr = msg.split(";");
					if (msgArr.length == 3) {
						boolean addData = false;
						String dataURI = msgArr[0];
						int nx = Integer.parseInt(msgArr[1].trim());
						int ny = Integer.parseInt(msgArr[2].trim());
						long t0 = System.currentTimeMillis();
						NcgridData gData = cacheData.getGridData(dataURI);
						float[] rData;
						if ( gData == null ) {
							addData = true;
							//rData = retrieveData(dataURI);
							rData = retrieveDataFromRetriever (dataURI);
							if ( rData == null ) {
								errorURI = msg;
								proces = false;
								if ( ncgribLogger.enableDiagnosticLogs() ) {
									logger.info("??? retrieveDataFromRetriever return NULL for dataURI("+dataURI+")");
								}
								return true;
							}
						}
						else {
							if ( ncgribLogger.enableDiagnosticLogs() ) {
								long t00 = System.currentTimeMillis();
								logger.info("++++ retrieve data (nx:"+gData.getNx() +"-ny:" + gData.getNy()+") from cache took: " + (t00-t0));
							}
							rData = gData.getData();
						}
						long t1 = System.currentTimeMillis();				        
						int rDataSize = rData.length;						
						IntByReference datSize = new IntByReference(rDataSize);
						logger.debug("retrieve data nx=" + nx +" ny=" + ny + " rDataSize=" + rDataSize);
						if ( (nx*ny) == rDataSize ) {
							if ( addData ) {
								cacheData.addGridData(dataURI, nx, ny, rData);
							}
							if ( flip ) {
//								logger.info ("*****flip grid data*****");
							gd.gem.db_returndata(flipData(rData, nx, ny),datSize);
						}
							else
								gd.gem.db_returndata(checkMissingData(rData),datSize);
						}
						else {
							logger.debug("retrieve data size("+rDataSize+") mismatch with navigation nx=" + nx +" ny=" + ny);
							errorURI = msg;
							proces = false;
						}
						long t2 = System.currentTimeMillis();
//						System.out.println("\tretrieve data took " + (t1-t0));
				        logger.debug("retrieve data took " + (t1-t0));
//				        System.out.println("\treturn data took " + (t2-t1));
				        logger.debug("return data took " + (t2-t1));
						return true;
					} else {
						errorURI = msg;
						proces = false;
						return true;
					}
				} catch (VizException e) {
					errorURI = msg;
					proces = false;
					return true;
				}
			} else {
				return true;
			}
		}
	}
	
	private class ReturnNavigationCallback implements gempak.DbCallbackWithMessage {

		@Override
		public boolean callback(String msg) {
			String navigationString=null;
			logger.debug("request navigation for: " + msg );
			long t0 = System.currentTimeMillis();
			if (gdfile.startsWith("{") && gdfile.endsWith("}") ) {
				navigationString = getEnsembleNavigation( msg );
			}
			else {
			    navigationString = getGridNavigationContent(spatialObj);
			}
			gd.gem.db_returnnav(navigationString);
			long t1 = System.currentTimeMillis();
//			System.out.println("\treturn navigation " + navigationString + " took " + (t1-t0));
			logger.debug("return navigation  " + navigationString + " took " + (t1-t0));
			return true;
		}
	}
	
	private class ReturnDataURICallback implements gempak.DbCallbackWithMessage {

		@Override
		public boolean callback(String msg) {
			try {
//				logger.debug("request datauri for:" + msg );
				long t0 = System.currentTimeMillis();
				//String dataURI = getDataURI(msg);
				String dataURI = getDataURIFromAssembler(msg);
				gd.gem.db_returnduri(dataURI);
				long t1 = System.currentTimeMillis();
				logger.debug("return dataURI " + dataURI + " took " + (t1-t0));
				return true;
			} catch (VizException e) {
				errorURI = msg;
				proces = false;
				return true;
			}
		}
	}
	
	private class ReturnSubgridCRSCallback implements gempak.DbCallbackWithMessage {

		@Override

		public boolean callback(String msg) {
			if ( ncgribLogger.enableDiagnosticLogs() ) {
				logger.info("Rcv'd new subg:" + msg);
			}
				createNewISpatialObj ( msg );
				return true;
		}
	}
	
	private class ReturnCycleForecastHoursCallback implements gempak.DbCallbackWithoutMessage {

		@Override
		public boolean callback() {
			long t0 = System.currentTimeMillis();
			String cycleFcstHrsString = getCycleFcstHrsString(dataForecastTimes);
			gd.gem.db_returnfhrs(cycleFcstHrsString);
			long t1 = System.currentTimeMillis();
//			System.out.println("\treturn cycle forecast hours string " + cycleFcstHrsString + " took " + (t1-t0));
			logger.debug("return cycle forecast hours string " + cycleFcstHrsString + " took " + (t1-t0));
			return true;
		}
	}
	
	private class ReturnFileNameCallback implements gempak.DbCallbackWithMessage {

		@Override
		public boolean callback(String msg) {
//			if (msg.contains("import")) {
				try {
					long t0 = System.currentTimeMillis();
					String fileNames = executeScript(msg);
//					System.out.println("Retrieved members:" + fileNames);
					gd.gem.db_returnflnm(fileNames);
					long t1 = System.currentTimeMillis();
//					System.out.println("\treturn file names string " + fileNames + " took " + (t1-t0));
					logger.debug("return file names string " + fileNames + " took " + (t1-t0));
					return true;
				} catch (VizException e) {
					errorURI = msg;
					proces = false;
					return true;
				}
//			} else {
//				return true;
//			}
		}
	}

	public NcFloatDataRecord execute() throws DgdrivException {
		
	   /*
		* Tag the gdfile string for ncgrib dataSource
		*/
		this.gdfile = getTaggedGdfile();
		proces = true;
//		System.out.println();

	   /*
        * TODO Work around solution - need to find away to set logging level programmatically
        */

		diagCallback = new DiagnosticsCallback();
		gd.gem.db_diagCallback (diagCallback);	
		
		flnmCallback = new ReturnFileNameCallback();
		gd.gem.db_flnmCallback(flnmCallback);
		fhrsCallback = new ReturnCycleForecastHoursCallback();
        gd.gem.db_fhrsCallback (fhrsCallback);
        navCallback = new ReturnNavigationCallback();
        gd.gem.db_navCallback (navCallback);
		dataCallback = new ReturnDataCallback();
		gd.gem.db_dataCallback (dataCallback);
		duriCallback = new ReturnDataURICallback();
		gd.gem.db_duriCallback (duriCallback);
		subgCrsCallback = new ReturnSubgridCRSCallback();
		gd.gem.db_subgCrsCallback (subgCrsCallback);
		long t0 = System.currentTimeMillis();
		
		gd.gem.in_bdta_(iret);
		if ( iret.getValue () != 0 ) {
			throw new DgdrivException("From in_bdta: error initializing GEMPAK");
		}
 
		gd.gem.gd_init_ (iret);
		if ( iret.getValue () != 0 ) {
			throw new DgdrivException("From gd_init: error initializing Grid library common area");
		}


		IntByReference mode = new IntByReference(0);
		gd.gem.gg_init_ (mode,iret);
		if ( iret.getValue () != 0 ) {
			throw new DgdrivException("From gg_init: error starting GPLT");
		}
	
		gd.gem.dg_intl_ (iret);
		if ( iret.getValue () != 0 ) {
			throw new DgdrivException("From dg_intl: error initializing Grid diagnostics common block");
		}
		
		gd.gem.db_init_ (iret);
		if ( iret.getValue () != 0 ) {
			throw new DgdrivException("From db_init: error initializing DB common area");
		}
				
		String currentTime = dbtimeToDattim(gdattim);
		gd.gem.db_wsetnavtime_ (currentTime, iret);
		if ( iret.getValue () != 0 ) {
			throw new DgdrivException ("From db_wsetnavtime: error setting the navigation time " + currentTime);
		}
		
		/*
		 * Process the gdfile string for ensemble request
		 */
		boolean isEns = true;
		if (this.gdfile.startsWith("{") && this.gdfile.endsWith("}") ) {
			prepareEnsembleDTInfo();
			isEnsCategory = true;
		}
		else {
			isEns = false;
			prepareGridDTInfo();
		}

		long t05 = System.currentTimeMillis();
		logger.debug("init and settime took: " + (t05-t0));
	    /*
         * Process the GDFILE input.
         */
        if (proces) {

            if ( gdfile.contains(":") ) {
            	if ( ! isEns ) {
            		gd.gem.db_wsetevtname_ (gdfile.split(":")[1], iret);
            	}

        	}
        	gd.gem.dgc_nfil_ (gdfile, "", iret);
        	if ( iret.getValue () != 0 ) {
        		gd.gem.erc_wmsg ("DG", iret, "", ier);
        		proces = false;
        	}
        	
        }
        long t06 = System.currentTimeMillis();
//        System.out.println("dgc_nfil took " + (t06-t05));
        logger.debug("dgc_nfil took: " + (t06-t05));
        
        /*
         * Process the GDATTIM input; setup the time server.
         */      
        if (proces) {
//        	if (this.gdfile.startsWith("{") && this.gdfile.endsWith("}")) {
//        		String fTime = "f06";
//        		gd.gem.dgc_ndtm_ (fTime, iret);
//    		}
//        	else {
//        		gd.gem.dgc_ndtm_ (dbtimeToDattim(gdattim), iret);
//        	}
        	gd.gem.dgc_ndtm_ (dbtimeToDattim(gdattim), iret);
        	if ( iret.getValue () != 0 ) {
        		gd.gem.erc_wmsg ("DG", iret, "", ier);
        		proces = false;
        	}
        }
        
        long t07 = System.currentTimeMillis();
        logger.debug("dgc_ndtm took: " + (t07-t06));
      
        if (proces) {
        	/*
    		 * Check if GAREA == "grid", if so, then set coladd= false to NOT add
    		 * a column to globe wrapping grids.
    		 */      
    		if ( garea.compareToIgnoreCase("GRID")== 0 ) {
    			coladd = new IntByReference(0);	
    		}
    		/* 
             * Set the attributes that do not vary within the time loop.
             */
            gd.gem.inc_scal (scale, iscale, iscalv, iret);
            long t07b = System.currentTimeMillis();
            logger.debug("inc_scal took: " + (t07b-t07));
            
            /*
             * Get the next time to process from time server.
             */
            gd.gem.dgc_ntim_(chngnv, coladd, time1, time2, gottm, iret );
            long t08 = System.currentTimeMillis();
            logger.debug("dgc_ntim took: " + (t08-t07b));

			if ( iret.getValue () != 0 ) {
				gd.gem.erc_wmsg ("DG", iret, "", ier);
				proces = false;
			} else {
				gd.gem.tgc_dual (time1, time2, time, iret);
				long t08b = System.currentTimeMillis();
	            logger.debug("tgc_dual took: " + (t08b-t08));
			}
        }
        

		/*
		 * Set the map projection and graphics area.
		 */
        long t09a = System.currentTimeMillis();
/*        if (proces) {
        	gd.gem.dgc_fixa_ ( garea, proj, gareabuf, prjbuf, iret);
        	if ( iret.getValue () != 0 ) {
        		gd.gem.erc_wmsg ("DG", iret, "", ier);
        		proces = false;
        	}
        }
*/        long t09 = System.currentTimeMillis();
        logger.debug("dgc_fixa took: " + (t09-t09a));

        /*
         * Fortran wrapper used and use byte array instead of String as input!!
         */
//        if (proces) {
//        	gd.gem.ggc_maps (prjbuf, gareabuf, satfil, drpflg, iret);
//        	if ( iret.getValue () != 0 ) {
//        		gd.gem.erc_wmsg ("DG", iret, "", ier);
//        		proces = false;
//        	}
//        }
        long t10 = System.currentTimeMillis();
        logger.debug("ggc_maps took: " + (t10-t09));

        /*
		 * Setup the grid subset that covers the graphics area.
		 */
        if ( this.ncgribPreferences != null && proces ) {
        	String []prefs = this.ncgribPreferences.split(";");
        	gd.gem.db_setsubgnav_(new Float(prefs[0]), new Float(prefs[1]), new Float(prefs[2]), new Float(prefs[3]), iret);
            if ( iret.getValue() != 0 ) {
                proces = false;
               }

            if (proces) {
            	gd.gem.dgc_subg_ (skip, maxgrd, ix1, iy1, ix2, iy2, iret);
            	if ( iret.getValue () != 0 ) {
            		proces = false;
            	}
            }
        }
        
        long t11 = System.currentTimeMillis();
        logger.debug("dgc_subg took: " + (t11-t10));

		/*
		 * Return grid dimension for grid diagnostic calculation.
		 */
        if (proces) {
        	gd.gem.dg_kxky_( kx, ky, iret);
        	if ( iret.getValue () != 0 ) {
        		gd.gem.erc_wmsg ("DG", iret, "", ier);
        		proces = false;
        	}
        }
        logger.debug("kx:" + kx.getValue() + "  ky:" + ky.getValue());
        float[] ugrid = null;
		float[] vgrid = null;
		int grid_size = kx.getValue()*ky.getValue();
		if ( kx.getValue() > 0 && ky.getValue() > 0 ) {
			ugrid = new float[grid_size];
			vgrid = new float[grid_size];
		} 
		else {
			proces = false;
		}

		long t012 = System.currentTimeMillis();
//		System.out.println("From gdc_nfil to dgc_grid took:" + (t012-t06));
		logger.debug("From gdc_nfil to dgc_grid took: " + (t012-t06));
		/*
		 * Compute the requested grid.
		 */
		if (proces) {
			
			
			if (scalar) {
				gd.gem.dgc_grid_( time, glevel, gvcord, gdpfun, pfunc, ugrid, igx, igy, 
						time1, time2, level1, level2, ivcord, parmu, iret);
			} else {
				gd.gem.dgc_vecr_ ( time, glevel,  gvcord,  gdpfun,  pfunc,
						ugrid, vgrid, igx,  igy, time1, time2, level1, level2, 
						ivcord, parmu, parmv, iret);
			}
			if (!proces) {
				throw new DgdrivException("Error retrieving data record "
						+ errorURI);
			}
			if ( iret.getValue () != 0 ) {
				gd.gem.erc_wmsg ("DG", iret, Native.toString(pfunc), ier);
				proces = false;
			}
		}
		long t013 = System.currentTimeMillis();
//		System.out.println("dgc_grid took " + (t013-t012));
		logger.debug("dgc_grid took: " + (t013-t012));
		
		/*
		 * Compute the scaling factor and scale the grid data.
		 */
		if (proces) {
			NcFloatDataRecord fds = new NcFloatDataRecord();
			IntByReference ix12 = new IntByReference(1);
		    IntByReference iy12 = new IntByReference(1);
		    if ( scalar ) {
		    	gd.gem.grc_sscl( iscale, igx, igy, ix12, iy12, igx, igy, ugrid, rmin, rmax, iret);		
		    }
		    else if ( arrowVector ) {
		    	gd.gem.grc_sscl( iscalv, igx, igy, ix12, iy12, igx, igy, ugrid, rmin, rmax, iret);
		    }
		    if ( flop ) {
//		    	logger.info ("====flop grid data 1=====");
			fds.setXdata(flopData(ugrid, igx.getValue(), igy.getValue()));
		    }
		    else
		    	fds.setXdata( revertGempakData2CAVE(ugrid) );
			fds.setDimension(2);				
			fds.setSizes(new long[] { igx.getValue(), igy.getValue()});
			fds.setVector(false);
						
			if (!scalar){  // vector
				if ( arrowVector ) {
					gd.gem.grc_sscl( iscalv, igx, igy, ix12, iy12, igx, igy, vgrid, rmin, rmax, iret);
				}
				if ( flop ) {
//					logger.info ("====flop grid data =====");
				fds.setYdata(flopData(vgrid, igx.getValue(), igy.getValue()));
				}
				else
					fds.setYdata( revertGempakData2CAVE(vgrid) );
				fds.setDimension(2);
				fds.setSizes(new long[] { igx.getValue(), igy.getValue()});
				fds.setVector(true);			
			}
			/*
			 *  Free memory for all internal grids
			 */
			gd.gem.dg_fall_( iret);
	        long t1 = System.currentTimeMillis();
	        logger.debug("Scaling took: " + (t1-t013));
//	        System.out.println("Scaling took:" + (t1-t013));
//	        printInfoMessage(t1 - t0);
	        return fds;
		}		
		else {
			/*
			 * Write error message only in case of failure
			 */
			gd.gem.er_gnumerr_(numerr, iret);
			int num = numerr.getValue();
			int index = 0;
			byte[] errmsg = new byte[BUFRLENGTH];
			StringBuilder strBuf = new StringBuilder();
			while (index < num) {
				numerr.setValue(index);
				gd.gem.er_gerrmsg_(numerr, errmsg, iret);
				strBuf.append(Native.toString(errmsg));
				strBuf.append("\n");
				index++;
			}
			return null;
        }
		
	}
	
	private void prepareGridDTInfo() {
		String alias = this.gdfile;
		String path = "A2DB_GRID";
		logger.debug("prepareGridDTInfo-- alias:" + alias + " gdfileOriginal:" + this.gdfileOriginal);
		String template = this.gdfileOriginal + "_db";
		if ( this.gdfileOriginal.contains(":")) {
			template = this.gdfileOriginal.substring(0, this.gdfileOriginal.indexOf(":")) + "_db";
		}
		IntByReference iret = new IntByReference(0);
//		System.out.println();
		gd.gem.db_seta2dtinfo_ (alias, path, template, iret);
		
	}

	private void prepareEnsembleDTInfo() {
		String thePath = "A2DB_GRID";
		StringBuilder sba = new StringBuilder();
		StringBuilder sbp = new StringBuilder();
		StringBuilder sbt = new StringBuilder();
		String [] gdfileArray = this.gdfileOriginal.substring(this.gdfileOriginal.indexOf("{")+1, this.gdfileOriginal.indexOf("}")).split(",");
		for (int igd=0;igd<gdfileArray.length; igd++){
			sbp.append(thePath);
			sbp.append("|");
			String perturbationNum = null;
			String ensName = null;
			if ( gdfileArray[igd].contains("|") ) {
				String [] tempArr = gdfileArray[igd].split("\\|");
				if ( tempArr[0].contains(":")) {
					String [] nameArr = tempArr[0].split(":");
					ensName = nameArr[0];
					if ( nameArr[0].contains("%")) {
						ensName = nameArr[0].split("%")[1];
					}
					perturbationNum = nameArr[1];
				}
				else {
					ensName = tempArr[0];
					if ( tempArr[0].contains("%")) {
						ensName = tempArr[0].split("%")[1];
					}
				}
				
				sba.append("A2DB_" + ensName);
				if ( perturbationNum != null ) {
					sba.append(":" + perturbationNum );
				}
//				System.out.println();
				sba.append("|");				
			    sbt.append(getEnsembleTemplate(ensName,perturbationNum)); 
			    sbt.append("|");
			}
			else {
				if ( gdfileArray[igd].contains(":")) {
					String [] nameArr = gdfileArray[igd].split(":");
					ensName = nameArr[0];
					if ( nameArr[0].contains("%")) {
						ensName = nameArr[0].split("%")[1];
					}
					perturbationNum = nameArr[1];
				}
				else {
					ensName = gdfileArray[igd];
					if ( gdfileArray[igd].contains("%")) {
						ensName = gdfileArray[igd].split("%")[1];
					}
				}
				
				sba.append("A2DB_" + ensName);
				if ( perturbationNum != null ) {
					sba.append(":" + perturbationNum );
				}
				sba.append("|");				
			    sbt.append(getEnsembleTemplate(ensName,perturbationNum)); 
			    sbt.append("|");
			}
		}
		String alias = sba.toString().substring(0, sba.toString().length()-1);
		String path = sbp.toString().substring(0, sbp.toString().length()-1);
		String template = sbt.toString().substring(0, sbt.toString().length()-1);
		IntByReference iret = new IntByReference(0);
//		System.out.println();
		logger.debug("prepareEnsembleDTInfo: alias="+alias + ", path="+path + ",template="+template);
		gd.gem.db_seta2dtinfo_ (alias, path, template, iret);
	}

	private String getEnsembleTemplate( String ensName, String perturbationNum) {
		String ensTemplate = null;
		
			try {
				int num = Integer.parseInt(perturbationNum);
				ensTemplate = ensName + "_db_" + num + "_YYYYMMDDHHfFFF";
			}
			catch ( Exception e) {
				ensTemplate = ensName + "_db_" + perturbationNum + "_YYYYMMDDHHfFFF";
			}
		
		
		/*
		HashMap<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
		rcMap.put( GridDBConstants.PLUGIN_NAME, new RequestConstraint(GridDBConstants.GRID_TBL_NAME) );
        rcMap.put( GridDBConstants.MODEL_NAME_QUERY, new RequestConstraint( ensName ) );
        if ( perturbationNum != null ) {
        	rcMap.put( GridDBConstants.ENSEMBLE_ID_QUERY, new RequestConstraint( String.valueOf(Integer.parseInt(perturbationNum))) );
        }
        DbQueryRequest request = new DbQueryRequest();
        request.addRequestField(GridDBConstants.EVENT_NAME_QUERY);
        request.setDistinct(true);
        request.setConstraints(rcMap);
        long t0 = System.currentTimeMillis();
        String ensTemplate = null;
        try {
        	DbQueryResponse response = (DbQueryResponse) ThriftClient
        	.sendRequest(request);
        	// extract list of results
        	List<Map<String, Object>> responseList = null;
        	if (response != null) {
        		responseList = response.getResults();
        	} else {
        		// empty list to simplify code
        		responseList = new ArrayList<Map<String, Object>>(0);
        	}
        	if (responseList.size() > 0) {
        		Object event = responseList.get(0).get(GridDBConstants.EVENT_NAME_QUERY);
        		if (event != null && event instanceof String) {
        			ensTemplate = ensName + "_db_" + (String) event + "_YYYYMMDDHHfFFF";
        		}
        	}
        } catch (VizException e) {
        	
        }
        long t1 = System.currentTimeMillis();
		if ( ensTemplate != null )
			logger.debug("getEnsembleTemplate("+ensTemplate+") for("+ensName+") took: " + (t1-t0));
		else
			logger.debug("??? getEnsembleTemplate(null) for("+ensName+") took: " + (t1-t0));
		*/
		logger.debug("getEnsembleTemplate("+ensTemplate+") for("+ensName+"," + perturbationNum+ ")");
		return ensTemplate;
/*        
		StringBuilder query = new StringBuilder();
		query.append("import GempakEnsembleTemplateGenerator\n");
		query.append("query = GempakEnsembleTemplateGenerator.GempakEnsembleTemplateGenerator('grid')\n");
		query.append("query.setEnsembleName('" + ensName + "')\n");
		if ( perturbationNum != null ) {
//			query.append("query.setEnsembleEventName('" + perturbationNum.toLowerCase() + "')\n");
			query.append("query.setEnsemblePerturbationNumber('" + Integer.parseInt(perturbationNum) + "')\n");
		}
		
		query.append("return query.execute()\n");

		String scriptToRun = query.toString();
		long t0 = System.currentTimeMillis();

		Object[] pdoList;
		try {
			pdoList = conn.connect(scriptToRun, null, 60000);
			String ensTemplate = (String) pdoList[0];
			long t1 = System.currentTimeMillis();
//	        System.out.println("\tgetEnsembleTemplate took: " + (t1-t0));
	        logger.info("getEnsembleTemplate("+ ensTemplate +") took: " + (t1-t0));
			return ensTemplate;
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}*/
	}

	private String getEnsTime(String string) {
		// TODO Auto-generated method stub
		return string;
	}

	private String getTaggedGdfile() {
		if ( this.dataSource.equalsIgnoreCase("grid")) {
//			String tag = this.dataSource + "_";
			String tag = "A2DB_";
			/*
			 * For gdfile containing event name do not have to do 
			 * anything as long as the tag is prefixed
			 * eg GHM:greg07e becomes A2DB_GHM:greg07e
			 */
			
			/*
			 * For gdfile containing ensemble have to preprocess
			 * eg {GFS|11/12, NAM|11/06} becomes {A2DB_GFS|11/12, A2DB_NAM|11/06}
			 */
			if (this.gdfileOriginal.startsWith("{") && this.gdfileOriginal.endsWith("}") ) {
				String [] gdfileArray = this.gdfileOriginal.substring(this.gdfileOriginal.indexOf("{")+1, this.gdfileOriginal.indexOf("}")).split(",");
				StringBuilder strb = new StringBuilder();
//				System.out.println();
				for (int igd=0;igd<gdfileArray.length; igd++){
					strb.append(tag);
					strb.append(gdfileArray[igd]);
					strb.append(",");
						
				}
				String retStr = strb.toString().substring(0, strb.toString().length()-1);
				return "{" + retStr + "}";
			}
			return tag + this.gdfile;
		}
		else {
			return this.gdfile;
		}
	}

	/**
	 * Create sub-grid coverage
	 * @param subgGempakFormat -- prj;nx;ny;lllat;lllon;urlat;urlon;angle1;angle2;angle3
	 */
	private void createNewISpatialObj ( String subgGempakFormat) {
		String []gareaStr = subgGempakFormat.split(";");
		
		if (subgSpatialObj == null && gareaStr != null && gareaStr.length >= 7) {
			if ( gareaStr[0].equalsIgnoreCase("CED")){
				createLatlonISPatialObj (Integer.parseInt(gareaStr[1]),Integer.parseInt(gareaStr[2]),
						Double.parseDouble(gareaStr[3]),Double.parseDouble(gareaStr[4]),
						Double.parseDouble(gareaStr[5]),Double.parseDouble(gareaStr[6]));
			} else if ( gareaStr[0].equalsIgnoreCase("LCC") ) {
				createLambertConformalISPatialObj (Integer.parseInt(gareaStr[1]),Integer.parseInt(gareaStr[2]),
						Double.parseDouble(gareaStr[3]),Double.parseDouble(gareaStr[4]));
			} else if ( gareaStr[0].equalsIgnoreCase("MER") ) {
				createMercatorISPatialObj (Integer.parseInt(gareaStr[1]),Integer.parseInt(gareaStr[2]),
						Double.parseDouble(gareaStr[3]),Double.parseDouble(gareaStr[4]),
						Double.parseDouble(gareaStr[5]),Double.parseDouble(gareaStr[6]));
			} else if ( gareaStr[0].equalsIgnoreCase("STR") ) {
				createPolarStereoISPatialObj (Integer.parseInt(gareaStr[1]),Integer.parseInt(gareaStr[2]),
						Double.parseDouble(gareaStr[3]),Double.parseDouble(gareaStr[4]));
			}
		}
	}
	
	/**
	 * Create Latitude/longitude coverage
	 * @param nx  - Number of points along a parallel
	 * @param ny  - Number of points along a meridian
	 * @param la1 - Latitude of first grid point
	 * @param lo1 - Longitude of the first grid point
	 * @param la2 - Latitude of the last grid point
	 * @param lo2 - Longitude of the last grid point
	 */
	private void createLatlonISPatialObj (int nx, int ny, double la1, double lo1,
			double la2, double lo2) {
		
		logger.debug("nx:" + nx + " ny:" + ny + " la1:" + la1 + " lo1:" + lo1 + " la2:" + la2 + " lo2:" + lo2);
		CustomLatLonCoverage cv = new CustomLatLonCoverage ();
		cv.setNx(nx);
		cv.setNy(ny);
		cv.setLa1(la1);
		cv.setLo1(lo1);
		cv.setLa2(la2);
		cv.setLo2(lo2);
		GridCoverage gc = (GridCoverage) spatialObj;
		
		LatLonGridCoverage llgc = (LatLonGridCoverage) gc;
		cv.setDx(llgc.getDx());
		cv.setDy(llgc.getDy());
		cv.setSpacingUnit(llgc.getSpacingUnit());
		if ( cv.build() ) {
			setSubgSpatialObj ((ISpatialObject)cv);
		}
	}
	
	/**
	 * Create Lambert-Conformal coverage
	 * @param nx	- Number of points along the x-axis 
	 * @param ny	- Number of points along the y-axis
	 * @param la1	- Latitude of the first grid point
	 * @param lo1	- Longitude of the first grid point
	 */
	private void createLambertConformalISPatialObj (int nx, int ny, double la1, double lo1 ) {
		
		CustomLambertConformalCoverage cv = new CustomLambertConformalCoverage ();
		cv.setNx(nx);
		cv.setNy(ny);
		cv.setLa1(la1);
		cv.setLo1(lo1);

		GridCoverage gc = (GridCoverage) spatialObj;
		
		LambertConformalGridCoverage llgc = (LambertConformalGridCoverage) gc;
		cv.setMajorAxis(llgc.getMajorAxis());
		cv.setMinorAxis(llgc.getMinorAxis());
		cv.setLatin1(llgc.getLatin1());
		cv.setLatin2(llgc.getLatin2());
		cv.setLov(llgc.getLov());
		cv.setDx(llgc.getDx());
		cv.setDy(llgc.getDy());
		cv.setSpacingUnit(llgc.getSpacingUnit());
		if ( cv.build() ) {
			setSubgSpatialObj ((ISpatialObject)cv);
		}
	}
	
	/**
	 * Create Mercator coverage
	 * @param nx  - Number of points along a parallel
	 * @param ny  - Number of points along a meridian
	 * @param la1 - Latitude of first grid point
	 * @param lo1 - Longitude of the first grid point
	 * @param la2 - Latitude of the last grid point
	 * @param lo2 - Longitude of the last grid point
	 */
	private void createMercatorISPatialObj (int nx, int ny, double la1, double lo1,
			double la2, double lo2) {
		
//		logger.info("nx:" + nx + " ny:" + ny + " la1:" + la1 + " lo1:" + lo1 + " la2:" + la2 + " lo2:" + lo2);
		CustomMercatorCoverage cv = new CustomMercatorCoverage ();
		cv.setNx(nx);
		cv.setNy(ny);
		cv.setLa1(la1);
		cv.setLo1(lo1);
		cv.setLa2(la2);
		cv.setLo2(lo2);
		GridCoverage gc = (GridCoverage) spatialObj;
		
		MercatorGridCoverage llgc = (MercatorGridCoverage) gc;
		cv.setMajorAxis(llgc.getMajorAxis());
		cv.setMinorAxis(llgc.getMinorAxis());
		cv.setDx(llgc.getDx());
		cv.setDy(llgc.getDy());
		cv.setSpacingUnit(llgc.getSpacingUnit());
		if ( cv.build() ) {
			setSubgSpatialObj ((ISpatialObject)cv);
		}
	}
	
	/**
	 * Create Polar-Stereo coverage
	 * @param nx	- Number of points along the x-axis 
	 * @param ny	- Number of points along the y-axis
	 * @param la1	- Latitude of the first grid point
	 * @param lo1	- Longitude of the first grid point
	 */
	private void createPolarStereoISPatialObj (int nx, int ny, double la1, double lo1 ) {
		
		CustomPolarStereoCoverage cv = new CustomPolarStereoCoverage ();
		cv.setNx(nx);
		cv.setNy(ny);
		cv.setLa1(la1);
		cv.setLo1(lo1);

		GridCoverage gc = (GridCoverage) spatialObj;
		
		PolarStereoGridCoverage llgc = (PolarStereoGridCoverage) gc;
		cv.setMajorAxis(llgc.getMajorAxis());
		cv.setMinorAxis(llgc.getMinorAxis());
		cv.setLov(llgc.getLov());
		cv.setLov(llgc.getLov());
		cv.setDx(llgc.getDx());
		cv.setDy(llgc.getDy());
		cv.setSpacingUnit(llgc.getSpacingUnit());
		if ( cv.build() ) {
			setSubgSpatialObj ((ISpatialObject)cv);
		}
	}
	
	private String getCycleFcstHrsString(ArrayList<DataTime> dataForecastTimes) {
		// TODO Auto-generated method stub
		StringBuilder resultsBuf = new StringBuilder();
		for (DataTime dt: dataForecastTimes) {
			resultsBuf.append(dbtimeToDattim(dt.toString()));
			resultsBuf.append("|");
		}
		return resultsBuf.substring(0, resultsBuf.length() - 1);
	}

	private String dbtimeToDattim(String aTime) {
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
	 * Flips the data from CAVE order and changes the missing data value from
	 * CAVE -999999.0f to GEMPAK -9999.0f
	 */
	private float[] flipData(float[] inGrid, int nx, int ny) {

		float[] outGridFlipped = new float[inGrid.length];

		int kk = 0;

		for (int jj = 0; jj < ny; jj++) {
			int m1 = nx * ny - nx * (jj + 1);
			int m2 = nx * ny - nx * jj;
			for (int ii = m1; ii < m2; ii++) {
				if (inGrid[ii] < -900000.0) {
					outGridFlipped[kk] = -9999.0f;
					kk++;
				} else {
					outGridFlipped[kk] = inGrid[ii];
					kk++;
				}
			}
		}

		return outGridFlipped;
	}
	
	/*
	 * Changes the missing data value from
	 * CAVE -999999.0f to GEMPAK -9999.0f
	 */
	private float[] checkMissingData( float[] inGrid ) {

		float[] outGridFlipped = new float[inGrid.length];


		for (int ii = 0; ii < inGrid.length; ii++) 	{	
			if (inGrid[ii] < -900000.0) {
				outGridFlipped[ii] = -9999.0f;
			} else {
				outGridFlipped[ii] = inGrid[ii];
			}
		}

		return outGridFlipped;
	}
	/*
	 * Flops the data from GEMPAK order and changes the missing data value from
	 * GEMPAK -9999.0f to CAVE -999999.0f
	 */
	private float[] flopData(float[] inGrid, int nx, int ny) {

		float[] outGridFlopped = new float[inGrid.length];
		int kk = 0;
		for (int jj = ny - 1; jj >= 0; jj--) {
			int m1 = nx * jj;
			int m2 = m1 + (nx - 1);
			for (int ii = m1; ii <= m2; ii++) {
				if (inGrid[ii] == -9999.0f) {
					outGridFlopped[kk] = -999999.0f;
					kk++;
				} else {
					outGridFlopped[kk] = inGrid[ii];
					kk++;
				}
			}

		}
		return outGridFlopped;
	}
	
	/*
	 * Revert data from GEMPAK order and changes the missing data value from
	 * GEMPAK -9999.0f to CAVE -999999.0f
	 */
	private float[] revertGempakData2CAVE( float[] inGrid ) {

		float[] outGridFlopped = new float[inGrid.length];


		for (int ii = 0; ii < inGrid.length; ii++) 	{	
			if (inGrid[ii] == -9999.0f) {
				outGridFlopped[ii] = -999999.0f;
			} else {
				outGridFlopped[ii] = inGrid[ii];
			}
		}
	
		return outGridFlopped;
	}	
	private String getEnsTimes () {
		String tmp1 = this.gdfile.substring(this.gdfile.indexOf("{")+1, this.gdfile.indexOf("}"));
		String[] tmp = tmp1.split(",");
		String tmp2 = "";
		for ( String st: tmp) {
			String tmp3[]  = st.split("\\|");
			tmp2 = tmp2 + tmp3[1];
			tmp2 = tmp2 + "|";
			
		}
		String returnStr = tmp2.substring(0, tmp2.length()-1);
		return returnStr;
	}
	
	private void printInfoMessage (long t) {
		String gdFile;
		if ( this.gdfile.startsWith("{") && this.gdfile.endsWith("}") ) {
			gdFile = this.gdfile.substring(this.gdfile.indexOf("{") + 1, this.gdfile.indexOf("}"));
		}
		else {
			gdFile = this.gdfile;
		}
		StringBuilder toprint = new StringBuilder();
//		System.out.print ( "\nCalculating " + gdFile.toUpperCase() + " ");
		toprint.append("Requesting " + gdFile.toUpperCase() + " ");
		
		/*
		if (this.vector) {
//			System.out.print(this.gvect.toUpperCase());
			toprint.append(this.gvect.toUpperCase());
		}
		else {
//			System.out.print(this.gfunc.toUpperCase());
			toprint.append(this.gfunc.trim().toUpperCase());
		}
		*/
		toprint.append(this.gdpfun.trim().toUpperCase());
		
//		System.out.print ( " ^" + this.gdattim + " @" + this.glevel + " %" + this.gvcord.toUpperCase());
//		System.out.print (" has taken --> " + t + " ms\n");
//		//System.out.println("\nMin: " + resultMin + "    Max: " + resultMax + "\n");
		toprint.append(" ^" + this.gdattim + " @" + this.glevel + " %" + this.gvcord.toUpperCase());
		toprint.append(" from " + this.dataSource + " took: " + t + " ms\n");
//		toprint.append("\nMin: " + resultMin + "    Max: " + resultMax + "\n");
//		System.out.println(toprint.toString());
		logger.debug(toprint.toString());
		
	}
	
	private static float[] retrieveData(String dataURI) throws VizException {

		long t001 = System.currentTimeMillis();
		IDataRecord dr = null;
		
		try {
			String fileName = getFilename(dataURI);
			String dataset = "Data";
			Request request = Request.ALL;

			IDataStore ds = DataStoreFactory.getDataStore(new File(fileName));
			dr = ds.retrieve("", dataURI + "/" + dataset, request);
//			dr = ds.retrieve(dataURI ,dataset, request);
			float[] data = (float[]) dr.getDataObject();
			long t002 = System.currentTimeMillis();
			if ( ncgribLogger.enableDiagnosticLogs() )
				logger.info("***Reading " + dataURI + " from hdf5 took: " + (t002-t001));
			//System.out.println("Reading from hdf5 took: " + (t002-t001));
			return data;
		} catch (Exception e) {
			throw new VizException("Error retrieving data for record:"
					+ dataURI, e);
		}
	}

	private float[] retrieveDataFromRetriever (String dataURI) throws VizException{

		long t001 = System.currentTimeMillis();
		IDataRecord dr = null;
		//create GridDataRetriever using datauri
		//setUnit for parameter
		//setWorldWrapColumns (1);
		//check its return value/exception and decide to update coverage or not
		GridDataRetriever dataRetriever = new GridDataRetriever (dataURI);
		boolean isWorldWrap = false;
		try {
			isWorldWrap = dataRetriever.setWorldWrapColumns (1);
		} catch (GridCoverageException e) {
			//ignore setWorldWrapColumns exception.
		}
		try {
			String gempakParm = cacheData.getGempakParam(dataURI);
			if ( gempakParm != null ) {
				dataRetriever.setUnit(UnitConv.deserializer(gempakParmInfo.getParmUnit(gempakParm)));
			}
		} catch (ConversionException e) {
			//ignore setUnit exception. use default units
		} catch (ParseException p) {
			//ignore deserializer exception.use default units
		}
		long t002 = System.currentTimeMillis();
		if ( ncgribLogger.enableDiagnosticLogs() )
			logger.info("***Initialize GridDataRetriever for " + dataURI + " took: " + (t002-t001));
		try {
			t001 = System.currentTimeMillis();
			FloatDataRecord dataRecord = dataRetriever.getDataRecord();
			float[] data = dataRecord.getFloatData();
			if ( isWorldWrap ) {
				setSubgSpatialObj ( (ISpatialObject)dataRetriever.getCoverage());
			}
			t002 = System.currentTimeMillis();
			if ( ncgribLogger.enableDiagnosticLogs() )
				logger.info("***Reading " + dataURI + " from hdf5 took: " + (t002-t001));
			return data;
		} catch (StorageException s) {
			if ( ncgribLogger.enableDiagnosticLogs() )
				logger.info("???? getDataRecord --- throw StorageException" );
			return null;
		}
	}

	public static String getFilename(String dataURI) {
		String filename = null;
		File file = null;
		String [] uriStr = dataURI.split("/");
		String path = uriStr[3];
		StringBuilder sb = new StringBuilder();
		String []tmStr = uriStr[2].split("_");
		String dataDateStr = tmStr[0];
		String fhrs = tmStr[2].substring(tmStr[2].indexOf("(") + 1,tmStr[2].indexOf(")"));
		String fhStr ;
		if ( fhrs == null) fhStr = "000";
		else {
			int number = 0;
			try {
				number = Integer.parseInt(fhrs);
			} catch (NumberFormatException e) {
				
			}
			fhStr = forecastHourFormat.format(number);
		}
		sb.append(path);
		sb.append("-");
		sb.append(dataDateStr);
		String dataTimeStr = tmStr[1].split(":")[0]+ "-FH-" + fhStr;
		sb.append("-");
		sb.append(dataTimeStr);
		sb.append(".h5");

		//if (DataMode.getSystemMode() == DataMode.THRIFT) {
		//	file = new File(File.separator + dataURI.split("/")[1]
		//			+ File.separator + path + File.separator + sb.toString());
		//} else if (DataMode.getSystemMode() == DataMode.PYPIES) {
			file = new File(dataURI.split("/")[1] + File.separator + path
					+ File.separator + sb.toString());
		//} else {
		//	file = new File(VizApp.getDataDir() + File.separator
		//			+ dataURI.split("/")[1] + File.separator + path
		//			+ File.separator + sb.toString());
		//}

		if (file != null)
			filename = file.getAbsolutePath();
		return filename;
	}
	
	private String getGridNavigationContent(ISpatialObject obj) {

		GridCoverage gc = (GridCoverage) obj;
		StringBuilder resultsBuf = new StringBuilder();

		if (gc instanceof LatLonGridCoverage) {
			/*
			 * LatLonGridCoverage
			 */
			LatLonGridCoverage llgc = (LatLonGridCoverage) gc;
			resultsBuf.append("CED");
			resultsBuf.append(";");
			resultsBuf.append(llgc.getNx());
			resultsBuf.append(";");
			resultsBuf.append(llgc.getNy());
			resultsBuf.append(";");
			Double dummy = llgc.getLa1() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = llgc.getLo1() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			double ddy = llgc.getDy() * (llgc.getNy()-1);
			if ( llgc.getFirstGridPointCorner() == Corner.UpperLeft) {
				dummy = (llgc.getLa1() - ddy) * 10000;
				this.flip = true;
			}
			else {
				dummy = (llgc.getLa1() + ddy) * 10000;
			}
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			double ddx = llgc.getDx () * ( llgc.getNx() -1 );
			dummy = (llgc.getLo1()+ddx) * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = -9999.0;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = llgc.getDx() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = llgc.getDy() * 10000;
			resultsBuf.append(dummy.intValue());
		} else if (gc instanceof LambertConformalGridCoverage) {
			resultsBuf.append("LCC");
			resultsBuf.append(";");
			LambertConformalGridCoverage lcgc = (LambertConformalGridCoverage) gc;
			if ( lcgc.getFirstGridPointCorner() == Corner.UpperLeft) {
				this.flip = true;
			}
			resultsBuf.append(lcgc.getNx());
			resultsBuf.append(";");
			resultsBuf.append(lcgc.getNy());
			resultsBuf.append(";");
			Double dummy = lcgc.getLa1() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = lcgc.getLo1() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = lcgc.getLatin1() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = lcgc.getLatin2() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = lcgc.getLov() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = lcgc.getDx() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = lcgc.getDy() * 10000;
			resultsBuf.append(dummy.intValue());
		} else if (gc instanceof MercatorGridCoverage) {
			MercatorGridCoverage mgc = (MercatorGridCoverage) gc;
			if ( mgc.getFirstGridPointCorner() == Corner.UpperLeft) {
				this.flip = true;
			}
			resultsBuf.append("MER");
			resultsBuf.append(";");
			resultsBuf.append(mgc.getNx());
			resultsBuf.append(";");
			resultsBuf.append(mgc.getNy());
			resultsBuf.append(";");
			Double dummy = mgc.getLa1() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = mgc.getLo1() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = mgc.getLatin() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = mgc.getLa2() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = mgc.getLo2() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = mgc.getDx() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = mgc.getDy() * 10000;
			resultsBuf.append(dummy.intValue());
		} else if (gc instanceof PolarStereoGridCoverage) {
			/*
			 * PolarStereoGridCoverage
			 */
			PolarStereoGridCoverage psgc = (PolarStereoGridCoverage) gc;
			if ( psgc.getFirstGridPointCorner() == Corner.UpperLeft) {
				this.flip = true;
			}
			resultsBuf.append("STR");
			resultsBuf.append(";");
			resultsBuf.append(psgc.getNx());
			resultsBuf.append(";");
			resultsBuf.append(psgc.getNy());
			resultsBuf.append(";");
			Double dummy = psgc.getLa1() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = psgc.getLo1() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = -9999.0;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = -9999.0;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = psgc.getLov() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = psgc.getDx() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = psgc.getDy() * 10000;
			resultsBuf.append(dummy.intValue());
		}

		String content = resultsBuf.toString();
		return content;

	}
	
	private void setGridFlip(ISpatialObject obj) {

		GridCoverage gc = (GridCoverage) obj;
		
		if ( gc.getFirstGridPointCorner() == Corner.UpperLeft) {
				this.flop = true;
		}
	}
	
	private String executeScript(String scriptToRun) throws VizException {

		if ( scriptToRun == null ) return null;
		logger.debug("executeScript: scriptToRun=" + scriptToRun );
		String []parms = scriptToRun.split("\\|");
		if ( parms.length < 4 ) return null;
		String modelName = parms[0];
		String dbTag = parms[1];
		String eventName = parms[2];
		String tmStr = constructTimeStr (parms[3]);

//		logger.info("executeScript:modelname=" + modelName + " dbTag=" +dbTag + " eventName="+eventName + " time=" + tmStr);
		HashMap<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
		rcMap.put( GridDBConstants.PLUGIN_NAME, new RequestConstraint(GridDBConstants.GRID_TBL_NAME) );
        rcMap.put( GridDBConstants.MODEL_NAME_QUERY, new RequestConstraint( modelName,ConstraintType.EQUALS ) );
        if ( !eventName.equalsIgnoreCase("null")) {
        	rcMap.put( GridDBConstants.ENSEMBLE_ID_QUERY, new RequestConstraint( eventName,ConstraintType.EQUALS ) );
        }
        rcMap.put( GridDBConstants.DATA_URI_QUERY, new RequestConstraint( tmStr,ConstraintType.LIKE ) );
        
        DbQueryRequest request = new DbQueryRequest();
        request.addRequestField(GridDBConstants.REF_TIME_QUERY);
        request.addRequestField(GridDBConstants.FORECAST_TIME_QUERY);
        request.setDistinct(true);
        request.setConstraints(rcMap);
		long t0 = System.currentTimeMillis();
        String retFileNames = "";
        try {
        	DbQueryResponse response = (DbQueryResponse) ThriftClient
        	.sendRequest(request);
        	// extract list of results
        	List<Map<String, Object>> responseList = null;
        	if (response != null) {
        		responseList = response.getResults();
        	} else {
        		// empty list to simplify code
        		responseList = new ArrayList<Map<String, Object>>(0);
        	}
        	String prefix = modelName + "_" + dbTag + "_" + eventName+"_";
        	for ( int i = 0; i < responseList.size(); i ++ ) {
        		Object fhrValue = responseList.get(i).get(GridDBConstants.FORECAST_TIME_QUERY);
        		Object refValue = responseList.get(i).get(GridDBConstants.REF_TIME_QUERY);
        		if ( fhrValue != null && fhrValue instanceof Integer &&
        				refValue != null && refValue instanceof Date ) {
        			int fhr = ((Integer)fhrValue).intValue()/3600;
        			DataTime refTime = new DataTime ((Date)refValue);
        			String []dts = refTime.toString().split(" ");
        			
        			String dt = dts[0].replace("-", "");
        			
        			String hh = dts[1].split(":")[0];
        			if ( retFileNames.length() > 0 ) retFileNames = retFileNames + "|";
        			retFileNames = retFileNames + prefix+dt + hh + "f" + forecastHourFormat.format(fhr);
        		}

        	}
        } catch (VizException e) {
        	
        }
		return retFileNames;
	}
	
	private String constructTimeStr (String gempakTimeStr ) {
		String gempakTimeStrCycle = gempakTimeStr.split("f")[0];
		gempakTimeStrCycle = gempakTimeStrCycle.replace("[0-9]", "%");
		if ( gempakTimeStrCycle.length() < 10 ) return null;
		
		String gempakTimeStrFFF = gempakTimeStr.split("f")[1];
		gempakTimeStrFFF     = gempakTimeStrFFF.replace("[0-9]", "%");
		
		String timeStr;
		try {
			int fhr = Integer.parseInt( gempakTimeStrFFF )/3600;
			
			timeStr = gempakTimeStrCycle.substring(0, 4) + "-" + gempakTimeStrCycle.substring(4, 6) + "-" +
					gempakTimeStrCycle.substring(6, 8) + "_" + gempakTimeStrCycle.substring(8, 10) + "%_("+fhr+")%";
		} catch (NumberFormatException e) {
			timeStr = gempakTimeStrCycle.substring(0, 4) + "-" + gempakTimeStrCycle.substring(4, 6) + "-" +
			gempakTimeStrCycle.substring(6, 8) + "_" + gempakTimeStrCycle.substring(8, 10) + "%_(" + gempakTimeStrFFF;
		}
		return timeStr;
	}
	
	private String getDataURI(String parameters) throws VizException {
		long t0 = System.currentTimeMillis();
		
		String datauri = cacheData.getDataURI(parameters);
		if ( datauri != null ) {
			if ( ncgribLogger.enableDiagnosticLogs() ) {
				long t00 = System.currentTimeMillis();
				logger.info("++++ getDataURI for("+parameters+") from cache took: " + (t00-t0));
			}
			return datauri;
		}
		//get all param/vcord units
		//call GridQueryAssembler to get request constrains
		
		String[] parmList = parameters.split("\\|");
		logger.debug ("enter getDataUri - parameters:"+ parameters);
		HashMap<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
		rcMap.put( GridDBConstants.PLUGIN_NAME, new RequestConstraint(GridDBConstants.GRID_TBL_NAME) );
        rcMap.put( GridDBConstants.MODEL_NAME_QUERY, new RequestConstraint( parmList[0] ) );

        if( !parmList[1].isEmpty()) {
        	if ( isEnsCategory ) {
        		if ( !parmList[1].equalsIgnoreCase("null"))
        			rcMap.put( GridDBConstants.ENSEMBLE_ID_QUERY, new RequestConstraint( parmList[1] ) );
        	}
        	else {
        		if ( gridRscData.getEnsembelMember() != null ) {
        			rcMap.put( GridDBConstants.ENSEMBLE_ID_QUERY, new RequestConstraint( parmList[1] ) );
        }
        		else {
        			rcMap.put( GridDBConstants.EVENT_NAME_QUERY, new RequestConstraint( parmList[1] ) );
        		}
        	}
        }
        rcMap.put( GridDBConstants.PARAMETER_QUERY, new RequestConstraint( parmList[2] ) );
        rcMap.put( GridDBConstants.LEVEL_ID_QUERY, new RequestConstraint( parmList[3] ) );

        // This is now querying modelInfo.level.levelonevalue (a Double) instead of glevel1 (an int)
        // so to get the constraint to work we will need ".0"
        // TODO : hack until we do this the right way.
        //
        String ll1 = null, ll2 = null;
        if ( parmList[4].contains(":")) {
        	ll1 = parmList[4].split(":")[0];
        	ll2 = parmList[4].split(":")[1];
        }
        else {
        	ll1 = parmList[4];
        }
        if ( ll1 == null && ll2 == null ) return null;
        if ( ll1 != null ) {
        	rcMap.put( GridDBConstants.LEVEL_ONE_QUERY, new RequestConstraint( ll1+".0" ) );
        }
        if ( ll2 != null ) {
        	rcMap.put( GridDBConstants.LEVEL_TWO_QUERY, new RequestConstraint( ll2+".0" ) );
        }
        ArrayList<String>  rslts = NcGridInventory.getInstance().searchNcGridInventory( rcMap, GridDBConstants.LEVEL_TWO_QUERY );
        
        long t11 = System.currentTimeMillis();
        if ( rslts == null || rslts.isEmpty() ) {
        	if ( ncgribLogger.enableDiagnosticLogs() )
        		logger.info ("???????query NcGridInventory DB return NULL for ("+parmList[0] +"," + parmList[1] +
					"," + parmList[2]+","+parmList[3]+","+parmList[4]+")===");
			return null;
		}
        if ( ncgribLogger.enableDiagnosticLogs() )
        	logger.info ("NcGridInventory.searchNcGridInventory:rslts=" + rslts.toString() + " took: " +(t11-t0));
        
        
        String [] tmpStr = rslts.get(0).split("/");
        
        // HACK: the inventory uses modelInfo.level to store the levels because they are in the URI and not glevels
        // but the uengin script won't work querying the modelInfo.level.leveltwovalue (???) so we have to query
        // glevel2 (an int) instead of the double modelInfo.level 

        String lvl1 = tmpStr[6];
        String lvl2 = tmpStr[7];        
        if( lvl2.equals( Level.getInvalidLevelValueAsString() ) ) {
        	lvl2 = "-9999.0";
        }
        
		String refTimeg = parmList[5].toUpperCase().split("F")[0];
		String refTime = GempakGrid.dattimToDbtime(refTimeg);
		refTime = refTime.substring(0, refTime.length()-2);
	    String fcstTimeg = parmList[5].toUpperCase().split("F")[1];
	    String fcstTime = Integer.toString(((Integer.parseInt(fcstTimeg))*3600));
	    
	    rcMap.remove( GridDBConstants.LEVEL_TWO_QUERY );
        rcMap.remove( GridDBConstants.LEVEL_ONE_QUERY );        
	    rcMap.put( GridDBConstants.LEVEL_TWO_QUERY, new RequestConstraint( lvl2 ) );
        rcMap.put( GridDBConstants.LEVEL_ONE_QUERY, new RequestConstraint( lvl1 ) );

	    
	    rcMap.put(GridDBConstants.REF_TIME_QUERY, new RequestConstraint(refTime));
	    rcMap.put(GridDBConstants.FORECAST_TIME_QUERY, new RequestConstraint(fcstTime));
	    
	    DbQueryRequest request = new DbQueryRequest();
        request.addRequestField(GridDBConstants.DATA_URI_QUERY);
        request.setConstraints(rcMap);
        
        DbQueryResponse response = (DbQueryResponse) ThriftClient
        .sendRequest(request);
     // extract list of results
        List<Map<String, Object>> responseList = null;
        if (response != null) {
            responseList = response.getResults();
        } else {
            // empty list to simplify code
            responseList = new ArrayList<Map<String, Object>>(0);
        }
        if (responseList.size() > 0) {
            Object dURI = responseList.get(0).get(GridDBConstants.DATA_URI_QUERY);
            if (dURI != null && dURI instanceof String) {
            	datauri = (String) dURI;
            }
        }
        long t1 = System.currentTimeMillis();
		if ( ncgribLogger.enableDiagnosticLogs() ) {
			if ( datauri != null )
				logger.info("### getDataURI("+datauri+") for("+parameters+") reftime:"+refTime+"("+Integer.parseInt(fcstTimeg)+") took: " + (t1-t0));
			else
				logger.info("??? getDataURI(null) for("+parameters+") reftime:"+refTime+"("+Integer.parseInt(fcstTimeg)+") took: " + (t1-t0));
		}
		
//		datauri = rec.getDataURI();
		if ( datauri != null )
			cacheData.addDataURI(parameters, datauri);
		return datauri;
	}

	private String getDataURIFromAssembler (String parameters) throws VizException {
		long t0 = System.currentTimeMillis();
		
		String datauri = cacheData.getDataURI(parameters);
		if ( datauri != null ) {
			if ( ncgribLogger.enableDiagnosticLogs() ) {
				long t00 = System.currentTimeMillis();
				logger.info("++++ getDataURIFromAssembler for("+parameters+") from cache took: " + (t00-t0));
		}
			return datauri;
		}
		Map<String, RequestConstraint> rcMap = getRequestConstraint (parameters);
		if ( ncgribLogger.enableDiagnosticLogs() ) {
			long t01 = System.currentTimeMillis();
			logger.info("++++ getRequestConstraint for("+parameters+") took: " + (t01-t0));
		}
		if ( rcMap == null ) return null;

		t0 = System.currentTimeMillis();
	    DbQueryRequest request = new DbQueryRequest();
        request.addRequestField(GridDBConstants.DATA_URI_QUERY);
        request.setConstraints(rcMap);

        DbQueryResponse response = (DbQueryResponse) ThriftClient
        .sendRequest(request);
     // extract list of results
        List<Map<String, Object>> responseList = null;
        if (response != null) {
            responseList = response.getResults();
        } else {
            // empty list to simplify code
            responseList = new ArrayList<Map<String, Object>>(0);
        }
        if (responseList.size() > 0) {
            Object dURI = responseList.get(0).get(GridDBConstants.DATA_URI_QUERY);
            if (dURI != null && dURI instanceof String) {
            	datauri = (String) dURI;
            }
        }
        long t1 = System.currentTimeMillis();
		if ( ncgribLogger.enableDiagnosticLogs() ) {
			String[] parmList = parameters.split("\\|");
			String refTimeg = parmList[5].toUpperCase().split("F")[0];
			String refTime = GempakGrid.dattimToDbtime(refTimeg);
			refTime = refTime.substring(0, refTime.length()-2);
		    String fcstTimeg = parmList[5].toUpperCase().split("F")[1];
		    String fcstTime = Integer.toString(((Integer.parseInt(fcstTimeg))*3600));
			if ( datauri != null )
				logger.info("### getDataURIFromAssembler("+datauri+") for("+parameters+") reftime:"+refTime+"("+Integer.parseInt(fcstTimeg)+") took: " + (t1-t0));
			else
				logger.info("??? getDataURIFromAssembler(null) for("+parameters+") reftime:"+refTime+"("+Integer.parseInt(fcstTimeg)+") took: " + (t1-t0));
		}
		
//		datauri = rec.getDataURI();
		if ( datauri != null )
			cacheData.addDataURI(parameters, datauri);
		return datauri;
	}

	public void setCycleForecastTimes(ArrayList<DataTime> dataTimes) {
		this.dataForecastTimes = dataTimes;
	}
	private int checkNativeLoggerLevel ( String msg ) {
		int lvl = 0;
    	for( String logType : nativeLogTypes ) {
        	if( msg.contains( logType ) ) {
        			break;
        	}
        	lvl ++;
    	}
		return lvl;
	}
	
	private String chechEnsembleGdFiles( String gdfile1 ) {
//		String [] members = {"gefs:01","gefs:02","gefs:03","gefs:12"};
		if (gdfile1.startsWith("{") && gdfile1.endsWith("}") ) {
			StringBuilder sba = new StringBuilder();
			String [] gdfileArray = gdfile1.substring(gdfile1.indexOf("{")+1, gdfile1.indexOf("}")).split(",");
			if ( gdfileArray.length == 0 ) return gdfile1;
			sba.append("{");
			for (int igd=0;igd<gdfileArray.length; igd++){
				if ( !gdfileArray[igd].contains(":") ) {
				    String model = gdfileArray[igd];
				    String wgt = null;
					if ( gdfileArray[igd].contains("%")) {
						String []wgts = gdfileArray[igd].split("%");
						wgt = wgts[0];
						model = wgts[1];
					}
					String cycleTime = null;
					if ( model.contains("|")) {
						String []tempAttrs = model.split("\\|");
						model = tempAttrs[0];
						cycleTime = tempAttrs[1];
					}
                    
                    ArrayList<String> members = ((NcEnsembleResourceData)gridRscData).getEnsembleMembersForModel(model);
                    if ( members != null && members.size() > 0) {
						int cnt = 0 ;
						int weight =-1;
						int mbrWt=0,sumWts=0;
						if ( wgt != null ) {
							weight = Integer.parseInt(wgt);
							mbrWt = weight/members.size();
						}
						for ( String mb : members) {
							cnt ++;
							if ( mbrWt !=0) {
								sumWts += mbrWt;
								if ( cnt == members.size()) {
									sba.append (String.valueOf(mbrWt+weight-sumWts));
								}
								else sba.append (String.valueOf(mbrWt));
								sba.append ("%");
							}
							sba.append(model);
							sba.append(":");
							sba.append (mb);
							if ( cycleTime != null ) {
								sba.append ("|");
								sba.append (cycleTime);
							}
							if ( cnt <  members.size())
								sba.append(",");
							}
					}
					else {
						sba.append(gdfileArray[igd]);
					}
				}
				else {
					sba.append(gdfileArray[igd]);
				}
				if ( igd < (gdfileArray.length -1) )
					sba.append(",");
			}
			sba.append("}");
			return sba.toString();
		}
		return gdfile1;
	}
	
	private String getEnsembleNavigation ( String msg){
		String navStr = null;
		logger.debug("getEnsembleNavigation: " + msg + " dataTime:" + dataForecastTimes.get(0).toString());
		String []tmpAttrs = msg.split("\\|");
		Map<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
		queryList.put(GridDBConstants.PLUGIN_NAME, new RequestConstraint( GridDBConstants.GRID_TBL_NAME, ConstraintType.EQUALS ) );
		queryList.put(GridDBConstants.MODEL_NAME_QUERY, 
        		new RequestConstraint( tmpAttrs[0], ConstraintType.EQUALS ) );
		if (tmpAttrs[1] != null && tmpAttrs[1].length() > 0 && (!tmpAttrs[1].equalsIgnoreCase("null"))) {
			queryList.put(GridDBConstants.ENSEMBLE_ID_QUERY, 
					new RequestConstraint( tmpAttrs[1], ConstraintType.EQUALS ) );
		}
		if ( tmpAttrs[2] != null && tmpAttrs[2].length() > 0) {
			String navTime = buildRefTime(tmpAttrs[2]);
			logger.debug("getEnsembleNavigation: " + navTime );
			queryList.put(GridDBConstants.DATA_TIME_QUERY, new RequestConstraint( navTime ) );
		}
		else {
			queryList.put(GridDBConstants.DATA_TIME_QUERY, new RequestConstraint( dataForecastTimes.get(0).toString() ) );
		}
		DbQueryRequest request = new DbQueryRequest();
        request.addRequestField(GridDBConstants.NAVIGATION_QUERY);
        request.setLimit(1);
        request.setConstraints(queryList);
        try {
        	DbQueryResponse response = (DbQueryResponse) ThriftClient
        	.sendRequest(request);
        	// extract list of results
        	List<Map<String, Object>> responseList = null;
        	if (response != null) {
        		responseList = response.getResults();
        	} else {
        		// empty list to simplify code
        		responseList = new ArrayList<Map<String, Object>>(0);
        	}
        	ISpatialObject cov = null;
        	if (responseList.size() > 0) {
        		Object spatialObj = responseList.get(0).get(GridDBConstants.NAVIGATION_QUERY);
        		if (spatialObj != null && spatialObj instanceof ISpatialObject) {
        			cov = (ISpatialObject) spatialObj;
        		}
		}
        
			if ( cov != null ) {
				navStr = getGridNavigationContent ( cov);
			}
        }catch ( VizException e) {
        	
        }
		
/*		LayerProperty prop = new LayerProperty();
		prop.setDesiredProduct(ResourceType.PLAN_VIEW);
		try {
			prop.setEntryQueryParameters(queryList, false);
			prop.setNumberOfImages(1);
		
			String script = null;
			script = ScriptCreator.createScript(prop);

			if (script == null) {
				return null;
			}
//System.out.println("HERESMYQUERY: "+script);
			Object[] pdoList = Connector.getInstance().connect(script, null, 60000);
			if ( pdoList !=null && pdoList.length>0 ) {
				ISpatialObject cov = ((GridRecord) pdoList[0]).getLocation();
				navStr = getGridNavigationContent ( cov);
			}
		}
		catch( VizException e) {
        	return null;
        }*/
		return navStr;
	}
	
	private String buildRefTime( String navTime) {
		StringBuilder reftime = new StringBuilder();
		String []dt = navTime.split("f");
		reftime.append(dt[0].substring(0, 4));
		reftime.append("-");
		reftime.append (dt[0].substring(4, 6));
		reftime.append("-");
		reftime.append (dt[0].substring(6, 8));
		reftime.append(" ");
		reftime.append (dt[0].substring(8,dt[0].length() ));
		reftime.append(":00:00.0 (");
		int ft =0;
		if ( dt[1] != null && dt[1].length() > 0 ){
			ft = Integer.parseInt(dt[1]);
		}
		reftime.append(String.valueOf(ft));
		reftime.append(")");
		return reftime.toString();
	}
	
	/*
	 * Use unified grid plugin mappinng
	 */
	private Map<String, RequestConstraint> getRequestConstraint ( String parameters ){
		GridQueryAssembler qAssembler = new GridQueryAssembler ("GEMPAK");
		String[] parmList = parameters.split("\\|");
		if ( ncgribLogger.enableDiagnosticLogs() ) {
			logger.info ("enter getRequestConstraint - parameters:"+ parameters);
		}
		qAssembler.setDatasetId(parmList[0]);
		
        if( !parmList[1].isEmpty()) {
        	if ( isEnsCategory ) {
        		if ( !parmList[1].equalsIgnoreCase("null"))
        			qAssembler.setEnsembleId(parmList[1] );
        	}
        	else {
        		if ( gridRscData.getEnsembelMember() != null ) {
        			qAssembler.setEnsembleId(parmList[1] );
        		}
        		else {
        			qAssembler.setSecondaryId(parmList[1]);
        		}
        	}
        }
        qAssembler.setParameterAbbreviation(parmList[2]);
        qAssembler.setMasterLevelName (parmList[3]);

        String ll1 = null, ll2 = null;
        if ( parmList[4].contains(":")) {
        	ll1 = parmList[4].split(":")[0];
        	ll2 = parmList[4].split(":")[1];
        }
        else {
        	ll1 = parmList[4];
        }
        if ( ll1 == null && ll2 == null ) return null;
        if ( ll1 != null ) {
        	Double level1;
        	try {
        		level1 = Double.valueOf(ll1);
        	} catch (NumberFormatException e) {
        		return null;
        	}
        	qAssembler.setLevelOneValue(level1);
        }
        if ( ll2 != null ) {
        	Double level2;
        	try {
        		level2 = Double.valueOf(ll2);
        	} catch (NumberFormatException e) {
        		return null;
        	}
        	qAssembler.setLevelTwoValue(level2);
        }
        else {
        	qAssembler.setLevelTwoValue(-9999.0);
        }
        
        qAssembler.setLevelUnits(gempakVcordInfo.getVcrdUnit(parmList[3]));
        Map<String, RequestConstraint> rcMap;
        try {
        	rcMap = qAssembler.getConstraintMap();
        } catch (CommunicationException e) {
        	if ( ncgribLogger.enableDiagnosticLogs() ) {
    			logger.info ("getConstraintMap - CommunicationException:" + e.toString());
    		}
        	return null;
        }
		String refTimeg = parmList[5].toUpperCase().split("F")[0];
		String refTime = GempakGrid.dattimToDbtime(refTimeg);
		refTime = refTime.substring(0, refTime.length()-2);
	    String fcstTimeg = parmList[5].toUpperCase().split("F")[1];
	    String fcstTime = Integer.toString(((Integer.parseInt(fcstTimeg))*3600));
	    
	    rcMap.put(GridDBConstants.REF_TIME_QUERY, new RequestConstraint(refTime));
	    rcMap.put(GridDBConstants.FORECAST_TIME_QUERY, new RequestConstraint(fcstTime));
		return rcMap;
	}
}

