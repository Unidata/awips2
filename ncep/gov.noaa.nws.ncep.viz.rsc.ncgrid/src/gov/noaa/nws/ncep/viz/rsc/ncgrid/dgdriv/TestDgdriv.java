package gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv;
/**
 * 
 */

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.sun.jna.Native;
import com.sun.jna.ptr.FloatByReference;
import com.sun.jna.ptr.IntByReference;

//import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
//import com.raytheon.uf.common.derivparam.tree.DataTree;
//import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceType;

import gov.noaa.nws.ncep.viz.gempak.grid.jna.GridDiag;
import gov.noaa.nws.ncep.viz.gempak.grid.jna.GridDiag.gempak;
import gov.noaa.nws.ncep.viz.gempak.util.GempakGrid;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv.DgdrivException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
//import gov.noaa.nws.ncep.viz.gempak.grid.inv.NcInventory;
import gov.noaa.nws.ncep.edex.common.dataRecords.NcFloatDataRecord;

import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.MercatorGridCoverage;
import com.raytheon.uf.common.gridcoverage.PolarStereoGridCoverage;
//import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
//import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;

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
 * </pre>
 *
 * @author tlee
 * @version 1.0
 */

public class TestDgdriv {
	//private static NcepLogger logger = NcepLoggerManager.getNcepLogger(TestDgdriv.class);
	private static GridDiag gd;
	private String gdfile, gdpfun, gdattim, glevel, gvcord, scale, garea, proj, dataSource;
	private boolean scalar;
	private String gdfileOriginal;
	
	private float resultMin=9999999.0f, resultMax=0.0f;
	private ISpatialObject spatialObj; 
	private ArrayList<DataTime> dataForecastTimes;
	private static Connector conn;
//	private NcInventory inventory;
	
   /*
    * TODO Work around solution - need to find away to set logging level programmatically
    */
	private static boolean nativeLogging = true;
//	private static boolean nativeLogging = false;
  

	public TestDgdriv() {
		/*
		 * Initialize GEMPLT, DG and grid libraries.
		 */
//		gd.initialize();
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
		scalar = false;
		dataForecastTimes = new ArrayList<DataTime>();
//	    dataTree = NcInventory.getInstance().getNcDataTree();
//	    inventory = NcInventory.getInstance();
		try {
			conn = Connector.getInstance();
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
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


	public void setGdfile (String gdfile) {
		this.gdfile = gdfile;
		this.gdfileOriginal = gdfile;
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
	}
	
	public void setSpatialObject(ISpatialObject spatialObject) {
		this.spatialObj = spatialObject;
		
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
    
    IntByReference maxgrd = new IntByReference(IMISSD);
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
	private String eventName;
	
	DiagnosticsCallback diagCallback=null;
	ReturnFileNameCallback flnmCallback = null;
	ReturnCycleForecastHoursCallback fhrsCallback = null;
	ReturnNavigationCallback navCallback = null;
	ReturnDataCallback dataCallback = null;
	ReturnDataURICallback duriCallback = null;
	
	private class DiagnosticsCallback implements gempak.DbCallbackWithMessage {

		@Override
		public boolean callback(String msg) {
			String sep = "::";
			if ( msg.contains("|debug") ) {
				String logMessage = msg.split("\\|")[0] + ":" + msg.split(sep)[1];
				System.out.println("\t\tC DEBUG MESSAGE " + logMessage);
				//logger.debug("C DEBUG MESSAGE " +logMessage);
			}
			else if ( msg.contains("|info") ) {
				String logMessage = msg.split("\\|")[0] + ":" + msg.split(sep)[1];
				System.out.println("\tC INFO MESSAGE " + logMessage);
				//logger.info("C INFO MESSAGE " +logMessage);
			}
			else if ( msg.contains("|error") ) {
				String logMessage = msg.split("\\|")[0] + ":" + msg.split(sep)[1];
				System.out.println("\nC ERROR MESSAGE " + logMessage);
				//logger.error("C ERROR MESSAGE " +logMessage);
			}
			
			return true;
		}
	}
	
	private class ReturnDataCallback implements gempak.DbCallbackWithMessage {

		@Override
		public boolean callback(String msg) {
			if (msg.contains("/")) {				
				try {
					String[] msgArr = msg.split(";");
					if (msgArr.length == 3) {
						String dataURI = msgArr[0];
						int nx = Integer.parseInt(msgArr[1].trim());
						int ny = Integer.parseInt(msgArr[2].trim());
						long t0 = System.currentTimeMillis();
						//float[] rData = retrieveData(dataURI);
						float[] rData = new float[nx*ny];
						for ( int j=0; j<(nx*ny); j++ ) rData[j]= (float)j;
						
						
						long t1 = System.currentTimeMillis();				        
						int rDataSize = rData.length;						
						IntByReference datSize = new IntByReference(rDataSize);
						gd.gem.db_returndata(flipData(rData, nx, ny),datSize);
						long t2 = System.currentTimeMillis();
						System.out.println("\tretrieve data took " + (t1-t0));
				        //logger.info("retrieve data took " + (t1-t0));
				        System.out.println("\treturn data took " + (t2-t1));
				        //logger.info("return data took " + (t2-t1));
						return true;
					} else {
						errorURI = msg;
						proces = false;
						return true;
					}
				} catch (Exception e) {
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
			long t0 = System.currentTimeMillis();
			//String navigationString = getGridNavigationContent(spatialObj);			
			String navigationString = "STR;147;110;2680;-1394750;-9999;-9999;-1050000;907550;907550";
			//logger.debug("Retrieved this navigation string " + navigationString);
			gd.gem.db_returnnav(navigationString);
			long t1 = System.currentTimeMillis();
			System.out.println("\treturn navigation " + navigationString + " took " + (t1-t0));
			//logger.info("return navigation  " + navigationString + " took " + (t1-t0));
			return true;
		}
	}
	
	private class ReturnDataURICallback implements gempak.DbCallbackWithMessage {

		@Override
//		public boolean callback(String msg) {
//			if (msg.contains("import")) {
//				try {
//					long t0 = System.currentTimeMillis();
//					String dataURI = executeScript(msg);				
//					gd.gem.db_returnduri(dataURI);
//					long t1 = System.currentTimeMillis();
//					System.out.println("\treturn dataURI " + dataURI + " took " + (t1-t0));
//					logger.info("return dataURI " + dataURI + " took " + (t1-t0));
//					return true;
//				} catch (VizException e) {
//					errorURI = msg;
//					proces = false;
//					return true;
//				}
//			} else {
//				return true;
//			}
//		}
		public boolean callback(String msg) {
			try {
				long t0 = System.currentTimeMillis();
				System.out.println("Looking for DURI: "+msg);
				//String dataURI = getDataURI(msg);
				String dataURI = "/ncgrib/2011-10-03_12:00:00.0_(6)/NAM104/GH/MB/200.0/-999999.0/null/null/NAM104/2/nam.t12z.grbgrd06.tm00.grib2/HGHT/PRES/45";
				System.out.println("GotBack DURI: "+dataURI);
				gd.gem.db_returnduri(dataURI);
				long t1 = System.currentTimeMillis();
				System.out.println("\treturn dataURI " + dataURI + " took " + (t1-t0));
				//logger.info("return dataURI " + dataURI + " took " + (t1-t0));
				return true;
			} catch (Exception e) {
				errorURI = msg;
				proces = false;
				return true;
			}
		}
	}
	
	private class ReturnCycleForecastHoursCallback implements gempak.DbCallbackWithoutMessage {

		@Override
		public boolean callback() {
			long t0 = System.currentTimeMillis();
			//String cycleFcstHrsString = getCycleFcstHrsString(dataForecastTimes);
			String cycleFcstHrsString = "111003/1200f003|111003/1200f006|111003/1200f009|111003/1200f012";
			gd.gem.db_returnfhrs(cycleFcstHrsString);
			long t1 = System.currentTimeMillis();
			System.out.println("\treturn cycle forecast hours string " + cycleFcstHrsString + " took " + (t1-t0));
			//logger.info("return cycle forecast hours string " + cycleFcstHrsString + " took " + (t1-t0));
			return true;
		}
	}
	
	private class ReturnFileNameCallback implements gempak.DbCallbackWithMessage {

		@Override
		public boolean callback(String msg) {
			if (msg.contains("import")) {
				try {
					long t0 = System.currentTimeMillis();
					String fileNames = executeScript(msg);
					System.out.println("Retrieved members:" + fileNames);
					gd.gem.db_returnflnm(fileNames);
					long t1 = System.currentTimeMillis();
					System.out.println("\treturn file names string " + fileNames + " took " + (t1-t0));
					//logger.info("return file names string " + fileNames + " took " + (t1-t0));
					return true;
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
		
		if ( nativeLogging ) {
			diagCallback = new DiagnosticsCallback();
			gd.gem.db_diagCallback (diagCallback);
		}		
		
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
		
		
//		gd.gem.init_driver(iret);
//		if ( iret.getValue() != 0 ) {
//			throw new DgdrivException("From init_driver:  error initializing...");
//		}
		
		String currentTime = dbtimeToDattim(gdattim);
		gd.gem.db_wsetnavtime_ (currentTime, iret);
		if ( iret.getValue () != 0 ) {
			throw new DgdrivException ("From db_wsetnavtime: error setting the navigation time " + currentTime);
		}
		
		/*
		 * Process the gdfile string for ensemble request
		 */
		if (this.gdfile.startsWith("{") && this.gdfile.endsWith("}") ) {
			prepareEnsembleDTInfo();
		}
		else {
			prepareGridDTInfo();
		}

		long t05 = System.currentTimeMillis();
		//logger.info("init and settime took: " + (t05-t0));
	    /*
         * Process the GDFILE input.
         */
        if (proces) {
            if ( gdfile.contains(":") ) {
        		gd.gem.db_wsetevtname_ (gdfile.split(":")[1], iret);
        	}
        	gd.gem.dgc_nfil_ (gdfile, "", iret);
        	if ( iret.getValue () != 0 ) {
        		gd.gem.erc_wmsg ("DG", iret, "", ier);
        		proces = false;
        	}
        	
        }
        long t06 = System.currentTimeMillis();
        System.out.println("dgc_nfil took " + (t06-t05));
        //logger.info("dgc_nfil took: " + (t06-t05));
        
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
        //logger.info("dgc_ndtm took: " + (t07-t06));
      
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
            //logger.info("inc_scal took: " + (t07b-t07));
            
            /*
             * Get the next time to process from time server.
             */
            gd.gem.dgc_ntim_(chngnv, coladd, time1, time2, gottm, iret );
            long t08 = System.currentTimeMillis();
            //logger.info("dgc_ntim took: " + (t08-t07b));

			if ( iret.getValue () != 0 ) {
				gd.gem.erc_wmsg ("DG", iret, "", ier);
				proces = false;
			} else {
				gd.gem.tgc_dual (time1, time2, time, iret);
				long t08b = System.currentTimeMillis();
	            //logger.info("tgc_dual took: " + (t08b-t08));
			}
        }
        

		/*
		 * Set the map projection and graphics area.
		 */
        long t09a = System.currentTimeMillis();
        if (proces) {
        	gd.gem.dgc_fixa_ ( garea, proj, gareabuf, prjbuf, iret);
        	if ( iret.getValue () != 0 ) {
        		gd.gem.erc_wmsg ("DG", iret, "", ier);
        		proces = false;
        	}
        }
        long t09 = System.currentTimeMillis();
        //logger.info("dgc_fixa took: " + (t09-t09a));

        /*
         * Fortran wrapper used and use byte array instead of String as input!!
         */
        if (proces) {
        	gd.gem.ggc_maps (prjbuf, gareabuf, satfil, drpflg, iret);
        	if ( iret.getValue () != 0 ) {
        		gd.gem.erc_wmsg ("DG", iret, "", ier);
        		proces = false;
        	}
        }
        long t10 = System.currentTimeMillis();
        //logger.info("ggc_maps took: " + (t10-t09));

		/*
		 * Setup the grid subset that covers the graphics area.
		 */
//        if (proces) {
//        	gd.gem.dgc_subg_ (skip, maxgrd, ix1, iy1, ix2, iy2, iret);
//        	if ( iret.getValue () != 0 ) {
//        		gd.gem.erc_wmsg ("DG", iret, "", ier);
//        		proces = false;
//        	}
//        }
        long t11 = System.currentTimeMillis();
        //logger.info("dgc_subg took: " + (t11-t10));

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
		System.out.println("From gdc_nfil to dgc_grid took:" + (t012-t06));
		//logger.info("From gdc_nfil to dgc_grid took: " + (t012-t06));
		/*
		 * Compute the requested grid.
		 */
		if (proces) {
			
			
			if (scalar) {
				gd.gem.dgc_grid_( time, glevel, gvcord, gdpfun, pfunc, ugrid, igx, igy, 
						time1, time2, level1, level2, ivcord, parmu, iret);
				System.out.println("RETURN FROM DGC_GRID: "+iret.getValue());
				if (iret.getValue() != 0 ) System.out.println("NONZERORETURNFROMDGC_GRID!! "+iret.getValue());
			} else {
				gd.gem.dgc_vecr_ ( time, glevel,  gvcord,  gdpfun,  pfunc,
						ugrid, vgrid, igx,  igy, time1, time2, level1, level2, 
						ivcord, parmu, parmv, iret);
				if (iret.getValue() != 0 ) System.out.println("NONZERORETURNFROMDGC_VECR!! "+iret.getValue());
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
		System.out.println("dgc_grid took " + (t013-t012));
		//logger.info("dgc_grid took: " + (t013-t012));
		
		/*
		 * Compute the scaling factor and scale the grid data.
		 */
		if (proces) {
			NcFloatDataRecord fds = new NcFloatDataRecord();
			IntByReference ix12 = new IntByReference(1);
		    IntByReference iy12 = new IntByReference(1);
			gd.gem.grc_sscl( iscale, igx, igy, ix12, iy12, igx, igy, ugrid, rmin, rmax, iret);				
			fds.setXdata(flopData(ugrid, igx.getValue(), igy.getValue()));
			fds.setDimension(2);				
			fds.setSizes(new long[] { igx.getValue(), igy.getValue()});
			fds.setVector(false);
			
			resultMin = rmin.getValue();
			resultMax = rmax.getValue();
			
			if (!scalar){  // vector
				resultMin = 100.0f;
				resultMax = 0.0f;
				gd.gem.grc_sscl( iscale, igx, igy, ix1, iy1, ix2, iy2, vgrid, rmin, rmax, iret);
				fds.setYdata(flopData(vgrid, igx.getValue(), igy.getValue()));
				fds.setDimension(2);
				fds.setSizes(new long[] { igx.getValue(), igy.getValue()});
				fds.setVector(true);
				
				for (int i = 0; i < grid_size; i++) {
					if (ugrid[i] == -9999.0 || vgrid[i] == -9999.0) continue;
					float speed = (float) Math.hypot(ugrid[i], vgrid[i]);
					if (speed > resultMax) resultMax = speed;
					if (speed < resultMin) resultMin = speed;
				}
				
			}
			/*
			 *  Free memory for all internal grids
			 */
			gd.gem.dg_fall_( iret);
	        long t1 = System.currentTimeMillis();
	        //logger.info("Scaling took: " + (t1-t013));
	        System.out.println("Scaling took:" + (t1-t013));
	        printInfoMessage(t1 - t0);
		//	if (flnmCallback==null || fhrsCallback==null ||
		//			diagCallback==null || 	navCallback==null ||
		//			dataCallback==null || duriCallback ==null )  System.out.println("NULL CALLBACK");
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
			StringBuffer strBuf = new StringBuffer();
			while (index < num) {
				numerr.setValue(index);
				gd.gem.er_gerrmsg_(numerr, errmsg, iret);
				strBuf.append(Native.toString(errmsg));
				strBuf.append("\n");
				index++;
			}
			throw new DgdrivException(strBuf.toString());
        }

	}
	
	private void prepareGridDTInfo() {
		String alias = this.gdfile;
		String path = "A2DB_GRID";
		String template = this.gdfileOriginal + "_db";
		if ( this.gdfileOriginal.contains(":")) {
			template = this.gdfileOriginal.substring(0, this.gdfileOriginal.indexOf(":")) + "_db";
		}
		IntByReference iret = new IntByReference(0);
		System.out.println();
		gd.gem.db_seta2dtinfo_ (alias, path, template, iret);
		
	}

	private void prepareEnsembleDTInfo() {
		String thePath = "A2DB_GRID";
		StringBuffer sba = new StringBuffer();
		StringBuffer sbp = new StringBuffer();
		StringBuffer sbt = new StringBuffer();
		String [] gdfileArray = this.gdfileOriginal.substring(this.gdfileOriginal.indexOf("{")+1, this.gdfileOriginal.indexOf("}")).split(",");
		for (int igd=0;igd<gdfileArray.length; igd++){
			sbp.append(thePath);
			sbp.append("|");
			if ( gdfileArray[igd].contains("|") ) {
				String [] tempArr = gdfileArray[igd].split("\\|");
				String ensName = tempArr[0];
				sba.append("A2DB_" + ensName);
				System.out.println();
				sba.append("|");				
			    sbt.append(getEnsembleTemplate(ensName)); 
			    sbt.append("|");
			}
			else {
				String ensName = gdfileArray[igd];
				sba.append("A2DB_" + ensName);
				sba.append("|");				
			    sbt.append(getEnsembleTemplate(ensName)); 
			    sbt.append("|");
			}
		}
		String alias = sba.toString().substring(0, sba.toString().length()-1);
		String path = sbp.toString().substring(0, sbp.toString().length()-1);
		String template = sbt.toString().substring(0, sbt.toString().length()-1);
		IntByReference iret = new IntByReference(0);
		System.out.println();
		gd.gem.db_seta2dtinfo_ (alias, path, template, iret);
	}

	private String getEnsembleTemplate( String ensName) {
		StringBuilder query = new StringBuilder();
		query.append("import GempakEnsembleTemplateGenerator\n");
		query.append("query = GempakEnsembleTemplateGenerator.GempakEnsembleTemplateGenerator('ncgrib')\n");
		query.append("query.setEnsembleName('" + ensName + "')\n");
		query.append("return query.execute()\n");

		String scriptToRun = query.toString();
		long t0 = System.currentTimeMillis();

		Object[] pdoList;
		try {
			pdoList = conn.connect(scriptToRun, null, 60000);
			String ensTemplate = (String) pdoList[0];
			long t1 = System.currentTimeMillis();
	        System.out.println("\tgetEnsembleTemplate took: " + (t1-t0));
	        //logger.info("getEnsembleTemplate took: " + (t1-t0));
			return ensTemplate;
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
	}

	private String getEnsTime(String string) {
		// TODO Auto-generated method stub
		return string;
	}

	private String getTaggedGdfile() {
		if ( this.dataSource.equalsIgnoreCase("ncgrib")) {
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
				StringBuffer strb = new StringBuffer();
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

	private String getCycleFcstHrsString(ArrayList<DataTime> dataForecastTimes) {
		// TODO Auto-generated method stub
		StringBuffer resultsBuf = new StringBuffer();
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
		System.out.println(toprint.toString());
		//logger.info(toprint.toString());
		
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
			float[] data = (float[]) dr.getDataObject();
			long t002 = System.currentTimeMillis();
			//ogger.info("Reading " + dataURI + " from hdf5 took: " + (t002-t001));
			//System.out.println("Reading from hdf5 took: " + (t002-t001));
			return data;
		} catch (Exception e) {
			throw new VizException("Error retrieving data for record:"
					+ dataURI, e);
		}
	}

	public static String getFilename(String dataURI) {
		String filename = null;
		File file = null;
		String path = dataURI.split("/")[3];
		StringBuffer sb = new StringBuffer(64);
		String dataDateStr = dataURI.split("/")[2].split("_")[0];
		String dataTimeStr = dataURI.split("/")[2].split("_")[1].split(":")[0]
				+ dataURI.split("/")[2].split("_")[1].split(":")[1];
		sb.append(dataDateStr);
		sb.append("-");
		sb.append(dataTimeStr);
		sb.append(".h5");

		//if (DataMode.getSystemMode() == DataMode.THRIFT) {
		//	file = new File(File.separator + dataURI.split("/")[1]
		//			+ File.separator + path + File.separator + sb.toString());
		//} else if (DataMode.getSystemMode() == DataMode.PYPIES) {
			file = new File(VizApp.getServerDataDir() + File.separator
					+ dataURI.split("/")[1] + File.separator + path
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
	
	private static String getGridNavigationContent(ISpatialObject obj) {

		GridCoverage gc = (GridCoverage) obj;
		StringBuffer resultsBuf = new StringBuffer();

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
			dummy = llgc.getLa2() * 10000;
			resultsBuf.append(dummy.intValue());
			resultsBuf.append(";");
			dummy = llgc.getLo2() * 10000;
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
	
	private String executeScript(String scriptToRun) throws VizException {
		long t0 = System.currentTimeMillis();

		Object[] pdoList;
		pdoList = conn.connect(scriptToRun, null, 60000);
		String navStr = (String) pdoList[0];
		long t1 = System.currentTimeMillis();
        System.out.println("\texecuteScript(dataURI) took: " + (t1-t0));
        //logger.info("executeScript took: " + (t1-t0));
		return navStr;
	}
	
	private String getDataURI(String parameters) throws VizException {
		long t0 = System.currentTimeMillis();
		
		String[] parmList = parameters.split("\\|");
		Map<String, RequestConstraint> queryList = new HashMap<String, RequestConstraint>();
		if ( !parmList[1].isEmpty()) {
//			queryList = inventory.getRequestConstraintMap(parmList[0], parmList[1],
//						parmList[2], parmList[3], parmList[4]);
		}
		else {
//			queryList = inventory.getRequestConstraintMap(parmList[0], parmList[2],
//					parmList[3], parmList[4]);		    
		}
		
		String refTimeg = parmList[5].toUpperCase().split("F")[0];
		String refTime = GempakGrid.dattimToDbtime(refTimeg);
		refTime = refTime.substring(0, refTime.length()-2);
	    String fcstTimeg = parmList[5].toUpperCase().split("F")[1];
	    String fcstTime = Integer.toString(((Integer.parseInt(fcstTimeg))*3600));
		
		
		if ( queryList == null )
			return null;
	
		queryList.put("dataTime.refTime", new RequestConstraint(refTime));
		queryList.put("dataTime.fcstTime", new RequestConstraint(fcstTime));

		LayerProperty prop = new LayerProperty();
		prop.setDesiredProduct(ResourceType.PLAN_VIEW);
		prop.setEntryQueryParameters(queryList, false);
		prop.setNumberOfImages(1);
		
		String script = null;
		script = ScriptCreator.createScript(prop);

		if (script == null)
			return null;

		Object[] pdoList = Connector.getInstance().connect(script, null, 60000);

		GridRecord rec = (GridRecord) pdoList[0];
		long t1 = System.currentTimeMillis();
        System.out.println("\tgetDataURI took: " + (t1-t0));
        //logger.info("getDataURI took: " + (t1-t0));
		return rec.getDataURI();
	}

	public void setCycleForecastTimes(ArrayList<DataTime> dataTimes) {
		this.dataForecastTimes = dataTimes;
	}
	
	
}

