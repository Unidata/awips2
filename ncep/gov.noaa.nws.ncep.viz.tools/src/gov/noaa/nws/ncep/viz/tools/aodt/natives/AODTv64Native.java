package gov.noaa.nws.ncep.viz.tools.aodt.natives;

import java.util.Calendar;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.ptr.FloatByReference;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * The AODTv64Native class emulates the Native C environment which aodtv64 is
 * run in. JNA is used to make direct calls to C library methods. The java-side
 * code is intended to maintain the legacy system's structure.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep. 5, 2009  150     	M. Li    	Initial creation
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */

public class AODTv64Native {
	
	/** Setting up Singleton */
    private static AODTv64Native instance;

    /** Setting up Singleton */
    public static AODTv64Native getInstance() {
        if (instance == null) {
            instance = new AODTv64Native();
        }
        return instance;
    }
	
    /**
     * Setting up the aodtv64 library to access libaodtv64.so The commented out text
     * are the prototypes for the legacy methods. The Java prototypes are the
     * JNA equivalents.
     */
	public interface aodtv64 extends Library {
		aodtv64 INSTANCE = (aodtv64) Native.loadLibrary("aodtv64", aodtv64.class);
		
		/* functions in odtapi.h */
		int aodtv64_sethistoryfile(String h_file);
		int aodtv64_gethistoryfile(String h_file);
		String aodtv64_gethistoryfile2();
		int aodtv64_setforecastfile(String str1, int int1, String str2);
		int aodtv64_getforecastfile(String str1,IntByReference intPtr,String str2); 
		int aodtv64_setdomain(int int1);
		int aodtv64_getdomain();
		int aodtv64_setIRimageinfo(int int1,int int2,int int3);
		int aodtv64_getIRimageinfo(IntByReference intPtr1,
				IntByReference intPtr2,
				IntByReference intPtr3,String str); 
		int aodtv64_setlocation( float f1, float f2, int int1);
		int aodtv64_getlocation( FloatByReference floatPtr1, 
				FloatByReference floatPtr2, IntByReference intPtr );
		int aodtv64_setmiscoptions(int int1,int int2);
		int aodtv64_getmiscoptions(IntByReference intPtr1,IntByReference intPtr2);
		int aodtv64_setstartstr(int int1,float float1);
		int aodtv64_getstartstr(IntByReference intPtr,FloatByReference floatPtr1);
		int aodtv64_setsstvalue(float float1);
		int aodtv64_getsstvalue(FloatByReference floatPtr1);
		int aodtv64_settopovalue(float float1);
		int aodtv64_gettopovalue(IntByReference intPtr);
		int aodtv64_getversion(String str);

		int aodtv64_runautomode1(FloatByReference floatPtr1,FloatByReference floatPtr2,
				IntByReference intPtr);
		int aodtv64_runautomode2(float float1,float float2,FloatByReference floatPtr1,
				FloatByReference floatPtr2,IntByReference intPtr);   
		int aodtv64_getwarmeyetemplocation(FloatByReference floatPtr1,FloatByReference floatPtr2);

		int aodtv64_readtopofile(String str,float float1,float float2,IntByReference intPtr); 
		//int aodtv64_loadIRimage(PointerByReference floatPtrPtr1,PointerByReference floatPtrPtr2,
		//		PointerByReference floatPtrPtr3,int int1,int int2);
		int aodtv64_loadIRimage(float[] temps, float[] lats, float[] lons, int numx,  int numy);

		
		int aodtv64_seteyecloudtemp();
		int aodtv64_scenetype();
		int aodtv64_intensity();
		int aodtv64_getscenetypes(IntByReference intPtr,IntByReference intPtr1,
				IntByReference intPtr2,IntByReference intPtr3);
		int aodtv64_setscenetypes(int int1,int int2,int int3,int int4);
		int aodtv64_scenemap(int int1,int int2,String str);  

		int aodtv64_setdatetime(int int1,int int2,String str1,String str2,int int3); 
		int aodtv64_historylistfmt(odtdata.ByReference odtdata1,int int1,String str1,String str2);
		String aodtv64_historylistfmt2(odtdata.ByReference odtdata1,int int1,String str1);
		int aodtv64_historybullfmt(odtdata.ByReference odtdata1,String str); 
		int aodtv64_historygetnextrec(int int1,PointerByReference odtdataPtrPtr1);
		int aodtv64_historydeleterec(IntByReference intPtr1,IntByReference intPtr2);
		int aodtv64_historyrecordinsert(IntByReference intPtr1,IntByReference intPtr2);
		int aodtv64_historywritefile(IntByReference intPtr);
		int aodtv64_bulletinoutput(String str);  
		String aodtv64_bulletinoutput2();
		
		int aodtv64_historyaddcomment(String str,IntByReference intPtr); 

		int aodtv64_atcfoutputfile(String str1,int int1,String str2); 

		int aodtv64_qmessage(int int1,int int2,String str1,String str2);  
		int aodtv64_qdiagnostics(String str); 

		int aodtv64_initialize();
		void aodtv64_freememory();
		
		/* odtlibdefs.h */
		public static final int kstart_v64=24;     /* inner cloud region analysis radius (km) */
		public static final int kend_v64=136;      /* outer cloud region analysis radius (km) */
		public static final int kenda_v64=190;     /* automated cursor position analysis radius (km) */
		public static final int keyerM_v64=24;     /* outer eye region search radius (km) - Manual position */
		public static final int keyerA_v64=75;     /* outer eye region search radius (km) - Auto position */
		public static final int kres_v64=4;        /* width of the cloud region analysis rings */

		public static final float[] tno_v64={(float)-9999.,(float) -8888.,
			(float)1.0,(float)1.1,(float)1.2,(float)1.3,(float)1.4,(float)1.5,(float) 1.6,(float)1.7,(float)1.8,(float)1.9,
			(float)2.0,(float)2.1,(float)2.2,(float)2.3,(float)2.4,(float)2.5,(float)2.6,(float)2.7,(float)2.8,(float)2.9,
			(float)3.0,(float)3.1,(float)3.2,(float)3.3,(float)3.4,(float)3.5,(float)3.6,(float)3.7,(float)3.8,(float)3.9,
		    (float)4.0,(float)4.1,(float)4.2,(float)4.3,(float)4.4,(float)4.5,(float)4.6,(float)4.7,(float)4.8,(float)4.9,
			(float)5.0,(float)5.1,(float)5.2,(float)5.3,(float)5.4,(float)5.5,(float)5.6,(float)5.7,(float)5.8,(float)5.9,
			(float)6.0,(float)6.1,(float)6.2,(float)6.3,(float)6.4,(float)6.5,(float)6.6,(float)6.7,(float)6.8,(float)6.9,
			(float)7.0,(float)7.1,(float)7.2,(float)7.3,(float)7.4,(float)7.5,(float)7.6,(float)7.7,(float)7.8,(float)7.9,(float)8.0};

		public static final float[][] pres_v64= {
				/* Atlantic pressure relationship values */
				{(float)-9999.0,(float)-8888.0,
				(float)1014.0,(float)1013.6,(float)1013.2,(float)1012.8,(float)1012.4,(float)1012.0,(float)1011.4,(float)1010.8,(float)1010.2,(float)1009.6,(float)
				  1009.0,(float)1008.2,(float)1007.4,(float)1006.6,(float)1005.8,(float)1005.0,(float)1004.0,(float)1003.0,(float)1002.0,(float)1001.0,(float)
				  1000.0,(float)998.8,(float)997.6,(float)996.4,(float)995.2,(float)994.0,(float)992.6,(float)991.2,(float)989.8,(float)988.4,(float)
				   987.0,(float)985.4,(float)983.8,(float)982.2,(float)980.6,(float)979.0,(float)977.2,(float)975.4,(float)973.6,(float)971.8,(float)
				   970.0,(float)968.0,(float)966.0,(float)964.0,(float)962.0,(float)960.0,(float)957.6,(float)955.2,(float)952.8,(float)950.4,(float)
				   948.0,(float)945.4,(float)942.8,(float)940.2,(float)937.6,(float)935.0,(float)932.2,(float)929.4,(float)926.6,(float)923.8,(float)
				   921.0,(float)918.0,(float)915.0,(float)912.0,(float)909.0,(float)906.0,(float)902.8,(float)899.6,(float)896.4,(float)893.2,(float)890.0},
				/* Pacific pressure relationship values */
				{(float)-9999.0,(float)-8888.0,(float)
				  1005.0,(float)1004.6,(float)1004.2,(float)1003.8,(float)1003.4,(float)1003.0,(float)1002.4,(float)1001.8,(float)1001.2,(float)1000.6,(float)
				  1000.0,(float)999.4,(float)998.8,(float)998.2,(float)997.6,(float)997.0,(float)995.8,(float)994.6,(float)993.4,(float)992.2,(float)
				   991.0,(float)989.6,(float)988.2,(float)986.8,(float)985.4,(float)984.0,(float)982.4,(float)980.8,(float)979.2,(float)977.6,(float)
				   976.0,(float)974.0,(float)972.0,(float)970.0,(float)968.0,(float)966.0,(float)963.6,(float)961.2,(float)958.8,(float)956.4,(float)
				   954.0,(float)951.4,(float)948.8,(float)946.2,(float)943.6,(float)941.0,(float)938.2,(float)935.4,(float)932.6,(float)929.8,(float)
				   927.0,(float)924.4,(float)921.8,(float)919.2,(float)916.6,(float)914.0,(float)910.8,(float)907.6,(float)904.4,(float)901.2,(float)
				   898.0,(float)894.2,(float)890.4,(float)886.6,(float)882.8,(float)879.0,(float)874.8,(float)870.6,(float)866.4,(float)862.2,(float)858.0} };

		/* Atlantic/Pacific pressure relationship values */
		public static final float[] wind_v64={(float)-9999.0,(float)-8888.0,(float)
			25.0,(float) 25.0,(float) 25.0,(float) 25.0,(float) 25.0,(float) 25.0,(float) 26.0,(float) 27.0,(float) 28.0,(float) 29.0,(float)
			30.0,(float) 31.0,(float) 32.0,(float) 33.0,(float) 34.0,(float) 35.0,(float) 37.0,(float) 39.0,(float) 41.0,(float) 43.0,(float)
			45.0,(float) 47.0,(float) 49.0,(float) 51.0,(float) 53.0,(float) 55.0,(float) 57.0,(float) 59.0,(float) 61.0,(float) 63.0,(float)
			65.0,(float) 67.4,(float) 69.8,(float) 72.2,(float) 74.6,(float) 77.0,(float) 79.6,(float) 82.2,(float) 84.8,(float) 87.4,(float)
			90.0,(float) 92.4,(float) 94.8,(float) 97.2,(float) 99.6,(float)102.0,(float)104.6,(float)107.2,(float)109.8,(float)112.4,(float)
			115.0,(float)117.4,(float)119.8,(float)122.2,(float)124.6,(float)127.0,(float)129.6,(float)132.2,(float)134.8,(float)137.4,(float)
			140.0,(float)143.0,(float)146.0,(float)149.0,(float)152.0,(float)155.0,(float)158.0,(float)161.0,(float)164.0,(float)167.0,(float)170.0};

		/* BD curve break points */
		public static final float[] ebd_v64={ (float)30.0,(float) -0.0,(float)-30.0,(float)-42.0,(float)-54.0,(float)
		               -64.0,(float)-70.0,(float)-76.0,(float)-80.0,(float)-100.0};

		/* functions in odtlibfuncs.h */
		
		/* these are functions that are called/shared within the AODT library */
		/* defined in odtfuncs.c */
		 double aodtv64_calctime( int int1, int int2 );
		 float aodtv64_slopecal( double double1,int int1 );
		 float aodtv64_getpwval( int int1,float float1);
		 float aodtv64_PWlandsea( float float1);
		 float aodtv64_ptovmax( float float1);
		 float aodtv64_ptotno( float float1);
		 int aodtv64_cmonth2julian( String str);
		 void aodtv64_julian2cmonth( int int1, String str );
		 void aodtv64_distance( float float1,float float2,float float3,float float4,int int1,
				 FloatByReference floatPtr1,FloatByReference floatPtr2 );
		 void aodtv64_distance2( float float1, float float2, float float3, float float4, 
				 FloatByReference floatPtr1, FloatByReference floatPtr2 );
		 float aodtv64_atoif( String str, int int1, int int2);
		 void aodtv64_calcskew( FloatByReference floatPtr1, int int1, FloatByReference floatPtr2, 
				 FloatByReference floatPtr3, FloatByReference floatPtr4 );
		 int aodtv64_idmyyd( int int1,int int2,int int3 );
		 void aodtv64_yddmy( int int1,IntByReference intPtr1,IntByReference intPtr2,IntByReference intPtr3 );
		 int  aodtv64_oceanbasin( float float1,float float2 );
		 int  aodtv64_sattypes( int int1,String str ); /* XXX */
		 int aodtv64_initcurrent(int int1);

		/* defined in odtintensity.c */
		 int aodtv64_calcintensity();

		/* defined in odtscene.c */
		 void aodtv64_classifyredo();

		/* defined in odtfft.c */
		 int aodtv64_fft( FloatByReference floatPtr1, FloatByReference floatPtr2, IntByReference intPtr2 );

		/* defined in odtauto.c */
		 void aodtv64_logspiral( float float1, float float2, float float3, int int1, 
				 IntByReference intPtr1, FloatByReference floatPtr1, FloatByReference floatPtr2 );

		/* defined in odthistory.c */
		 int aodtv64_datetime(int int1,int int2,String str1,String str2,int int3); /* XXX */
		 int aodtv64_deletehistoryrec(IntByReference intPtr );
		 int aodtv64_listhistory(odtdata[] odtdata1,int int1,String str1,String str2); /* XXX */

		 /* odtlib.h */ 
		 public static final int TRUE = 1;
		 public static final int FALSE = 0;
		 
		 public static final int mxcdsz = 640;
		 public static final int maxd = 500;
		 public static final int slots = 150;
		 public static final int bufsiz = maxd * maxd;
		 
		 /* odtremap.h */
		 int mapper(int int1, int int2);
		 int domap(int int1, FloatByReference floatPtr1, FloatByReference floatPtr2, int int2, int int3);
		 void corner(int int1, int int2, int int3,FloatByReference floatPtr1,FloatByReference floatPtr2);
		 int uinit();
		 int init(int int1, int int2, IntByReference intPtr1);
		 int umap(int int1, int int2, IntByReference intPtr1,IntByReference intPtr2);
		 int findpoint(float float1, float float2, IntByReference intPtr1, IntByReference intPtr2);
		 
		 public static class FILE extends com.sun.jna.PointerType {
			 public FILE(com.sun.jna.Pointer pointer) {
				 super(pointer);
			 }
			 public FILE() {
				 super();
			 }
		 }
		
	}	
	
	/*
	public static void main(String[] args) {
		System.out.println("aodtv64.......");
		aodtv64.INSTANCE.aodtv64_initialize();
		aodtv64.INSTANCE.aodtv64_setmiscoptions(0, 0);
		aodtv64.INSTANCE.aodtv64_setIRimageinfo(2090252, 1200000, -1);
	}
	*/
	/** The aodtv64 instance we will use to call the native library */
    public aodtv64 aodt;
    
    public int idxLand;
    public int idxSearch;
    public int idxDomain;
    public int idxSceneType;
    
    public float centerLat;
    public float centerLon;
    public DataTime currentDate;
    public final int NUMX = 105;
    public float[] temps = new float[NUMX * NUMX];
    public float[] lats  = new float[NUMX * NUMX];
    public float[] lons  = new float[NUMX * NUMX];
    
    private int eye_type = -1;
	private int cloud_type = -1;
    
	private String hist_file = null;
    private String output;

    public int run_aodt() {
    	aodt  = aodtv64.INSTANCE;
    	int ier = -9999;
    	
    	/* set land and search options */
    	aodt.aodtv64_setmiscoptions(idxLand, idxSearch);
    	
    	/* Initialize    */
    	aodt.aodtv64_initialize();
    	
    	/* Set history file */
    	//System.out.println("NATIVE run_aodt...hist_file"+hist_file);
    	if (hist_file != null && hist_file.length() > 0) {
    		hist_file = LocalizationManager.getUserDir() + hist_file;
    		ier = aodt.aodtv64_sethistoryfile(hist_file);
    		if (ier < 0) {
    			System.out.println("ERROR: invalid in aodtv64_sethistoryfile, ier="+ier);
    			return ier;
    		}
    	}
    	
    	/* Set satellite date/time info in AODT library  */
    	if (currentDate != null) {
    		int year = currentDate.getRefTimeAsCalendar().get(Calendar.YEAR);
    		int day  = currentDate.getRefTimeAsCalendar().get(Calendar.DAY_OF_YEAR);
    		int hour = currentDate.getRefTimeAsCalendar().get(Calendar.HOUR_OF_DAY);
    		int minute = currentDate.getRefTimeAsCalendar().get(Calendar.MINUTE);
    		
    		int img_dat = year * 1000 + day;
    		int img_tim = hour * 10000 + minute * 100;
    		aodt.aodtv64_setIRimageinfo(img_dat, img_tim, -1);
    	}
    	else {
    		System.out.println("ERROR: invalid currentDate in run_aodt");
    		return ier;
    	}
    	
    	/* Set cursor location from GUI */
    	ier = aodt.aodtv64_setlocation(centerLat, centerLon*(-1.0F), 0);
    	if ( ier < 0) {
    		System.out.println("ERROR: invalid lat/lon, ier="+ier);
    		return ier;
    	}
    	
    	/* Set oceanic domain flag in AODT library */
    	ier = aodt.aodtv64_setdomain(idxDomain);
    	if ( ier < 0) {
    		System.out.println("ERROR: aodtv64_setdomain, ier ="+ ier);
    		return ier;
    	}
    	
    	/* Set satellite image data array  */
    	aodt.aodtv64_loadIRimage(temps, lats, lons, NUMX, NUMX);
    	if ( ier < 0) {
    		System.out.println("ERROR: aodtv64_loadIRimage, ier ="+ ier);
    		return ier;
    	}
    	
    	/* Set eye and cloud temp values in AODT library */
    	aodt.aodtv64_seteyecloudtemp();
    	if ( ier < 0) {
    		System.out.println("ERROR: aodtv64_seteyecloudtemp, ier ="+ ier);
    		return ier;
    	}
    	
    	/* determine scene type */
    	aodt.aodtv64_scenetype();
    	if ( ier < 0) {
    		System.out.println("ERROR: aodtv64_scenetype, ier ="+ ier);
    		return ier;
    	}
    	
    	/* Override scene type, if desired by user */
    	if (idxSceneType > 0) {
    		getSceneTypes(idxSceneType);
    		
			IntByReference intPtr = new IntByReference();
			IntByReference intPtr1 = new IntByReference();
			IntByReference intPtr2 = new IntByReference();
			IntByReference intPtr3 = new IntByReference();
			aodt.aodtv64_getscenetypes(intPtr, intPtr1, intPtr2, intPtr3);
			aodt.aodtv64_setscenetypes(eye_type, cloud_type, intPtr.getValue(), intPtr1.getValue());
    	}
    	
    	/* Calculate intensity  */
    	ier = aodt.aodtv64_intensity();
    	if ( ier < 0) {
    		System.out.println("ERROR: aodtv64_intensity, ier ="+ ier);
    		return ier;
    	}
    	
    	/* AODT intensity estimate output */
    	output = "";
    	output = aodt.aodtv64_bulletinoutput2();

    	/* insert current intensity analysis into history file */
    	if ( hist_file != null && hist_file.length() > 0) {

    		IntByReference hmods = new IntByReference();
    		IntByReference hrecs = new IntByReference();
    		ier = aodt.aodtv64_historyrecordinsert(hmods, hrecs);
    		if (ier < 0) {
    			System.out.println("ERROR: aodtv64_historyrecordinsert, ier="+ier);
    			return ier;
    		}

    		/* write updated history records to file */
    		ier = aodt.aodtv64_historywritefile(hrecs);
    		if (ier < 0) {
    			System.out.println("ERROR: aodtv64_historywritefile, ier="+ier);
    		}
    		
    	}
    	
    	return 0;
    }
    
    
    public void setCenterLatLon( float lat, float lon ) {
    	this.centerLat = lat;
    	this.centerLon = lon;
    }
    
    public void setCurrentDate(DataTime currentDate) {
		this.currentDate = currentDate;
	}

	public void setIdxLand(int idxLand) {
		this.idxLand = idxLand;
	}

	public void setIdxSearch(int idxSearch) {
		this.idxSearch = idxSearch;
	}

	public void setIdxDomain(int idxDomain) {
		this.idxDomain = idxDomain;
	}
    
	public void setIdxSceneType(int idx) {
		this.idxSceneType = idx;
	}
	
	public void setIRImageInfo(float[] lat, float[] lon, float[] temp) {
		this.lats = lat;
		this.lons = lon;
		this.temps = temp;
	}
	
	public String getOuput() {
		return output;
	}
	
	public String getHistoryFile() {
		return hist_file;
	}
	
	public void setHistoryFile(String file) {
		this.hist_file = file;
	}
	
	private void getSceneTypes(int user_pick) {
		 if ( user_pick <= 0 || user_pick > 11 ) {

	         eye_type   = -1;
	         cloud_type = -1;
	         return;
	    }

	    if ( user_pick <= 6 ) {

	         eye_type   = user_pick - 1;
	         cloud_type = 0;
	         return;
	    }

	    if ( user_pick <= 11 ) {

	         eye_type   = 6;
	         cloud_type = user_pick - 7;
	         return;
	    }

	}
    
}
