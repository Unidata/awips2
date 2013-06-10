/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.natives.NsharpNative
 * 
 * This java class performs the NSHARP NsharpNative functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/23/2010	229			Chin Chen	Initial coding
 * 5/2012		736			T. Lee		Added cave_ccl, cave_soar, cave_dmpi, 
 *										cave_wmax, cave_mdpi_windex, nc_mix_height
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.ui.nsharp.natives;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingLayer;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import java.nio.ByteBuffer;
import java.util.List;

//Chin-T import com.raytheon.uf.common.sounding.SoundingLayer;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Structure;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.FloatByReference;


public class NsharpNative {
	public static int SNDG_SFC=0;
	public static int SNDG_OBS=   1;
	public static int SNDG_MDL=   2;
	public static int SNDG_PFC=   3;
	public static int SNDG_ACARS= 4;
	public static int SNDG_ARCH=  5;
	
	public NsharpLibrary nsharpLib;
	
	/** Setting up Singleton */
    /* DECP private static NsharpNative instance;*/

    public NsharpNative() {
		super();
		//nsharpLib = NsharpLibrary.INSTANCE;
		nsharpLib = (NsharpLibrary) Native.loadLibrary("bignsharp", NsharpLibrary.class);
		nsharpLib.initStaticGlobalsMem();
	}
    
    public void setSarsSupcellFileName(){
    	NsharpConfigManager configMgr = NsharpConfigManager.getInstance();
		String sarsFilePath = configMgr.getBigNsharpFlPath(NcPathConstants.NSHARP_NLIST_FILE);
		ByteBuffer sarsBuf=null;
		if(sarsFilePath!=null){
			sarsBuf = ByteBuffer.allocate(sarsFilePath.length());
			sarsBuf.put(sarsFilePath.getBytes());
		}
		String supercellFilePath = configMgr.getBigNsharpFlPath(NcPathConstants.NSHARP_SUP_FILE);
		
		ByteBuffer supBuf=null;
		if(supercellFilePath!=null){
			supBuf = ByteBuffer.allocate(supercellFilePath.length());
			supBuf.put(supercellFilePath.getBytes());
		}
		int sarslm=sarsBuf.limit();
		int suplm=supBuf.limit();
		//System.out.println("Nsharp setSarsSupcellFileName: sarFilepath="+sarsFilePath+ " supFilepath="+ supercellFilePath);
		//Chin: for unknown reason calling setSarsSupcellFileName() without passing ByteBuffer limit will cause one extra bytes
		// set at end of file name. Therefore, passing buffer size limit specifically to fix this issue.
		if(sarsBuf!=null&& supBuf!=null){
			nsharpLib.setSarsSupcellFileName(sarsBuf,sarslm,supBuf,suplm);//(sarsFilePathAry,supercellFilePathAry);
		}
    }
 
    public void populateSndgData(List<NcSoundingLayer> soundingLys) {
    	if((soundingLys != null) && (soundingLys.size() > 0)){
    		//Note: one has to initialize Structure array this way....
    		NsharpLibrary.CaveSndgParms[] snDataArray =  (NsharpLibrary.CaveSndgParms[])(new NsharpLibrary.CaveSndgParms().toArray(soundingLys.size()));

    		/*if (winDir == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA){
    			winDir = NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA;
    		}
    		if (winSpd == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA){
    			winSpd = NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA;
    		}*/
    		float omega,pres, ht, tem, dew, ws, wd; 
    		int skipNumber=0;
    		for (int i =0; i< soundingLys.size() ; i++){
    			NcSoundingLayer sndLy = soundingLys.get(i);
    			pres = sndLy.getPressure();
    			if(pres <100  ){
    				skipNumber++;
    				continue;
    			}
    			omega = sndLy.getOmega();
    			ht = sndLy.getGeoHeight();
    			tem = sndLy.getTemperature();
    			dew = sndLy.getDewpoint(); 
    			wd = sndLy.getWindDirection();
    			ws = sndLy.getWindSpeed();
    			if (omega == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA){
    				omega = NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA;
        		}
    			if (pres == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA){
    				pres = NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA;
        		}
    			if (ht == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA){
    				ht = NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA;
        		}
    			if (tem == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA){
    				tem = NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA;
        		}
    			if (dew == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA){
    				dew = NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA;
        		}
    			if (ws == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA){
    				ws = NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA;
        		}
    			if (wd == NsharpNativeConstants.NSHARP_NATIVE_INVALID_DATA){
    				wd = NsharpNativeConstants.NSHARP_LEGACY_LIB_INVALID_DATA;
        		}
    			snDataArray[i-skipNumber].fillData(omega, pres, ht, tem,
    					dew, wd, ws);
    		}

    		nsharpLib.populateSndgDataStatic(snDataArray, soundingLys.size()-skipNumber , 1);//use 1 for testing for now Chin...
    	}
    }
  
       
    public interface NsharpLibrary extends Library {
    	//library name is libbignsharp.so. but only use "bignsharp" as lib name when loading
    	NsharpLibrary INSTANCE = (NsharpLibrary) Native.loadLibrary("bignsharp", NsharpLibrary.class);
    	
    	/// <i>native declaration : line 1</i>
    	public static class _lplvalues extends Structure {
    		/// C type : char[40]
    		public byte[] desc = new byte[(40)];
    		public float pres;
    		public float temp;
    		public float dwpt;
    		public float presval;
    		public short flag;
    		public  _lplvalues() {
    			super();
    		}
    		/// @param desc C type : char[40]
    		public  _lplvalues(byte desc[], float pres, float temp, float dwpt, float presval,short flag) {
    			super();
    			if (desc.length != this.desc.length) 
    				throw new java.lang.IllegalArgumentException("Wrong array size !");
    			this.desc = desc;
    			this.pres = pres;
    			this.temp = temp;
    			this.dwpt = dwpt;
    			this.presval = presval;
    			this.flag = flag;
    		}
    		protected ByReference newByReference() { return new ByReference(); }
    		protected ByValue newByValue() { return new ByValue(); }
    		protected _lplvalues newInstance() { return new _lplvalues(); }
    		public static class ByReference extends _lplvalues implements Structure.ByReference {
    			
    		}
    		public static class ByValue extends _lplvalues implements Structure.ByValue {
    			
    		}
    	}
    	/// <i>native declaration : line 10</i>
    	public static class _parcel extends Structure{
    		public float lplpres;
    		public float lpltemp;
    		public float lpldwpt;
    		public float blayer;
    		public float tlayer;
    		public float entrain;
    		public float lclpres;
    		public float lfcpres;
    		public float elpres;
    		public float mplpres;
    		public float bplus;
    		public float bminus;
    		public float bfzl;
    		public float cape3km;
    		public float cape6km;
    		public float wm10c;
    		public float wm30c;
    		public float li5;
    		public float li3;
    		public float brn;
    		public float limax;
    		public float limaxpres;
    		public float cap;
    		public float cappres;

    		public  _parcel() {
    			super();
    		}
    		protected ByReference newByReference() { return new ByReference(); }
    		protected ByValue newByValue() { return new ByValue(); }
    		protected _parcel newInstance() { return new _parcel(); }
    		public static class ByReference extends _parcel implements Structure.ByReference {
    			
    		}
    		public static class ByValue extends _parcel implements Structure.ByValue {
    			
    		}
    	}
    	
    	//from our own SndgParms
    	public static class CaveSndgParms extends com.sun.jna.Structure {
    		public float omega;
    		public float pres;
    		public float hght;
    		public float temp;
    		public float dwpt;
    		public float drct;
    		public float sped;
    		public  CaveSndgParms() {
    			super();
    		}
    		public  CaveSndgParms(float omega, float pres, float hght, float temp, float dwpt, float drct, float sped) {
    			super();
    			this.omega = omega;
    			this.pres = pres;
    			this.hght = hght;
    			this.temp = temp;
    			this.dwpt = dwpt;
    			this.drct = drct;
    			this.sped = sped;
    		}
    		
    		public void fillData(float omega, float pres, float hght, float temp, float dwpt, float drct, float sped) {
    			this.omega = omega;
    			this.pres = pres;
    			this.hght = hght;
    			this.temp = temp;
    			this.dwpt = dwpt;
    			this.drct = drct;
    			this.sped = sped;
    		}
    		protected ByReference newByReference() { return new ByReference(); }
    		protected ByValue newByValue() { return new ByValue(); }
    		protected CaveSndgParms newInstance() { return new CaveSndgParms(); }
    		public static class ByReference extends CaveSndgParms implements Structure.ByReference {
    			
    		}
    		public static class ByValue extends CaveSndgParms implements Structure.ByValue {
    			
    		}
    		
    	}
    	public static class SarsInfoStr extends Structure {
    		public static int SARS_STRING_LEN=40;
    		public static int SARS_STRING_LINES = 12;
    		/// max=10
    		public int numHailstr;
    		/// C type : char[10][60]
    		public byte[] hailStr = new byte[SARS_STRING_LINES * SARS_STRING_LEN];
    		/// C type : int[10]
    		public int[] hailStrColor = new int[(SARS_STRING_LINES)];
    		/// C type : char[2][60]
    		//public byte[] sighailStr = new byte[2 * 60];
    		//public int sighailStrColor;
    		/// max=10
    		public int numsupcellstr;
    		/// C type : char[10][60]
    		public byte[] supcellStr = new byte[SARS_STRING_LINES * SARS_STRING_LEN];
    		/// C type : int[10]
    		public int[] supcellStrColor = new int[(SARS_STRING_LINES)];
    		/// C type : char[2][60]
    		//public byte[] torStr = new byte[2 * 60];
    		//public int torStrColor;
    		public  SarsInfoStr() {
    			super();
    		}
    		protected ByReference newByReference() { return new ByReference(); }
    		protected ByValue newByValue() { return new ByValue(); }
    		protected SarsInfoStr newInstance() { return new SarsInfoStr(); }
    		public static class ByReference extends SarsInfoStr implements Structure.ByReference {
    			
    		}
    		public static class ByValue extends SarsInfoStr implements Structure.ByValue {
    			
    		}
			public int getNumHailstr() {
				return numHailstr;
			}
			public byte[] getHailStr() {
				return hailStr;
			}
			public int[] getHailStrColor() {
				return hailStrColor;
			}
			/*public byte[] getSighailStr() {
				return sighailStr;
			}
			public int getSighailStrColor() {
				return sighailStrColor;
			}*/
			public int getNumsupcellstr() {
				return numsupcellstr;
			}
			public byte[] getSupcellStr() {
				return supcellStr;
			}
			public int[] getSupcellStrColor() {
				return supcellStrColor;
			}
			/*public byte[] getTorStr() {
				return torStr;
			}
			public int getTorStrColor() {
				return torStrColor;
			}*/
    		
    	}
    	public static class FireInfoStr extends Structure {
    		public int sfcRhColor=31;
    		public int pwColor=31;
    		public int blMaxColor=31;
    		public int fosbergColor=31;
    		public byte[] sfcRh = new byte[(60)];
    		public byte[] sfc = new byte[(60)];
    		public byte[] zeroOneKmRh = new byte[(60)];
    		public byte[] zeroOneKmMean = new byte[(60)];
    		public byte[] blMeanRh = new byte[(60)];
    		public byte[] blMean = new byte[(60)];
    		public byte[] pw = new byte[(60)];
    		public byte[] blMax = new byte[(60)];
    		public byte[] fosberg = new byte[(60)];
			public FireInfoStr() {
				super();
				// TODO Auto-generated constructor stub
			}
			public static class ByReference extends FireInfoStr implements Structure.ByReference {

    		}
    		public static class ByValue extends FireInfoStr implements Structure.ByValue {

    		}
			protected ByReference newByReference() { return new ByReference(); }
    		protected ByValue newByValue() { return new ByValue(); }
			public int getSfcRhColor() {
				return sfcRhColor;
			}
			public int getPwColor() {
				return pwColor;
			}
			public int getBlMaxColor() {
				return blMaxColor;
			}
			public int getFosbergColor() {
				return fosbergColor;
			}
			public byte[] getSfcRh() {
				return sfcRh;
			}
			public byte[] getSfc() {
				return sfc;
			}
			public byte[] getZeroOneKmRh() {
				return zeroOneKmRh;
			}
			public byte[] getZeroOneKmMean() {
				return zeroOneKmMean;
			}
			public byte[] getBlMeanRh() {
				return blMeanRh;
			}
			public byte[] getBlMean() {
				return blMean;
			}
			public byte[] getPw() {
				return pw;
			}
			public byte[] getBlMax() {
				return blMax;
			}
			public byte[] getFosberg() {
				return fosberg;
			}
    		
    	}
    	public static class WinterInfoStr extends Structure {
    		public float mopw;
    		/*public float htop;
    		public float hbot;
    		public float mrh;
    		public float mq;
    		public float mo;
    		public float pw;
    		public float pLevel;*/
    		public byte[] oprh = new byte[(60)];
    		public byte[] layerDepth = new byte[(60)];
    		public byte[] meanLayerRh = new byte[(60)];
    		public byte[] meanLayerMixRat = new byte[(60)];
    		public byte[] meanLayerPw = new byte[(60)];
    		public byte[] meanLayerOmega = new byte[(60)];
    		public byte[] initPhase = new byte[(100)];
    		public byte[] tempProfile1 = new byte[(60)];
    		public byte[] tempProfile2 = new byte[(60)];
    		public byte[] tempProfile3 = new byte[(60)];
    		public byte[] wetbulbProfile1 = new byte[(60)];
    		public byte[] wetbulbProfile2 = new byte[(60)];
    		public byte[] wetbulbProfile3 = new byte[(60)];
    		public byte[] bestGuess1 = new byte[(60)];
    		public byte[] bestGuess2 = new byte[(60)];
			public WinterInfoStr() {
				super();
				// TODO Auto-generated constructor stub
			}
			public static class ByReference extends WinterInfoStr implements Structure.ByReference {

    		}
    		public static class ByValue extends WinterInfoStr implements Structure.ByValue {

    		}
			protected ByReference newByReference() { return new ByReference(); }
    		protected ByValue newByValue() { return new ByValue(); }
			public float getMopw() {
				return mopw;
			}
			
			public byte[] getOprh() {
				return oprh;
			}
			public byte[] getLayerDepth() {
				return layerDepth;
			}
			public byte[] getMeanLayerRh() {
				return meanLayerRh;
			}
			public byte[] getMeanLayerMixRat() {
				return meanLayerMixRat;
			}
			public byte[] getMeanLayerPw() {
				return meanLayerPw;
			}
			public byte[] getMeanLayerOmega() {
				return meanLayerOmega;
			}
			public byte[] getInitPhase() {
				return initPhase;
			}
			public byte[] getTempProfile1() {
				return tempProfile1;
			}
			public byte[] getTempProfile2() {
				return tempProfile2;
			}
			public byte[] getTempProfile3() {
				return tempProfile3;
			}
			public byte[] getWetbulbProfile1() {
				return wetbulbProfile1;
			}
			public byte[] getWetbulbProfile2() {
				return wetbulbProfile2;
			}
			public byte[] getWetbulbProfile3() {
				return wetbulbProfile3;
			}
			public byte[] getBestGuess1() {
				return bestGuess1;
			}
			public byte[] getBestGuess2() {
				return bestGuess2;
			}
    		
    	}
    	public static int MAX_CLOUD_LAYER=20;
    	//cloudTypeFM value defined in caveNsharp.c OVC=1, BKN=2, SCT=3*/
    	public static String[] CLOUD_TYPE = {"dummy","OVC", "BKN", "SCT"}; 
    	public static class CloudInfoStr extends Structure {
    		/* FM: Fred Mosher's Algorithm */
    		public int sizeFM;
    		public float[] preStartFM= new float[MAX_CLOUD_LAYER];
    		public float[] preEndFM= new float[MAX_CLOUD_LAYER];
    		public int[] cloudTypeFM= new int[MAX_CLOUD_LAYER];
    		/* CE: Chernykh and Eskridge Algorithm */
    		public int sizeCE;
    		public float[] preStartCE= new float[MAX_CLOUD_LAYER];
    		public float[] preEndCE= new float[MAX_CLOUD_LAYER];
    		
    		public  CloudInfoStr() {
    			super();
    			sizeFM=0;
    			sizeCE=0;
    		}
    		
    		protected ByReference newByReference() { return new ByReference(); }
    		protected ByValue newByValue() { return new ByValue(); }
    		protected CloudInfoStr newInstance() { return new CloudInfoStr(); }
     		public static class ByReference extends CloudInfoStr implements Structure.ByReference {

    		}
    		public static class ByValue extends CloudInfoStr implements Structure.ByValue {

    		}
			public int getSizeFM() {
				return sizeFM;
			}

			public int[] getCloudTypeFM() {
				return cloudTypeFM;
			}

			public int getSizeCE() {
				return sizeCE;
			}

			public float[] getPreStartCE() {
				return preStartCE;
			}

			public float[] getPreEndCE() {
				return preEndCE;
			}

			public float[] getPreStartFM() {
				return preStartFM;
			}

			public float[] getPreEndFM() {
				return preEndFM;
			}
			
    		
    	}
    	public static class StormSlinkyStr extends Structure {
    		public int size;
    		public float tottim;
    		public float angl;
    		/// C type : float[200][2]
    		public float[] tsuv = new float[200 * 2];
    		public int[] color = new int[200];
    		public  StormSlinkyStr() {
    			super();
    			size=-1;
    		}
    		
    		protected ByReference newByReference() { return new ByReference(); }
    		protected ByValue newByValue() { return new ByValue(); }
    		protected StormSlinkyStr newInstance() { return new StormSlinkyStr(); }
     		public static class ByReference extends StormSlinkyStr implements Structure.ByReference {

    		}
    		public static class ByValue extends StormSlinkyStr implements Structure.ByValue {

    		}

    		public float getTottim() {
    			return tottim;
    		}
    		public void setTottim(float tottim) {
    			this.tottim = tottim;
    		}
    		public float getAngl() {
    			return angl;
    		}
    		public void setAngl(float angl) {
    			this.angl = angl;
    		}
    		public float[] getTsuv() {
    			return tsuv;
    		}
    		public void setTsuv(float[] tsuv) {
    			this.tsuv = tsuv;
    		}
    		public int getSize() {
    			return size;
    		}
    		public void setSize(int size) {
    			this.size = size;
    		}
			public int[] getColor() {
				return color;
			}
			public void setColor(int[] color) {
				this.color = color;
			}
    		
    	}
    	
    	//From our own caveNsharp.c
    	float aglT(int l1high, int l2high);
    	void initStaticGlobalsMem();
    	//void setSarsSupcellFileName(char sarsFlName[], char supercellFlName[]);
    	//@java.lang.Deprecated 
    	//void setSarsSupcellFileName(Pointer sarsFlName, Pointer supercellFlName);
    	void setSarsSupcellFileName(ByteBuffer sarsFlName, int sarsLen,ByteBuffer supercellFlName, int supLen);
     	void populateSndgData(NsharpLibrary.CaveSndgParms snDataArray[], int arraySize,  int datatype);  
     	int  populateSndgDataStatic(CaveSndgParms snDataArray[], int arraySize, int datatype);
     	void get_lpvaluesData(NsharpLibrary._lplvalues pParcel);
    	//void populateSndgTestData() ;
     	void get_surface(FloatByReference pressure, FloatByReference temp, FloatByReference  dewpt);
     	void get_surfaceWind(FloatByReference windSp, FloatByReference windDir);
     	//void get_top(FloatByReference pressure, FloatByReference temp, FloatByReference dewpt);
     	void get_storm(FloatByReference speed, FloatByReference direction);
     	void set_storm(float speed, float  direction);
     	void cave_visual1 ( float lower, float upper, float pres, float temp, float dwpt , NsharpLibrary.StormSlinkyStr stmSlinky);
     	void draw_Clouds( NsharpLibrary.CloudInfoStr cloudInfo );
     	void getWinterInfo(NsharpLibrary.WinterInfoStr winterInfo );
     	void getFireInfo(NsharpLibrary.FireInfoStr  fireInfo);
     	void getSarsInfo(NsharpLibrary.SarsInfoStr  sarsInfo);
     	//float cave_bulk_rich ( float lplpres, float bplus,FloatByReference brnshear );
     	float cave_bulk_rich2 ( FloatByReference brnshear );
     	float cave_ship();
     	int cave_ww_type();
     	float cave_criticalAngel();
     	void get_effectLayertopBotPres(FloatByReference topP, FloatByReference botP);
    	void nc_mix_height(FloatByReference pres, FloatByReference drct, FloatByReference sped, FloatByReference del_t, FloatByReference lr, FloatByReference drct_mean, FloatByReference sped_mean, FloatByReference drct_max, FloatByReference sped_max, short flag);
    	float cave_mdpi_windex (FloatByReference windex);
    	int cave_dmpi();
    	void cave_ccl (float mixr, FloatByReference pccl, FloatByReference tccl, FloatByReference zccl);
    	void cave_soar (float slev, FloatByReference ctax, FloatByReference zconv, FloatByReference tconv, FloatByReference zthrm, FloatByReference tthrm, FloatByReference soar, FloatByReference ttrig);
    	float cave_wmax (float[] vvel);    	
     	//void printSfcInfo();
     	//void showSndgData();
     	//From basic.h
    	/**
    	 * Original signature : <code>float i_temp(float)</code><br>
    	 * <i>native declaration : line 57</i>
    	 */
    	float itemp(float pres);
    	/**
    	 * Original signature : <code>float i_dwpt(float)</code><br>
    	 * <i>native declaration : line 58</i>
    	 */
    	float idwpt(float pres);
    	/**
    	 * Original signature : <code>float i_hght(float)</code><br>
    	 * <i>native declaration : line 59</i>
    	 */
     	float ihght(float pres);
    	/**
    	 * Original signature : <code>float i_vtmp(float)</code><br>
    	 * <i>native declaration : line 60</i>
    	 */
    	float ivtmp(float pres);
    	/**
    	 * Original signature : <code>float i_wdir(float)</code><br>
    	 * <i>native declaration : line 61</i>
    	 */
    	float iwdir(float pres);
    	/**
    	 * Original signature : <code>float i_wspd(float)</code><br>
    	 * <i>native declaration : line 62</i>
    	 */
     	float iwspd(float pres);
    	/**
    	 * Original signature : <code>float i_wndu(float)</code><br>
    	 * <i>native declaration : line 63</i>
    	 */
    	float iwndu(float pres);
    	/**
    	 * Original signature : <code>float i_wndv(float)</code><br>
    	 * <i>native declaration : line 64</i>
    	 */
    	float iwndv(float pres);
    	/**
    	 * Original signature : <code>float i_pres(float)</code><br>
    	 * <i>native declaration : line 65</i>
    	 */
    	float ipres(float hght);
    	/**
    	 * Original signature : <code>float i_omeg(float)</code><br>
    	 * <i>native declaration : line 66</i>
    	 */
    	float iomeg(float pres);
    	/**
    	 * Original signature : <code>sfc()</code><br>
    	 * <i>native declaration : line 67</i>
    	 */
     	int sfc();
    	/**
    	 * Original signature : <code>float top_pres()</code><br>
    	 * <i>native declaration : line 68</i>
    	 */
    	float top_pres();
    	/**
    	 * Original signature : <code>qc(float)</code><br>
    	 * <i>native declaration : line 69</i>
    	 */
    	int qc(float value);
    	/**
    	 * Original signature : <code>char* qc2(float, char*, short)</code><br>
    	 * <i>native declaration : line 70</i><br>
    	 * @deprecated use the safer methods {@link #qc2(float, java.nio.ByteBuffer, short)} and {@link #qc2(float, com.sun.jna.Pointer, short)} instead
    	 */
    	@java.lang.Deprecated 
    	Pointer qc2(float value, Pointer label, short prec);
    	/**
    	 * Original signature : <code>char* qc2(float, char*, short)</code><br>
    	 * <i>native declaration : line 70</i>
    	 */
    	Pointer qc2(float value, ByteBuffer label, short prec);
    	/**
    	 * Original signature : <code>float ftom(float)</code><br>
    	 * <i>native declaration : line 71</i>
    	 */
    	float ftom(float value);
    	/**
    	 * Original signature : <code>float mtof(float)</code><br>
    	 * <i>native declaration : line 72</i>
    	 */
    	float mtof(float value);
    	/**
    	 * Original signature : <code>float ftoc(float)</code><br>
    	 * <i>native declaration : line 73</i>
    	 */
    	float ftoc(float value);
    	/**
    	 * Original signature : <code>float ctof(float)</code><br>
    	 * <i>native declaration : line 74</i>
    	 */
    	float ctof(float value);
    	/**
    	 * Original signature : <code>float agl(float)</code><br>
    	 * <i>native declaration : line 75</i>
    	 */
    	float agl(float height);
    	/**
    	 * Original signature : <code>float msl(float)</code><br>
    	 * <i>native declaration : line 76</i>
    	 */
    	float msl(float height);
    	/**
    	 * Original signature : <code>float kt_to_mps(float)</code><br>
    	 * <i>native declaration : line 77</i>
    	 */
    	float kt_to_mps(float spd);
    	/**
    	 * Original signature : <code>float virtemp(float, float, float)</code><br>
    	 * <i>native declaration : line 78</i>
    	 */
    	float virtemp(float pres, float temp, float dwpt);
    	
    	/**
    	 * from skparams.h
    	 */
    	float Mean_thetae(FloatByReference param, float lower, float upper);
    	/**
    	 * Original signature : <code>float bulk_rich(_parcel, float*)</code><br>
    	 * <i>native declaration : line 24</i><br>
    	 * @deprecated use the safer methods {@link #bulk_rich(nsharp.NsharpLibrary._parcel.ByValue, java.nio.FloatBuffer)} and {@link #bulk_rich(nsharp.NsharpLibrary._parcel.ByValue, com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z9bulk_rich7_parcelPf", "?bulk_rich@@YAM7_parcelPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float bulk_rich(NsharpLibrary._parcel.ByValue pcl, FloatByReference brnshear);
    	/**
    	 * Original signature : <code>float cnvtv_temp(float*, float)</code><br>
    	 * <i>native declaration : line 25</i><br>
    	 * @deprecated use the safer methods {@link #cnvtv_temp(java.nio.FloatBuffer, float)} and {@link #cnvtv_temp(com.sun.jna.ptr.FloatByReference, float)} instead
    	 */
    	//@Mangling({"_Z10cnvtv_tempPff", "?cnvtv_temp@@YAMPAMM@Z"}) 
    	//@java.lang.Deprecated 
    	float cnvtv_temp(FloatByReference param, float mincinh);
    	/**
    	 * Original signature : <code>void define_parcel(short, float)</code><br>
    	 * <i>native declaration : line 26</i>
    	 */
    	/*flag   -   Parcel selection.                             */
    	/*            -1 = Use Previous Selection                    */
    	/*             1 = Observed sfc parcel                       */
    	/*             2 = Fcst sfc parcel                           */
    	/*             3 = Most unstable parcel                        */
    	/*             4 = Mean mixlyr parcel                      */
    	/*             5 = User defined parcel                       */
    	/*             6 = Eff                                       */
    	/*                                                           */
    	/*  pres   -   Pressure(mb) of user defined parcel.   		*/
    	
    	void define_parcel(short flag, float pres);
    	/**
    	 * Original signature : <code>float delta_t(float*)</code><br>
    	 * <i>native declaration : line 27</i><br>
    	 * @deprecated use the safer methods {@link #delta_t(java.nio.FloatBuffer)} and {@link #delta_t(com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z7delta_tPf", "?delta_t@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float delta_t(FloatByReference param);
    	/**
    	 * Original signature : <code>float ehi(float, float)</code><br>
    	 * <i>native declaration : line 28</i>
    	 */
    	//@Mangling({"_Z3ehiff", "?ehi@@YAMMM@Z"}) 
    	float ehi(float cape, float hel);
    	/**
    	 * Original signature : <code>grab_level(float)</code><br>
    	 * <i>native declaration : line 29</i>
    	 */
    	//@Mangling({"_Z10grab_levelf", "?grab_level@@YAXM@Z"}) 
    	int grab_level(float pres);
    	/**
    	 * Original signature : <code>float k_index(float*)</code><br>
    	 * <i>native declaration : line 30</i><br>
    	 * @deprecated use the safer methods {@link #k_index(java.nio.FloatBuffer)} and {@link #k_index(com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z7k_indexPf", "?k_index@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float k_index(FloatByReference param);
    	/**
    	 * Original signature : <code>float lapse_rate(float*, float, float)</code><br>
    	 * <i>native declaration : line 31</i><br>
    	 * @deprecated use the safer methods {@link #lapse_rate(java.nio.FloatBuffer, float, float)} and {@link #lapse_rate(com.sun.jna.ptr.FloatByReference, float, float)} instead
    	 */
    	//@Mangling({"_Z10lapse_ratePfff", "?lapse_rate@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float lapse_rate(FloatByReference param, float lower, float upper);
    	/**
    	 * Original signature : <code>void low_inv(float*, float*)</code><br>
    	 * <i>native declaration : line 32</i><br>
    	 * @deprecated use the safer methods {@link #low_inv(java.nio.FloatBuffer, java.nio.FloatBuffer)} and {@link #low_inv(com.sun.jna.ptr.FloatByReference, com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z7low_invPfPf", "?low_inv@@YAXPAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	void low_inv(FloatByReference inv_mb, FloatByReference inv_dC);
     	/**
    	 * Original signature : <code>float max_temp(float*, float)</code><br>
    	 * <i>native declaration : line 33</i><br>
    	 * @deprecated use the safer methods {@link #max_temp(java.nio.FloatBuffer, float)} and {@link #max_temp(com.sun.jna.ptr.FloatByReference, float)} instead
    	 */
    	//@Mangling({"_Z8max_tempPff", "?max_temp@@YAMPAMM@Z"}) 
    	//@java.lang.Deprecated 
    	float max_temp(FloatByReference param, float mixlyr);
    	/**
    	 * Original signature : <code>float mean_dwpt(float*, float, float)</code><br>
    	 * <i>native declaration : line 34</i><br>
    	 * @deprecated use the safer methods {@link #mean_dwpt(java.nio.FloatBuffer, float, float)} and {@link #mean_dwpt(com.sun.jna.ptr.FloatByReference, float, float)} instead
    	 */
    	//@Mangling({"_Z9mean_dwptPfff", "?mean_dwpt@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float mean_dwpt(FloatByReference param, float lower, float upper);
    	/**
    	 * Original signature : <code>float mean_mixratio(float*, float, float)</code><br>
    	 * <i>native declaration : line 35</i><br>
    	 * @deprecated use the safer methods {@link #mean_mixratio(java.nio.FloatBuffer, float, float)} and {@link #mean_mixratio(com.sun.jna.ptr.FloatByReference, float, float)} instead
    	 */
    	//@Mangling({"_Z13mean_mixratioPfff", "?mean_mixratio@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float mean_mixratio(FloatByReference param, float lower, float upper);
   	/**
    	 * Original signature : <code>float mean_relhum(float*, float, float)</code><br>
    	 * <i>native declaration : line 36</i><br>
    	 * @deprecated use the safer methods {@link #mean_relhum(java.nio.FloatBuffer, float, float)} and {@link #mean_relhum(com.sun.jna.ptr.FloatByReference, float, float)} instead
    	 */
    	//@Mangling({"_Z11mean_relhumPfff", "?mean_relhum@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float mean_relhum(FloatByReference param, float lower, float upper);
     	/**
    	 * Original signature : <code>float mean_theta(float*, float, float)</code><br>
    	 * <i>native declaration : line 37</i><br>
    	 * @deprecated use the safer methods {@link #mean_theta(java.nio.FloatBuffer, float, float)} and {@link #mean_theta(com.sun.jna.ptr.FloatByReference, float, float)} instead
    	 */
    	//@Mangling({"_Z10mean_thetaPfff", "?mean_theta@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float mean_theta(FloatByReference param, float lower, float upper);
    	/**
    	 * Original signature : <code>float Mean_WBtemp(float*, float, float)</code><br>
    	 * <i>native declaration : line 38</i><br>
    	 * @deprecated use the safer methods {@link #Mean_WBtemp(java.nio.FloatBuffer, float, float)} and {@link #Mean_WBtemp(com.sun.jna.ptr.FloatByReference, float, float)} instead
    	 */
    	//@Mangling({"_Z11Mean_WBtempPfff", "?Mean_WBtemp@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float Mean_WBtemp(FloatByReference param, float lower, float upper);
     	/**
    	 * Original signature : <code>void mix_height(float*, float*, float*, float*, float*, float*, float*, short)</code><br>
    	 * <i>native declaration : line 39</i><br>
    	 * @deprecated use the safer methods {@link #mix_height(java.nio.FloatBuffer, java.nio.FloatBuffer, java.nio.FloatBuffer, java.nio.FloatBuffer, java.nio.FloatBuffer, java.nio.FloatBuffer, java.nio.FloatBuffer, short)} and {@link #mix_height(com.sun.jna.ptr.FloatByReference, com.sun.jna.ptr.FloatByReference, com.sun.jna.ptr.FloatByReference, com.sun.jna.ptr.FloatByReference, com.sun.jna.ptr.FloatByReference, com.sun.jna.ptr.FloatByReference, com.sun.jna.ptr.FloatByReference, short)} instead
    	 */
    	//@Mangling({"_Z10mix_heightPfPfPfPfPfPfPfi", "?mix_height@@YAXPAMPAMPAMPAMPAMPAMPAMH@Z"}) 
    	//@java.lang.Deprecated 
    	void mix_height(FloatByReference mh_mb, FloatByReference mh_drct, FloatByReference mh_sped, FloatByReference mh_dC, FloatByReference mh_lr, FloatByReference mh_drct_max, FloatByReference mh_sped_max, short flag);
    	/**
    	 * Original signature : <code>float old_cnvtv_temp(float*)</code><br>
    	 * <i>native declaration : line 42</i><br>
    	 * @deprecated use the safer methods {@link #old_cnvtv_temp(java.nio.FloatBuffer)} and {@link #old_cnvtv_temp(com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z14old_cnvtv_tempPf", "?old_cnvtv_temp@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float old_cnvtv_temp(FloatByReference param);
    	/**
    	 * Original signature : <code>float parcel(float, float, float, float, float, _parcel*)</code><br>
    	 * <i>native declaration : line 43</i>
    	 */
    	//@Mangling({"_Z6parcelfffffP7_parcel", "?parcel@@YAMMMMMMPA7_parcel@Z"}) 
    	float parcel(float lower, float upper, float pres, float temp, float dwpt, NsharpLibrary._parcel pcl);
    	/**
    	 * Original signature : <code>float parcelx(float, float, float, float, float, _parcel*)</code><br>
    	 * <i>native declaration : line 45</i>
    	 */
    	//@Mangling({"_Z7parcelxfffffP7_parcel", "?parcelx@@YAMMMMMMPA7_parcel@Z"}) 
    	float parcelx(float lower, float upper, float pres, float temp, float dwpt, NsharpLibrary._parcel pcl);
    	/**
    	 * Original signature : <code>float precip_water(float*, float, float)</code><br>
    	 * <i>native declaration : line 47</i><br>
    	 * @deprecated use the safer methods {@link #precip_water(java.nio.FloatBuffer, float, float)} and {@link #precip_water(com.sun.jna.ptr.FloatByReference, float, float)} instead
    	 */
    	//@Mangling({"_Z12precip_waterPfff", "?precip_water@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float precip_water(FloatByReference param, float lower, float upper);
    	/**
    	 * Original signature : <code>float Rogash_QPF(float*)</code><br>
    	 * <i>native declaration : line 48</i><br>
    	 * @deprecated use the safer methods {@link #Rogash_QPF(java.nio.FloatBuffer)} and {@link #Rogash_QPF(com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z10Rogash_QPFPf", "?Rogash_QPF@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float Rogash_QPF(FloatByReference param);
    	
    	float dcape(FloatByReference level, FloatByReference drtemp);
     	/**
    	 * Original signature : <code>float sweat_index(float*)</code><br>
    	 * <i>native declaration : line 49</i><br>
    	 * @deprecated use the safer methods {@link #sweat_index(java.nio.FloatBuffer)} and {@link #sweat_index(com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z11sweat_indexPf", "?sweat_index@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float sweat_index(FloatByReference param);
    	/**
    	 * Original signature : <code>float temp_lvl(float, float*)</code><br>
    	 * <i>native declaration : line 50</i><br>
    	 * @deprecated use the safer methods {@link #temp_lvl(float, java.nio.FloatBuffer)} and {@link #temp_lvl(float, com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z8temp_lvlfPf", "?temp_lvl@@YAMMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float temp_lvl(float temp, FloatByReference param);
     	/**
    	 * Original signature : <code>float ThetaE_diff(float*)</code><br>
    	 * <i>native declaration : line 51</i><br>
    	 * @deprecated use the safer methods {@link #ThetaE_diff(java.nio.FloatBuffer)} and {@link #ThetaE_diff(com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z11ThetaE_diffPf", "?ThetaE_diff@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float ThetaE_diff(FloatByReference param);
     	/**
    	 * Original signature : <code>float top_moistlyr(float*)</code><br>
    	 * <i>native declaration : line 52</i><br>
    	 * @deprecated use the safer methods {@link #top_moistlyr(java.nio.FloatBuffer)} and {@link #top_moistlyr(com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z12top_moistlyrPf", "?top_moistlyr@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float top_moistlyr(FloatByReference param);
     	/**
    	 * Original signature : <code>float t_totals(float*, float*, float*)</code><br>
    	 * <i>native declaration : line 53</i><br>
    	 * @deprecated use the safer methods {@link #t_totals(java.nio.FloatBuffer, java.nio.FloatBuffer, java.nio.FloatBuffer)} and {@link #t_totals(com.sun.jna.ptr.FloatByReference, com.sun.jna.ptr.FloatByReference, com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z8t_totalsPfPfPf", "?t_totals@@YAMPAMPAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float t_totals(FloatByReference param, FloatByReference ct, FloatByReference vt);
     	/**
    	 * Original signature : <code>float unstbl_lvl(float*, float, float)</code><br>
    	 * <i>native declaration : line 54</i><br>
    	 * @deprecated use the safer methods {@link #unstbl_lvl(java.nio.FloatBuffer, float, float)} and {@link #unstbl_lvl(com.sun.jna.ptr.FloatByReference, float, float)} instead
    	 */
    	//@Mangling({"_Z10unstbl_lvlPfff", "?unstbl_lvl@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float unstbl_lvl(FloatByReference param, float lower, float upper);
     	/**
    	 * Original signature : <code>float vert_tot(float*)</code><br>
    	 * <i>native declaration : line 55</i><br>
    	 * @deprecated use the safer methods {@link #vert_tot(java.nio.FloatBuffer)} and {@link #vert_tot(com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z8vert_totPf", "?vert_tot@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float vert_tot(FloatByReference param);
    	/**
    	 * Original signature : <code>float wb_lvl(float, float*)</code><br>
    	 * <i>native declaration : line 56</i><br>
    	 * @deprecated use the safer methods {@link #wb_lvl(float, java.nio.FloatBuffer)} and {@link #wb_lvl(float, com.sun.jna.ptr.FloatByReference)} instead
    	 */
    	//@Mangling({"_Z6wb_lvlfPf", "?wb_lvl@@YAMMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float wb_lvl(float temp, FloatByReference param);
     	
    	//from wind.h
    	float helicity ( float lower, float upper, float sdir, float sspd,
    			FloatByReference phel, FloatByReference nhel );
    	void sr_wind ( float pbot, float ptop, float stdir, float stspd,
    			FloatByReference mnu, FloatByReference mnv, FloatByReference wdir, FloatByReference wspd );
    	void mean_wind ( float pbot, float ptop, FloatByReference mnu, FloatByReference mnv,
    			FloatByReference wdir, FloatByReference wspd );
    	void wind_shear ( float pbot, float ptop, FloatByReference shu, FloatByReference shv,
    			FloatByReference sdir, FloatByReference smag );
    	float ucomp( float wdir, float wspd );
    	float vcomp( float wdir, float wspd );
	
    	//from thermo.h
    	float thetae ( float pres, float temp, float dwpt );
    	void drylift ( float p1, float t1, float td1, FloatByReference p2, FloatByReference t2 );
    	float wetlift ( float pres, float temp, float pres2 );
    	float wetbulb ( float pres, float temp, float dwpt );
    	void effective_inflow_layer_thermo(float ecape, float ecinh, FloatByReference p_bot,FloatByReference p_top);
    	float relh(float pres, FloatByReference param);
    	//from xwvid.c
    	short vert_coords ( float hgt, float maxhgt );
    	void corfidi_MCS_motion(FloatByReference fpu, FloatByReference fpv, FloatByReference fp_dir, FloatByReference fp_spd, FloatByReference bpu, FloatByReference bpv, FloatByReference bp_dir, FloatByReference bp_spd);
    	void bunkers_left_motion(FloatByReference u, FloatByReference v, FloatByReference dir, FloatByReference spd);
    	void bunkers_storm_motion(FloatByReference u, FloatByReference v, FloatByReference dir, FloatByReference spd);
    	float damaging_wind();
    	float esp();
    	float coniglio1();
    	float scp(float stdir, float stspd);
    	float sigtorn_cin(float stdir, float stspd);
    	float sigtorn(float stdir, float stspd);
    	float sigtorn_fixed(float stdir, float stspd);
    	float sigtorn_test(float stdir, float stspd);
    	
    	
    	float advection_layer(FloatByReference param, float lower, float upper);
    }
    /*
    public interface Nsharp95Library extends Library {
    	//library name is libnsharp.so. but only use "nsharp" as lib name when loading
    	NsharpLibrary INSTANCE = (NsharpLibrary) Native.loadLibrary("nsharp", NsharpLibrary.class);
    	/// <i>native declaration : line 1</i>
    	public static class _lplvalues extends Structure {
    		/// C type : char[40]
    		public byte[] desc = new byte[(40)];
    		public float pres;
    		public float temp;
    		public float dwpt;
    		public short flag;
    		public  _lplvalues() {
    			super();
    		}
    		/// @param desc C type : char[40]
    		public  _lplvalues(byte desc[], float pres, float temp, float dwpt, short flag) {
    			super();
    			if (desc.length != this.desc.length) 
    				throw new java.lang.IllegalArgumentException("Wrong array size !");
    			this.desc = desc;
    			this.pres = pres;
    			this.temp = temp;
    			this.dwpt = dwpt;
    			this.flag = flag;
    		}
    		protected ByReference newByReference() { return new ByReference(); }
    		protected ByValue newByValue() { return new ByValue(); }
    		protected _lplvalues newInstance() { return new _lplvalues(); }
    		public static class ByReference extends _lplvalues implements Structure.ByReference {
    			
    		}
    		public static class ByValue extends _lplvalues implements Structure.ByValue {
    			
    		}
    	}
    	/// <i>native declaration : line 10</i>
    	public static class _parcel extends Structure{
    		public float lplpres;
    		public float lpltemp;
    		public float lpldwpt;
    		public float blayer;
    		public float tlayer;
    		public float entrain;
    		public float lclpres;
    		public float lfcpres;
    		public float elpres;
    		public float mplpres;
    		public float bplus;
    		public float bminus;
    		public float bfzl;
    		public float li5;
    		public float li3;
    		public float brn;
    		public float limax;
    		public float limaxpres;
    		public float cap;
    		public float cappres;
    		public  _parcel() {
    			super();
    		}
    		protected ByReference newByReference() { return new ByReference(); }
    		protected ByValue newByValue() { return new ByValue(); }
    		protected _parcel newInstance() { return new _parcel(); }
    		public static class ByReference extends _parcel implements Structure.ByReference {
    			
    		}
    		public static class ByValue extends _parcel implements Structure.ByValue {
    			
    		}
    	}
    	
    	//from our own SndgParms
    	public static class SndgParms extends com.sun.jna.Structure {
    		public float omega;
    		public float pres;
    		public float hght;
    		public float temp;
    		public float dwpt;
    		public float drct;
    		public float sped;
    		public  SndgParms() {
    			super();
    		}
    		public  SndgParms(float omega, float pres, float hght, float temp, float dwpt, float drct, float sped) {
    			super();
    			this.omega = omega;
    			this.pres = pres;
    			this.hght = hght;
    			this.temp = temp;
    			this.dwpt = dwpt;
    			this.drct = drct;
    			this.sped = sped;
    		}
    		
    		public void fillData(float omega, float pres, float hght, float temp, float dwpt, float drct, float sped) {
    			this.omega = omega;
    			this.pres = pres;
    			this.hght = hght;
    			this.temp = temp;
    			this.dwpt = dwpt;
    			this.drct = drct;
    			this.sped = sped;
    		}
    		protected ByReference newByReference() { return new ByReference(); }
    		protected ByValue newByValue() { return new ByValue(); }
    		protected SndgParms newInstance() { return new SndgParms(); }
    		public static class ByReference extends SndgParms implements Structure.ByReference {
    			
    		}
    		public static class ByValue extends SndgParms implements Structure.ByValue {
    			
    		}
    		
    	}
    	
    	public static class StormSlinkyStr extends Structure {
    		public int size;
    		public float tottim;
    		public float angl;
    		/// C type : float[200][2]
    		public float[] tsuv = new float[200 * 2];
    		public  StormSlinkyStr() {
    			super();
    			size=-1;
    		}
    		/// @param tsuv C type : float[200][2]
    		public  StormSlinkyStr(int size, float tottim, float angl, float tsuv[]) {
    			super();
    			this.size = size;
    			this.tottim = tottim;
    			this.angl = angl;
    			if (tsuv.length != this.tsuv.length) 
    				throw new java.lang.IllegalArgumentException("Wrong array size !");
    			this.tsuv = tsuv;
    		}
    		protected ByReference newByReference() { return new ByReference(); }
    		protected ByValue newByValue() { return new ByValue(); }
    		protected StormSlinkyStr newInstance() { return new StormSlinkyStr(); }
     		public static class ByReference extends StormSlinkyStr implements Structure.ByReference {

    		}
    		public static class ByValue extends StormSlinkyStr implements Structure.ByValue {

    		}

    		public float getTottim() {
    			return tottim;
    		}
    		public void setTottim(float tottim) {
    			this.tottim = tottim;
    		}
    		public float getAngl() {
    			return angl;
    		}
    		public void setAngl(float angl) {
    			this.angl = angl;
    		}
    		public float[] getTsuv() {
    			return tsuv;
    		}
    		public void setTsuv(float[] tsuv) {
    			this.tsuv = tsuv;
    		}
    		public int getSize() {
    			return size;
    		}
    		public void setSize(int size) {
    			this.size = size;
    		}

    	}
    	
    	//From our own caveNsharp.c
     	void populateSndgData(NsharpLibrary.CaveSndgParms snDataArray[], int arraySize, float winDir, float winSpd);   	
    	void get_lpvaluesData(NsharpLibrary._lplvalues pParcel);
    	void populateSndgTestData() ;
     	void get_surface(FloatByReference pressure, FloatByReference temp, FloatByReference  dewpt);
     	void get_surfaceWind(FloatByReference windSp, FloatByReference windDir);
     	//void get_top(FloatByReference pressure, FloatByReference temp, FloatByReference dewpt);
     	void get_storm(FloatByReference speed, FloatByReference direction);
     	void set_storm(float speed, float  direction);
     	void cave_visual1 ( float lower, float upper, float pres, float temp, float dwpt , NsharpLibrary.StormSlinkyStr stmSlinky);
     	float cave_bulk_rich ( float lplpres, float bplus,FloatByReference brnshear );
     	//void printSfcInfo();
     	//void showSndgData();
     	//From basic.h
    	
    	float i_temp(float pres);
    	
    	float i_dwpt(float pres);
    	
     	float i_hght(float pres);
    	
    	float i_vtmp(float pres);
    	
    	float i_wdir(float pres);
    	
     	float i_wspd(float pres);
    	
    	float i_wndu(float pres);
    	
    	float i_wndv(float pres);
    	
    	float i_pres(float hght);
    	
    	float i_omeg(float pres);
    	
     	int sfc();
    	
    	float top_pres();
    	
    	int qc(float value);
    	
    	@java.lang.Deprecated 
    	Pointer qc2(float value, Pointer label, short prec);
    	
    	Pointer qc2(float value, ByteBuffer label, short prec);
    	
    	float ftom(float value);
    	
    	float mtof(float value);
    	
    	float ftoc(float value);
    	
    	float ctof(float value);
    	
    	float agl(float height);
    	
    	float msl(float height);
    	
    	float kt_to_mps(float spd);
    	
    	float virtemp(float pres, float temp, float dwpt);
    	
    	//*
    	 //* from skparams.h
    	 //
    	
    	//@Mangling({"_Z9bulk_rich7_parcelPf", "?bulk_rich@@YAM7_parcelPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float bulk_rich(NsharpLibrary._parcel.ByValue pcl, FloatByReference brnshear);
    	
    	//@Mangling({"_Z10cnvtv_tempPff", "?cnvtv_temp@@YAMPAMM@Z"}) 
    	//@java.lang.Deprecated 
    	float cnvtv_temp(FloatByReference param, float mincinh);
    	
    	/*flag   -   Parcel selection.                             */
    	/*            -1 = Use Previous Selection                    */
    	/*             1 = Observed sfc parcel                       */
    	/*             2 = Fcst sfc parcel                           */
    	/*             3 = Mean mixlyr parcel                        */
    	/*             4 = Most unstable parcel                      */
    	/*             5 = User defined parcel                       */
    	/*                                                           */
    	/*  pres   -   Pressure(mb) of user defined parcel.   		*/
    	/*
    	void define_parcel(short flag, float pres);
    	
    	//@Mangling({"_Z7delta_tPf", "?delta_t@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float delta_t(FloatByReference param);
    	
    	//@Mangling({"_Z3ehiff", "?ehi@@YAMMM@Z"}) 
    	float ehi(float cape, float hel);
    	
    	//@Mangling({"_Z10grab_levelf", "?grab_level@@YAXM@Z"}) 
    	int grab_level(float pres);
    	
    	//@Mangling({"_Z7k_indexPf", "?k_index@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float k_index(FloatByReference param);
    	
    	//@Mangling({"_Z10lapse_ratePfff", "?lapse_rate@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float lapse_rate(FloatByReference param, float lower, float upper);
    	
    	//@Mangling({"_Z7low_invPfPf", "?low_inv@@YAXPAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	void low_inv(FloatByReference inv_mb, FloatByReference inv_dC);
     	
    	//@Mangling({"_Z8max_tempPff", "?max_temp@@YAMPAMM@Z"}) 
    	//@java.lang.Deprecated 
    	float max_temp(FloatByReference param, float mixlyr);
    	
    	//@Mangling({"_Z9mean_dwptPfff", "?mean_dwpt@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float mean_dwpt(FloatByReference param, float lower, float upper);
    	
    	//@Mangling({"_Z13mean_mixratioPfff", "?mean_mixratio@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float mean_mixratio(FloatByReference param, float lower, float upper);
   	
    	//@Mangling({"_Z11mean_relhumPfff", "?mean_relhum@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float mean_relhum(FloatByReference param, float lower, float upper);
     	
    	//@Mangling({"_Z10mean_thetaPfff", "?mean_theta@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float mean_theta(FloatByReference param, float lower, float upper);
    	
    	//@Mangling({"_Z11Mean_WBtempPfff", "?Mean_WBtemp@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float Mean_WBtemp(FloatByReference param, float lower, float upper);
     	
    	//@Mangling({"_Z10mix_heightPfPfPfPfPfPfPfi", "?mix_height@@YAXPAMPAMPAMPAMPAMPAMPAMH@Z"}) 
    	//@java.lang.Deprecated 
    	void mix_height(FloatByReference mh_mb, FloatByReference mh_drct, FloatByReference mh_sped, FloatByReference mh_dC, FloatByReference mh_lr, FloatByReference mh_drct_max, FloatByReference mh_sped_max, short flag);
    	
    	//@Mangling({"_Z14old_cnvtv_tempPf", "?old_cnvtv_temp@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float old_cnvtv_temp(FloatByReference param);
    	
    	//@Mangling({"_Z6parcelfffffP7_parcel", "?parcel@@YAMMMMMMPA7_parcel@Z"}) 
    	float parcel(float lower, float upper, float pres, float temp, float dwpt, NsharpLibrary._parcel pcl);
    	
    	//@Mangling({"_Z7parcelxfffffP7_parcel", "?parcelx@@YAMMMMMMPA7_parcel@Z"}) 
    	float parcelx(float lower, float upper, float pres, float temp, float dwpt, NsharpLibrary._parcel pcl);
    	
    	//@Mangling({"_Z12precip_waterPfff", "?precip_water@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float precip_water(FloatByReference param, float lower, float upper);
    	
    	//@Mangling({"_Z10Rogash_QPFPf", "?Rogash_QPF@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float Rogash_QPF(FloatByReference param);
     	
    	//@Mangling({"_Z11sweat_indexPf", "?sweat_index@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float sweat_index(FloatByReference param);
    	
    	//@Mangling({"_Z8temp_lvlfPf", "?temp_lvl@@YAMMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float temp_lvl(float temp, FloatByReference param);
     	
    	//@Mangling({"_Z11ThetaE_diffPf", "?ThetaE_diff@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float ThetaE_diff(FloatByReference param);
     	
    	//@Mangling({"_Z12top_moistlyrPf", "?top_moistlyr@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float top_moistlyr(FloatByReference param);
     	
    	//@Mangling({"_Z8t_totalsPfPfPf", "?t_totals@@YAMPAMPAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float t_totals(FloatByReference param, FloatByReference ct, FloatByReference vt);
     	
    	//@Mangling({"_Z10unstbl_lvlPfff", "?unstbl_lvl@@YAMPAMMM@Z"}) 
    	//@java.lang.Deprecated 
    	float unstbl_lvl(FloatByReference param, float lower, float upper);
     	
    	//@Mangling({"_Z8vert_totPf", "?vert_tot@@YAMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float vert_tot(FloatByReference param);
    	
    	//@Mangling({"_Z6wb_lvlfPf", "?wb_lvl@@YAMMPAM@Z"}) 
    	//@java.lang.Deprecated 
    	float wb_lvl(float temp, FloatByReference param);
     	
    	//from wind.h
    	float helicity ( float lower, float upper, float sdir, float sspd,
    			FloatByReference phel, FloatByReference nhel );
    	void sr_wind ( float pbot, float ptop, float stdir, float stspd,
    			FloatByReference mnu, FloatByReference mnv, FloatByReference wdir, FloatByReference wspd );
    	void mean_wind ( float pbot, float ptop, FloatByReference mnu, FloatByReference mnv,
    			FloatByReference wdir, FloatByReference wspd );
    	void wind_shear ( float pbot, float ptop, FloatByReference shu, FloatByReference shv,
    			FloatByReference sdir, FloatByReference smag );
    	float ucomp( float wdir, float wspd );
    	float vcomp( float wdir, float wspd );
	
    	//from thermo.h
    	float thetae ( float pres, float temp, float dwpt );
    	void drylift ( float p1, float t1, float td1, FloatByReference p2, FloatByReference t2 );
    	float wetlift ( float pres, float temp, float pres2 );
    	float wetbulb ( float pres, float temp, float dwpt );
    	//from xwvid.c
    	short vert_coords ( float hgt, float maxhgt );
    } */
	
}
