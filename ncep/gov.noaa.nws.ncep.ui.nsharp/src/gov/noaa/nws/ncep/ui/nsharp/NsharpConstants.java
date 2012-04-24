package gov.noaa.nws.ncep.ui.nsharp;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.NsharpConstants
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/23/2010	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

import java.text.DecimalFormat;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;

public class NsharpConstants {
    public static Rectangle NSHARP_SkewTRectangle = new Rectangle(0, 0, 3200, 1850);
    public static Rectangle NSHARP_HodoRectangle = new Rectangle(0, 0, 100, 100);
    public static final int DEFAULT_CANVAS_HEIGHT=700;
    public static final int DEFAULT_CANVAS_WIDTH=1100;
    //public static double TEMPERATURE_MIN = -115.0;

    public  static float KnotsToMetersPerSecond = 0.5144444F;
    public static final UnitConverter kelvinToCelsius = SI.KELVIN
    	.getConverterTo(SI.CELSIUS);
    public static final UnitConverter metersToFeet = SI.METER
    	.getConverterTo(NonSI.FOOT);

    public static final UnitConverter feetToMeters = NonSI.FOOT
    	.getConverterTo(SI.METRE);

    public static double WIND_SPEED_MIN = 0.0;

    public static double WIND_SPEED_MAX = 250.0;

    public static double WIND_DIR_MIN = 0.0;

    public static double WIND_DIR_MAX = 360.0;

    public static double PRESSURE_MIN = 100.0;

    public static double PRESSURE_MAX = 973.0;
    
    
    // horizontal pressure line that will be drawn.
    public static final double[] PRESSURE_MAIN_LEVELS = { 1050, 1000, 850,
            700, 500,  300, 200, 100 };
    public static final double[] PRESSURE_MARK_LEVELS = { 1050, 1000, 950, 900, 850, 800,
        750, 700, 650, 600, 550, 500, 450, 400, 350, 300, 250, 200, 150,
        100 };
    public static final double[] PRESSURE_NUMBERING_LEVELS = { 1000, 850,
        700, 500,  300, 200, 100 };

    //Icing pressure level 1000, 900,800,700,600, 500,400,  300
    public static final double ICING_PRESSURE_LEVEL_BOTTOM = 1000;
    public static final double ICING_PRESSURE_LEVEL_TOP = 300;
    public static final double ICING_PRESSURE_LEVEL_INC = 100;
    //Icing relative humidity 100, 90,80,70,60, 50,40,  30,20,10,0
    public static final double ICING_RELATIVE_HUMIDITY_LEFT = 0;
    public static final double ICING_RELATIVE_HUMIDITY_RIGHT = 100;
    public static final double ICING_RELATIVE_HUMIDITY_INC = 10;
    //Icing temperature (C) 25,20,15,10,5,0,-5,-10, -15,-20,-25
    public static final double ICING_TEMPERATURE_RIGHT = 25;
    public static final double ICING_TEMPERATURE_LEFT = -25;
    public static final double ICING_TEMPERATURE_INC = 5;

    //Turbulence pressure level 1000, 900,800,700,600, 500,400,  300, 200, 100
    public static final double TURBULENCE_PRESSURE_LEVEL_TOP =100;
    public static final double TURBULENCE_PRESSURE_LEVEL_BOTTOM =1000;
    public static final double TURBULENCE_PRESSURE_LEVEL_INC =100;
    //Turbulence LN Richardson Number -2,-1, 0, 1,2,3,4,5,6,7,8
    public static final double TURBULENCE_LN_RICHARDSON_NUMBER_LEFT =8;
    public static final double TURBULENCE_LN_RICHARDSON_NUMBER_RIGHT =-2;
    public static final double TURBULENCE_LN_RICHARDSON_NUMBER_INC =-1;
    //Turbulence wind shear TKE 50,45,40,35,30,25,20,15,10,5,0
    public static final double TURBULENCE_WIND_SHEAR_TKE_LEFT = 0;
    public static final double TURBULENCE_WIND_SHEAR_TKE_RIGHT = 50;
    public static final double TURBULENCE_WIND_SHEAR_TKE_INC = 5;

    // lightGray.
    public static final RGB backgroundColor = new RGB(191, 191, 191);

    /**
     * Color for moist adiabat lines
     */
    public static final RGB moistAdiabatColor = new RGB(0, 127, 255);
    public static final RGB degreeSpokeColor = new RGB(238,238,238);

    /**
     * Color for dry adiabat lines
     */
    public static final RGB dryAdiabatColor = new RGB(70,130,180);

    /**
     * Color for mixing ratio lines
     */
    public static final RGB mixingRatioColor = new RGB(23, 255, 23);

    /**
     * Color for temperature lines
     */
    public static final RGB temperatureColor = new RGB(0,102,255);

    /**
     * Color for pressure lines
     */
    public static final RGB pressureColor = new RGB(191, 191, 191);

    /**
     * Color for wetbulb lines
     */
    public static final RGB wetBulbColor = new RGB(0, 255, 255);

    public static final int wetBulbLineWidth = 1;

    /**
     * parameter and label color
     */
    public static final RGB labelColor = new RGB(191, 191, 191);

    public static final RGB editColor = new RGB(255, 165, 0);

    public static final RGB pointEditColor = new RGB(255, 0, 0);

    public static final int editLineWidth = 2;

    public static final RGB parcelColor = new RGB(132, 112, 255);

    public static final int parcelLineWidth = 2;

    public static final int moistAdiabaticIncrement = 5;

    // lightGray.
    public static final RGB pointColor = new RGB(255, 255, 0);

    public static DecimalFormat pressFormat = new DecimalFormat("####.#");

    public static DecimalFormat tempFormat = new DecimalFormat("###.#");

    public static DecimalFormat windFormat = new DecimalFormat("###.#");

    public static char DEGREE_SYMBOL = '\u00B0';
    public static char SQUARE_SYMBOL = '\u00B2';
    public static char THETA_SYMBOL = '\u03D1';
    public static char PERCENT_SYMBOL = '\u0025';

    public static double endpointRadius = 4;

    public static final int LABEL_PADDING = 5;

    public static double bottom = NsharpWxMath.getSkewTXY(1050, 0).y;

    public static double top = NsharpWxMath.getSkewTXY(100, 0).y;

    public static double height = top - bottom;

    public static double left =  -height*0.35;//(-height / 2) - 1;

    public static double right = height*0.65;//(height / 2) + 1;

    public static double center = (left + right) / 2;

    public  static RGB color_red = new RGB(255,0,0);//red
    public  static RGB color_green = new RGB(0,255,0);//green
    public  static RGB color_yellow_green = new RGB(154,205,50);//green
	public  static RGB color_yellow = new RGB(255,255,0);//yellow
	public  static RGB color_yellow_DK = new RGB(238,238,0);//yellow
	public  static RGB color_cyan = new RGB(0,255,255); //cyan
	public  static RGB color_cyan_md = new RGB(0,238,238); //cyan_md, cyan2
	public  static RGB color_navy = new RGB(0,0,128); //navy
	public  static RGB color_violet = new RGB(125,0,255);//violet
	public  static RGB color_violet_red = new RGB(208,32,144);//violet
	public  static RGB color_violet_md = new RGB(208,32,144);//md-violet
	public  static RGB color_white = new RGB(255,250,250);//white
	public  static RGB color_black = new RGB(0,0,0);//black
	public  static RGB color_orange = new RGB(255,122, 66);//orange
	public  static RGB color_babypink = new RGB(249,207, 221);//
	public  static RGB color_deeppink = new RGB(255,20, 147);//
	public  static RGB color_hotpink = new RGB(255,105, 180);//
	public  static RGB color_pink = new RGB(255, 192, 203);
	public  static RGB color_stellblue = new RGB(70,130,180);
	public  static RGB color_royalblue = new RGB(65,105,225);
	public  static RGB color_skyblue = new RGB(135,206,235);
	public  static RGB color_lightblue = new RGB(173, 216, 230);
	public  static RGB color_dodgerblue = new RGB(30,144,255);
	public  static RGB color_chocolate = new RGB(210,105,30);
	public  static RGB color_firebrick = new RGB(178,34,34);
	public  static RGB color_gold = new RGB(255,215,0);
	public  static RGB color_magenta = new RGB(255,0,255);
	public static final RGB[] COLOR_ARRAY = {color_green, color_violet,color_yellow,color_hotpink,
		color_stellblue,color_yellow_green,color_royalblue,color_violet_red,color_orange,color_deeppink,
		color_dodgerblue, color_chocolate,color_navy};
    // horizontal height line that will be drawn.
    public static final int[] HEIGHT_LEVEL_METERS = {/*16000,*/ 15000, 12000, 9000, 6000, 3000, 2000 };
    public static final int[] HEIGHT_LEVEL_FEET = {50000, 45000, 40000, 35000, 30000, 25000, 20000, 15000, 10000, 5000, 2500 };

    public static final int OMEGA_X_TOP = -40;
    public static final int OMEGA_Y_TOP = 200;//225;
    public static final int SKEWT_REC_X_ORIG = OMEGA_X_TOP + 160; 
    public static final int SKEWT_REC_Y_ORIG = OMEGA_Y_TOP-75; 
    public static final int SKEWT_REC_WIDTH = 1000;
    public static final int SKEWT_REC_HEIGHT = 1002;
    public static final int SKEWT_VIEW_X_END = SKEWT_REC_X_ORIG + SKEWT_REC_WIDTH;
    public static final int SKEWT_VIEW_Y_END = SKEWT_REC_Y_ORIG + SKEWT_REC_HEIGHT;
    public static final int OMEGA_Y_BOT = SKEWT_VIEW_Y_END-15;
    public static final int ICING_REC_X_ORIG =SKEWT_REC_X_ORIG;
    public static final int ICING_REC_Y_ORIG = SKEWT_REC_Y_ORIG;
    public static final int ICING_REC_WIDTH = SKEWT_REC_WIDTH;
    public static final int ICING_REC_HEIGHT = SKEWT_REC_HEIGHT-50;
    public static final int ICING_VIEW_X_END = ICING_REC_X_ORIG + ICING_REC_WIDTH;
    public static final int ICING_VIEW_Y_END = ICING_REC_Y_ORIG + ICING_REC_HEIGHT;
    public static final int TURB_REC_X_ORIG =SKEWT_REC_X_ORIG;
    public static final int TURB_REC_Y_ORIG = SKEWT_REC_Y_ORIG ;
    public static final int TURB_REC_WIDTH = SKEWT_REC_WIDTH;
    public static final int TURB_REC_HEIGHT = SKEWT_REC_HEIGHT -50;
    public static final int TURB_VIEW_X_END = TURB_REC_X_ORIG + TURB_REC_WIDTH;
    public static final int TURB_VIEW_Y_END = TURB_REC_Y_ORIG + TURB_REC_HEIGHT;
    public static final int DATAPANEL_REC_WIDTH = 750;
    public static final int DATAPANEL_REC_HEIGHT = 630;
    public static final int INSET_REC_WIDTH = DATAPANEL_REC_WIDTH/2;//250;//300;
    public static final int INSET_REC_HEIGHT = DATAPANEL_REC_HEIGHT/2;//SKEWT_REC_HEIGHT/3;;
    public static final int WIND_BOX_WIDTH = 175;
    public static final int WIND_BOX_HEIGHT = SKEWT_REC_HEIGHT;
    public static final int WIND_BOX_X_ORIG = SKEWT_REC_X_ORIG+SKEWT_REC_WIDTH;
    public static final int WIND_BOX_Y_ORIG = SKEWT_REC_Y_ORIG;
    
    public static final int VERTICAL_WIND_WIDTH = 175;
    public static final int VERTICAL_WIND_HEIGHT = SKEWT_REC_HEIGHT;
    public static final int VERTICAL_WIND_X_ORIG = WIND_BOX_X_ORIG+ WIND_BOX_WIDTH;
    public static final int VERTICAL_WIND_Y_ORIG = SKEWT_REC_Y_ORIG;
    
    public static final int HODO_REC_X_ORIG = VERTICAL_WIND_X_ORIG  + VERTICAL_WIND_WIDTH;
    public static final int HODO_REC_Y_ORIG = SKEWT_REC_Y_ORIG;
    public static final int HODO_REC_WIDTH = SKEWT_REC_WIDTH;//750;//1000;
    public static final int HODO_REC_HEIGHT = SKEWT_REC_HEIGHT;///3 * 2;
    public static final int HODO_VIEW_X_END = HODO_REC_X_ORIG + HODO_REC_WIDTH;
    public static final int HODO_VIEW_Y_END = HODO_REC_Y_ORIG + HODO_REC_HEIGHT;
    public static final float HODO_CENTER_X = HODO_REC_X_ORIG + (float)(5.00/12.00) * HODO_REC_WIDTH;
    public static final float HODO_CENTER_Y = HODO_REC_Y_ORIG + (float)(1.00/2.00) * HODO_REC_HEIGHT;
    
    public static final int WIND_MOTION_REC_X_ORIG = HODO_REC_X_ORIG;
    public static final int WIND_MOTION_REC_Y_ORIG = HODO_REC_Y_ORIG;
    public static final int WIND_MOTION_REC_WIDTH = 175;
    public static final int WIND_MOTION_REC_HEIGHT = 150;
    public static final int WIND_MOTION_VIEW_X_END = WIND_MOTION_REC_X_ORIG+WIND_MOTION_REC_WIDTH;
    public static final int WIND_MOTION_VIEW_Y_END = WIND_MOTION_REC_Y_ORIG+WIND_MOTION_REC_HEIGHT;

    public static final int DATA_TIMELINE_REC_X_ORIG = HODO_VIEW_X_END;
    public static final int DATA_TIMELINE_REC_Y_ORIG = HODO_REC_Y_ORIG;
    public static final int DATA_TIMELINE_REC_WIDTH =330;//280;
    public static final int DATA_TIMELINE_REC_HEIGHT = SKEWT_REC_HEIGHT-90;
    public static final int DATA_TIMELINE_VIEW_X_END = DATA_TIMELINE_REC_X_ORIG+DATA_TIMELINE_REC_WIDTH;
    public static final int DATA_TIMELINE_VIEW_Y_END = DATA_TIMELINE_REC_Y_ORIG+DATA_TIMELINE_REC_HEIGHT;
    public static final int DATA_TIMELINE_NEXT_PAGE_END = DATA_TIMELINE_REC_Y_ORIG+ 30;
    public static final int DATA_TIMELINE_NOTATION_Y_START = DATA_TIMELINE_VIEW_Y_END;//- 100;
    public static final int DATA_TIMELINE_SORT_X_START = DATA_TIMELINE_REC_X_ORIG+(7*DATA_TIMELINE_REC_WIDTH/18);
    public static final int STATION_ID_REC_X_ORIG = DATA_TIMELINE_VIEW_X_END;
    public static final int STATION_ID_REC_Y_ORIG = DATA_TIMELINE_REC_Y_ORIG;
    public static final int STATION_ID_REC_WIDTH = 300;
    public static final int STATION_ID_REC_HEIGHT = DATA_TIMELINE_REC_HEIGHT;
    public static final int STATION_ID_VIEW_X_END = STATION_ID_REC_X_ORIG+STATION_ID_REC_WIDTH;
    public static final int STATION_ID_VIEW_Y_END = STATION_ID_REC_Y_ORIG+STATION_ID_REC_HEIGHT;
    //public static final int STATION_ID_NOTATION_Y_START = STATION_ID_VIEW_Y_END-90;
    public static final int COLOR_NOTATION_REC_X_ORIG = DATA_TIMELINE_REC_X_ORIG;
    public static final int COLOR_NOTATION_REC_Y_ORIG = DATA_TIMELINE_VIEW_Y_END;
    public static final int COLOR_NOTATION_REC_WIDTH = DATA_TIMELINE_REC_WIDTH+STATION_ID_REC_WIDTH;
    public static final int COLOR_NOTATION_REC_HEIGHT = SKEWT_REC_HEIGHT-DATA_TIMELINE_REC_HEIGHT;
    public static final int COLOR_NOTATION_VIEW_X_END = COLOR_NOTATION_REC_X_ORIG+COLOR_NOTATION_REC_WIDTH;
    public static final int COLOR_NOTATION_VIEW_Y_END = COLOR_NOTATION_REC_Y_ORIG+COLOR_NOTATION_REC_HEIGHT;
    
 
    public static final int DATAPANEL1_REC_X_ORIG = OMEGA_X_TOP+150;
    public static final int DATAPANEL1_REC_Y_ORIG = SKEWT_VIEW_Y_END + 50;
    public static final int DATAPANEL1_REC_WIDTH = DATAPANEL_REC_WIDTH;
    public static final int DATAPANEL1_REC_HEIGHT = DATAPANEL_REC_HEIGHT;
    public static final int DATAPANEL1_VIEW_X_END = DATAPANEL1_REC_X_ORIG+DATAPANEL1_REC_WIDTH;
    public static final int DATAPANEL1_VIEW_Y_END = DATAPANEL1_REC_Y_ORIG+DATAPANEL1_REC_HEIGHT;

    public static final int DATAPANEL2_REC_X_ORIG = DATAPANEL1_VIEW_X_END;
    public static final int DATAPANEL2_REC_Y_ORIG = DATAPANEL1_REC_Y_ORIG;
    public static final int DATAPANEL2_REC_WIDTH = DATAPANEL_REC_WIDTH;
    public static final int DATAPANEL2_REC_HEIGHT = DATAPANEL_REC_HEIGHT;
    public static final int DATAPANEL2_VIEW_X_END = DATAPANEL2_REC_X_ORIG+DATAPANEL2_REC_WIDTH;
    public static final int DATAPANEL2_VIEW_Y_END = DATAPANEL2_REC_Y_ORIG+DATAPANEL2_REC_HEIGHT;

    public static final int DATAPANEL3_REC_X_ORIG = DATAPANEL2_VIEW_X_END;
    public static final int DATAPANEL3_REC_Y_ORIG = DATAPANEL1_REC_Y_ORIG;
    public static final int DATAPANEL3_REC_WIDTH = DATAPANEL_REC_WIDTH;
    public static final int DATAPANEL3_REC_HEIGHT = DATAPANEL_REC_HEIGHT;
    public static final int DATAPANEL3_VIEW_X_END = DATAPANEL3_REC_X_ORIG+DATAPANEL3_REC_WIDTH;
    public static final int DATAPANEL3_VIEW_Y_END = DATAPANEL3_REC_Y_ORIG+DATAPANEL3_REC_HEIGHT;

    public static final int DATAPANEL4_REC_X_ORIG = DATAPANEL3_VIEW_X_END;
    public static final int DATAPANEL4_REC_Y_ORIG = DATAPANEL1_REC_Y_ORIG;
    public static final int DATAPANEL4_REC_WIDTH = DATAPANEL_REC_WIDTH;
    public static final int DATAPANEL4_REC_HEIGHT = DATAPANEL_REC_HEIGHT;
    public static final int DATAPANEL4_VIEW_X_END = DATAPANEL4_REC_X_ORIG+DATAPANEL4_REC_WIDTH;
    public static final int DATAPANEL4_VIEW_Y_END = DATAPANEL4_REC_Y_ORIG+DATAPANEL4_REC_HEIGHT;
    
    public static final int SRWINDS_REC_X_ORIG = DATAPANEL2_VIEW_X_END;
    public static final int SRWINDS_REC_Y_ORIG = DATAPANEL1_REC_Y_ORIG;
    public static final int SRWINDS_REC_WIDTH = INSET_REC_WIDTH;
    public static final int SRWINDS_REC_HEIGHT = INSET_REC_HEIGHT;
    public static final int SRWINDS_VIEW_X_END = SRWINDS_REC_X_ORIG+SRWINDS_REC_WIDTH;
    public static final int SRWINDS_VIEW_Y_END = SRWINDS_REC_Y_ORIG+SRWINDS_REC_HEIGHT;
    
    public static final int STORMSLINKY_REC_X_ORIG = SRWINDS_VIEW_X_END;
    public static final int STORMSLINKY_REC_Y_ORIG = DATAPANEL1_REC_Y_ORIG;
    public static final int STORMSLINKY_REC_WIDTH = INSET_REC_WIDTH;
    public static final int STORMSLINKY_REC_HEIGHT = INSET_REC_HEIGHT;
    public static final int STORMSLINKY_VIEW_X_END = STORMSLINKY_REC_X_ORIG+STORMSLINKY_REC_WIDTH;
    public static final int STORMSLINKY_VIEW_Y_END = STORMSLINKY_REC_Y_ORIG+STORMSLINKY_REC_HEIGHT;
    
    public static final int THETAP_REC_X_ORIG = SRWINDS_REC_X_ORIG;
    public static final int THETAP_REC_Y_ORIG = SRWINDS_VIEW_Y_END; 
    public static final int THETAP_REC_WIDTH = INSET_REC_WIDTH;
    public static final int THETAP_REC_HEIGHT = INSET_REC_HEIGHT;
    public static final int THETAP_VIEW_X_END = THETAP_REC_X_ORIG+THETAP_REC_WIDTH;
    public static final int THETAP_VIEW_Y_END = THETAP_REC_Y_ORIG+THETAP_REC_HEIGHT;
    
    //same position as THETAP_REC
    public static final int THETAH_REC_X_ORIG = SRWINDS_REC_X_ORIG;
    public static final int THETAH_REC_Y_ORIG = SRWINDS_VIEW_Y_END;
    public static final int THETAH_REC_WIDTH = INSET_REC_WIDTH;
    public static final int THETAH_REC_HEIGHT = INSET_REC_HEIGHT;
    public static final int THETAH_VIEW_X_END = THETAH_REC_X_ORIG+THETAH_REC_WIDTH;
    public static final int THETAH_VIEW_Y_END = THETAH_REC_Y_ORIG+THETAH_REC_HEIGHT;
    
    public static final int PSBLWATCH_REC_X_ORIG = THETAP_VIEW_X_END;
    public static final int PSBLWATCH_REC_Y_ORIG = THETAP_REC_Y_ORIG;
    public static final int PSBLWATCH_REC_WIDTH = INSET_REC_WIDTH;
    public static final int PSBLWATCH_REC_HEIGHT = INSET_REC_HEIGHT;
    public static final int PSBLWATCH_VIEW_X_END = PSBLWATCH_REC_X_ORIG+PSBLWATCH_REC_WIDTH;
    public static final int PSBLWATCH_VIEW_Y_END = PSBLWATCH_REC_Y_ORIG+PSBLWATCH_REC_HEIGHT;
    
    //same position as PSBLWATCH_REC
    public static final int SRWINDVTRS_REC_X_ORIG = THETAP_VIEW_X_END;
    public static final int SRWINDVTRS_REC_Y_ORIG = THETAP_REC_Y_ORIG;
    public static final int SRWINDVTRS_REC_WIDTH = INSET_REC_WIDTH;
    public static final int SRWINDVTRS_REC_HEIGHT = INSET_REC_HEIGHT;
    public static final int SRWINDVTRS_VIEW_X_END = SRWINDVTRS_REC_X_ORIG+SRWINDVTRS_REC_WIDTH;
    public static final int SRWINDVTRS_VIEW_Y_END = SRWINDVTRS_REC_Y_ORIG+SRWINDVTRS_REC_HEIGHT;

    public static final int CHAR_HEIGHT = 25;
    //Dialog
    //public static final int dialogX = 300;
    public static int btnWidth = 120;
	public static int btnHeight = 20;
	public static int labelGap = 20;
	public static int btnGapX = 5;
	public static int btnGapY = 5;
	public static int listWidth = 160;
	public static int listHeight = 80;
	public static int filelistWidth = 120;
	public static int filelistHeight = 100;
	public static int dsiplayPanelSize = 2;
	
	public static int GRAPH_SKEWT = 0; 
	public static int GRAPH_ICING = 1; 
	public static int GRAPH_TURB = 2; 
	public static int MAX_GRAPH_MODE = 3;
	
	public static String getNlistFile() {
		return NcPathManager.getInstance().getStaticFile( 
				  NcPathConstants.NSHARP_NLIST_FILE ).getAbsolutePath();
    }
	public static String getSupFile() {
		return NcPathManager.getInstance().getStaticFile( 
				  NcPathConstants.NSHARP_SUP_FILE ).getAbsolutePath();
    }
	
	//Line configuration. Line name listing order in this array should be in order with constant defined below it.
	public static String[] lineNameArray= {"Temperature", "Dew Point", "Parcel","Virtual Temp","Wetbulb","Wind Barb","Overlay 1", "Overlay 2", "Compare 1", "Compare 2","Compare 3","Compare 4","Compare 5","Compare 6","Compare 7","Compare 8","Compare 9","Compare 10",
											"Icing RH", "Icing Temp", "Icing EPI", "Turbulence Ln", "Turbulence WindShear"};
	public static int LINE_TEMP = 0;
	public static int LINE_DEWP = 1;
	public static int LINE_PARCEL = 2;
	public static int LINE_VIRTUAL_TEMP =3;
	public static int LINE_WETBULB = 4;
	public static int LINE_WIND_BARB = 5;
	public static int LINE_OVERLAY1 = 6;
	public static int LINE_OVERLAY2= 7;
	public static int LINE_COMP1 = 8;
	public static int LINE_COMP2 = 9;
	public static int LINE_COMP3 = 10;
	public static int LINE_COMP4 = 11;
	public static int LINE_COMP5 = 12;
	public static int LINE_COMP6 = 13;
	public static int LINE_COMP7 = 14;
	public static int LINE_COMP8 = 15;
	public static int LINE_COMP9 = 16;
	public static int LINE_COMP10 = 17;
	public static int LINE_ICING_RH = 18;
	public static int LINE_ICING_TEMP= 19;
	public static int LINE_ICING_EPI=20;
	public static int LINE_TURBULENCE_LN = 21;
	public static int LINE_TURBULENCE_WS = 22;
	
	//defaultLineProperty should be listed in sync with lineNameArray for each line
	public static NsharpLineProperty[] defaultLineProperty = 
	{
		new NsharpLineProperty(LineStyle.SOLID, 2,color_red ),
		new NsharpLineProperty(LineStyle.SOLID, 2,color_green ),
		new NsharpLineProperty(LineStyle.SHORT_DASHED, 1,color_white ),
		new NsharpLineProperty(LineStyle.SHORT_DASHED, 2,color_red ),
		new NsharpLineProperty(LineStyle.SOLID, 1,wetBulbColor ),
		new NsharpLineProperty(LineStyle.SOLID, 1,color_yellow ),
		new NsharpLineProperty(LineStyle.SOLID, 2,color_red ),
		new NsharpLineProperty(LineStyle.SOLID, 2,color_green ),
		new NsharpLineProperty(LineStyle.SOLID, 2,color_red ),
		new NsharpLineProperty(LineStyle.SOLID, 2,color_green ),
		new NsharpLineProperty(LineStyle.SOLID, 2, new RGB (155, 0, 220) ),
		new NsharpLineProperty(LineStyle.SOLID, 2,new RGB (30, 144, 255)  ),
		new NsharpLineProperty(LineStyle.SOLID, 2,new RGB (255, 215, 0) ),
		new NsharpLineProperty(LineStyle.SOLID, 2,new RGB (0, 255, 255) ),
		new NsharpLineProperty(LineStyle.SOLID, 2,new RGB (139, 71, 38) ),
		new NsharpLineProperty(LineStyle.SOLID, 2,new RGB (139, 0, 139) ),
		new NsharpLineProperty(LineStyle.SOLID, 2,new RGB (0, 139, 0) ),
		new NsharpLineProperty(LineStyle.SOLID, 2,new RGB (144, 238, 144) ),
		new NsharpLineProperty(LineStyle.SOLID, 2,color_green ),
		new NsharpLineProperty(LineStyle.SOLID, 2,color_red ),
		new NsharpLineProperty(LineStyle.SOLID, 2,color_magenta ),
		new NsharpLineProperty(LineStyle.SOLID, 2,color_magenta ),
		new NsharpLineProperty(LineStyle.SOLID, 2,new RGB (255, 174, 185)  )
		
	};
}
