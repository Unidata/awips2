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
import java.util.HashMap;

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

    //public static double WIND_SPEED_MIN = 0.0;

    //public static double WIND_SPEED_MAX = 250.0;

   // public static double WIND_DIR_MIN = 0.0;

    //public static double WIND_DIR_MAX = 360.0;
    
    public static final int WINDBARB_DISTANCE_DEFAULT= 600; // in meters
    public static final float WINDBARB_WIDTH = 2;
    public static final float WINDBARB_SIZE = 2.5f;

    public static double PRESSURE_MIN = 100.0;

    public static double PRESSURE_MAX = 973.0;
    
    public static final double MAX_PRESSURE = 1050;
    public static final double MIN_PRESSURE = 100;
    // horizontal pressure line that will be drawn.
    public static final Integer[] PRESSURE_MAIN_LEVELS = { 1000, 850,
            700, 500,  300, 200,150};
    public static final int[] PRESSURE_MARK_LEVELS = { 1000, 950, 900, 850, 800,
        750, 700, 650, 600, 550, 500, 450, 400, 350, 300, 250, 200, 150
      };
    public static final int[] PRESSURE_NUMBERING_LEVELS = { 1000, 850,
        700, 500,  300, 200, 150 };

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

    public static double left =  -height*0.45;//(-height / 2) - 1;

    public static double right = height*0.55;//(height / 2) + 1;

    public static double center = (left + right) / 2;

    public  static RGB color_vanilla = new RGB(255,239,206);
    public  static RGB color_red = new RGB(255,0,0);//red
    public  static RGB color_green = new RGB(0,255,0);//green
    public  static RGB color_darkgreen = new RGB(0x2f,0x4f,0x2f);//green
    public  static RGB color_mdgreen = new RGB(0x34,0x80,0x17);//green
    public  static RGB color_coral = new RGB(0xf0,0x80,0x80);
    public  static RGB color_lawngreen = new RGB(119,255,0);//green
    public  static RGB color_yellow_green = new RGB(154,205,50);//green
	public  static RGB color_yellow = new RGB(255,255,0);//yellow
	public  static RGB color_yellow_DK = new RGB(238,238,0);//yellow
	public  static RGB color_cyan = new RGB(0,255,255); //cyan
	public  static RGB color_cyan_md = new RGB(0,238,238); //cyan_md, cyan2
	public  static RGB color_navy = new RGB(0,0,128); //navy
	public  static RGB color_apricot = new RGB(251,206,177);
	public  static RGB color_plum = new RGB(0xB9,0x3B,0x8F);
	public  static RGB color_purple = new RGB(0x8E,0x35,0xEF);
	public  static RGB color_violet = new RGB(125,0,255);//violet
	public  static RGB color_violet_red = new RGB(208,32,144);//violet
	public  static RGB color_violet_md = new RGB(208,32,144);//md-violet
	public  static RGB color_white = new RGB(255,255,255);//white
	public  static RGB color_brown = new RGB(166,104,41);//brown
	public  static RGB color_black = new RGB(0,0,0);//black
	public  static RGB color_orangered = new RGB(255,0x45, 0);//orangered
	public  static RGB color_orange = new RGB(255,122, 66);//orange
	public  static RGB color_darkorange = new RGB(255,140, 0);//orange
	public  static RGB color_babypink = new RGB(249,207, 221);//
	public  static RGB color_deeppink = new RGB(255,20, 147);//
	public  static RGB color_hotpink = new RGB(255,105, 180);//
	public  static RGB color_pink = new RGB(255, 192, 203);
	public  static RGB color_blue = new RGB(0,0,255);
	public  static RGB color_stellblue = new RGB(70,130,180);
	public  static RGB color_royalblue = new RGB(65,105,225);
	public  static RGB color_skyblue = new RGB(135,206,235);
	public  static RGB color_lightblue = new RGB(173, 223,255);
	public  static RGB color_dodgerblue = new RGB(30,144,255);
	public  static RGB color_chocolate = new RGB(210,105,30);
	public  static RGB color_firebrick = new RGB(178,34,34);
	public  static RGB color_gold = new RGB(255,215,0);
	public  static RGB color_magenta = new RGB(255,0,255);
	public  static RGB color_maroon = new RGB(0x80,0,0);
	public static final RGB[] COLOR_ARRAY = {color_green, color_violet,color_yellow,color_hotpink,
		color_stellblue,color_yellow_green,color_royalblue,color_violet_red,color_orange,color_deeppink,
		color_dodgerblue, color_chocolate,color_navy};
	
	public static final HashMap<Integer, RGB> gempakColorToRGB = new HashMap<Integer, RGB>(){  
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			put(1,NsharpConstants.color_vanilla);
			put(2,NsharpConstants.color_red);
			put(3,NsharpConstants.color_green);
			put(4,NsharpConstants.color_blue);
			put(5,NsharpConstants.color_yellow);
			put(6,NsharpConstants.color_cyan);
			put(7,NsharpConstants.color_magenta);
			put(8,NsharpConstants.color_brown);
			put(9,NsharpConstants.color_coral);
			put(10,NsharpConstants.color_apricot);
			put(11,NsharpConstants.color_pink);
			put(12,NsharpConstants.color_deeppink);
			put(13,NsharpConstants.color_violet_md);
			put(14,NsharpConstants.color_maroon);
			put(15,NsharpConstants.color_firebrick);
			put(16,NsharpConstants.color_orangered);
			put(17,NsharpConstants.color_orange);
			put(18,NsharpConstants.color_darkorange);
			put(19,NsharpConstants.color_gold);
			put(20,NsharpConstants.color_yellow_DK);
			put(21,NsharpConstants.color_lawngreen);
			put(22,NsharpConstants.color_mdgreen);
			put(23,NsharpConstants.color_darkgreen);
			put(24,NsharpConstants.color_blue);
			put(25,NsharpConstants.color_lightblue);
			put(26,NsharpConstants.color_skyblue);
			put(27,NsharpConstants.color_cyan_md);
			put(28,NsharpConstants.color_violet);
			put(29,NsharpConstants.color_purple);
			put(30,NsharpConstants.color_plum);
			put(31,NsharpConstants.color_white);
			put(32,NsharpConstants.color_black);
		}
	};
    // horizontal height line that will be drawn.
    public static final int[] HEIGHT_LEVEL_METERS = {/*16000,*/ 15000, 12000, 9000, 6000, 3000, 2000 };
    public static final int[] HEIGHT_LEVEL_FEET = {50000, 45000, 40000, 35000, 30000, 25000, 20000, 15000, 10000, 5000, 2500 };

    /***
     * Chin: this implementation will be obsoleted when D2D changes its D2D Nsharp implementation to use 
     * multiple panes design.
     * SINGLE PANE IMPLEMENTATIONS start
     * Obsoleting implementation
     * 
     ****/
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
    //public static final int HODO_VIEW_Y_END = HODO_REC_Y_ORIG + HODO_REC_HEIGHT;
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
    public static final int DATA_TIMELINE_REC_WIDTH =350;//280;
    public static final int DATA_TIMELINE_REC_HEIGHT = SKEWT_REC_HEIGHT-100;
    public static final int DATA_TIMELINE_VIEW_X_END = DATA_TIMELINE_REC_X_ORIG+DATA_TIMELINE_REC_WIDTH;
    public static final int DATA_TIMELINE_VIEW_Y_END = DATA_TIMELINE_REC_Y_ORIG+DATA_TIMELINE_REC_HEIGHT;
    public static final int DATA_TIMELINE_NEXT_PAGE_END = DATA_TIMELINE_REC_Y_ORIG+ 30;
    public static final int DATA_TIMELINE_NOTATION_Y_START = DATA_TIMELINE_VIEW_Y_END;//- 100;
    //public static final int DATA_TIMELINE_SORT_X_START = DATA_TIMELINE_REC_X_ORIG+(7*DATA_TIMELINE_REC_WIDTH/18);
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
    /****
     * SINGLE PANE IMPLEMENTATIONS end
     * 
     ******/
    
    
    /***
     * 
     * MULTIPLE PANES IMPLEMENTATIONS start
     * 
     * 
     ****/
    /*public static  final int DISPLAY_SKEWT=0;
    public static  final int DISPLAY_WITO= DISPLAY_SKEWT+1; //Wind box + InferredTemp + Omega
    public static  final int DISPLAY_INSET= DISPLAY_WITO+1;
    public static  final int DISPLAY_HODO= DISPLAY_INSET+1;
    public static  final int DISPLAY_TIMESTN= DISPLAY_HODO+1;
    public static  final int DISPLAY_DATA=DISPLAY_TIMESTN+1;
    public static  final int DISPLAY_SPC_GRAPHS= DISPLAY_DATA+1;
    public static  final int DISPLAY_TOTAL= DISPLAY_SPC_GRAPHS+1;*/
    public static  final int CHAR_HEIGHT_ = 15;
    
	//note: dimensions are used as reference for its components inside its pane and only relative within its own pane.
    //Sizes defined here have no significant meaning between two different panes. 
    public static final int DISPLAY_WIDTH= 1600;
    public static final int DISPLAY_HEIGHT= 820;
    public static final int SKEWT_PANE_REC_WIDTH = DISPLAY_WIDTH/2 * 11 /16; //800 *11/16=550
    public static final int SKEWT_PANE_REC_HEIGHT = DISPLAY_HEIGHT * 8 /10; // 820*0.8 =656;
    public static final int WITO_PANE_REC_WIDTH = DISPLAY_WIDTH/2 - SKEWT_PANE_REC_WIDTH; //800-550=250
    public static final int WITO_PANE_REC_HEIGHT = DISPLAY_HEIGHT * 8 /10; // 820*0.8 =656;
    public static final int HODO_PANE_REC_WIDTH = (DISPLAY_WIDTH/2) * 13/20;//800x0.65=520;;
    public static final int HODO_PANE_REC_HEIGHT = DISPLAY_HEIGHT * 7 /10; // 820*0.7 =574;
    public static final int TIMESTN_PANE_REC_WIDTH = DISPLAY_WIDTH/2 - HODO_PANE_REC_WIDTH;//800-520=280
    public static final int TIMESTN_PANE_REC_HEIGHT = HODO_PANE_REC_HEIGHT; //574
    public static final int INSET_PANE_REC_WIDTH = DISPLAY_WIDTH/2;
    public static final int INSET_PANE_REC_HEIGHT = DISPLAY_HEIGHT * 2 /10; // 820*0.2 =164;
    public static final int DATA_PANE_REC_WIDTH =  DISPLAY_WIDTH/2; //800
    public static final int DATA_PANE_REC_HEIGHT = DISPLAY_HEIGHT * 3 /10; // 820*0.3 =246;
    public static Rectangle SKEWT_DISPLAY_REC = new Rectangle(0, 0, SKEWT_PANE_REC_WIDTH, SKEWT_PANE_REC_HEIGHT);
    public static Rectangle WITO_DISPLAY_REC = new Rectangle(0, 0, WITO_PANE_REC_WIDTH, WITO_PANE_REC_HEIGHT);
    public static Rectangle HODO_DISPLAY_REC = new Rectangle(0, 0, HODO_PANE_REC_WIDTH, HODO_PANE_REC_HEIGHT);
    public static Rectangle TIMESTN_DISPLAY_REC = new Rectangle(0, 0, TIMESTN_PANE_REC_WIDTH, TIMESTN_PANE_REC_HEIGHT);
    public static Rectangle FUTURE_DISPLAY_REC = new Rectangle(0, 0, TIMESTN_PANE_REC_WIDTH, TIMESTN_PANE_REC_HEIGHT);
    public static Rectangle DATA_DISPLAY_REC = new Rectangle(0, 0, DATA_PANE_REC_WIDTH, DATA_PANE_REC_HEIGHT);
    public static Rectangle INSET_DISPLAY_REC = new Rectangle(0, 0, INSET_PANE_REC_WIDTH, INSET_PANE_REC_HEIGHT);
    public static Rectangle SPC_GRAPH_DISPLAY_REC = new Rectangle(0, 0, DATA_PANE_REC_WIDTH, DATA_PANE_REC_HEIGHT);
    public static final int SKEWT_WIDTH = SKEWT_PANE_REC_WIDTH;//=550
    public static final int SKEWT_HEIGHT = SKEWT_PANE_REC_HEIGHT;//570
    public static final int SKEWT_X_ORIG = 0;
    public static final int SKEWT_Y_ORIG = 0;//70; 
    public static final int SKEWT_X_END = SKEWT_X_ORIG + SKEWT_WIDTH;
    public static final int SKEWT_Y_END = SKEWT_Y_ORIG + SKEWT_HEIGHT;
    public static final int ICING_X_ORIG =SKEWT_X_ORIG;
    public static final int ICING_Y_ORIG = SKEWT_Y_ORIG;
    public static final int ICING_WIDTH = SKEWT_WIDTH;
    public static final int ICING_HEIGHT = SKEWT_HEIGHT-25;
    public static final int ICING_X_END = ICING_X_ORIG + ICING_WIDTH;
    public static final int ICING_Y_END = ICING_Y_ORIG + ICING_HEIGHT;
    public static final int TURB_X_ORIG =SKEWT_X_ORIG;
    public static final int TURB_Y_ORIG = SKEWT_Y_ORIG ;
    public static final int TURB_WIDTH = SKEWT_WIDTH;
    public static final int TURB_HEIGHT = SKEWT_HEIGHT -25;
    public static final int TURB_X_END = TURB_X_ORIG + TURB_WIDTH;
    public static final int TURB_Y_END = TURB_Y_ORIG + TURB_HEIGHT;
    public static final int WIND_BX_WIDTH = WITO_PANE_REC_WIDTH/3;;
    public static final int WIND_BX_HEIGHT = SKEWT_HEIGHT;
    public static final int WIND_BX_X_ORIG = 0;
    public static final int WIND_BX_Y_ORIG = SKEWT_Y_ORIG;
    public static final int VRTCAL_WIND_WIDTH = WITO_PANE_REC_WIDTH/3;;
    public static final int VRTCAL_WIND_HEIGHT = SKEWT_HEIGHT;
    public static final int VRTCAL_WIND_X_ORIG = WIND_BX_X_ORIG+ WIND_BX_WIDTH;
    public static final int VRTCAL_WIND_Y_ORIG = SKEWT_Y_ORIG;
    public static final int VRTCAL_WIND_X_END = VRTCAL_WIND_X_ORIG  + VRTCAL_WIND_WIDTH;
    public static final int VRTCAL_WIND_Y_END = VRTCAL_WIND_Y_ORIG  + VRTCAL_WIND_HEIGHT;
    public static final int OMEGA_X_ORIG = VRTCAL_WIND_X_END;
    public static final int OMEGA_Y_ORIG = SKEWT_Y_ORIG;
    public static final int OMEGA_WIDTH = WITO_PANE_REC_WIDTH/3;
    public static final int OMEGA_HEIGHT = SKEWT_HEIGHT;
    public static final int OMEGA_Y_END = SKEWT_Y_END;
    public static final int OMEGA_MAGNIFICATION_FACTOR = 20;
    public static final int HODO_X_ORIG = 0;
    public static final int HODO_Y_ORIG = 0;//40;
    public static final int HODO_WIDTH = HODO_PANE_REC_WIDTH;
    public static final int HODO_HEIGHT = HODO_PANE_REC_HEIGHT - HODO_Y_ORIG;
    public static final int HODO_X_END = HODO_X_ORIG + HODO_WIDTH;
    public static final float HODO_CENTER_X_ = HODO_X_ORIG + (float)(5.00/12.00) * HODO_WIDTH;
    public static final float HODO_CENTER_Y_ = HODO_Y_ORIG + (float)(1.00/2.00) * HODO_HEIGHT;
    public static final int HODO_COORDINATE_X1 = -50;
    public static final int HODO_COORDINATE_X1_STD = -90;
     public static final int HODO_COORDINATE_Y1 = 75;
    public static final int HODO_COORDINATE_X2 = 70;
    public static final int HODO_COORDINATE_Y2 = -95;
    
    public static final int DATA_TIMELINE_X_ORIG = 0;
    public static final int DATA_TIMELINE_Y_ORIG = 40;
    public static final int DATA_TIMELINE_WIDTH = TIMESTN_PANE_REC_WIDTH/2;
    public static final int COLOR_NOTATION_HEIGHT = 110;
    public static final int DATA_TIMELINE_HEIGHT = TIMESTN_PANE_REC_HEIGHT-DATA_TIMELINE_Y_ORIG-COLOR_NOTATION_HEIGHT;
    public static final int DATA_TIMELINE_X_END = DATA_TIMELINE_X_ORIG+DATA_TIMELINE_WIDTH;
    public static final int DATA_TIMELINE_Y_END = DATA_TIMELINE_Y_ORIG+DATA_TIMELINE_HEIGHT;
    public static final int DATA_TIMELINE_NEXT_PAGE_END_ = DATA_TIMELINE_Y_ORIG+ CHAR_HEIGHT_;
    public static final int STATION_ID_X_ORIG = DATA_TIMELINE_X_END;
    public static final int STATION_ID_Y_ORIG = DATA_TIMELINE_Y_ORIG;
    public static final int STATION_ID_WIDTH = DATA_TIMELINE_WIDTH;
    public static final int STATION_ID_HEIGHT = DATA_TIMELINE_HEIGHT;
    public static final int STATION_ID_X_END = STATION_ID_X_ORIG+STATION_ID_WIDTH;
    public static final int STATION_ID_Y_END = STATION_ID_Y_ORIG+STATION_ID_HEIGHT;
    public static final int COLOR_NOTATION_X_ORIG = DATA_TIMELINE_X_ORIG;
    public static final int COLOR_NOTATION_Y_ORIG = DATA_TIMELINE_Y_END;
    public static final int COLOR_NOTATION_WIDTH = DATA_TIMELINE_WIDTH+STATION_ID_WIDTH;
    public static final int COLOR_NOTATION_X_END = COLOR_NOTATION_X_ORIG+COLOR_NOTATION_WIDTH;
    public static final int COLOR_NOTATION_Y_END = COLOR_NOTATION_Y_ORIG+COLOR_NOTATION_HEIGHT;
    public static final int DATAPANEL1_X_ORIG = 0;
    public static final int DATAPANEL1_Y_ORIG = 0;
    public static final int DATAPANEL1_WIDTH = DATA_PANE_REC_WIDTH/2;
    public static final int DATAPANEL1_HEIGHT = DATA_PANE_REC_HEIGHT;
    public static final int DATAPANEL1_X_END = DATAPANEL1_X_ORIG+DATAPANEL1_WIDTH;
    public static final int DATAPANEL1_Y_END = DATAPANEL1_Y_ORIG+DATAPANEL1_HEIGHT;
    public static final int DATAPANEL2_X_ORIG = DATAPANEL1_X_END;
    public static final int DATAPANEL2_Y_ORIG = DATAPANEL1_Y_ORIG;
    public static final int DATAPANEL2_WIDTH = DATAPANEL1_WIDTH;
    public static final int DATAPANEL2_HEIGHT = DATAPANEL1_HEIGHT;
    public static final int DATAPANEL2_X_END =  DATAPANEL2_X_ORIG+DATAPANEL2_WIDTH;
    public static final int DATAPANEL2_Y_END = DATAPANEL2_Y_ORIG+DATAPANEL2_HEIGHT;
    public static final int INSET_X_ORIG = 0;
    public static final int INSET_Y_ORIG = 0;
    public static final int INSET_WIDTH = INSET_PANE_REC_WIDTH/4;
    public static final int INSET_HEIGHT = INSET_PANE_REC_HEIGHT;
        
    public static final int SRWINDS_X_ORIG = INSET_X_ORIG;
    public static final int SRWINDS_Y_ORIG = INSET_Y_ORIG;
    public static final int SRWINDS_X_END = SRWINDS_X_ORIG+INSET_WIDTH;
    public static final int SRWINDS_Y_END = SRWINDS_Y_ORIG+INSET_HEIGHT;
    
    public static final int STORMSLINKY_X_ORIG = SRWINDS_X_END;
    public static final int STORMSLINKY_Y_ORIG = SRWINDS_Y_ORIG;
    public static final int STORMSLINKY_X_END = STORMSLINKY_X_ORIG+INSET_WIDTH;
    public static final int STORMSLINKY_Y_END = STORMSLINKY_Y_ORIG+INSET_HEIGHT;
    
    public static final int THETAP_X_ORIG = STORMSLINKY_X_END;
    public static final int THETAP_Y_ORIG = STORMSLINKY_Y_ORIG; 
    public static final int THETAP_X_END = THETAP_X_ORIG+INSET_WIDTH;
    public static final int THETAP_Y_END = THETAP_Y_ORIG+INSET_HEIGHT;
    
    //same position as THETAP
    public static final int THETAH_X_ORIG = STORMSLINKY_X_END;
    public static final int THETAH_Y_ORIG = STORMSLINKY_Y_ORIG;
    public static final int THETAH_X_END = THETAH_X_ORIG+INSET_WIDTH;
    public static final int THETAH_Y_END = THETAH_Y_ORIG+INSET_HEIGHT;
    
    public static final int PSBLWATCH_X_ORIG = THETAP_X_END;
    public static final int PSBLWATCH_Y_ORIG = THETAP_Y_ORIG;
    public static final int PSBLWATCH_X_END = PSBLWATCH_X_ORIG+INSET_WIDTH;
    public static final int PSBLWATCH_Y_END = PSBLWATCH_Y_ORIG+INSET_HEIGHT;
    
    //same position as PSBLWATCH
    public static final int SRWINDVTRS_X_ORIG = THETAP_X_END;
    public static final int SRWINDVTRS_Y_ORIG = THETAP_Y_ORIG;
    public static final int SRWINDVTRS_X_END = SRWINDVTRS_X_ORIG+INSET_WIDTH;
    public static final int SRWINDVTRS_Y_END = SRWINDVTRS_Y_ORIG+INSET_HEIGHT;
    
    //public static final String PANE_LEGACY_CFG_STR = "Legacy Configuration (obsoleting)";
    public static final String PANE_DEF_CFG_1_STR = "Default Configuration 1";
    public static final String PANE_DEF_CFG_2_STR = "Default Configuration 2";
    public static final String PANE_SPCWS_CFG_STR = "SPC Wide Screen Configuration";
    public static final String PANE_SIMPLE_D2D_CFG_STR = "D2D Skewt Standard Screen Configuration";
    public static final String[] PANE_CONFIGURATION_NAME = {/*PANE_DEF_CFG_1_STR, PANE_DEF_CFG_2_STR,*/PANE_SPCWS_CFG_STR,PANE_SIMPLE_D2D_CFG_STR};//, PANE_LEGACY_CFG_STR};
     
    //pane width and height ratio to full canvas size
    //  pane default configuration 1  
    // full canvas consists of  left group and right group
    // left group has left top and bottom  groups
    // left top group contains skewt and wito panes
    // left bottom group contains time/stn and insets panes
    // right group has hodo and data panes
    public static final double PANE_DEF_CFG_1_LEFT_GP_WIDTH_RATIO = 0.75;
    public static final double PANE_DEF_CFG_1_LEFT_TOP_GP_HEIGHT_RATIO = 0.8;
    public static final double PANE_DEF_CFG_1_RIGHT_TOP_GP_HEIGHT_RATIO = 0.4;
    public static final double PANE_DEF_CFG_1_SKEWT_WIDTH_RATIO = 0.8;
    public static final double PANE_DEF_CFG_1_SKEWT_HEIGHT_RATIO = PANE_DEF_CFG_1_LEFT_TOP_GP_HEIGHT_RATIO;
    public static final double PANE_DEF_CFG_1_WITO_WIDTH_RATIO = 1-PANE_DEF_CFG_1_SKEWT_WIDTH_RATIO;
    public static final double PANE_DEF_CFG_1_WITO_HEIGHT_RATIO = PANE_DEF_CFG_1_SKEWT_HEIGHT_RATIO;
    public static final double PANE_DEF_CFG_1_HODO_WIDTH_RATIO = 1;
    public static final double PANE_DEF_CFG_1_HODO_HEIGHT_RATIO = PANE_DEF_CFG_1_RIGHT_TOP_GP_HEIGHT_RATIO;
    public static final double PANE_DEF_CFG_1_INSET_WIDTH_RATIO = 0.5;
    public static final double PANE_DEF_CFG_1_INSET_HEIGHT_RATIO = 1-PANE_DEF_CFG_1_SKEWT_HEIGHT_RATIO;
    public static final double PANE_DEF_CFG_1_TIMESTN_WIDTH_RATIO = 1-PANE_DEF_CFG_1_INSET_WIDTH_RATIO;
    public static final double PANE_DEF_CFG_1_TIMESTN_HEIGHT_RATIO = PANE_DEF_CFG_1_INSET_HEIGHT_RATIO;
    public static final double PANE_DEF_CFG_1_DATA_WIDTH_RATIO = 1;
    public static final double PANE_DEF_CFG_1_DATA_HEIGHT_RATIO = 1-PANE_DEF_CFG_1_HODO_HEIGHT_RATIO;
    //  pane default configuration 2
    // full canvas consists of  left group and right group
    // both groups contains top and bottom  groups
    // left top group contains skewt and wito panes
    // left bottom group contains insets panes
    // right top group has hodo and time/stn panes
    // right bottom contains data panes
    public static final double PANE_DEF_CFG_2_LEFT_GP_WIDTH_RATIO = 0.5;
    public static final double PANE_DEF_CFG_2_LEFT_TOP_GP_HEIGHT_RATIO = 0.8;
    public static final double PANE_DEF_CFG_2_RIGHT_TOP_GP_HEIGHT_RATIO = 0.7;
    public static final double PANE_DEF_CFG_2_SKEWT_WIDTH_RATIO = 0.85;
    public static final double PANE_DEF_CFG_2_SKEWT_HEIGHT_RATIO = PANE_DEF_CFG_2_LEFT_TOP_GP_HEIGHT_RATIO;
    public static final double PANE_DEF_CFG_2_WITO_WIDTH_RATIO = 1-PANE_DEF_CFG_2_SKEWT_WIDTH_RATIO;
    public static final double PANE_DEF_CFG_2_WITO_HEIGHT_RATIO = PANE_DEF_CFG_2_SKEWT_HEIGHT_RATIO;
    public static final double PANE_DEF_CFG_2_HODO_WIDTH_RATIO = 0.65;
    public static final double PANE_DEF_CFG_2_HODO_HEIGHT_RATIO = PANE_DEF_CFG_2_RIGHT_TOP_GP_HEIGHT_RATIO;
    public static final double PANE_DEF_CFG_2_INSET_WIDTH_RATIO = 1;
    public static final double PANE_DEF_CFG_2_INSET_HEIGHT_RATIO = 1-PANE_DEF_CFG_2_SKEWT_HEIGHT_RATIO;
    public static final double PANE_DEF_CFG_2_TIMESTN_WIDTH_RATIO = 0.35;
    public static final double PANE_DEF_CFG_2_TIMESTN_HEIGHT_RATIO = PANE_DEF_CFG_2_HODO_HEIGHT_RATIO;
    public static final double PANE_DEF_CFG_2_DATA_WIDTH_RATIO = 1;
    public static final double PANE_DEF_CFG_2_DATA_HEIGHT_RATIO = 1-PANE_DEF_CFG_2_HODO_HEIGHT_RATIO;
    
    //  pane SPC wide screen configuration
    public static final double PANE_SPCWS_CFG_TOP_GP_HEIGHT_RATIO = 0.714; //5/7
    public static final double PANE_SPCWS_CFG_BOT_GP_HEIGHT_RATIO = 1-PANE_SPCWS_CFG_TOP_GP_HEIGHT_RATIO;
    //skewt, wito, hodo/inset (hodo stack on inset) panes are located on top group 
    public static final double PANE_SPCWS_CFG_SKEWT_WIDTH_RATIO = 0.4938;//5/10.125;
    public static final double PANE_SPCWS_CFG_SKEWT_HEIGHT_RATIO = 1;
    public static final double PANE_SPCWS_CFG_WITO_WIDTH_RATIO = 0.1111;//1.125/10.125;
    public static final double PANE_SPCWS_CFG_WITO_HEIGHT_RATIO = 1;
    public static final double PANE_SPCWS_CFG_HODO_WIDTH_RATIO = 1- PANE_SPCWS_CFG_SKEWT_WIDTH_RATIO-PANE_SPCWS_CFG_WITO_WIDTH_RATIO;
    public static final double PANE_SPCWS_CFG_HODO_HEIGHT_RATIO = 0.825;//4.125/5; 
    public static final double PANE_SPCWS_CFG_INSET_WIDTH_RATIO = PANE_SPCWS_CFG_HODO_WIDTH_RATIO;
    public static final double PANE_SPCWS_CFG_INSET_HEIGHT_RATIO = 1- PANE_SPCWS_CFG_HODO_HEIGHT_RATIO;
    //data and other graphs panes are located on bottom group
    public static final double PANE_SPCWS_CFG_DATA_WIDTH_RATIO = 0.5;
    public static final double PANE_SPCWS_CFG_DATA_HEIGHT_RATIO = 1;
    public static final double PANE_SPCWS_CFG_SPC_GRAPHS_WIDTH_RATIO = 0.5;
    public static final double PANE_SPCWS_CFG_SPC_GRAPHS_HEIGHT_RATIO = 1;

    //simple D2D pane configuration. 
    // full canvas consists of  top and bottom group
    public static final double PANE_SIMPLE_D2D_CFG_TOP_GP_HEIGHT_RATIO = 0.71;
    public static final double PANE_SIMPLE_D2D_CFG_BOT_GP_HEIGHT_RATIO = 1-PANE_SIMPLE_D2D_CFG_TOP_GP_HEIGHT_RATIO;
    //  top group contains left (skewt) and right groups (time/stn stack on future pane) 
    public static final double PANE_SIMPLE_D2D_CFG_SKEWT_WIDTH_RATIO = 0.75;
    public static final double PANE_SIMPLE_D2D_CFG_SKEWT_HEIGHT_RATIO = 1;
    public static final double PANE_SIMPLE_D2D_CFG_TIMESTN_WIDTH_RATIO = 1-PANE_SIMPLE_D2D_CFG_SKEWT_WIDTH_RATIO;
    public static final double PANE_SIMPLE_D2D_CFG_TIMESTN_HEIGHT_RATIO = 0.5;
    public static final double PANE_SIMPLE_D2D_CFG_FUTURE_WIDTH_RATIO = PANE_SIMPLE_D2D_CFG_TIMESTN_WIDTH_RATIO;
    public static final double PANE_SIMPLE_D2D_CFG_FUTURE_HEIGHT_RATIO = 1-PANE_SIMPLE_D2D_CFG_TIMESTN_HEIGHT_RATIO;
    // bottom group has hodo on left and data pane on right
    public static final double PANE_SIMPLE_D2D_CFG_HODO_WIDTH_RATIO = 0.34;
    public static final double PANE_SIMPLE_D2D_CFG_HODO_HEIGHT_RATIO = 1; 
    public static final double PANE_SIMPLE_D2D_CFG_DATA_WIDTH_RATIO = 1-PANE_SIMPLE_D2D_CFG_HODO_WIDTH_RATIO;
    public static final double PANE_SIMPLE_D2D_CFG_DATA_HEIGHT_RATIO = 1;
    /***
     * 
     * MULTIPLE PANES IMPLEMENTATIONS end
     * 
     * 
     ****/
    
    
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
	
	public static int  SKEWT_EDIT_MODE_EDITPOINT = 0;
	public static int  SKEWT_EDIT_MODE_MOVELINE = 1;
	public static int  SKEWT_EDIT_MODE_MODIFYRANGE = 2;
	
	public static String getNlistFile() {
		return NcPathManager.getInstance().getStaticFile( 
				  NcPathConstants.NSHARP_NLIST_FILE ).getAbsolutePath();
    }
	public static String getSupFile() {
		return NcPathManager.getInstance().getStaticFile( 
				  NcPathConstants.NSHARP_SUP_FILE ).getAbsolutePath();
    }
	
	//Line configuration. Line name listing order in this array should be in order with constant defined below it.
	public static String[] lineNameArray= {"Temperature", "Dew Point", "Parcel Tv","Parcel","DCAPE","Virtual Temp","Wetbulb","Wind Barb","Overlay 1", "Overlay 2", "Compare 1", "Compare 2","Compare 3","Compare 4","Compare 5","Compare 6","Compare 7","Compare 8","Compare 9","Compare 10",
											"Icing RH", "Icing Temp", "Icing EPI", "Turbulence Ln", "Turbulence WindShear"};
	public static int LINE_TEMP = 0;
	public static int LINE_DEWP = LINE_TEMP+1;
	public static int LINE_PARCEL_TV = LINE_DEWP+1;
	public static int LINE_PARCEL = LINE_PARCEL_TV+1;
	public static int LINE_DCAPE =LINE_PARCEL+1;
	public static int LINE_VIRTUAL_TEMP =LINE_DCAPE+1;
	public static int LINE_WETBULB = LINE_VIRTUAL_TEMP+1;
	public static int LINE_WIND_BARB = LINE_WETBULB+1;
	public static int LINE_OVERLAY1 = LINE_WIND_BARB+1;
	public static int LINE_OVERLAY2= LINE_OVERLAY1+1;
	public static int LINE_COMP1 = LINE_OVERLAY2+1;
	public static int LINE_COMP2 = LINE_COMP1+1;
	public static int LINE_COMP3 = LINE_COMP2+1;
	public static int LINE_COMP4 = LINE_COMP3+1;
	public static int LINE_COMP5 = LINE_COMP4+1;
	public static int LINE_COMP6 = LINE_COMP5+1;
	public static int LINE_COMP7 = LINE_COMP6+1;
	public static int LINE_COMP8 = LINE_COMP7+1;
	public static int LINE_COMP9 = LINE_COMP8+1;
	public static int LINE_COMP10 = LINE_COMP9+1;
	public static int LINE_ICING_RH = LINE_COMP10+1;
	public static int LINE_ICING_TEMP= LINE_ICING_RH+1;
	public static int LINE_ICING_EPI= LINE_ICING_TEMP+1;
	public static int LINE_TURBULENCE_LN = LINE_ICING_EPI+1;
	public static int LINE_TURBULENCE_WS = LINE_TURBULENCE_LN+1;
	
	//defaultLineProperty should be listed in sync with lineNameArray for each line
	public static NsharpLineProperty[] defaultLineProperty = 
	{
		new NsharpLineProperty(LineStyle.SOLID, 2,color_red ),
		new NsharpLineProperty(LineStyle.SOLID, 2,color_green ),
		new NsharpLineProperty(LineStyle.SHORT_DASHED, 1,color_white ),
		new NsharpLineProperty(LineStyle.DOTS, 1,color_darkorange ),
		new NsharpLineProperty(LineStyle.DOTS, 2,color_white ),
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
	//data page name and its plotting function key definitions
	public final static int PAGE_SUMMARY1 = 1;
	public final static int PAGE_SUMMARY2 = 2;
	public final static int PAGE_PARCEL_DATA = 3;
	public final static int PAGE_THERMODYNAMIC_DATA = 4;
	public final static int PAGE_OPC_DATA = 5;
	public final static int PAGE_MIXING_HEIGHT = 6;
	public final static int PAGE_STORM_RELATIVE = 7;
	public final static int PAGE_MEAN_WIND = 8;
	public final static int PAGE_CONVECTIVE_INITIATION = 9;
	public final static int PAGE_SEVERE_POTENTIAL = 10;
	public final static int PAGE_MAX_NUMBER = PAGE_SEVERE_POTENTIAL;
	public static String[] PAGE_NAME_ARRAY = {
		"", //a dummy one
		"Summary 1 Page",
		"Summary 2 Page",
		"Parcel Data Page",
		"Thermodynamic Data Page",
		"Opc Low Level Stability Page",
		"Mixing Height Page",
		"Storm Relative Page",
		"Mean Wind Page",
		"Convective Initiation Page",
		"Severe Potential Page"
	};
	public enum State {
		CURRENT, ACTIVE, INACTIVE,NOTAVAIL ,OVERLAY, AVAIL//was , DISABLED
	}
	public enum SPCGraph {
		EBS, STP, SHIP, WINTER, FIRE, HAIL, SARS
	}
	
}
