package gov.noaa.nws.ncep.gempak.parameters.hilo;

import org.eclipse.swt.graphics.RGB;

/** <p>
*<pre>
* SOFTWARE HISTORY
* Date          Ticket#     Engineer     Description
* ------------ ---------- ----------- --------------------------
* Oct 19,2010   323         X. Guo       Initial Creation
* Nov 09,2010               X. Guo       Change double to float
*                                         
* </pre>
* @author xguo
* @version 1
* @see $GEMPAK/help/hlx/hilo.hl2
*/

public class HILOBuilder {
	//Color High/Low
	private RGB [] colors;   			//default 0 -- black
	
	//Input Color 
	private int [] inputColors;        //default 32
	
	//Symbols High/Low
	private String[] symbols;           //default 'H'/'L'
	
	//Symbol Marker Number High/Low
	private int [] symbolMarkerNums;    //default -9999
	
	//Symbol Type High/Low                
	private int [] symbolTypes;         //default 1
	
	//Flag to plot High/Low values
	private boolean [] symbolPlotValues;//default false
	
	//Number of High/Low decidal places
	private int [] iprecn;              //default 0
	
	//Range High/low Minval/Maxval
	private float [][] range;          //default all data
	
	//Radius
	private int radius;                 //default 3
	
	//Count High/Low
	private int [] counts ;             //default 20
	
	//Interpolation flag
	private boolean interp;              //default NO
	
	private int symbolMissed = -9999;
	
	private static final int HILO_NUM = 2;
	
	private static final float HILO_MIN_RANGE_VAL = -3.4E+38F;
	
	private static final float HILO_MAX_RANGE_VAL = 3.4E+38F;
	
	/**The default constructor <code>HILOBuilder</code> assigns default values to the state variables.
     **/
	protected  HILOBuilder () {
		//Init colors High/Low
		colors = new RGB[HILO_NUM];
		
		//Init Input colors
		inputColors = new int[HILO_NUM];
		
		//Init Symbols High/Low
		symbols = new String[HILO_NUM];
		
		//Init Symbol Marker Number High/Low
		symbolMarkerNums = new int[HILO_NUM];
		
		//Init Symbol Type High/Low
		symbolTypes = new int[HILO_NUM];
		
		//Init Symbol plot Value flag
		symbolPlotValues = new boolean[HILO_NUM];
		
		//Init number of decimal places
		iprecn = new int[HILO_NUM];
		
		//Init Symbol Range High/low Maxval/Minval
		range = new float[HILO_NUM][HILO_NUM];
		
		//Init symbol count High/Low
		counts = new int[HILO_NUM];
		
		setAllHiLoDefaultValues ();
	}
	
	/**
	 * Set all default values
	 */
	
	public void setAllHiLoDefaultValues () {
		setDefaultColors ();
		setInputGempakDefaultColors ();
		setSymbolDefaultValues ();
		setMarkerNumberDefaultValues ();
		setSymbolTypeDefaultValues ();
		setSymbolPlotDefaultValues ();
		setPrecisionDefaultValues ();
		setRangeDefaultValues ();
		setSymbolCountDefaultValues ();
		setRadiusDefaultValue ();
		setInterpDefaultValue ();
	}
	
	/**
	 * Set high/low color default values
	 * @return NULL
	 */
	private void setDefaultColors () {
		for ( int ii = 0 ; ii < HILO_NUM ; ii ++ ) {
			colors[ii] = new RGB(0, 0, 0);
		}
	}
	
	/**
	 * Set input Gempak high/low color default values
	 * @return
	 */
	private void setInputGempakDefaultColors () {
		for ( int ii = 0 ; ii < HILO_NUM ; ii ++ ) {
			inputColors[ii] = 32;
		}
	}
	
	/**
	 * Set High/low Sysmbol default values
	 * @return
	 */
	private void setSymbolDefaultValues () {
		symbols[0] = "H";
		symbols[1] = "L";
	}
	
	/**
	 * Set High/low marker number default values
	 * @return
	 */
	private void setMarkerNumberDefaultValues () {
		for ( int ii = 0 ; ii < HILO_NUM ; ii ++ ) {
			symbolMarkerNums[ii] = symbolMissed;
		}
	}
	
	/**
	 * Set high/low symbol type default values
	 * @return
	 */
	private void setSymbolTypeDefaultValues () {
		for ( int ii = 0 ; ii < HILO_NUM ; ii ++ ) {
			symbolTypes[ii] = 1;
		}
	}
	
	/**
	 * Set high/low symbol plot default values
	 * @return
	 */
	private void setSymbolPlotDefaultValues () {
		for ( int ii = 0 ; ii < HILO_NUM ; ii ++ ) {
			symbolPlotValues[ii] = false;
		}
	}
	
	/**
	 * Set high/low precision default values
	 * @return
	 */
	private void setPrecisionDefaultValues () {
		for ( int ii = 0 ; ii < HILO_NUM ; ii ++ ) {
			iprecn[ii] = 0;
		}
	}
	
	/**
	 * Set high/low range maxval/minval default values
	 * @return
	 */
	private void setRangeDefaultValues () {
		for ( int ii = 0 ; ii < HILO_NUM ; ii ++ ) {
			for ( int jj = 0 ; jj < HILO_NUM ; jj ++ ) {
				if ( jj == 0 ) this.range[ii][jj] = HILO_MIN_RANGE_VAL;
				else this.range[ii][jj] = HILO_MAX_RANGE_VAL;
			}
		}
	}
	
	/**
	 * Set high/low symbol count default value
	 * @return
	 */
	private void setSymbolCountDefaultValues () {
		for ( int ii = 0 ; ii < HILO_NUM ; ii ++ ) {
			counts[ii] = 20;
		}
	}
	
	/**
	 * Set Radius default value
	 * @return
	 */
	private void setRadiusDefaultValue () {
		radius = 3;
	}
	
	/**
	 * Set interpolation default value
	 * @return
	 */
	private void setInterpDefaultValue () {
		this.interp = false;
	}
	/*
	 * get color high
	 */
	public RGB getColorHi () {
		return colors[0];
	}
	
	/*
	 * set color high
	 */
	public void setColorHi ( RGB col) {
		this.colors[0] = col;
	}
	
	/*
	 * get color low
	 */
	public RGB getColorLo () {
		return colors[1];
	}

	/*
	 * set color low
	 */
	public void setColorLo ( RGB col ) {
		this.colors[1] = col;
	}
	
	/*
	 * get input color high
	 */
	public int getInputColorHi () {
		return inputColors[0];
	}
	
	/*
	 * set input color high
	 */
	public void setInputColorHi ( int col ) {
		this.inputColors[0] = col;
	}
	
	/*
	 * get color low
	 */
	public int getInputColorLo () {
		return inputColors[1];
	}
	
	/*
	 * set color low
	 */
	public void setInputColorLo (int col) {
		this.inputColors[1] = col;
	}
	
	/*
	 * get symbol High
	 */
	public String getSymbolHi () {
		return symbols[0];
	}
	
	/*
	 * set symbol High
	 */
	public void setSymbolHi ( String symbolHi ) {
		this.symbols[0] = symbolHi;
	}
	
	/*
	 * get symbol High marker number
	 */
	public int getSymbolHiMarkerNumber () {
		return symbolMarkerNums[0];
	}
	
	/*
	 * set symbol High marker number
	 */
	public void setSymbolHiMarkerNumber ( int mkNum ) {
		this.symbolMarkerNums[0] = mkNum;
	}
	
	/*
	 * get symbol High Type
	 */
	public int getSymbolHiType () {
		return symbolTypes[0];
	}
	
	/*
	 * set symbol High Type
	 */
	public void setSymbolHiType ( int symbolType ) {
		this.symbolTypes[0] = symbolType;
	}
	
	/*
	 * get Symbol High Plot Value
	 */
	public boolean getSymbolHiPlotValue () {
		return symbolPlotValues[0];
	}
	
	/*
	 * set Symbol High Plot Value
	 */
	public void setSymbolHiPlotValue ( boolean symbolPlVal ) {
		this.symbolPlotValues[0] = symbolPlVal;
	}
	
	/*
	 * get Symbol High number of decimal place
	 */
	public int getSymbolHiNumOfDecPls () {
		return iprecn[0];
	}
	
	/*
	 * set Symbol High number of decimal place
	 */
	public void setSymbolHiNumOfDecPls ( int symbolNOD ) {
		if ( symbolNOD < 0 ) symbolNOD = 0;
		if ( symbolNOD > 9 ) symbolNOD = 9;
		this.iprecn[0] = symbolNOD;
	}
	
	/*
	 * get symbol Low
	 */
	public String getSymbolLo () {
		return symbols[1];
	}

	/*
	 * set symbol Low
	 */
	public void setSymbolLo ( String symbolLo ) {
		this.symbols[1] = symbolLo;
	}
	
	/*
	 * get symbol Low marker number
	 */
	public int getSymbolLoMarkerNumber () {
		return symbolMarkerNums[1];
	}
	
	/*
	 * set symbol Low marker number
	 */
	public void setSymbolLoMarkerNumber ( int mkNum ) {
		this.symbolMarkerNums[1] = mkNum;
	}
	
	/*
	 * get symbol Low Type
	 */
	public int getSymbolLoType () {
		return symbolTypes[1];
	}
	
	/*
	 * set symbol Low Type
	 */
	public void setSymbolLoType ( int symbolType ) {
		this.symbolTypes[1] = symbolType;
	}
	
	/*
	 * get Symbol Low Plot Value
	 */
	public boolean getSymbolLoPlotValue () {
		return symbolPlotValues[1];
	}

	/*
	 * set Symbol Low Plot Value
	 */
	public void setSymbolLoPlotValue ( boolean symbolPlVal ) {
		this.symbolPlotValues[1] = symbolPlVal;
	}
	
	/*
	 * get Symbol Low number of decimal place
	 */
	public int getSymbolLoNumOfDecPls () {
		return iprecn[1];
	}
	
	/*
	 * set Symbol Low number of decimal place
	 */
	public void setSymbolLoNumOfDecPls ( int symbolNOD ) {
		if ( symbolNOD < 0 ) symbolNOD = 0;
		if ( symbolNOD > 9 ) symbolNOD = 9;
		this.iprecn[1] = symbolNOD;
	}
	
	/*
	 * get Range High Maxval
	 */
	public float getRangeHiMaxval () {
		return range[0][1];
	}
	
	/*
	 * set Range High Maxval
	 */
	public void setRangeHiMaxval ( float val ) {
		this.range[0][1] = val;
	}
	
	/*
	 * get Range High Minval
	 */
	public float getRangeHiMinval () {
		return range[0][0];
	}
	
	/*
	 * set Range High Minval
	 */
	public void setRangeHiMinval ( float val ) {
		this.range[0][0] = val;
	}
	
	/*
	 * get Range Low Maxval
	 */
	public float getRangeLoMaxval () {
		return range[1][1];
	}
	
	/*
	 * set Range Low Maxval
	 */
	public void setRangeLoMaxval ( float val ) {
		this.range[1][1] = val;
	}
	
	/*
	 * get Range Low Minval
	 */
	public float getRangeLoMinval () {
		return range[1][0];
	}
	
	/*
	 * set Range Low Minval
	 */
	public void setRangeLoMinval ( float val ) {
		this.range[1][0] = val;
	}
	
	/*
	 * get Radius
	 */
	public int getRadius () {
		return radius;
	}
	
	/*
	 * set Radius
	 */
	public void setRadius ( int radius ) {
		this.radius = radius ;
	}
	
	/*
	 * get Count High
	 */
	public int getCountHi () {
		return counts[0];
	}
	
	/*
	 * set Count High
	 */
	public void setCountHi ( int count ) {
		this.counts[0] = count ;
	}
	
	/*
	 * get Count Low
	 */
	public int getCountLo () {
		return counts[1];
	}
	
	/*
	 * set Count Low
	 */
	public void setCountLo ( int count) {
		this.counts[1] = count ;
	}
	
	/*
	 * get Interp
	 */
	public boolean getInterp () {
		return interp;
	}
	
	/*
	 * set Interp
	 */
	public void setInterp ( boolean interp ) {
		this.interp = interp;
	}
}
