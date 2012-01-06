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
package com.raytheon.viz.hydro.timeseries.util;

import java.io.Serializable;
import java.util.Date;

import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Object holds the metadata for the individual traces.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 25, 2008	1194		mpduff	Initial creation
 * Mar 04, 2011 7758        Jingtao added isSelectionCheckOn flag for 
 * 							pop up menu
 * Apr 05, 2011 8732        jpiatt  Added product_id.
 * June,1, 2011 9499        djingtao change setDur()
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class TraceData implements Serializable {
	private static final long serialVersionUID = -4345330013536912094L;
	private static final String CODES = "IUCJHBTFQAKLDWMYZSRPX";
	private static final int[] INT_CODE_ARRAY = { 0, 1, 15, 30, 1001, 1002,
			1003, 1004, 1006, 1008, 1012, 1018, 2001, 2007, 3001, 4001, 5000,
			5001, 5002, 5004, 5005 };

	private int npts = 0;
	private int traceColor;

	private double ymin;
	private double ymax;
	private double value_ymin;
	private double value_ymax;

	private Date xmin;
	private Date xmax;
	private Date basistime;
	private Date productTime;

	private TimeSeriesPoint[] tsData = new TimeSeriesPoint[HydroConstants.MAX_POINTS];
	private TimeSeriesPoint[] previousTsData;
	private TimeSeriesPoint[] zoomedTsData;

	private int[] lineData;

	private String name = null;
	private String lid = null;
	private String pe = null;
	private String dur = "0";
	private String ts = null;
	private String extremum = null;
	private String cdur = null;
	private String product_id = null;

	private String colorName = null;

	private boolean isForecast = false;

	private String label = null;

	private boolean isSelected = false;

	private boolean traceOn = true;

	private boolean traceValid = true;

	private boolean selectionCheckOn = true;

	public TraceData() {

	}

	/**
	 * Copy constructor.
	 * 
	 * @param data
	 */
	public TraceData(TraceData data) {
		basistime = data.basistime;
		cdur = data.cdur;
		product_id = data.product_id;
		colorName = data.colorName;
		dur = data.dur;
		extremum = data.extremum;
		isForecast = data.isForecast;
		isSelected = data.isSelected;
		label = data.label;
		lid = data.lid;
		lineData = data.lineData;
		name = data.name;
		npts = data.npts;
		pe = data.pe;
		previousTsData = data.previousTsData;
		productTime = data.productTime;
		traceColor = data.traceColor;
		traceOn = data.traceOn;
		ts = data.ts;
		tsData = data.tsData;
		value_ymax = data.value_ymax;
		value_ymin = data.value_ymin;
		xmax = data.xmax;
		xmin = data.xmin;
		ymax = data.ymax;
		ymin = data.ymin;
		zoomedTsData = data.zoomedTsData;
		selectionCheckOn = data.selectionCheckOn;
	}

	/**
	 * 
	 * @return pop up check box selection
	 */
	public boolean isSelectionCheckOn() {
		return selectionCheckOn;
	}

	/**
	 * 
	 * @param selectionCheckOn
	 */
	public void setSelectionCheckOn(boolean selectionCheckOn) {
		this.selectionCheckOn = selectionCheckOn;
	}

	/**
	 * @return the npts
	 */
	public int getNpts() {
		if ((npts == 0) || (npts == -9999)) {
			return tsData.length;
		}
		return npts;
		// return tsData.length;
	}

	/**
	 * @param npts
	 *            the npts to set
	 */
	public void setNpts(int npts) {
		this.npts = npts;
	}

	/**
	 * @return the traceColor
	 */
	public int getTraceColor() {
		return traceColor;
	}

	/**
	 * @param traceColor
	 *            the traceColor to set
	 */
	public void setTraceColor(int traceColor) {
		this.traceColor = traceColor;
	}

	/**
	 * @return the ymin
	 */
	public double getYmin() {
		return ymin;
	}

	/**
	 * @param ymin
	 *            the ymin to set
	 */
	public void setYmin(double ymin) {
		this.ymin = ymin;
	}

	/**
	 * @return the ymax
	 */
	public double getYmax() {
		return ymax;
	}

	/**
	 * @param ymax
	 *            the ymax to set
	 */
	public void setYmax(double ymax) {
		this.ymax = ymax;
	}

	/**
	 * @return the value_ymin
	 */
	public double getValue_ymin() {
		return value_ymin;
	}

	/**
	 * @param value_ymin
	 *            the value_ymin to set
	 */
	public void setValue_ymin(double value_ymin) {
		this.value_ymin = value_ymin;
	}

	/**
	 * @return the value_ymax
	 */
	public double getValue_ymax() {
		return value_ymax;
	}

	/**
	 * @param value_ymax
	 *            the value_ymax to set
	 */
	public void setValue_ymax(double value_ymax) {
		this.value_ymax = value_ymax;
	}

	/**
	 * @return the xmin
	 */
	public Date getXmin() {
		return xmin;
	}

	/**
	 * @param xmin
	 *            the xmin to set
	 */
	public void setXmin(Date xmin) {
		this.xmin = xmin;
	}

	/**
	 * @return the xmax
	 */
	public Date getXmax() {
		return xmax;
	}

	/**
	 * @param xmax
	 *            the xmax to set
	 */
	public void setXmax(Date xmax) {
		this.xmax = xmax;
	}

	/**
	 * @return the basistime
	 */
	public Date getBasistime() {
		return basistime;
	}

	/**
	 * @param basistime
	 *            the basistime to set
	 */
	public void setBasistime(Date basistime) {
		this.basistime = basistime;
	}

	/**
	 * @return the tsData
	 */
	public TimeSeriesPoint[] getTsData() {
		return tsData;
	}

	/**
	 * @param tsData
	 *            the tsData to set
	 */
	public void setTsData(TimeSeriesPoint[] tsData) {
		this.tsData = tsData;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the lid
	 */
	public String getLid() {
		return lid;
	}

	/**
	 * @param lid
	 *            the lid to set
	 */
	public void setLid(String lid) {
		this.lid = lid;
	}

	/**
	 * @return the pe
	 */
	public String getPe() {
		return pe;
	}

	/**
	 * @param pe
	 *            the pe to set
	 */
	public void setPe(String pe) {
		this.pe = pe;
	}

	/**
	 * @return the dur
	 */
	public String getDur() {
		return dur;
	}

	/**
	 * @param dur
	 *            the dur to set
	 */
	public void setDur(String dur) {
		if (dur.matches("\\d*")) {
			this.dur = dur;
		} else {
			// 21 characters that I care about
			char[] charCodeArray = CODES.toCharArray();

			for (int i = 0; i < charCodeArray.length; i++) {
				if (dur.equalsIgnoreCase(String.valueOf(charCodeArray[i]))) {
					dur = String.valueOf(INT_CODE_ARRAY[i]);
					this.dur = dur;
					break;
				}
			}
		}
	}

	/**
	 * @return the ts
	 */
	public String getTs() {
		return ts;
	}

	/**
	 * @param ts
	 *            the ts to set
	 */
	public void setTs(String ts) {
		this.ts = ts;
	}

	/**
	 * @return the product_id
	 */
	public String getProductId() {
		return product_id;
	}

	/**
	 * @param product_id
	 *            the product_id to set
	 */
	public void setProductId(String product_id) {
		this.product_id = product_id;
	}

	/**
	 * @return the extremum
	 */
	public String getExtremum() {
		return extremum;
	}

	/**
	 * @param extremum
	 *            the extremum to set
	 */
	public void setExtremum(String extremum) {
		this.extremum = extremum;
	}

	/**
	 * @return the cdur
	 */
	public String getCdur() {
		return cdur;
	}

	/**
	 * @param cdur
	 *            the cdur to set
	 */
	public void setCdur(String cdur) {
		this.cdur = cdur;
	}

	/**
	 * @return the colorName
	 */
	public String getColorName() {
		return colorName;
	}

	/**
	 * @param colorName
	 *            the colorName to set
	 */
	public void setColorName(String colorName) {
		this.colorName = colorName.trim();
	}

	/**
	 * @return the isForecast
	 */
	public boolean isForecast() {
		return isForecast;
	}

	/**
	 * @param isForecast
	 *            the isForecast to set
	 */
	public void setForecast(boolean isForecast) {
		this.isForecast = isForecast;
	}

	/**
	 * Set the PEDTSE values from a single string
	 * 
	 * @param pc
	 *            the PEDTSE values as a String
	 */
	public void setPc(String pc) {
		setPe(pc.substring(0, 2));
		setDur(pc.substring(2, 3));
		setTs(pc.substring(3, 5));
		setExtremum(pc.substring(5));

		// Set forecast if ts is F or C
		this.setForecast(false);
		if (getTs().startsWith("F") || getTs().startsWith("f")
				|| getTs().startsWith("C") || getTs().startsWith("c")) {
			this.setForecast(true);
		}
	}

	/**
	 * @return the label
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * @param label
	 *            the label to set
	 */
	public void setLabel(String label) {
		this.label = label;
	}

	/**
	 * @return the isSelected
	 */
	public boolean isSelected() {
		return isSelected;
	}

	/**
	 * @param isSelected
	 *            the isSelected to set
	 */
	public void setSelected(boolean isSelected) {
		this.isSelected = isSelected;
	}

	/**
	 * @return the lineData
	 */
	public int[] getLineData() {
		return lineData;
	}

	/**
	 * @param lineData
	 *            the lineData to set
	 */
	public void setLineData(int[] lineData) {
		this.lineData = lineData;
	}

	/**
	 * @return the previousLineData
	 */
	public TimeSeriesPoint[] getPreviousTsData() {
		return previousTsData;
	}

	/**
	 * @param previousLineData
	 *            the previousLineData to set
	 */
	public void setPreviousTsData(TimeSeriesPoint[] previousTsData) {
		this.previousTsData = previousTsData;
	}

	/**
	 * @return the zoomedTsData
	 */
	public TimeSeriesPoint[] getZoomedTsData() {
		return zoomedTsData;
	}

	/**
	 * @param zoomedTsData
	 *            the zoomedTsData to set
	 */
	public void setZoomedTsData(TimeSeriesPoint[] zoomedTsData) {
		this.zoomedTsData = zoomedTsData;
	}

	/**
	 * @return the traceOn
	 */
	public boolean isTraceOn() {
		return traceOn;
	}

	/**
	 * @param traceOn
	 *            the traceOn to set
	 */
	public void setTraceOn(boolean traceOn) {
		this.traceOn = traceOn;
	}

	/**
	 * @return the traceValid
	 */
	public boolean isTraceValid() {
		return traceValid;
	}

	/**
	 * @param traceValid
	 *            the traceValid to set
	 */
	public void setTraceValid(boolean traceValid) {
		this.traceValid = traceValid;
	}

	/**
	 * @return the productTime
	 */
	public Date getProductTime() {
		return productTime;
	}

	/**
	 * @param productTime
	 *            the productTime to set
	 */
	public void setProductTime(Date productTime) {
		this.productTime = productTime;
	}
}
