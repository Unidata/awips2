/*
 * IntlSigmetResourceData
 * 
 * Date created 13 April 2010
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.intlsig.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.EditElement;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscResourceAttr;
import gov.noaa.nws.ncep.viz.resources.misc.IMiscResourceData.MiscRscAttrs;

/**
 * IntlSigmetResourceData - Displays International SIGMET data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 13-Apr-2010    244      Archana  Initial creation.
 * 09-Jun-2010    244      Archana  Renamed flightLevelOrPresMxWndEnable
 *                                  to flightLevelEnable
 * </pre>
 * 
 * @author Archana
 * @version 1.0
 */
@SuppressWarnings("unused")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "IntlSigmetResourceData")
public class IntlSigmetResourceData extends
		AbstractNatlCntrsRequestableResourceData implements IMiscResourceData,
		INatlCntrsResourceData {

	@XmlElement
	protected String legendName;

	// --------------- Thunderstorm Attributes-----------------------
	@XmlElement
	protected boolean thunderstormEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB thunderstormColor;

	@XmlElement
	protected int thunderstormLineWidth;

	@XmlElement
	protected int thunderstormSymbolWidth;

	@XmlElement
	protected float thunderstormSymbolSize;

	// --------------- Turbulence Attributes----------------------

	@XmlElement
	protected boolean turbulenceEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB turbulenceColor;

	@XmlElement
	protected int turbulenceLineWidth;

	@XmlElement
	protected int turbulenceSymbolWidth;

	@XmlElement
	protected float turbulenceSymbolSize;

	// -----------------Hurricane Attributes---------------------------
	@XmlElement
	protected boolean hurricaneEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB hurricaneColor;

	@XmlElement
	protected int hurricaneSymbolWidth;

	@XmlElement
	protected float hurricaneSymbolSize;

	// --------------Tropical Storm Attributes-----------------------------
	@XmlElement
	protected boolean tropicalStormEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB tropicalStormColor;

	@XmlElement
	protected int tropicalStormSymbolWidth;

	@XmlElement
	protected float tropicalStormSymbolSize;

	// ---------------Tropical Depression Attributes------------------------

	@XmlElement
	protected boolean tropicalDepressionEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB tropicalDepressionColor;

	@XmlElement
	protected int tropicalDepressionSymbolWidth;

	@XmlElement
	protected float tropicalDepressionSymbolSize;

	// -------------- Volcanic Ash Cloud Attributes-----------------------

	@XmlElement
	protected boolean volcanicAshCloudEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB volcanicAshCloudColor;

	@XmlElement
	protected int volcanicAshCloudLineWidth;

	@XmlElement
	protected int volcanicAshCloudSymbolWidth;

	@XmlElement
	protected float volcanicAshCloudSymbolSize;

	// ---------------Mountain Wave Attributes--------------------------
	@XmlElement
	protected boolean mountainWaveEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB mountainWaveColor;

	@XmlElement
	protected int mountainWaveLineWidth;

	@XmlElement
	protected int mountainWaveSymbolWidth;

	@XmlElement
	protected float mountainWaveSymbolSize;

	// --------------- Tropical Cyclone Attributes----------------------
	@XmlElement
	protected boolean tropicalCycloneEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB tropicalCycloneColor;

	@XmlElement
	protected int tropicalCycloneSymbolWidth;

	@XmlElement
	protected float tropicalCycloneSymbolSize;

	// --------------- Squall Line Attributes-------------------

	@XmlElement
	protected boolean squallLineEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB squallLineColor;

	@XmlElement
	protected int squallLineWidth;

	@XmlElement
	protected int squallLineSymbolWidth;

	@XmlElement
	protected float squallLineSymbolSize;

	// ---------------- CAT Attributes----------------

	@XmlElement
	protected boolean catEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB catColor;

	@XmlElement
	protected int catLineWidth;

	@XmlElement
	protected int catSymbolWidth;

	@XmlElement
	protected float catSymbolSize;

	// ---------------- Icing Attributes -------------------

	@XmlElement
	protected boolean icingEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB icingColor;

	@XmlElement
	protected int icingLineWidth;

	@XmlElement
	protected int icingSymbolWidth;

	@XmlElement
	protected float icingSymbolSize;

	// ----------------Hail Attributes----------------------

	@XmlElement
	protected boolean hailEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB hailColor;

	@XmlElement
	protected int hailLineWidth;

	@XmlElement
	protected int hailSymbolWidth;

	@XmlElement
	protected float hailSymbolSize;

	// ----------------Dust storm Attributes ----------------
	@XmlElement
	protected boolean dustStormEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB dustStormColor;

	@XmlElement
	protected int dustStormLineWidth;

	@XmlElement
	protected int dustStormSymbolWidth;

	@XmlElement
	protected float dustStormSymbolSize;

	// ---------------Sand storm Attributes ----------------

	@XmlElement
	protected boolean sandStormEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB sandStormColor;

	@XmlElement
	protected int sandStormLineWidth;

	@XmlElement
	protected int sandStormSymbolWidth;

	@XmlElement
	protected float sandStormSymbolSize;

	// ----------------Cumulonimbus Attributes -----------------

	@XmlElement
	protected boolean cumulonimbusEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB cumulonimbusColor;

	@XmlElement
	protected int cumulonimbusLineWidth;

	@XmlElement
	protected int cumulonimbusSymbolWidth;

	@XmlElement
	protected float cumulonimbusSymbolSize;
	// ----------------Low level wind shear Attributes --------------

	@XmlElement
	protected boolean lowLevelWindShearEnable;

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB lowLevelWindShearColor;

	@XmlElement
	protected int lowLevelWindShearLineWidth;

	@XmlElement
	protected int lowLevelWindShearSymbolWidth;

	@XmlElement
	protected float lowLevelWindShearSymbolSize;

	// ------------------------------------------------------------------

	@XmlElement
	protected boolean symbolEnable;

	@XmlElement
	protected boolean timeEnable;

	@XmlElement
	protected boolean nameOrNumberEnable;

	@XmlElement
	protected boolean motionEnable;

	@XmlElement
	protected boolean flightLevelEnable;

	// -------------------------Getters and Setters---------------------

	/**
	 * @return the sourceName
	 */
	public String getLegendName() {
		return legendName;
	}

	/**
	 * @param legendName
	 *            the legendName to set
	 */
	public void setLegendName(String legendName) {
		this.legendName = legendName;
	}

	/**
	 * @return the thunderstormEnable
	 */
	public boolean getThunderstormEnable() {
		return thunderstormEnable;
	}

	/**
	 * @param thunderstormEnable
	 *            the thunderstormEnable to set
	 */
	public void setThunderstormEnable(boolean thunderstormEnable) {
		this.thunderstormEnable = thunderstormEnable;
	}

	/**
	 * @return the thunderstormColor
	 */
	public RGB getThunderstormColor() {
		return thunderstormColor;
	}

	/**
	 * @param thunderstormColor
	 *            the thunderstormColor to set
	 */
	public void setThunderstormColor(RGB thunderstormColor) {
		this.thunderstormColor = thunderstormColor;
	}

	/**
	 * @return the thunderstormLineWidth
	 */
	public int getThunderstormLineWidth() {
		return thunderstormLineWidth;
	}

	/**
	 * @param thunderstormLineWidth
	 *            the thunderstormLineWidth to set
	 */
	public void setThunderstormLineWidth(int thunderstormLineWidth) {
		this.thunderstormLineWidth = thunderstormLineWidth;
	}

	/**
	 * @return the thunderstormSymbolWidth
	 */
	public int getThunderstormSymbolWidth() {
		return thunderstormSymbolWidth;
	}

	/**
	 * @param thunderstormSymbolWidth
	 *            the thunderstormSymbolWidth to set
	 */
	public void setThunderstormSymbolWidth(int thunderstormSymbolWidth) {
		this.thunderstormSymbolWidth = thunderstormSymbolWidth;
	}

	/**
	 * @return the thunderstormSymbolSize
	 */
	public float getThunderstormSymbolSize() {
		return thunderstormSymbolSize;
	}

	/**
	 * @param thunderstormSymbolSize
	 *            the thunderstormSymbolSize to set
	 */
	public void setThunderstormSymbolSize(float thunderstormSymbolSize) {
		this.thunderstormSymbolSize = thunderstormSymbolSize;
	}

	/**
	 * @return the turbulenceEnable
	 */
	public boolean getTurbulenceEnable() {
		return turbulenceEnable;
	}

	/**
	 * @param turbulenceEnable
	 *            the turbulenceEnable to set
	 */
	public void setTurbulenceEnable(boolean turbulenceEnable) {
		this.turbulenceEnable = turbulenceEnable;
	}

	/**
	 * @return the turbulenceColor
	 */
	public RGB getTurbulenceColor() {
		return turbulenceColor;
	}

	/**
	 * @param turbulenceColor
	 *            the turbulenceColor to set
	 */
	public void setTurbulenceColor(RGB turbulenceColor) {
		this.turbulenceColor = turbulenceColor;
	}

	/**
	 * @return the turbulenceLineWidth
	 */
	public int getTurbulenceLineWidth() {
		return turbulenceLineWidth;
	}

	/**
	 * @param turbulenceLineWidth
	 *            the turbulenceLineWidth to set
	 */
	public void setTurbulenceLineWidth(int turbulenceLineWidth) {
		this.turbulenceLineWidth = turbulenceLineWidth;
	}

	/**
	 * @return the turbulenceSymbolWidth
	 */
	public int getTurbulenceSymbolWidth() {
		return turbulenceSymbolWidth;
	}

	/**
	 * @param turbulenceSymbolWidth
	 *            the turbulenceSymbolWidth to set
	 */
	public void setTurbulenceSymbolWidth(int turbulenceSymbolWidth) {
		this.turbulenceSymbolWidth = turbulenceSymbolWidth;
	}

	/**
	 * @return the turbulenceSymbolSize
	 */
	public float getTurbulenceSymbolSize() {
		return turbulenceSymbolSize;
	}

	/**
	 * @param turbulenceSymbolSize
	 *            the turbulenceSymbolSize to set
	 */
	public void setTurbulenceSymbolSize(float turbulenceSymbolSize) {
		this.turbulenceSymbolSize = turbulenceSymbolSize;
	}

	/**
	 * @return the hurricaneEnable
	 */
	public boolean getHurricaneEnable() {
		return hurricaneEnable;
	}

	/**
	 * @param hurricaneEnable
	 *            the hurricaneEnable to set
	 */
	public void setHurricaneEnable(boolean hurricaneEnable) {
		this.hurricaneEnable = hurricaneEnable;
	}

	/**
	 * @return the hurricaneColor
	 */
	public RGB getHurricaneColor() {
		return hurricaneColor;
	}

	/**
	 * @param hurricaneColor
	 *            the hurricaneColor to set
	 */
	public void setHurricaneColor(RGB hurricaneColor) {
		this.hurricaneColor = hurricaneColor;
	}

	/**
	 * @return the hurricaneSymbolWidth
	 */
	public int getHurricaneSymbolWidth() {
		return hurricaneSymbolWidth;
	}

	/**
	 * @param hurricaneSymbolWidth
	 *            the hurricaneSymbolWidth to set
	 */
	public void setHurricaneSymbolWidth(int hurricaneSymbolWidth) {
		this.hurricaneSymbolWidth = hurricaneSymbolWidth;
	}

	/**
	 * @return the hurricaneSymbolSize
	 */
	public float getHurricaneSymbolSize() {
		return hurricaneSymbolSize;
	}

	/**
	 * @param hurricaneSymbolSize
	 *            the hurricaneSymbolSize to set
	 */
	public void setHurricaneSymbolSize(float hurricaneSymbolSize) {
		this.hurricaneSymbolSize = hurricaneSymbolSize;
	}

	/**
	 * @return the tropicalStormEnable
	 */
	public boolean getTropicalStormEnable() {
		return tropicalStormEnable;
	}

	/**
	 * @param tropicalStormEnable
	 *            the tropicalStormEnable to set
	 */
	public void setTropicalStormEnable(boolean tropicalStormEnable) {
		this.tropicalStormEnable = tropicalStormEnable;
	}

	/**
	 * @return the tropicalStormColor
	 */
	public RGB getTropicalStormColor() {
		return tropicalStormColor;
	}

	/**
	 * @param tropicalStormColor
	 *            the tropicalStormColor to set
	 */
	public void setTropicalStormColor(RGB tropicalStormColor) {
		this.tropicalStormColor = tropicalStormColor;
	}

	/**
	 * @return the tropicalStormSymbolWidth
	 */
	public int getTropicalStormSymbolWidth() {
		return tropicalStormSymbolWidth;
	}

	/**
	 * @param tropicalStormSymbolWidth
	 *            the tropicalStormSymbolWidth to set
	 */
	public void setTropicalStormSymbolWidth(int tropicalStormSymbolWidth) {
		this.tropicalStormSymbolWidth = tropicalStormSymbolWidth;
	}

	/**
	 * @return the tropicalStormSymbolSize
	 */
	public float getTropicalStormSymbolSize() {
		return tropicalStormSymbolSize;
	}

	/**
	 * @param tropicalStormSymbolSize
	 *            the tropicalStormSymbolSize to set
	 */
	public void setTropicalStormSymbolSize(float tropicalStormSymbolSize) {
		this.tropicalStormSymbolSize = tropicalStormSymbolSize;
	}

	/**
	 * @return the tropicalDepressionEnable
	 */
	public boolean getTropicalDepressionEnable() {
		return tropicalDepressionEnable;
	}

	/**
	 * @param tropicalDepressionEnable
	 *            the tropicalDepressionEnable to set
	 */
	public void setTropicalDepressionEnable(boolean tropicalDepressionEnable) {
		this.tropicalDepressionEnable = tropicalDepressionEnable;
	}

	/**
	 * @return the tropicalDepressionColor
	 */
	public RGB getTropicalDepressionColor() {
		return tropicalDepressionColor;
	}

	/**
	 * @param tropicalDepressionColor
	 *            the tropicalDepressionColor to set
	 */
	public void setTropicalDepressionColor(RGB tropicalDepressionColor) {
		this.tropicalDepressionColor = tropicalDepressionColor;
	}

	/**
	 * @return the tropicalDepressionSymbolWidth
	 */
	public int getTropicalDepressionSymbolWidth() {
		return tropicalDepressionSymbolWidth;
	}

	/**
	 * @param tropicalDepressionSymbolWidth
	 *            the tropicalDepressionSymbolWidth to set
	 */
	public void setTropicalDepressionSymbolWidth(
			int tropicalDepressionSymbolWidth) {
		this.tropicalDepressionSymbolWidth = tropicalDepressionSymbolWidth;
	}

	/**
	 * @return the tropicalDepressionSymbolSize
	 */
	public float getTropicalDepressionSymbolSize() {
		return tropicalDepressionSymbolSize;
	}

	/**
	 * @param tropicalDepressionSymbolSize
	 *            the tropicalDepressionSymbolSize to set
	 */
	public void setTropicalDepressionSymbolSize(
			float tropicalDepressionSymbolSize) {
		this.tropicalDepressionSymbolSize = tropicalDepressionSymbolSize;
	}

	/**
	 * @return the volcanicAshCloudEnable
	 */
	public boolean getVolcanicAshCloudEnable() {
		return volcanicAshCloudEnable;
	}

	/**
	 * @param volcanicAshCloudEnable
	 *            the volcanicAshCloudEnable to set
	 */
	public void setVolcanicAshCloudEnable(boolean volcanicAshCloudEnable) {
		this.volcanicAshCloudEnable = volcanicAshCloudEnable;
	}

	/**
	 * @return the volcanicAshCloudColor
	 */
	public RGB getVolcanicAshCloudColor() {
		return volcanicAshCloudColor;
	}

	/**
	 * @param volcanicAshCloudColor
	 *            the volcanicAshCloudColor to set
	 */
	public void setVolcanicAshCloudColor(RGB volcanicAshCloudColor) {
		this.volcanicAshCloudColor = volcanicAshCloudColor;
	}

	/**
	 * @return the volcanicAshCloudLineWidth
	 */
	public int getVolcanicAshCloudLineWidth() {
		return volcanicAshCloudLineWidth;
	}

	/**
	 * @param volcanicAshCloudLineWidth
	 *            the volcanicAshCloudLineWidth to set
	 */
	public void setVolcanicAshCloudLineWidth(int volcanicAshCloudLineWidth) {
		this.volcanicAshCloudLineWidth = volcanicAshCloudLineWidth;
	}

	/**
	 * @return the volcanicAshCloudSymbolWidth
	 */
	public int getVolcanicAshCloudSymbolWidth() {
		return volcanicAshCloudSymbolWidth;
	}

	/**
	 * @param volcanicAshCloudSymbolWidth
	 *            the volcanicAshCloudSymbolWidth to set
	 */
	public void setVolcanicAshCloudSymbolWidth(int volcanicAshCloudSymbolWidth) {
		this.volcanicAshCloudSymbolWidth = volcanicAshCloudSymbolWidth;
	}

	/**
	 * @return the volcanicAshCloudSymbolSize
	 */
	public float getVolcanicAshCloudSymbolSize() {
		return volcanicAshCloudSymbolSize;
	}

	/**
	 * @param volcanicAshCloudSymbolSize
	 *            the volcanicAshCloudSymbolSize to set
	 */
	public void setVolcanicAshCloudSymbolSize(float volcanicAshCloudSymbolSize) {
		this.volcanicAshCloudSymbolSize = volcanicAshCloudSymbolSize;
	}

	/**
	 * @return the mountainWaveEnable
	 */
	public boolean getMountainWaveEnable() {
		return mountainWaveEnable;
	}

	/**
	 * @param mountainWaveEnable
	 *            the mountainWaveEnable to set
	 */
	public void setMountainWaveEnable(boolean mountainWaveEnable) {
		this.mountainWaveEnable = mountainWaveEnable;
	}

	/**
	 * @return the mountainWaveColor
	 */
	public RGB getMountainWaveColor() {
		return mountainWaveColor;
	}

	/**
	 * @param mountainWaveColor
	 *            the mountainWaveColor to set
	 */
	public void setMountainWaveColor(RGB mountainWaveColor) {
		this.mountainWaveColor = mountainWaveColor;
	}

	/**
	 * @return the mountainWaveLineWidth
	 */
	public int getMountainWaveLineWidth() {
		return mountainWaveLineWidth;
	}

	/**
	 * @param mountainWaveLineWidth
	 *            the mountainWaveLineWidth to set
	 */
	public void setMountainWaveLineWidth(int mountainWaveLineWidth) {
		this.mountainWaveLineWidth = mountainWaveLineWidth;
	}

	/**
	 * @return the mountainWaveSymbolWidth
	 */
	public int getMountainWaveSymbolWidth() {
		return mountainWaveSymbolWidth;
	}

	/**
	 * @param mountainWaveSymbolWidth
	 *            the mountainWaveSymbolWidth to set
	 */
	public void setMountainWaveSymbolWidth(int mountainWaveSymbolWidth) {
		this.mountainWaveSymbolWidth = mountainWaveSymbolWidth;
	}

	/**
	 * @return the mountainWaveSymbolSize
	 */
	public float getMountainWaveSymbolSize() {
		return mountainWaveSymbolSize;
	}

	/**
	 * @param mountainWaveSymbolSize
	 *            the mountainWaveSymbolSize to set
	 */
	public void setMountainWaveSymbolSize(float mountainWaveSymbolSize) {
		this.mountainWaveSymbolSize = mountainWaveSymbolSize;
	}

	/**
	 * @return the tropicalCycloneEnable
	 */
	public boolean getTropicalCycloneEnable() {
		return tropicalCycloneEnable;
	}

	/**
	 * @param tropicalCycloneEnable
	 *            the tropicalCycloneEnable to set
	 */
	public void setTropicalCycloneEnable(boolean tropicalCycloneEnable) {
		this.tropicalCycloneEnable = tropicalCycloneEnable;
	}

	/**
	 * @return the tropicalCycloneColor
	 */
	public RGB getTropicalCycloneColor() {
		return tropicalCycloneColor;
	}

	/**
	 * @param tropicalCycloneColor
	 *            the tropicalCycloneColor to set
	 */
	public void setTropicalCycloneColor(RGB tropicalCycloneColor) {
		this.tropicalCycloneColor = tropicalCycloneColor;
	}

	/**
	 * @return the tropicalCycloneSymbolWidth
	 */
	public int getTropicalCycloneSymbolWidth() {
		return tropicalCycloneSymbolWidth;
	}

	/**
	 * @param tropicalCycloneSymbolWidth
	 *            the tropicalCycloneSymbolWidth to set
	 */
	public void setTropicalCycloneSymbolWidth(int tropicalCycloneSymbolWidth) {
		this.tropicalCycloneSymbolWidth = tropicalCycloneSymbolWidth;
	}

	/**
	 * @return the tropicalCycloneSymbolSize
	 */
	public float getTropicalCycloneSymbolSize() {
		return tropicalCycloneSymbolSize;
	}

	/**
	 * @param tropicalCycloneSymbolSize
	 *            the tropicalCycloneSymbolSize to set
	 */
	public void setTropicalCycloneSymbolSize(float tropicalCycloneSymbolSize) {
		this.tropicalCycloneSymbolSize = tropicalCycloneSymbolSize;
	}

	/**
	 * @return the squallLineEnable
	 */
	public boolean getSquallLineEnable() {
		return squallLineEnable;
	}

	/**
	 * @param squallLineEnable
	 *            the squallLineEnable to set
	 */
	public void setSquallLineEnable(boolean squallLineEnable) {
		this.squallLineEnable = squallLineEnable;
	}

	/**
	 * @return the squallLineColor
	 */
	public RGB getSquallLineColor() {
		return squallLineColor;
	}

	/**
	 * @param squallLineColor
	 *            the squallLineColor to set
	 */
	public void setSquallLineColor(RGB squallLineColor) {
		this.squallLineColor = squallLineColor;
	}

	/**
	 * @return the squallLineWidth
	 */
	public int getSquallLineWidth() {
		return squallLineWidth;
	}

	/**
	 * @param squallLineWidth
	 *            the squallLineWidth to set
	 */
	public void setSquallLineWidth(int squallLineWidth) {
		this.squallLineWidth = squallLineWidth;
	}

	/**
	 * @return the squallLineSymbolWidth
	 */
	public int getSquallLineSymbolWidth() {
		return squallLineSymbolWidth;
	}

	/**
	 * @param squallLineSymbolWidth
	 *            the squallLineSymbolWidth to set
	 */
	public void setSquallLineSymbolWidth(int squallLineSymbolWidth) {
		this.squallLineSymbolWidth = squallLineSymbolWidth;
	}

	/**
	 * @return the squallLineSymbolSize
	 */
	public float getSquallLineSymbolSize() {
		return squallLineSymbolSize;
	}

	/**
	 * @param squallLineSymbolSize
	 *            the squallLineSymbolSize to set
	 */
	public void setSquallLineSymbolSize(float squallLineSymbolSize) {
		this.squallLineSymbolSize = squallLineSymbolSize;
	}

	/**
	 * @return the catEnable
	 */
	public boolean getCatEnable() {
		return catEnable;
	}

	/**
	 * @param catEnable
	 *            the catEnable to set
	 */
	public void setCatEnable(boolean catEnable) {
		this.catEnable = catEnable;
	}

	/**
	 * @return the catColor
	 */
	public RGB getCatColor() {
		return catColor;
	}

	/**
	 * @param catColor
	 *            the catColor to set
	 */
	public void setCatColor(RGB catColor) {
		this.catColor = catColor;
	}

	/**
	 * @return the catLineWidth
	 */
	public int getCatLineWidth() {
		return catLineWidth;
	}

	/**
	 * @param catLineWidth
	 *            the catLineWidth to set
	 */
	public void setCatLineWidth(int catLineWidth) {
		this.catLineWidth = catLineWidth;
	}

	/**
	 * @return the catSymbolWidth
	 */
	public int getCatSymbolWidth() {
		return catSymbolWidth;
	}

	/**
	 * @param catSymbolWidth
	 *            the catSymbolWidth to set
	 */
	public void setCatSymbolWidth(int catSymbolWidth) {
		this.catSymbolWidth = catSymbolWidth;
	}

	/**
	 * @return the catSymbolSize
	 */
	public float getCatSymbolSize() {
		return catSymbolSize;
	}

	/**
	 * @param catSymbolSize
	 *            the catSymbolSize to set
	 */
	public void setCatSymbolSize(float catSymbolSize) {
		this.catSymbolSize = catSymbolSize;
	}

	/**
	 * @return the icingEnable
	 */
	public boolean getIcingEnable() {
		return icingEnable;
	}

	/**
	 * @param icingEnable
	 *            the icingEnable to set
	 */
	public void setIcingEnable(boolean icingEnable) {
		this.icingEnable = icingEnable;
	}

	/**
	 * @return the icingColor
	 */
	public RGB getIcingColor() {
		return icingColor;
	}

	/**
	 * @param icingColor
	 *            the icingColor to set
	 */
	public void setIcingColor(RGB icingColor) {
		this.icingColor = icingColor;
	}

	/**
	 * @return the icingLineWidth
	 */
	public int getIcingLineWidth() {
		return icingLineWidth;
	}

	/**
	 * @param icingLineWidth
	 *            the icingLineWidth to set
	 */
	public void setIcingLineWidth(int icingLineWidth) {
		this.icingLineWidth = icingLineWidth;
	}

	/**
	 * @return the icingSymbolWidth
	 */
	public int getIcingSymbolWidth() {
		return icingSymbolWidth;
	}

	/**
	 * @param icingSymbolWidth
	 *            the icingSymbolWidth to set
	 */
	public void setIcingSymbolWidth(int icingSymbolWidth) {
		this.icingSymbolWidth = icingSymbolWidth;
	}

	/**
	 * @return the icingSymbolSize
	 */
	public float getIcingSymbolSize() {
		return icingSymbolSize;
	}

	/**
	 * @param icingSymbolSize
	 *            the icingSymbolSize to set
	 */
	public void setIcingSymbolSize(float icingSymbolSize) {
		this.icingSymbolSize = icingSymbolSize;
	}

	/**
	 * @return the hailEnable
	 */
	public boolean getHailEnable() {
		return hailEnable;
	}

	/**
	 * @param hailEnable
	 *            the hailEnable to set
	 */
	public void setHailEnable(boolean hailEnable) {
		this.hailEnable = hailEnable;
	}

	/**
	 * @return the hailColor
	 */
	public RGB getHailColor() {
		return hailColor;
	}

	/**
	 * @param hailColor
	 *            the hailColor to set
	 */
	public void setHailColor(RGB hailColor) {
		this.hailColor = hailColor;
	}

	/**
	 * @return the hailLineWidth
	 */
	public int getHailLineWidth() {
		return hailLineWidth;
	}

	/**
	 * @param hailLineWidth
	 *            the hailLineWidth to set
	 */
	public void setHailLineWidth(int hailLineWidth) {
		this.hailLineWidth = hailLineWidth;
	}

	/**
	 * @return the hailSymbolWidth
	 */
	public int getHailSymbolWidth() {
		return hailSymbolWidth;
	}

	/**
	 * @param hailSymbolWidth
	 *            the hailSymbolWidth to set
	 */
	public void setHailSymbolWidth(int hailSymbolWidth) {
		this.hailSymbolWidth = hailSymbolWidth;
	}

	/**
	 * @return the hailSymbolSize
	 */
	public float getHailSymbolSize() {
		return hailSymbolSize;
	}

	/**
	 * @param hailSymbolSize
	 *            the hailSymbolSize to set
	 */
	public void setHailSymbolSize(float hailSymbolSize) {
		this.hailSymbolSize = hailSymbolSize;
	}

	/**
	 * @return the dustStormEnable
	 */
	public boolean getDustStormEnable() {
		return dustStormEnable;
	}

	/**
	 * @param dustStormEnable
	 *            the dustStormEnable to set
	 */
	public void setDustStormEnable(boolean dustStormEnable) {
		this.dustStormEnable = dustStormEnable;
	}

	/**
	 * @return the dustStormColor
	 */
	public RGB getDustStormColor() {
		return dustStormColor;
	}

	/**
	 * @param dustStormColor
	 *            the dustStormColor to set
	 */
	public void setDustStormColor(RGB dustStormColor) {
		this.dustStormColor = dustStormColor;
	}

	/**
	 * @return the dustStormLineWidth
	 */
	public int getDustStormLineWidth() {
		return dustStormLineWidth;
	}

	/**
	 * @param dustStormLineWidth
	 *            the dustStormLineWidth to set
	 */
	public void setDustStormLineWidth(int dustStormLineWidth) {
		this.dustStormLineWidth = dustStormLineWidth;
	}

	/**
	 * @return the dustStormSymbolWidth
	 */
	public int getDustStormSymbolWidth() {
		return dustStormSymbolWidth;
	}

	/**
	 * @param dustStormSymbolWidth
	 *            the dustStormSymbolWidth to set
	 */
	public void setDustStormSymbolWidth(int dustStormSymbolWidth) {
		this.dustStormSymbolWidth = dustStormSymbolWidth;
	}

	/**
	 * @return the dustStormSymbolSize
	 */
	public float getDustStormSymbolSize() {
		return dustStormSymbolSize;
	}

	/**
	 * @param dustStormSymbolSize
	 *            the dustStormSymbolSize to set
	 */
	public void setDustStormSymbolSize(float dustStormSymbolSize) {
		this.dustStormSymbolSize = dustStormSymbolSize;
	}

	/**
	 * @return the sandStormEnable
	 */
	public boolean getSandStormEnable() {
		return sandStormEnable;
	}

	/**
	 * @param sandStormEnable
	 *            the sandStormEnable to set
	 */
	public void setSandStormEnable(boolean sandStormEnable) {
		this.sandStormEnable = sandStormEnable;
	}

	/**
	 * @return the sandStormColor
	 */
	public RGB getSandStormColor() {
		return sandStormColor;
	}

	/**
	 * @param sandStormColor
	 *            the sandStormColor to set
	 */
	public void setSandStormColor(RGB sandStormColor) {
		this.sandStormColor = sandStormColor;
	}

	/**
	 * @return the sandStormLineWidth
	 */
	public int getSandStormLineWidth() {
		return sandStormLineWidth;
	}

	/**
	 * @param sandStormLineWidth
	 *            the sandStormLineWidth to set
	 */
	public void setSandStormLineWidth(int sandStormLineWidth) {
		this.sandStormLineWidth = sandStormLineWidth;
	}

	/**
	 * @return the sandStormSymbolWidth
	 */
	public int getSandStormSymbolWidth() {
		return sandStormSymbolWidth;
	}

	/**
	 * @param sandStormSymbolWidth
	 *            the sandStormSymbolWidth to set
	 */
	public void setSandStormSymbolWidth(int sandStormSymbolWidth) {
		this.sandStormSymbolWidth = sandStormSymbolWidth;
	}

	/**
	 * @return the sandStormSymbolSize
	 */
	public float getSandStormSymbolSize() {
		return sandStormSymbolSize;
	}

	/**
	 * @param sandStormSymbolSize
	 *            the sandStormSymbolSize to set
	 */
	public void setSandStormSymbolSize(float sandStormSymbolSize) {
		this.sandStormSymbolSize = sandStormSymbolSize;
	}

	/**
	 * @return the cumulonimbusEnable
	 */
	public boolean getCumulonimbusEnable() {
		return cumulonimbusEnable;
	}

	/**
	 * @param cumulonimbusEnable
	 *            the cumulonimbusEnable to set
	 */
	public void setCumulonimbusEnable(boolean cumulonimbusEnable) {
		this.cumulonimbusEnable = cumulonimbusEnable;
	}

	/**
	 * @return the cumulonimbusColor
	 */
	public RGB getCumulonimbusColor() {
		return cumulonimbusColor;
	}

	/**
	 * @param cumulonimbusColor
	 *            the cumulonimbusColor to set
	 */
	public void setCumulonimbusColor(RGB cumulonimbusColor) {
		this.cumulonimbusColor = cumulonimbusColor;
	}

	/**
	 * @return the cumulonimbusLineWidth
	 */
	public int getCumulonimbusLineWidth() {
		return cumulonimbusLineWidth;
	}

	/**
	 * @param cumulonimbusLineWidth
	 *            the cumulonimbusLineWidth to set
	 */
	public void setCumulonimbusLineWidth(int cumulonimbusLineWidth) {
		this.cumulonimbusLineWidth = cumulonimbusLineWidth;
	}

	/**
	 * @return the cumulonimbusSymbolWidth
	 */
	public int getCumulonimbusSymbolWidth() {
		return cumulonimbusSymbolWidth;
	}

	/**
	 * @param cumulonimbusSymbolWidth
	 *            the cumulonimbusSymbolWidth to set
	 */
	public void setCumulonimbusSymbolWidth(int cumulonimbusSymbolWidth) {
		this.cumulonimbusSymbolWidth = cumulonimbusSymbolWidth;
	}

	/**
	 * @return the cumulonimbusSymbolSize
	 */
	public float getCumulonimbusSymbolSize() {
		return cumulonimbusSymbolSize;
	}

	/**
	 * @param cumulonimbusSymbolSize
	 *            the cumulonimbusSymbolSize to set
	 */
	public void setCumulonimbusSymbolSize(float cumulonimbusSymbolSize) {
		this.cumulonimbusSymbolSize = cumulonimbusSymbolSize;
	}

	/**
	 * @return the lowLevelWindShearEnable
	 */
	public boolean getLowLevelWindShearEnable() {
		return lowLevelWindShearEnable;
	}

	/**
	 * @param lowLevelWindShearEnable
	 *            the lowLevelWindShearEnable to set
	 */
	public void setLowLevelWindShearEnable(boolean lowLevelWindShearEnable) {
		this.lowLevelWindShearEnable = lowLevelWindShearEnable;
	}

	/**
	 * @return the lowLevelWindShearColor
	 */
	public RGB getLowLevelWindShearColor() {
		return lowLevelWindShearColor;
	}

	/**
	 * @param lowLevelWindShearColor
	 *            the lowLevelWindShearColor to set
	 */
	public void setLowLevelWindShearColor(RGB lowLevelWindShearColor) {
		this.lowLevelWindShearColor = lowLevelWindShearColor;
	}

	/**
	 * @return the lowLevelWindShearLineWidth
	 */
	public int getLowLevelWindShearLineWidth() {
		return lowLevelWindShearLineWidth;
	}

	/**
	 * @param lowLevelWindShearLineWidth
	 *            the lowLevelWindShearLineWidth to set
	 */
	public void setLowLevelWindShearLineWidth(int lowLevelWindShearLineWidth) {
		this.lowLevelWindShearLineWidth = lowLevelWindShearLineWidth;
	}

	/**
	 * @return the lowLevelWindShearSymbolWidth
	 */
	public int getLowLevelWindShearSymbolWidth() {
		return lowLevelWindShearSymbolWidth;
	}

	/**
	 * @param lowLevelWindShearSymbolWidth
	 *            the lowLevelWindShearSymbolWidth to set
	 */
	public void setLowLevelWindShearSymbolWidth(int lowLevelWindShearSymbolWidth) {
		this.lowLevelWindShearSymbolWidth = lowLevelWindShearSymbolWidth;
	}

	/**
	 * @return the lowLevelWindShearSymbolSize
	 */
	public float getLowLevelWindShearSymbolSize() {
		return lowLevelWindShearSymbolSize;
	}

	/**
	 * @param lowLevelWindShearSymbolSize
	 *            the lowLevelWindShearSymbolSize to set
	 */
	public void setLowLevelWindShearSymbolSize(float lowLevelWindShearSymbolSize) {
		this.lowLevelWindShearSymbolSize = lowLevelWindShearSymbolSize;
	}

	/**
	 * @return the symbolEnable
	 */
	public boolean getSymbolEnable() {
		return symbolEnable;
	}

	/**
	 * @param symbolEnable
	 *            the symbolEnable to set
	 */
	public void setSymbolEnable(boolean symbolEnable) {
		this.symbolEnable = symbolEnable;
	}

	/**
	 * @return the timeEnable
	 */
	public boolean getTimeEnable() {
		return timeEnable;
	}

	/**
	 * @param timeEnable
	 *            the timeEnable to set
	 */
	public void setTimeEnable(boolean timeEnable) {
		this.timeEnable = timeEnable;
	}

	/**
	 * @return the nameOrNumberEnable
	 */
	public boolean getNameOrNumberEnable() {
		return nameOrNumberEnable;
	}

	/**
	 * @param nameOrNumberEnable
	 *            the nameOrNumberEnable to set
	 */
	public void setNameOrNumberEnable(boolean nameOrNumberEnable) {
		this.nameOrNumberEnable = nameOrNumberEnable;
	}

	/**
	 * @return the motionEnable
	 */
	public boolean getMotionEnable() {
		return motionEnable;
	}

	/**
	 * @param motionEnable
	 *            the motionEnable to set
	 */
	public void setMotionEnable(boolean motionEnable) {
		this.motionEnable = motionEnable;
	}

	/**
	 * @return the flightLevelEnable
	 */
	public boolean getFlightLevelEnable() {
		return flightLevelEnable;
	}

	/**
	 * @param flightLevelEnable
	 *            the flightLevelEnable to set
	 */
	public void setFlightLevelEnable(
			boolean flightLevelEnable) {
		this.flightLevelEnable = flightLevelEnable;
	}

	// ---------------------------End of Getters and Setters-------------------------
    @Override
    public boolean isEventResource() {
    	return true;
    }
    
	@Override
	public MiscRscAttrs getMiscResourceAttrs() {
		MiscRscAttrs attrs = new MiscRscAttrs(5);

		attrs.addAttr(new MiscResourceAttr("thunderstormEnable",
				"Thunderstorm", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("thunderstormColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("thunderstormLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("thunderstormSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("thunderstormSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("turbulenceEnable", "Turbulence",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("turbulenceColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("turbulenceLineWidth", "Line Width",
				EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("turbulenceSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("turbulenceSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("hurricaneEnable", "Hurricane",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("hurricaneColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("", "",
				EditElement.LABEL, 3));		
		attrs.addAttr(new MiscResourceAttr("hurricaneSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("hurricaneSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("tropicalStormEnable",
				"Tropical Storm", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("tropicalStormColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("","", EditElement.LABEL, 3));
		attrs.addAttr(new MiscResourceAttr("tropicalStormSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("tropicalStormSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("tropicalDepressionEnable",
				"Tropical Depression", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("tropicalDepressionColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("", "",
				EditElement.LABEL, 3));
		attrs.addAttr(new MiscResourceAttr("tropicalDepressionSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("tropicalDepressionSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("volcanicAshCloudEnable",
				"Volcanic Ash Cloud", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("volcanicAshCloudColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("volcanicAshCloudLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("volcanicAshCloudSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("volcanicAshCloudSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("mountainWaveEnable",
				"Mountain Wave", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("mountainWaveColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("mountainWaveLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("mountainWaveSymbolWidth", "Symbol Width",
				EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("mountainWaveSymbolSize", "Symbol Size",
				EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("tropicalCycloneEnable",
				"Tropical Cyclone", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("tropicalCycloneColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("", "",
				EditElement.LABEL, 3));
		attrs.addAttr(new MiscResourceAttr("tropicalCycloneSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("tropicalCycloneSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("squallLineEnable", "Squall Line",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("squallLineColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("squallLineWidth", "Line Width",
				EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("squallLineSymbolWidth", "Symbol Width",
				EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("squallLineSymbolSize", "Symbol Size",
				EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("catEnable", "CAT",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("catColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("catLineWidth", "Line Width",
				EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("catSymbolWidth", "Symbol Width",
				EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("catSymbolSize", "Symbol Size",
				EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("icingEnable", "Icing",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("icingColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("icingLineWidth", "Line Width",
				EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("icingSymbolWidth", "Symbol Width",
				EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("icingSymbolSize", "Symbol Size",
				EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("hailEnable", "Hail",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("hailColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("hailLineWidth", "Line Width",
				EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("hailSymbolWidth", "Symbol Width",
				EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("hailSymbolSize", "Symbol Size",
				EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("dustStormEnable", "Dust Storm",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("dustStormColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("dustStormLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("dustStormSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("dustStormSymbolSize", "Symbol Size",
				EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("sandStormEnable", "Sand Storm",
				EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("sandStormColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("sandStormLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("sandStormSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("sandStormSymbolSize", "Symbol Size",
				EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("cumulonimbusEnable",
				"Cumulonimbus", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("cumulonimbusColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("cumulonimbusLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("cumulonimbusSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("cumulonimbusSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr("lowLevelWindShearEnable",
				"Low Level Wind Shear", EditElement.CHECK_BOX, 1));
		attrs.addAttr(new MiscResourceAttr("lowLevelWindShearColor", "",
				EditElement.COLOR_SELECTOR, 2));
		attrs.addAttr(new MiscResourceAttr("lowLevelWindShearLineWidth",
				"Line Width", EditElement.SPINNER, 3));
		attrs.addAttr(new MiscResourceAttr("lowLevelWindShearSymbolWidth",
				"Symbol Width", EditElement.SPINNER, 4));
		attrs.addAttr(new MiscResourceAttr("lowLevelWindShearSymbolSize",
				"Symbol Size", EditElement.SPINNER, 5));

		attrs.addAttr(new MiscResourceAttr(null, null,
						EditElement.SEPARATOR, 1));

		attrs.addAttr(new MiscResourceAttr("symbolEnable", "Symbol",
				EditElement.CHECK_BOX, 1));

		attrs.addAttr(new MiscResourceAttr("timeEnable", "Time",
				EditElement.CHECK_BOX, 1));

		attrs.addAttr(new MiscResourceAttr("nameOrNumberEnable", "Name/Number",
				EditElement.CHECK_BOX, 1));

		attrs.addAttr(new MiscResourceAttr("motionEnable", "Motion",
				EditElement.CHECK_BOX, 1));

		attrs.addAttr(new MiscResourceAttr("flightLevelEnable",
				"Flight Level", EditElement.CHECK_BOX, 1));

		return attrs;
	}

	/**
	 * Creates an International SIGMET resource.
	 * 
	 * @throws VizException
	 */
	public IntlSigmetResourceData() throws VizException {
		super();
		this.nameGenerator = new AbstractNameGenerator() {
			@Override
			public String getName(AbstractVizResource<?, ?> resource) {
				if (legendName != null) {
					return legendName;
				}
				return "International SIGMET";
			}
		};
		// color = RESOURCE_LEGEND_COLOR;
	}

	// @Override
	protected AbstractVizResource<?, ?> constructResource(
			LoadProperties loadProperties,
			com.raytheon.uf.common.dataplugin.PluginDataObject[] objects)
			throws VizException {
		return new IntlSigmetResource(this, loadProperties);
	}
}
