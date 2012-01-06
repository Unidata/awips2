/*
 * WstmResourceData
 * 
 * Date created (November 05, 2010)
 *
 *  This code has been developed by the SIB for use in the AWIPS2 system. 
 */

package gov.noaa.nws.ncep.viz.rsc.wstm.rsc;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.RGBColorAdapter;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Provides the user-editable attributes for the WSTM resource
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05-Nov-2010    247      Archana     Initial creation.
 * 22-Apr-2011    439      G. Hull     don't set timeMatchMethod to BEFORE_OR_EQUAL
 *                                                
 * @author archana
 * </pre>
 */
@SuppressWarnings("unused")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "WstmResourceData")
public class WstmResourceData extends AbstractNatlCntrsRequestableResourceData
        implements INatlCntrsResourceData {

    @XmlElement
    protected String legendName;

    // --------Winter Storm Warning
    // attributes-----------------------------------
    @XmlElement
    private Boolean wstmWarningEnable = true;

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB wstmWarningColor;

    @XmlElement
    private Integer wstmWarningLineWidth = 3;

    @XmlElement
    private Integer wstmWarningSymbolWidth = 3;

    @XmlElement
    private Float wstmWarningSymbolSize = 1.0f;

    /*------------Winter Storm Watch-------------*/

    @XmlElement
    private Boolean wstmWatchEnable = true;

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB wstmWatchColor;

    @XmlElement
    private Integer wstmWatchLineWidth = 3;

    @XmlElement
    private Integer wstmWatchSymbolWidth = 3;

    @XmlElement
    private Float wstmWatchSymbolSize = 1.0f;

    /*------------------Winter Storm Advisory------------------------*/
    @XmlElement
    private Boolean wstmAdvisoryEnable = true;

    @XmlElement
    @XmlJavaTypeAdapter(RGBColorAdapter.class)
    protected RGB wstmAdvisoryColor;

    @XmlElement
    private Integer wstmAdvisoryLineWidth = 3;

    @XmlElement
    private Integer wstmAdvisorySymbolWidth = 3;

    @XmlElement
    private Float wstmAdvisorySymbolSize = 1.0f;

    @XmlElement
    private Boolean timeEnable = true;

    @XmlElement
    private Boolean zoneNameEnable = false;

    @XmlElement
    private Boolean outlineEnable = false;

    /**
     * @return the legendName
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
     * @return the wstmWarningEnable
     */
    public Boolean getWstmWarningEnable() {
        return wstmWarningEnable;
    }

    /**
     * @param wstmWarningEnable
     *            the wstmWarningEnable to set
     */
    public void setWstmWarningEnable(Boolean wstmWarningEnable) {
        this.wstmWarningEnable = new Boolean(wstmWarningEnable);
    }

    /**
     * @return the wstmWarningColor
     */
    public RGB getWstmWarningColor() {
        return wstmWarningColor;
    }

    /**
     * @param wstmWarningColor
     *            the wstmWarningColor to set
     */
    public void setWstmWarningColor(RGB wstmWarningColor) {
        this.wstmWarningColor = new RGB(wstmWarningColor.red,
                wstmWarningColor.green, wstmWarningColor.blue);
    }

    /**
     * @return the wstmWarningLineWidth
     */
    public Integer getWstmWarningLineWidth() {
        return wstmWarningLineWidth;
    }

    /**
     * @param wstmWarningLineWidth
     *            the wstmWarningLineWidth to set
     */
    public void setWstmWarningLineWidth(Integer wstmWarningLineWidth) {
        this.wstmWarningLineWidth = new Integer(wstmWarningLineWidth);
    }

    /**
     * @return the wstmWarningSymbolWidth
     */
    public Integer getWstmWarningSymbolWidth() {
        return wstmWarningSymbolWidth;
    }

    /**
     * @param wstmWarningSymbolWidth
     *            the wstmWarningSymbolWidth to set
     */
    public void setWstmWarningSymbolWidth(Integer wstmWarningSymbolWidth) {
        this.wstmWarningSymbolWidth = new Integer(wstmWarningSymbolWidth);
    }

    /**
     * @return the wstmWarningSymbolSize
     */
    public Float getWstmWarningSymbolSize() {
        return wstmWarningSymbolSize;
    }

    /**
     * @param wstmWarningSymbolSize
     *            the wstmWarningSymbolSize to set
     */
    public void setWstmWarningSymbolSize(Float wstmWarningSymbolSize) {
        this.wstmWarningSymbolSize = new Float(wstmWarningSymbolSize);
    }

    /**
     * @return the wstmWatchEnable
     */
    public Boolean getWstmWatchEnable() {
        return wstmWatchEnable;
    }

    /**
     * @param wstmWatchEnable
     *            the wstmWatchEnable to set
     */
    public void setWstmWatchEnable(Boolean wstmWatchEnable) {
        this.wstmWatchEnable = new Boolean(wstmWatchEnable);
    }

    /**
     * @return the wstmWatchColor
     */
    public RGB getWstmWatchColor() {
        return wstmWatchColor;
    }

    /**
     * @param wstmWatchColor
     *            the wstmWatchColor to set
     */
    public void setWstmWatchColor(RGB wstmWatchColor) {
        this.wstmWatchColor = new RGB(wstmWatchColor.red, wstmWatchColor.green,
                wstmWatchColor.blue);
    }

    /**
     * @return the wstmWatchLineWidth
     */
    public Integer getWstmWatchLineWidth() {
        return wstmWatchLineWidth;
    }

    /**
     * @param wstmWatchLineWidth
     *            the wstmWatchLineWidth to set
     */
    public void setWstmWatchLineWidth(Integer wstmWatchLineWidth) {
        this.wstmWatchLineWidth = new Integer(wstmWatchLineWidth);
    }

    /**
     * @return the wstmWatchSymbolWidth
     */
    public Integer getWstmWatchSymbolWidth() {
        return wstmWatchSymbolWidth;
    }

    /**
     * @param wstmWatchSymbolWidth
     *            the wstmWatchSymbolWidth to set
     */
    public void setWstmWatchSymbolWidth(Integer wstmWatchSymbolWidth) {
        this.wstmWatchSymbolWidth = new Integer(wstmWatchSymbolWidth);
    }

    /**
     * @return the wstmWatchSymbolSize
     */
    public Float getWstmWatchSymbolSize() {
        return wstmWatchSymbolSize;
    }

    /**
     * @param wstmWatchSymbolSize
     *            the wstmWatchSymbolSize to set
     */
    public void setWstmWatchSymbolSize(Float wstmWatchSymbolSize) {
        this.wstmWatchSymbolSize = new Float(wstmWatchSymbolSize);
    }

    /**
     * @return the wstmAdvisoryEnable
     */
    public Boolean getWstmAdvisoryEnable() {
        return wstmAdvisoryEnable;
    }

    /**
     * @param wstmAdvisoryEnable
     *            the wstmAdvisoryEnable to set
     */
    public void setWstmAdvisoryEnable(Boolean wstmAdvisoryEnable) {
        this.wstmAdvisoryEnable = new Boolean(wstmAdvisoryEnable);
    }

    /**
     * @return the wstmAdvisoryColor
     */
    public RGB getWstmAdvisoryColor() {
        return wstmAdvisoryColor;
    }

    /**
     * @param wstmAdvisoryColor
     *            the wstmAdvisoryColor to set
     */
    public void setWstmAdvisoryColor(RGB wstmAdvisoryColor) {
        this.wstmAdvisoryColor = new RGB(wstmAdvisoryColor.red,
                wstmAdvisoryColor.green, wstmAdvisoryColor.blue);
    }

    /**
     * @return the wstmAdvisoryLineWidth
     */
    public Integer getWstmAdvisoryLineWidth() {
        return wstmAdvisoryLineWidth;
    }

    /**
     * @param wstmAdvisoryLineWidth
     *            the wstmAdvisoryLineWidth to set
     */
    public void setWstmAdvisoryLineWidth(Integer wstmAdvisoryLineWidth) {
        this.wstmAdvisoryLineWidth = new Integer(wstmAdvisoryLineWidth);
    }

    /**
     * @return the wstmAdvisorySymbolWidth
     */
    public Integer getWstmAdvisorySymbolWidth() {
        return wstmAdvisorySymbolWidth;
    }

    /**
     * @param wstmAdvisorySymbolWidth
     *            the wstmAdvisorySymbolWidth to set
     */
    public void setWstmAdvisorySymbolWidth(Integer wstmAdvisorySymbolWidth) {
        this.wstmAdvisorySymbolWidth = new Integer(wstmAdvisorySymbolWidth);
    }

    /**
     * @return the wstmAdvisorySymbolSize
     */
    public Float getWstmAdvisorySymbolSize() {
        return wstmAdvisorySymbolSize;
    }

    /**
     * @param wstmAdvisorySymbolSize
     *            the wstmAdvisorySymbolSize to set
     */
    public void setWstmAdvisorySymbolSize(Float wstmAdvisorySymbolSize) {
        this.wstmAdvisorySymbolSize = new Float(wstmAdvisorySymbolSize);
    }

    /**
     * @return the timeEnable
     */
    public Boolean getTimeEnable() {
        return timeEnable;
    }

    /**
     * @param timeEnable
     *            the timeEnable to set
     */
    public void setTimeEnable(Boolean timeEnable) {
        this.timeEnable = new Boolean(timeEnable);
    }

    /**
     * @return the zoneNameEnable
     */
    public Boolean getZoneNameEnable() {
        return zoneNameEnable;
    }

    /**
     * @param zoneNameEnable
     *            the zoneNameEnable to set
     */
    public void setZoneNameEnable(Boolean zoneNameEnable) {
        this.zoneNameEnable = new Boolean(zoneNameEnable);
    }

    /**
     * @return the outlineEnable
     */
    public Boolean getOutlineEnable() {
        return outlineEnable;
    }

    /**
     * @param outlineEnable
     *            the outlineEnable to set
     */
    public void setOutlineEnable(Boolean outlineEnable) {
        this.outlineEnable = new Boolean(outlineEnable);
    }

    // @Override
    // public MiscRscAttrs getMiscResourceAttrs() {
    // MiscRscAttrs attrs = new MiscRscAttrs(5);
    // attrs.addAttr(new MiscResourceAttr("wstmWarningEnable",
    // "Winter storm warning", EditElement.CHECK_BOX, 1));
    // attrs.addAttr(new MiscResourceAttr("wstmWarningColor", "",
    // EditElement.COLOR_SELECTOR, 2));
    // attrs.addAttr(new MiscResourceAttr("wstmWarningLineWidth",
    // "Line Width", EditElement.SPINNER, 3));
    // attrs.addAttr(new MiscResourceAttr("wstmWarningSymbolWidth",
    // "Symbol Width", EditElement.SPINNER, 4));
    // attrs.addAttr(new MiscResourceAttr("wstmWarningSymbolSize",
    // "Symbol Size", EditElement.SPINNER, 5));
    //
    // attrs.addAttr(new MiscResourceAttr("wstmWatchEnable",
    // "Winter storm watch", EditElement.CHECK_BOX, 1));
    // attrs.addAttr(new MiscResourceAttr("wstmWatchColor", "",
    // EditElement.COLOR_SELECTOR, 2));
    // attrs.addAttr(new MiscResourceAttr("wstmWatchLineWidth",
    // "Line Width", EditElement.SPINNER, 3));
    // attrs.addAttr(new MiscResourceAttr("wstmWatchSymbolWidth",
    // "Symbol Width", EditElement.SPINNER, 4));
    // attrs.addAttr(new MiscResourceAttr("wstmWatchSymbolSize",
    // "Symbol Size", EditElement.SPINNER, 5));
    //
    // attrs.addAttr(new MiscResourceAttr("wstmAdvisoryEnable",
    // "Winter storm advisory", EditElement.CHECK_BOX, 1));
    // attrs.addAttr(new MiscResourceAttr("wstmAdvisoryColor", "",
    // EditElement.COLOR_SELECTOR, 2));
    // attrs.addAttr(new MiscResourceAttr("wstmAdvisoryLineWidth",
    // "Line Width", EditElement.SPINNER, 3));
    // attrs.addAttr(new MiscResourceAttr("wstmAdvisorySymbolWidth",
    // "Symbol Width", EditElement.SPINNER, 4));
    // attrs.addAttr(new MiscResourceAttr("wstmAdvisorySymbolSize",
    // "Symbol Size", EditElement.SPINNER, 5));
    // return attrs;
    // }

    // @Override
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {
        WstmResource wstmResource = new WstmResource(this, loadProperties);
        return wstmResource;
    }

    public WstmResourceData() {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                if (legendName != null) {
                    return legendName;
                }
                return "WSTM";
            }
        };
    }

    @Override
    public boolean equals(Object obj) {
        return true;
    }
}
