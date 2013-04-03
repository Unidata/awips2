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
package com.raytheon.edex.plugin.grib.util;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlAccessorType(XmlAccessType.NONE)
public class GridModel implements ISerializableObject {

    /** The title of the model */
    @XmlElement
    private String title;

    /** The model name */
    @XmlElement
    private String name;

    /** The center associated with this model */
    @XmlElement
    private Integer center;

    /** The NCEP grid associated with this model */
    @XmlElement
    private String grid;

    @XmlElementWrapper(name = "grids")
    @XmlElement(name = "id")
    private ArrayList<String> grids;

    @XmlElement
    private String subcenter;

    /** The generating processes associated with this model */
    @XmlElementWrapper(name = "process")
    @XmlElement(name = "id")
    private ArrayList<Integer> process;

    @XmlElement
    private String alias;

    /**
     * The intrinsic temporal resolution of the data.
     */
    @XmlElement
    private Integer dt;

    @XmlElement
    private String paramInfo;

    @XmlElement
    private boolean analysisOnly;

    /**
     * use datasetInfo instead
     */
    @Deprecated
    public String getTitle() {
        return title;
    }

    /**
     * use datasetInfo instead
     */
    @Deprecated
    public void setTitle(String title) {
        this.title = title;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getGrid() {
        return grid;
    }

    public void setGrid(String grid) {
        this.grid = grid;
    }

    public ArrayList<Integer> getProcess() {
        return process;
    }

    public void setProcess(ArrayList<Integer> process) {
        this.process = process;
    }

    public Integer getCenter() {
        return center;
    }

    public void setCenter(Integer center) {
        this.center = center;
    }

    public String getSubCenter() {
        return this.subcenter;
    }

    public void setSubCenter(String subcenter) {
        this.subcenter = subcenter;
    }

    /**
     * use datasetInfo instead
     */
    @Deprecated
    public void setAlias(String alias) {
        this.alias = alias;
    }

    /**
     * use datasetInfo instead
     */
    @Deprecated
    public String getAlias() {
        return alias;
    }

    /**
     * use datasetInfo instead
     */
    @Deprecated
    public Integer getDt() {
        return dt;
    }

    /**
     * use datasetInfo instead
     */
    @Deprecated
    public void setDt(Integer dt) {
        this.dt = dt;
    }

    /**
     * @return the paramInfo
     */
    @Deprecated
    public String getParamInfo() {
        return paramInfo;
    }

    /**
     * @param paramInfo
     *            the paramInfo to set
     */
    @Deprecated
    public void setParamInfo(String paramInfo) {
        this.paramInfo = paramInfo;
    }

    public boolean getAnalysisOnly() {
        return analysisOnly;
    }

    public ArrayList<String> getGrids() {
        return grids;
    }

    public void setGrids(ArrayList<String> grids) {
        this.grids = grids;
    }

    /**
     * Returns a list containing both the grid element if present and the grids
     * element if present
     * 
     * @return
     */
    public List<String> getAllGrids() {
        List<String> grids = new ArrayList<String>();
        if (this.grids != null) {
            grids.addAll(this.grids);
        }
        if (this.grid != null) {
            grids.add(this.grid);
        }
        return grids;
    }
}