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

package com.raytheon.edex.util.grib;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Container class defining composite models.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 09, 2010  4638     bphillip    Initial Creation
 * Oct 15, 2013  2473     bsteffen    Remove deprecated ISerializableObject.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "compositeModel")
@XmlAccessorType(XmlAccessType.NONE)
public class CompositeModel {

    /** The composite model name */
    @XmlElement
    private String modelName;

    /** The grid name for this composite model */
    @XmlElement
    private String grid;

    /** The component models that contribute to this composite model */
    @XmlElement
    private String componentModels;

    /**
     * Creates a new CompositeModel object
     */
    public CompositeModel() {

    }

    /**
     * @return the modelName
     */
    public String getModelName() {
        return modelName;
    }

    /**
     * @param modelName
     *            the modelName to set
     */
    public void setModelName(String modelName) {
        this.modelName = modelName;
    }

    /**
     * @return the componentModels
     */
    public String getComponentModels() {
        return componentModels;
    }

    /**
     * Gets the component models as a list
     * 
     * @return The list of component models
     */
    public List<String> getModelList() {
        String[] mods = componentModels.split(":");
        List<String> modelList = new ArrayList<String>();
        for (String mod : mods) {
            modelList.add(mod);
        }
        return modelList;
    }

    /**
     * @param componentModels
     *            the componentModels to set
     */
    public void setComponentModels(String componentModels) {
        this.componentModels = componentModels;
    }

    /**
     * @return the grid
     */
    public String getGrid() {
        return grid;
    }

    /**
     * @param grid
     *            the grid to set
     */
    public void setGrid(String grid) {
        this.grid = grid;
    }

}
