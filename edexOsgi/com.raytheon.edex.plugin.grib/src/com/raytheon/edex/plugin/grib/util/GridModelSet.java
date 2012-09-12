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
import java.util.Collection;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "gribModelSet")
@XmlAccessorType(XmlAccessType.NONE)
public class GridModelSet implements ISerializableObject {

    /**
     * List of models for/from the XML.
     */
    @XmlElements( { @XmlElement(name = "model", type = GridModel.class) })
    private ArrayList<GridModel> models;

    /**
     * Returns the list of models.
     * 
     * @return
     */
    public ArrayList<GridModel> getModels() {
        return models;
    }

    /**
     * Set the list of models.
     * 
     * @param models
     */
    public void setModels(ArrayList<GridModel> models) {
        this.models = models;
    }
    
    /**
     * Adds models to this set
     * @param models The models to add
     */
    public void addModels(Collection<GridModel> models){
        if(this.models == null){
            this.models = new ArrayList<GridModel>();
        }
        this.models.addAll(models);
    }

}
