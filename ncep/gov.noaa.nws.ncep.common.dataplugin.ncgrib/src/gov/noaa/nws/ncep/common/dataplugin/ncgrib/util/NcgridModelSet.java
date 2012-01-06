package gov.noaa.nws.ncep.common.dataplugin.ncgrib.util;

import java.util.ArrayList;
import java.util.Collection;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "ncgribModelSet")
@XmlAccessorType(XmlAccessType.NONE)
public class NcgridModelSet implements ISerializableObject {

    /**
     * List of models for/from the XML.
     */
    @XmlElements( { @XmlElement(name = "model", type = NcgridModel.class) })
    private ArrayList<NcgridModel> models;

    /**
     * Returns the list of models.
     * 
     * @return
     */
    public synchronized ArrayList<NcgridModel> getModels() {
        return models;
    }

    /**
     * Set the list of models.
     * 
     * @param models
     */
    public synchronized void setModels(ArrayList<NcgridModel> models) {
        this.models = models;
    }
    
    /**
     * Adds models to this set
     * @param models The models to add
     */
    public synchronized void addModels(Collection<NcgridModel> models){
        this.models.addAll(models);
    }

}
