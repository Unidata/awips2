package com.raytheon.uf.common.hydro.areal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Areal Shapefile Definition.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2020 19533      mgamazaychikov Initial creation
 *
 * </pre>
 *
 * @author mgamazaychikov
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ArealShapefile {

    @XmlAttribute(name = "name")
    private String name;

    @XmlAttribute(name = "filename")
    private String filename;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getFilename() {
        return filename;
    }

    public void setFilename(String filename) {
        this.filename = filename;
    }

}