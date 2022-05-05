package com.raytheon.uf.common.hydro.areal;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;
/**
 * Areal Shapefile List Definition.
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
@XmlRootElement(name = "arealShapefileList")
@XmlAccessorType(XmlAccessType.NONE)
public class ArealShapefileXML {
    @XmlElements({ @XmlElement(name = "arealShapefile", type = ArealShapefile.class) })
    private List<ArealShapefile> arealShapefileList = new ArrayList<>();;

    public List<ArealShapefile> getArealShapefileList() {
        return arealShapefileList;
    }

    public void setArealShapefileList(List<ArealShapefile> arealType) {
        this.arealShapefileList = arealType;
    }

    public ArealShapefile getShapefileSelection(String name) {
        Iterator<ArealShapefile> it = this.arealShapefileList.iterator();
        while (it.hasNext()) {
            ArealShapefile sel = it.next();
            if (sel.getName().equals(name)) {
                return it.next();
            }
        }
        return null;
    }
}
