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
package com.raytheon.edex.rpgenvdata;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name="configuration")
@XmlAccessorType(XmlAccessType.FIELD)
public class Configuration {
    
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Model {
        @XmlAttribute public String name;
        @XmlAttribute public String description;
    }
    
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class ClipRadius {
        @XmlAttribute public double value;
        @XmlAttribute public String units;
    }
    
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name="parameter")
    public static class ParameterDef {
        @XmlAttribute public String id;
        @XmlAttribute public String name;
        @XmlAttribute public String type;
        @XmlAttribute public String units;
        @XmlAttribute public String value;
    }
    
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Field {
        @XmlAttribute public String name;
        @XmlAttribute(required=true) public String description;
        @XmlAttribute public String units;
        @XmlAttribute private Float acceptableInventoryPercentage; 
        @XmlElement(name="level")
        Level[] levels;
        
        @XmlTransient
        public float getAcceptableInventoryRatio() {
            if (acceptableInventoryPercentage != null)
                return acceptableInventoryPercentage.floatValue() / 100.0f;
            else
                return 1;
        }
    }
    
    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Level {
        @XmlAttribute public String name;
        @XmlAttribute public String description;
        @XmlAttribute public String units;
        @XmlAttribute @XmlList public String[] levels;
    }

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class InventoryHint {
        @XmlAttribute public String levelType;
        @XmlAttribute public int nLevels;
    }

    public Model model = new Model();
    public ClipRadius clipRadius = new ClipRadius();
    public Integer timeStepHint;
    
    @XmlElement(name="parameter")
    public ParameterDef[] parameters = new ParameterDef[0];
    @XmlElement(name="field")
    public Field[] fields = new Field[0];
    @XmlElement(name="inventoryHint")
    public InventoryHint[] inventoryHints= new InventoryHint[0];
    
    private static JAXBContext jaxbContext;
    private static Unmarshaller unmarshaller;
    
    public static JAXBContext getJAXBContext() throws JAXBException {
        if (jaxbContext == null) {
            synchronized (Configuration.class) {
                if (jaxbContext == null) {
                    jaxbContext = JAXBContext.newInstance(Configuration.class);
                    unmarshaller = jaxbContext.createUnmarshaller();
                }
            }
        }
        return jaxbContext;
    }
    
    public static Unmarshaller getUnmashaller() throws JAXBException {
        getJAXBContext();
        return unmarshaller;
    }
}
