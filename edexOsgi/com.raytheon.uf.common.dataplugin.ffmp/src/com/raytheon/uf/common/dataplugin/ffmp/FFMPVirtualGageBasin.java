package com.raytheon.uf.common.dataplugin.ffmp;

import java.util.Comparator;
import java.util.Date;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Holds VGB FFMP basin PC and PP data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02may10      3937       dhladky     Setup
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class FFMPVirtualGageBasin extends FFMPBasin implements
        ISerializableObject {

    public FFMPVirtualGageBasin() {

    }

    @DynamicSerializeElement
    public String lid;

    public String getLid() {
        return lid;
    }

    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * useful constructor
     */
    public FFMPVirtualGageBasin(String lid, Long pfaf, boolean aggregated) {
        setLid(lid);
        setPfaf(pfaf);
        setAggregated(aggregated);
        values = new TreeMap<Date, Float>(new Comparator<Date>() {
            @Override
            public int compare(Date o1, Date o2) {
                // Null checks?
                return (o2.before(o1) ? -1 : (o1.equals(o2) ? 0 : 1));
            }

        });

    }

    public String toString() {

        StringBuffer buff = new StringBuffer();
        buff.append("LID: " + lid + "\n");
        buff.append("PFAF ID: " + pfaf + "\n");
        buff.append("Aggregated : " + aggregated + "\n");
        for (Date date : values.keySet()) {
            buff.append("Value : " + values.get(date) + "\n");
        }
        return buff.toString();
    }

}
