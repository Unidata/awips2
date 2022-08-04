package com.raytheon.viz.texteditor.alarmalert.util;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AAPACombined implements ISerializableObject {

    @XmlElement
    @DynamicSerializeElement
    private List<AlarmAlertProduct> aaList = null;

    @XmlElement
    @DynamicSerializeElement
    private List<AlarmAlertProduct> paList = null;

    public AAPACombined() {

    }

    /**
     * @param aaList
     *            the aaList to set
     */
    public void setAaList(List<AlarmAlertProduct> aaList) {
        this.aaList = aaList;
    }

    /**
     * @return the aaList
     */
    public List<AlarmAlertProduct> getAaList() {
        return aaList;
    }

    /**
     * @param paList
     *            the paList to set
     */
    public void setPaList(List<AlarmAlertProduct> paList) {
        this.paList = paList;
    }

    /**
     * @return the paList
     */
    public List<AlarmAlertProduct> getPaList() {
        return paList;
    }
}
