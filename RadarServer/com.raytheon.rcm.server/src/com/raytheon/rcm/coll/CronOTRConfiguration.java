package com.raytheon.rcm.coll;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
public class CronOTRConfiguration {
    @XmlElement(name="cronOTR")
    public List<CronOTR> cronOTRList = new ArrayList<CronOTR>();
}
