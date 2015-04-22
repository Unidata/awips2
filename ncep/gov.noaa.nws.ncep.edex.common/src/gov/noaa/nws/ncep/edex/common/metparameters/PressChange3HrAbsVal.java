package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Maps to "pressChange3Hour" field of the HDF5 table/decoder, This used to be
 * the original PressChange3Hr.java/P03C, which was unsigned, but all values
 * would become positive. See TTR 923
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PressChange3HrAbsVal extends AbstractMetParameter implements javax.measure.quantity.Pressure, ISerializableObject {
    /**
         * 
         */
    private static final long serialVersionUID = 4636092028758506639L;

    public PressChange3HrAbsVal() {
        super(UNIT);
    }

}