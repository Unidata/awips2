package gov.noaa.nws.ncep.edex.common.metparameters;

import gov.noaa.nws.ncep.edex.common.metparameters.MetParameterFactory.DeriveMethod;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.PRLibrary.InvalidValueException;

import javax.measure.quantity.Dimensionless;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#      Engineer    Description
 * ------------ ----------   ----------- --------------------------
 * Sep 26, 2014 Redmine 4318 srussell    Initial creation
 * 
 * </pre>
 * 
 * @author srussell
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SignificantWaveHeight extends AbstractMetParameter implements
        Dimensionless, ISerializableObject {

    @DynamicSerializeElement
    private static final long serialVersionUID = 1L;

    public SignificantWaveHeight() {
        super(UNIT);
    }

    @DeriveMethod
    public SignificantWaveHeight derive(WindWaveHeight w,
            PredomSwellWaveHeight p) throws InvalidValueException,
            NullPointerException {

        if (w.hasValidValue() && p.hasValidValue()) {
            Amount swh = PRLibrary.prSGHT(w, p);
            this.setValue(swh);
        } else {
            this.setValueToMissing();
        }

        return this;
    }

}