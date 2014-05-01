package gov.noaa.nws.ncep.edex.common.metparameters;

import javax.measure.quantity.Frequency;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
/**
 * Maps to the GEMPAK parameter BVSQ
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class BruntVaisalaFreqSquared extends AbstractMetParameter implements Frequency,
ISerializableObject{
      	
	/**
	 * 
	 */
	private static final long serialVersionUID = -2945684208078186666L;

	public BruntVaisalaFreqSquared(){
		super( UNIT );
	}

//	@Override
//	protected AbstractMetParameter create() {
//		BruntVaisalaFrequencySquared b = new BruntVaisalaFrequencySquared();
//		return b;
//	}

}