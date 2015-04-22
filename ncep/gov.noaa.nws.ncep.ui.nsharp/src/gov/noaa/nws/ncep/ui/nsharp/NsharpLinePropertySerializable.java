package gov.noaa.nws.ncep.ui.nsharp;
/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.NsharpLinePropertySerializable
 * 
 * This java class performs the surface station locator functions.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/13/2012   			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


@XmlAccessorType(XmlAccessType.NONE)
public class NsharpLinePropertySerializable {
    @XmlElement(name = "Line")
    public LinePropertyItem[] items;

    @XmlType(name = "lineItem")
    public static class LinePropertyItem {
        @XmlAttribute(name = "lineName")
        public String lineName;

        @XmlElement(name = "lineProperty")
        public NsharpLineProperty lineProperty;
    }

}
