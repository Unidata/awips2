/*
 * Created on Jul 2, 2003
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package ohd.hseb.measurement;
/**
 * @author gobsc
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class MeasuringUnitConversionException extends Exception
{
    public MeasuringUnitConversionException(MeasuringUnit fromUnit, MeasuringUnit toUnit)
    {  
    	super("Unable to convert " + fromUnit.getName() + " to " + toUnit.getName());
    
    }
}
