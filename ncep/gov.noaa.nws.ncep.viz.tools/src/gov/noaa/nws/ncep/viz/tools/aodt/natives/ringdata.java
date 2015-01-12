/**
 * 
 * 
 * This java class performs the NSHARP NsharpNative functions.
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -------      -------     --------    -----------
 * ??/??/???    ?                       Initial coding
 * 05/08/2014   3108        bkowal      Updated structure classes for the JNA upgrade.
 * 
 *
 * </pre>
 * 
 * @author ?
 * @version 1.0
 */
package gov.noaa.nws.ncep.viz.tools.aodt.natives;

import java.util.Arrays;
import java.util.List;

import com.sun.jna.Structure;

public class ringdata extends Structure {
    public float dist;

    public float angle;

    public float temp;

    public ringdata.ByReference nextrec;

    public ringdata() {
        super();
    }

    public ringdata(float dist, float angle, float temp,
            ringdata.ByReference nextrec) {
        super();
        this.dist = dist;
        this.angle = angle;
        this.temp = temp;
        this.nextrec = nextrec;
    }

    protected ByReference newByReference() {
        return new ByReference();
    }

    protected ByValue newByValue() {
        return new ByValue();
    }

    protected ringdata newInstance() {
        return new ringdata();
    }

    public static class ByReference extends ringdata implements
            com.sun.jna.Structure.ByReference {
    }

    public static class ByValue extends ringdata implements
            com.sun.jna.Structure.ByValue {
    }

    @Override
    protected List getFieldOrder() {
        return Arrays
                .asList(new String[] { "dist", "angle", "temp", "nextrec" });
    }

}
