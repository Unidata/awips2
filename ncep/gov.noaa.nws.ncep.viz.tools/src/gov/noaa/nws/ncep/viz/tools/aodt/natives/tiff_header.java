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

public class tiff_header extends Structure {
    // / Byte order
    public int order;

    // / Version
    public int version;

    // / Pointer
    public int point;

    public tiff_header() {
        super();
    }

    /**
     * @param order
     *            Byte order<br>
     * @param version
     *            Version<br>
     * @param point
     *            Pointer
     */
    public tiff_header(int order, int version, int point) {
        super();
        this.order = order;
        this.version = version;
        this.point = point;
    }

    protected ByReference newByReference() {
        return new ByReference();
    }

    protected ByValue newByValue() {
        return new ByValue();
    }

    protected tiff_header newInstance() {
        return new tiff_header();
    }

    public static class ByReference extends tiff_header implements
            com.sun.jna.Structure.ByReference {
    }

    public static class ByValue extends tiff_header implements
            com.sun.jna.Structure.ByValue {
    }

    @Override
    protected List getFieldOrder() {
        return Arrays.asList(new String[] { "order", "version", "point" });
    }

}
