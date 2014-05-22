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

public class tiff_record extends Structure {
    // / TIFF tag
    public int tag;

    // / Data type
    public int type;

    // / Length
    public int length;

    // / Pointer or value
    public int voff;

    public tiff_record() {
        super();
    }

    /**
     * @param tag
     *            TIFF tag<br>
     * @param type
     *            Data type<br>
     * @param length
     *            Length<br>
     * @param voff
     *            Pointer or value
     */
    public tiff_record(int tag, int type, int length, int voff) {
        super();
        this.tag = tag;
        this.type = type;
        this.length = length;
        this.voff = voff;
    }

    protected ByReference newByReference() {
        return new ByReference();
    }

    protected ByValue newByValue() {
        return new ByValue();
    }

    protected tiff_record newInstance() {
        return new tiff_record();
    }

    public static class ByReference extends tiff_record implements
            com.sun.jna.Structure.ByReference {
    }

    public static class ByValue extends tiff_record implements
            com.sun.jna.Structure.ByValue {
    }

    @Override
    protected List getFieldOrder() {
        return Arrays.asList(new String[] { "tag", "type", "length", "voff" });
    }

}
