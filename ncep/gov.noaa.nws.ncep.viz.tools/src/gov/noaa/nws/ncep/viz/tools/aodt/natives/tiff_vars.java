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

public class tiff_vars extends Structure {
    public int nbits;

    public int photo;

    public int unit;

    public int in_lines;

    public int in_elems;

    public int out_lines;

    public int out_elems;

    public int[] ratx = new int[(2)];

    public int[] raty = new int[(2)];

    public tiff_vars() {
        super();
    }

    public tiff_vars(int nbits, int photo, int unit, int in_lines,
            int in_elems, int out_lines, int out_elems, int ratx[], int raty[]) {
        super();
        this.nbits = nbits;
        this.photo = photo;
        this.unit = unit;
        this.in_lines = in_lines;
        this.in_elems = in_elems;
        this.out_lines = out_lines;
        this.out_elems = out_elems;
        if (ratx.length != this.ratx.length)
            throw new java.lang.IllegalArgumentException("Wrong array size !");
        this.ratx = ratx;
        if (raty.length != this.raty.length)
            throw new java.lang.IllegalArgumentException("Wrong array size !");
        this.raty = raty;
    }

    protected ByReference newByReference() {
        return new ByReference();
    }

    protected ByValue newByValue() {
        return new ByValue();
    }

    protected tiff_vars newInstance() {
        return new tiff_vars();
    }

    public static class ByReference extends tiff_vars implements
            com.sun.jna.Structure.ByReference {
    }

    public static class ByValue extends tiff_vars implements
            com.sun.jna.Structure.ByValue {
    }

    @Override
    protected List getFieldOrder() {
        return Arrays.asList(new String[] { "nbits", "photo", "unit",
                "in_lines", "in_elems", "out_lines", "out_elems", "ratx",
                "raty" });
    }

}
