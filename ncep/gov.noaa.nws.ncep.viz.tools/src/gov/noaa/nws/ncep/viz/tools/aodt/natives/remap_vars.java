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

import gov.noaa.nws.ncep.viz.tools.aodt.natives.AODTv64Native.aodtv64.FILE;

import com.sun.jna.Structure;

public class remap_vars extends Structure {
    // / Block size (bytes) input file
    public int in_bfw;

    // / Block size (bytes) output file
    public int out_bfw;

    // / Number of splines/line
    public int nspl;

    // / Number of splines/elem
    public int nspe;

    // / Source blocksize
    public int slb;

    // / Dest blocksize
    public int dlb;

    // / Number of corners in line
    public int ncl;

    // / Number of corners in elem
    public int nce;

    // / Input file descriptor
    public FILE in_fd;

    // / Output file descriptor
    public FILE out_fd;

    public remap_vars() {
        super();
    }

    protected ByReference newByReference() {
        return new ByReference();
    }

    protected ByValue newByValue() {
        return new ByValue();
    }

    protected remap_vars newInstance() {
        return new remap_vars();
    }

    public static class ByReference extends remap_vars implements
            com.sun.jna.Structure.ByReference {
    }

    public static class ByValue extends remap_vars implements
            com.sun.jna.Structure.ByValue {
    }

    @Override
    protected List getFieldOrder() {
        return Arrays.asList(new String[] { "in_bfw", "out_bfw", "nspl",
                "nspe", "slb", "dlb", "ncl", "nce", "in_fd", "out_fd" });
    }

}
