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

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

public class odtdata extends Structure {
    public irdata IR;

    public odtdata.ByReference nextrec;

    public odtdata() {
        super();
    }

    public odtdata(Pointer p) {
        useMemory(p);
    }

    public odtdata(irdata IR, odtdata.ByReference nextrec) {
        super();
        this.IR = IR;
        this.nextrec = nextrec;
    }

    public ByReference newByReference() {
        return new ByReference();
    }

    public ByValue newByValue() {
        return new ByValue();
    }

    public odtdata newInstance() {
        return new odtdata();
    }

    public static class ByReference extends odtdata implements
            com.sun.jna.Structure.ByReference {
    }

    public static class ByValue extends odtdata implements
            com.sun.jna.Structure.ByValue {
    }

    @Override
    protected List getFieldOrder() {
        return Arrays.asList(new String[] { "IR", "nextrec" });
    }

}
