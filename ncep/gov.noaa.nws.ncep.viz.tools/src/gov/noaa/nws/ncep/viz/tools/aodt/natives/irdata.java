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

public class irdata extends Structure {
    public static class ByReference extends irdata implements
            Structure.ByReference {
    }

    public int date;

    public int time;

    public float TrawO;

    public float Traw;

    public float Tfinal;

    public float Tfinal3;

    public float CI;

    public float eyet;

    public float warmt;

    public float cloudt;

    public float cloudt2;

    public float cwcloudt;

    public float latitude;

    public float longitude;

    public float warmlatitude;

    public float warmlongitude;

    public float eyesize;

    public float eyestdv;

    public float cloudsymave;

    public int sattype;

    public int eyescene;

    public int cloudscene;

    public int eyesceneold;

    public int cloudsceneold;

    public int rule9;

    public int rule8;

    public int land;

    public int eyefft;

    public int cloudfft;

    public int cwring;

    public int ringcb;

    public int ringcbval;

    public int ringcbvalmax;

    public float ringcblatmax;

    public float ringcblonmax;

    public float CIadjp;

    public float sst;

    public float TIEraw;

    public float TIEavg;

    public int TIEflag;

    public int autopos;

    public int LBflag;

    public float rmw;

    public byte[] comment = new byte[(50)];

    public irdata() {
        super();
    }

    public irdata(Pointer p) {
        useMemory(p);
    }

    protected ByReference newByReference() {
        return new ByReference();
    }

    protected ByValue newByValue() {
        return new ByValue();
    }

    protected irdata newInstance() {
        return new irdata();
    }

    // public static class ByReference extends irdata implements ByReference {}
    public static class ByValue extends irdata implements
            com.sun.jna.Structure.ByValue {
    }

    @Override
    protected List getFieldOrder() {
        return Arrays.asList(new String[] { "date", "time", "TrawO", "Traw",
                "Tfinal", "Tfinal3", "CI", "eyet", "warmt", "cloudt",
                "cloudt2", "cwcloudt", "latitude", "longitude", "warmlatitude",
                "warmlongitude", "eyesize", "eyestdv", "cloudsymave",
                "sattype", "eyescene", "cloudscene", "eyesceneold",
                "cloudsceneold", "rule9", "rule8", "land", "eyefft",
                "cloudfft", "cwring", "ringcb", "ringcbval", "ringcbvalmax",
                "ringcblatmax", "ringcblonmax", "CIadjp", "sst", "TIEraw",
                "TIEavg", "TIEflag", "autopos", "LBflag", "rmw", "comment" });
    }
}
