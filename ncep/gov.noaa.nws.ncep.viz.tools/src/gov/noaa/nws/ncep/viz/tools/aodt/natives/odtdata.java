package gov.noaa.nws.ncep.viz.tools.aodt.natives;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

public class odtdata extends Structure{
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
     public ByReference newByReference() { return new ByReference(); }
     public ByValue newByValue() { return new ByValue(); }
     public odtdata newInstance() { return new odtdata(); }
     public static class ByReference extends odtdata implements com.sun.jna.Structure.ByReference {}
     public static class ByValue extends odtdata implements com.sun.jna.Structure.ByValue {}

}
