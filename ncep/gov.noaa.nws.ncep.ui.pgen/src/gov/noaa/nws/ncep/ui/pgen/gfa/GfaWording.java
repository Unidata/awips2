/*
 * gov.noaa.nws.ncep.ui.pgen.gfa.GfaWording
 * 
 * July 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.gfa;

import static gov.noaa.nws.ncep.ui.pgen.gfa.Gfa.nvl;

/**
 * GFA wording value object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/10		#223		M.Laryukhin	Initial creation
 * 
 * </pre>
 * 
 * @author M.Laryukhin
 * @version 1
 */
public class GfaWording {
	String fromCondsDvlpg = "";
	String fromCondsEndg = "";
	String genOlk = "NO";
	String condsContg = "";
	String otlkCondsDvlpg = "";
	String otlkCondsEndg = "";

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(200);
		
		sb.append("<fromCondsDvlpg>" + nvl(fromCondsDvlpg) + "</fromCondsDvlpg>\n");
		sb.append("<fromCondsEndg>" + nvl(fromCondsEndg) + "</fromCondsEndg>\n");
		sb.append("<genOlk>" + nvl(genOlk) + "</genOlk>\n");
		sb.append("<condsContg>" + nvl(condsContg) + "</condsContg>\n");
		sb.append("<otlkCondsDvlpg>" + nvl(otlkCondsDvlpg) + "</otlkCondsDvlpg>\n");
		sb.append("<otlkCondsEndg>" + nvl(otlkCondsEndg) + "</otlkCondsEndg>\n");

		return sb.toString();
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof GfaWording)){
			return false;
		}
		GfaWording w = (GfaWording) obj;
		return fromCondsDvlpg.equals(w.fromCondsDvlpg) 
				&& fromCondsEndg.equals(w.fromCondsEndg)
				&& genOlk.equals(w.genOlk) 
				&& condsContg.equals(w.condsContg)
				&& otlkCondsDvlpg.equals(w.otlkCondsDvlpg) 
				&& otlkCondsEndg.equals(w.otlkCondsEndg);
	}

	public String getFromCondsDvlpg() {
		return fromCondsDvlpg;
	}

	public String getFromCondsEndg() {
		return fromCondsEndg;
	}

	public String getGenOlk() {
		return genOlk;
	}

	public String getOtlkCondsDvlpg() {
		return otlkCondsDvlpg;
	}

	public String getOtlkCondsEndg() {
		return otlkCondsEndg;
	}

	public String getCondsContg() {
		return condsContg;
	}
}