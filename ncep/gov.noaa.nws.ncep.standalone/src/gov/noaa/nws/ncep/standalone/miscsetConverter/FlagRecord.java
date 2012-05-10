/**
 * 
 */
package gov.noaa.nws.ncep.standalone.miscsetConverter;

/**
 * @author bhebbard
 *
 */
public class FlagRecord {

	private String flagName;
	private String enable;

	public FlagRecord (String flagName,
			           String enable) {
		this.flagName = flagName.trim();
		this.enable = enable.trim();
		if (!this.enable.equals("0") && !this.enable.equals("1")) {
			System.out.println("[ERROR:  Invalid value for FLAG '" + this.flagName + "' :  '" + this.enable + "' ]");
		}
	}
	
	/**
	 * @return the flagName
	 */
	public String getFlagName() {
		return flagName;
	}

	/**
	 * @return the enable
	 */
	public String getEnable() {
		return enable;
	}

	public FlagRecord() {
		// TODO Auto-generated constructor stub
	}
	
}
