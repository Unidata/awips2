/**
 * This code has been developed by NWS/OST to support AWIPS II
 * 
 */
package gov.noaa.nws.ost.edex.plugin.binlightning;

/**
 * BinLightningDataDecryptionException
 *
 * @author Wufeng Zhou
 *
 */
public class BinLightningDataDecryptionException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private byte[] data = null;
	
	/**
	 * 
	 */
	public BinLightningDataDecryptionException(String message) {
		super(message);
	}

	/**
	 * @param message
	 */
	public BinLightningDataDecryptionException(String message, byte[] data) {
		super(message);
		this.data = data;
	}

	/**
	 * @return the data
	 */
	public byte[] getData() {
		return data;
	}

}
